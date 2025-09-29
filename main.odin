package honey

import "base:intrinsics"
@(require) import "core:fmt"
@(require) import "core:math"
import "core:math/linalg"
import "core:simd"
import "core:simd/x86"
import ray "vendor:raylib"

main :: proc() {

    // Initialize window.
    ray.InitWindow(SCREEN_W * FACTOR, SCREEN_H * FACTOR, "Honey Software Renderer")
    defer ray.CloseWindow()

    // Match monitor FPS.
    // ray.SetTargetFPS(ray.GetMonitorRefreshRate(ray.GetCurrentMonitor()))

    // Construct image.
    image := ray.GenImageColor(SCREEN_W, SCREEN_H, ray.RED)
    defer ray.UnloadImage(image)

    // Construct texture from image.
    texture := ray.LoadTextureFromImage(image)
    defer ray.UnloadTexture(texture)

    screen = {
        pixels = cast([^]Color)image.data,
        width  = cast(int)image.width,
        height = cast(int)image.height,
        min    = {0.0, 0.0},
        max    = {cast(f32)image.width, cast(f32)image.height},
    }

    mesh := Mesh(Vertex) {
        vertices = {
            Vertex{position = {-0.3, -0.5, 0}, uv = {0, 0}, color = {1.0, 0.0, 0.0, 1.0}},
            Vertex{position = {+1, -1, 0}, uv = {1, 0}, color = {0.0, 1.0, 0.0, 1.0}},
            Vertex{position = {+0.8, +0.8, 0}, uv = {1, 1}, color = {0.0, 0.0, 1.0, 1.0}},
            Vertex{position = {-1, +1, 0}, uv = {0, 1}, color = {1.0, 1.0, 1.0, 1.0}},
        },
        indices  = {0, 1, 2, 0, 2, 3},
    }

    // Main loop.
    main_loop: for !ray.WindowShouldClose() {

        // Populate image data for this frame.
        for y in 0 ..< screen.height {
            for x in 0 ..< screen.width {
                set_pixel(x, y, ray.DARKGRAY)
            }
        }

        // Render mesh.
        render_mesh(mesh)

        // Update texture with image contents.
        ray.UpdateTexture(texture, image.data)

        // Flush texture to the screen.
        ray.BeginDrawing()
        ray.DrawTextureEx(texture, {0, 0}, 0, FACTOR, ray.WHITE)
        ray.DrawFPS(10, 10)
        ray.EndDrawing()

        // Exit when pressing escape.
        if ray.IsKeyPressed(.ESCAPE) {
            break main_loop
        }
    }
}

SCREEN_W :: 960
SCREEN_H :: 540
FACTOR :: 2

Vector2 :: ray.Vector2
Vector3 :: ray.Vector3
Vector4 :: ray.Vector4
Matrix :: ray.Matrix
Color :: ray.Color

// -----------------------------------------------------------------------------

screen: struct {
    pixels:        [^]Color,
    width, height: int,
    min, max:      Vector2,
}

set_pixel :: #force_inline proc(x, y: int, color: Color) #no_bounds_check {
    screen.pixels[(y * screen.width) + x] = color
}

get_pixel :: #force_inline proc(x, y: int) -> Color #no_bounds_check {
    return get_pixel_ptr(x, y)[0]
}

get_pixel_ptr :: #force_inline proc(x, y: int) -> [^]Color #no_bounds_check {
    return screen.pixels[(y * screen.width) + x:]
}

// -----------------------------------------------------------------------------

Vertex :: struct {
    position: Vector3,
    uv:       Vector2,
    color:    Vector4,
}

Mesh :: struct($TVertex: typeid) {
    vertices: []TVertex,
    indices:  []int,
}

// FOR NOW
vertices: [dynamic]Vertex

render_mesh :: proc(mesh: Mesh(Vertex)) {

    for i := 0; i < len(mesh.indices); i += 3 {

        a := mesh.vertices[mesh.indices[i + 0]]
        b := mesh.vertices[mesh.indices[i + 1]]
        c := mesh.vertices[mesh.indices[i + 2]]

        render_triangle(a, b, c)
    }
}

render_triangle :: proc(a, b, c: Vertex) {

    // Overall software rendering algorithm:
    // - Transform vertex stream (vertex shader)
    // - Clip (primitive assembly)
    // - Fill (depth test and fragment shader)

    // Transform
    a_ndc := vertex_shader(a)
    b_ndc := vertex_shader(b)
    c_ndc := vertex_shader(c)

    // TODO: Perspective interpolation (1/w)
    // TODO: Clip?

    // 0 triangles (3 verts behind)
    // 1 triangles (3 verts in front OR 2 verts behind)
    // 2 triangles (2 verts in front)

    // parts := clip_triangle(a_ndc, b_ndc, c_ndc)
    // for a, b, c in parts {

    // Map NDC -> Viewport
    a_ndc = map_to_viewport(a_ndc)
    b_ndc = map_to_viewport(b_ndc)
    c_ndc = map_to_viewport(c_ndc)

    // TODO: Barycentric weights?

    // TODO: Fill triangle.
    // - Barycentric bbox?
    // - Scanline?

    // Render the triangle!
    rasterize_triangle({a_ndc.xy, b_ndc.xy, c_ndc.xy}, a, b, c)

    // }

    map_to_viewport :: proc(ndc: Vector4) -> Vector4 {

        pos := (ndc.xy + 1.0) / 2.0
        pos.x *= cast(f32)screen.width
        pos.y *= cast(f32)screen.height

        return {pos.x, pos.y, ndc.z, ndc.w}
    }
}

rasterize_triangle :: proc(positions: []Vector2, a, b, c: Vertex) #no_bounds_check {

    if !ray.IsKeyDown(.SPACE) {
        rasterize_triangle_simd(positions, a, b, c)
        return
    }

    assert(len(positions) == 3)

    // Find triangle bounds.
    triangle_min := linalg.min(positions[0], positions[1], positions[2])
    triangle_max := linalg.max(positions[0], positions[1], positions[2])

    // Clamp triangle bounds to screen bounds.
    triangle_min = linalg.clamp(triangle_min, screen.min, screen.max)
    triangle_max = linalg.clamp(triangle_max, screen.min, screen.max)

    for y in triangle_min.y ..< triangle_max.y {
        for x in triangle_min.x ..< triangle_max.x {

            p := Vector2{x, y}

            w0 := edge(positions[1].xy, positions[2].xy, p)
            w1 := edge(positions[2].xy, positions[0].xy, p)
            w2 := edge(positions[0].xy, positions[1].xy, p)

            wT := w0 + w1 + w2

            w0 /= wT
            w1 /= wT
            w2 /= wT

            if w0 >= 0 && w1 >= 0 && w2 >= 0 {

                // Interpolate the vertex.
                interpolated_vertex := interpolate_vertex(a, b, c, w0, w1, w2)

                // Run the 'fragment shader' for this pixel
                color := linalg.array_cast(fragment_shader(interpolated_vertex) * 0xFF, u8)
                set_pixel(cast(int)x, cast(int)y, cast(ray.Color)color)
            }
        }
    }

    edge :: proc(a, b, c: Vector2) -> f32 {
        ab, ac := a - b, a - c
        return (ac.y * ab.x) - (ac.x * ab.y)
    }
}

@(private = "file", enable_target_feature = "sse2")
rasterize_triangle_simd :: proc(positions: []Vector2, a, b, c: Vertex) #no_bounds_check {

    assert(len(positions) == 3)

    // Find triangle bounds.
    triangle_min := linalg.min(positions[0], positions[1], positions[2])
    triangle_max := linalg.max(positions[0], positions[1], positions[2])

    // Clamp triangle bounds to screen bounds.
    triangle_min = linalg.clamp(triangle_min, screen.min, screen.max)
    triangle_max = linalg.clamp(triangle_max, screen.min, screen.max)

    ab12 := positions[1].xy - positions[2].xy
    ab20 := positions[2].xy - positions[0].xy
    ab01 := positions[0].xy - positions[1].xy

    for y in triangle_min.y ..< triangle_max.y {

        py := simd.f32x4(y)

        for x := triangle_min.x; x < triangle_max.x; x += 4 {

            px := simd.f32x4{x + 0, x + 1, x + 2, x + 3}

            w0 := edge(positions[1].xy, ab12, px, py)
            w1 := edge(positions[2].xy, ab20, px, py)
            w2 := edge(positions[0].xy, ab01, px, py)

            wT := w0 + w1 + w2

            w0 /= wT
            w1 /= wT
            w2 /= wT

            write_mask := transmute(simd.u32x4)(~u128(0)) // w0 >= 0 && w1 >= 0 && w2 >= 0
            write_mask = simd.bit_and(write_mask, simd.lanes_ge(w0, simd.f32x4(0)))
            write_mask = simd.bit_and(write_mask, simd.lanes_ge(w1, simd.f32x4(0)))
            write_mask = simd.bit_and(write_mask, simd.lanes_ge(w2, simd.f32x4(0)))

            // TODO: Interpolate the vertex.
            // TODO: Run the 'fragment shader' for this pixel
            R := interpolate_barycentric_simd(a.color.r, b.color.r, c.color.r, w0, w1, w2)
            G := interpolate_barycentric_simd(a.color.g, b.color.g, c.color.g, w0, w1, w2)
            B := interpolate_barycentric_simd(a.color.b, b.color.b, c.color.b, w0, w1, w2)
            A := interpolate_barycentric_simd(a.color.a, b.color.a, c.color.a, w0, w1, w2)

            // TODO: simd_pack_color(color_f32) -> color_u32 ?
            Ri := transmute(simd.u32x4)x86._mm_cvtps_epi32(R * 0xFF)
            Gi := simd.shl(transmute(simd.u32x4)x86._mm_cvtps_epi32(G * 0xFF), 8)
            Bi := simd.shl(transmute(simd.u32x4)x86._mm_cvtps_epi32(B * 0xFF), 16)
            Ai := simd.shl(transmute(simd.u32x4)x86._mm_cvtps_epi32(A * 0xFF), 24)
            pixels := simd.bit_or(Ri, simd.bit_or(Gi, simd.bit_or(Bi, Ai)))

            // Write rasterized pixel into image.
            pixel_ptr := cast(^simd.u32x4)get_pixel_ptr(cast(int)x, cast(int)y)
            pixel_ptr^ = simd.bit_or(
                simd.bit_and(pixels, write_mask), //
                simd.bit_and_not(pixel_ptr^, write_mask),
            )
        }
    }

    edge :: proc(a, ab: Vector2, cx, cy: simd.f32x4) -> simd.f32x4 {
        return ((simd.f32x4(a.y) - cy) * simd.f32x4(ab.x)) - ((simd.f32x4(a.x) - cx) * simd.f32x4(ab.y))
    }

    interpolate_barycentric_simd :: proc(a, b, c, w0, w1, w2: simd.f32x4) -> simd.f32x4 {
        return (a * w0) + (b * w1) + (c * w2)
    }
}

interpolate_barycentric :: proc(a, b, c: $T, w0, w1, w2: f32) -> T {
    return (a * w0) + (b * w1) + (c * w2)
}

interpolate_vertex :: proc(a, b, c: Vertex, w0, w1, w2: f32) -> (out: Vertex) {
    // out.position = interpolate_barycentric(a.position, b.position, c.position, w0, w1, w2)
    // out.uv = interpolate_barycentric(a.uv, b.uv, c.uv, w0, w1, w2)
    out.color = interpolate_barycentric(a.color, b.color, c.color, w0, w1, w2)
    return
}

vertex_shader :: proc(vertex: Vertex) -> Vector4 {
    return {vertex.position.x, vertex.position.y, vertex.position.z, 1.0}
}

fragment_shader :: proc(vertex: Vertex) -> Vector4 {
    return vertex.color
}
