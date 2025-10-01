package honey

import "base:intrinsics"
@(require) import "core:fmt"
@(require) import "core:math"
import "core:math/linalg"
import "core:mem"
import "core:simd"
import "core:simd/x86"
import ray "vendor:raylib"

main :: proc() {

    fmt.printfln("has-hardware-simd: {}", simd.HAS_HARDWARE_SIMD)
    fmt.println("--------")

    // Initialize window.
    ray.InitWindow(SCREEN_W * FACTOR, SCREEN_H * FACTOR, "Honey Software Renderer")
    defer ray.CloseWindow()

    // Match monitor FPS.
    // ray.SetTargetFPS(ray.GetMonitorRefreshRate(ray.GetCurrentMonitor()))

    // Construct image.
    screen_image := ray.GenImageColor(SCREEN_W, SCREEN_H, ray.RED)
    defer ray.UnloadImage(screen_image)

    // Construct texture from image.
    screen_texture := ray.LoadTextureFromImage(screen_image)
    defer ray.UnloadTexture(screen_texture)

    screen_w := cast(int)screen_image.width
    screen_h := cast(int)screen_image.height

    screen = {
        width = screen_w,
        height = screen_h,
        data = (cast([^]Color)screen_image.data)[0:screen_w * screen_h],
        simd = {     //
            data = cast(#simd[4]uintptr)screen_image.data,
        },
        min = {0.0, 0.0},
        max = {cast(f32)screen_image.width, cast(f32)screen_image.height},
    }

    // ...
    assets.image1 = image_load("kenney.png")
    assets.image2 = image_load("kenney2.png")

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
                image_set_pixel(screen, x, y, ray.DARKGRAY)
            }
        }

        // Render mesh.
        render_mesh(mesh)

        // Update texture with image contents.
        ray.UpdateTexture(screen_texture, raw_data(screen.data))

        // Flush texture to the screen.
        ray.BeginDrawing()
        ray.DrawTextureEx(screen_texture, {0, 0}, 0, FACTOR, ray.WHITE)
        ray.DrawText(
            fmt.ctprintf(
                "{:0.2f} ms ({} fps), use-simd: {}",
                1000.0 / cast(f32)ray.GetFPS(),
                ray.GetFPS(),
                toggles.use_simd,
            ),
            10,
            10,
            20,
            ray.WHITE,
        )
        ray.EndDrawing()

        // Exit when pressing escape.
        if ray.IsKeyPressed(.ESCAPE) {
            break main_loop
        }

        // Toggle SIMD code paths.
        if ray.IsKeyPressed(.Z) {
            toggles.use_simd = !toggles.use_simd
        }
    }
}

SCREEN_W :: 960
SCREEN_H :: 540
FACTOR :: 2

screen: struct {
    using _:  Image,
    min, max: Vector2,
}

assets: struct {
    image1: Image,
    image2: Image,
}

toggles: struct {
    use_simd: bool,
} = {
    use_simd = true,
}

// -----------------------------------------------------------------------------

Vector2 :: ray.Vector2
Vector3 :: ray.Vector3
Vector4 :: ray.Vector4
Matrix :: ray.Matrix

// -----------------------------------------------------------------------------

Color :: ray.Color

color_mix :: ray.ColorLerp

// -----------------------------------------------------------------------------

Image :: struct {
    data:          []Color,
    width, height: int,
    simd:          struct {
        data: #simd[4]uintptr,
    },
}

image_load :: proc(path: cstring) -> (image: Image) {

    ray_image := ray.LoadImage(path)
    defer ray.UnloadImage(ray_image)

    aligned_memory, err := mem.make_aligned([]Color, ray_image.width * ray_image.height, 16)
    if err != .None {
        fmt.panicf("Unable to allocate aligned image memory: {}", err)
    }

    copy(aligned_memory, (cast([^]Color)ray_image.data)[:len(aligned_memory)])

    image = {
        data = aligned_memory,
        width = cast(int)ray_image.width,
        height = cast(int)ray_image.height,
        simd = {     //
            data = cast(uintptr)raw_data(aligned_memory),
        },
    }

    return
}

image_set_pixel :: #force_inline proc(image: Image, x, y: int, color: Color) #no_bounds_check {
    image_get_pixel_ptr(image, x, y)[0] = color
}

image_get_pixel :: #force_inline proc(image: Image, x, y: int) -> Color #no_bounds_check {
    return image_get_pixel_ptr(image, x, y)[0]
}

image_get_pixel_ptr :: #force_inline proc(image: Image, x, y: int) -> [^]Color #no_bounds_check {
    return raw_data(image.data)[(y * image.width) + x:]
}

// Nearest sample an image.
image_sample :: proc(image: Image, uv: [2]f32) -> Color #no_bounds_check {

    x := cast(int)(uv.x * cast(f32)(image.width - 1))
    y := cast(int)(uv.y * cast(f32)(image.height - 1))

    return image_get_pixel(image, x, y)
}

// Nearest sample an image using SIMD operations.
image_sample_simd :: proc(image: Image, U, V: #simd[4]f32) -> #simd[4]u32 #no_bounds_check {

    U, V := U, V

    U = simd.clamp(U, 0, 1)
    V = simd.clamp(V, 0, 1)

    // Computes the offset within the image data.
    xs := cast(#simd[4]i32)(U * cast(f32)(image.width - 1))
    ys := cast(#simd[4]i32)(V * cast(f32)(image.height - 1))
    offsets := (ys * cast(i32)image.width) + xs

    // Read the requested image data.
    addrs := image.simd.data + cast(#simd[4]uintptr)(offsets * size_of(Color))
    return simd.gather(cast(#simd[4]rawptr)addrs, simd.u32x4(0), simd.u32x4(1))
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
    if toggles.use_simd {
        rasterize_triangle_simd(a_ndc.xy, b_ndc.xy, c_ndc.xy, a, b, c, assets.image1)
    } else {
        rasterize_triangle(a_ndc.xy, b_ndc.xy, c_ndc.xy, a, b, c, assets.image2)
    }

    // }

    map_to_viewport :: proc(ndc: Vector4) -> Vector4 {

        pos := (ndc.xy + 1.0) / 2.0
        pos.x *= cast(f32)screen.width
        pos.y *= cast(f32)screen.height

        return {pos.x, pos.y, ndc.z, ndc.w}
    }
}

rasterize_triangle :: proc(v0, v1, v2: Vector2, a, b, c: Vertex, image: Image) #no_bounds_check {

    // Find triangle bounds.
    triangle_min := linalg.min(v0, v1, v2)
    triangle_max := linalg.max(v0, v1, v2)

    // Clamp triangle bounds to screen bounds.
    triangle_min = linalg.clamp(triangle_min, screen.min, screen.max)
    triangle_max = linalg.clamp(triangle_max, screen.min, screen.max)

    ab12 := v1.xy - v2.xy
    ab20 := v2.xy - v0.xy
    ab01 := v0.xy - v1.xy

    for y in triangle_min.y ..< triangle_max.y {
        for x in triangle_min.x ..< triangle_max.x {

            p := Vector2{x, y}

            w0 := edge(v1.xy, ab12, p)
            w1 := edge(v2.xy, ab20, p)
            w2 := edge(v0.xy, ab01, p)

            if w0 < 0 || w1 < 0 || w2 < 0 {
                continue
            }

            wT := w0 + w1 + w2

            w0 /= wT
            w1 /= wT
            w2 /= wT

            // ---- FRAGMENT SHADER BEGIN ---

            fragment_uv := interpolate(a.uv, b.uv, c.uv, w0, w1, w2)
            color := image_sample(image, fragment_uv)

            // ---- FRAGMENT SHADER END ---

            image_set_pixel(screen, cast(int)x, cast(int)y, color)
        }
    }

    edge :: #force_inline proc(a, ab, c: Vector2) -> f32 {
        return ((a.y - c.y) * ab.x) - ((a.x - c.x) * ab.y)
    }

    interpolate :: proc(a, b, c: $T, w0, w1, w2: f32) -> T {
        return (a * w0) + (b * w1) + (c * w2)
    }
}

@(private = "file")
rasterize_triangle_simd :: proc(v0, v1, v2: Vector2, a, b, c: Vertex, image: Image) #no_bounds_check {

    // Find triangle bounds.
    triangle_min := linalg.min(v0, v1, v2)
    triangle_max := linalg.max(v0, v1, v2)

    // Clamp triangle bounds to screen bounds.
    triangle_min = linalg.clamp(triangle_min, screen.min, screen.max)
    triangle_max = linalg.clamp(triangle_max, screen.min, screen.max)

    ab12 := v1.xy - v2.xy
    ab20 := v2.xy - v0.xy
    ab01 := v0.xy - v1.xy

    for y in triangle_min.y ..< triangle_max.y {

        py := cast(simd.f32x4)y

        for x := triangle_min.x; x < triangle_max.x; x += 4 {

            px: simd.f32x4 = {x + 0, x + 1, x + 2, x + 3}

            w0 := edge(v1.xy, ab12, px, py)
            w1 := edge(v2.xy, ab20, px, py)
            w2 := edge(v0.xy, ab01, px, py)

            write_mask := transmute(simd.u32x4)(~u128(0)) // w0 >= 0 && w1 >= 0 && w2 >= 0
            write_mask = write_mask & simd.lanes_ge(w0, simd.f32x4(0))
            write_mask = write_mask & simd.lanes_ge(w1, simd.f32x4(0))
            write_mask = write_mask & simd.lanes_ge(w2, simd.f32x4(0))

            if simd.reduce_or(write_mask) != 0 {

                wT := w0 + w1 + w2

                w0 /= wT
                w1 /= wT
                w2 /= wT

                // ---- FRAGMENT SHADER BEGIN ---

                U := interpolate(a.uv.x, b.uv.x, c.uv.x, w0, w1, w2)
                V := interpolate(a.uv.y, b.uv.y, c.uv.y, w0, w1, w2)

                pixels := image_sample_simd(image, U, V)

                // ---- FRAGMENT SHADER END ---

                // Write rasterized pixel into image.
                pixel_ptr := cast(^simd.u32x4)image_get_pixel_ptr(screen, cast(int)x, cast(int)y)
                pixel_ptr^ = (pixels & write_mask) | (pixel_ptr^ &~ write_mask)
            }
        }
    }

    edge :: #force_inline proc(a, ab: Vector2, cx, cy: simd.f32x4) -> simd.f32x4 {
        return ((a.y - cy) * ab.x) - ((a.x - cx) * ab.y)
    }

    interpolate :: proc(a, b, c, w0, w1, w2: simd.f32x4) -> simd.f32x4 {
        return (a * w0) + (b * w1) + (c * w2)
    }

    // @(enable_target_feature = "sse2")
    // convert_f32_to_u32_simd :: proc(R, G, B, A: simd.f32x4) -> simd.u32x4 {
    //     Ri := transmute(simd.u32x4)x86._mm_cvtps_epi32(R * 0xFF)
    //     Gi := simd.shl(transmute(simd.u32x4)x86._mm_cvtps_epi32(G * 0xFF), 8)
    //     Bi := simd.shl(transmute(simd.u32x4)x86._mm_cvtps_epi32(B * 0xFF), 16)
    //     Ai := simd.shl(transmute(simd.u32x4)x86._mm_cvtps_epi32(A * 0xFF), 24)
    //     return simd.bit_or(Ri, simd.bit_or(Gi, simd.bit_or(Bi, Ai)))
    // }
}

vertex_shader :: proc(vertex: Vertex) -> Vector4 {
    return {vertex.position.x, vertex.position.y, vertex.position.z, 1.0}
}

fragment_shader :: proc(vertex: Vertex) -> Vector4 {
    return vertex.color
}
