package honey

import "base:intrinsics"
@(require) import "core:fmt"
@(require) import "core:math"
import "core:math/linalg"
import "core:mem"
import "core:simd"

main :: proc() {

    fmt.printfln("has-hardware-simd: {}", simd.HAS_HARDWARE_SIMD)

    // Initialize window.
    initialize_window(SCREEN_W * FACTOR, SCREEN_H * FACTOR, "Honey Software Renderer")
    defer destroy_window()

    // ...
    assets.image1 = image_load("kenney.png")
    assets.image2 = image_load("kenney2.png")

    w := cast(f32)assets.image1.size.x / 2.0
    h := cast(f32)assets.image1.size.y / 2.0

    mesh := Mesh(Vertex) {
        vertices = {
            Vertex{position = {-w, -h, 0}, uv = {0, 0}, color = {1.0, 0.0, 0.0, 1.0}},
            Vertex{position = {+w, -h, 0}, uv = {1, 0}, color = {0.0, 1.0, 0.0, 1.0}},
            Vertex{position = {+w, +h, 0}, uv = {1, 1}, color = {0.0, 0.0, 1.0, 1.0}},
            Vertex{position = {-w, +h, 0}, uv = {0, 1}, color = {1.0, 1.0, 1.0, 1.0}},
        },
        indices  = {0, 1, 2, 0, 2, 3},
    }

    // Main loop.
    main_loop: for is_window_open() {

        // Populate image data for this frame.
        image_clear(screen, transmute(Color)(u32(0xFF181818)))

        wiggle := (math.sin(get_time()) + 1.0) / 2.0

        // Render mesh.
        transform :=
            linalg.matrix_ortho3d_f32(0, cast(f32)screen.size.x, 0, cast(f32)screen.size.y, -1, 1) *
            linalg.matrix4_translate_f32({w, h, 0}) *
            linalg.matrix4_scale_f32({0.66, 0.66, 0.66}) *
            linalg.matrix4_rotate_f32(wiggle, {0.0, 0.0, 1.0})

        render_mesh(mesh, assets.image1, transform)

        status := fmt.tprintf("use-simd: {}", toggles.use_simd)
        update_screen_pixels(status)

        // Exit when pressing escape.
        if is_key_pressed(.ESCAPE) {
            break main_loop
        }

        // Toggle SIMD code paths.
        if is_key_pressed(.Z) {
            toggles.use_simd = !toggles.use_simd
        }
    }
}

SCREEN_W :: 960
SCREEN_H :: 540
FACTOR :: 2

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

render_mesh :: proc(mesh: Mesh(Vertex), image: Image, transform: Matrix) {

    for i := 0; i < len(mesh.indices); i += 3 {

        a := mesh.vertices[mesh.indices[i + 0]]
        b := mesh.vertices[mesh.indices[i + 1]]
        c := mesh.vertices[mesh.indices[i + 2]]

        render_triangle(a, b, c, image, transform)
    }
}

render_triangle :: proc(a, b, c: Vertex, image: Image, transform: Matrix) {

    // Overall software rendering algorithm:
    // - Transform vertex stream (vertex shader)
    // - Clip (primitive assembly)
    // - Fill (depth test and fragment shader)

    // Transform
    a_ndc := vertex_shader(a, transform)
    b_ndc := vertex_shader(b, transform)
    c_ndc := vertex_shader(c, transform)

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
        rasterize_triangle_simd(a_ndc.xy, b_ndc.xy, c_ndc.xy, a, b, c, image)
    } else {
        rasterize_triangle(a_ndc.xy, b_ndc.xy, c_ndc.xy, a, b, c, image)
    }

    // }

    map_to_viewport :: proc(ndc: Vector4) -> Vector4 {

        pos := (ndc.xy + 1.0) / 2.0
        pos.x *= cast(f32)screen.size.x
        pos.y *= cast(f32)screen.size.y

        return {pos.x, pos.y, ndc.z, ndc.w}
    }

    vertex_shader :: proc(vertex: Vertex, transform: Matrix) -> Vector4 {
        return transform * Vector4{vertex.position.x, vertex.position.y, vertex.position.z, 1.0}
    }
}

rasterize_triangle :: proc(v0, v1, v2: Vector2, a, b, c: Vertex, image: Image) #no_bounds_check {

    screen_size := linalg.array_cast(screen.size, f32)

    // Find triangle bounds.
    triangle_min := linalg.min(v0, v1, v2)
    triangle_max := linalg.max(v0, v1, v2)

    // Clamp triangle bounds to screen bounds.
    triangle_min = linalg.clamp(triangle_min, 0, screen_size)
    triangle_max = linalg.clamp(triangle_max, 0, screen_size)

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
    screen_size := linalg.array_cast(screen.size, f32)
    triangle_min = linalg.clamp(triangle_min, 0, screen_size)
    triangle_max = linalg.clamp(triangle_max, 0, screen_size)

    // SIMD reads and writes need to be 16 byte aligned.
    align_min_x := cast(f32)mem.align_backward_int(cast(int)triangle_min.x, 4)
    align_max_x := cast(f32)mem.align_forward_int(cast(int)triangle_max.x, 4)

    ab12 := v1.xy - v2.xy
    ab20 := v2.xy - v0.xy
    ab01 := v0.xy - v1.xy

    for y in triangle_min.y ..< triangle_max.y {

        py := cast(simd.f32x4)y

        for x := align_min_x; x < align_max_x; x += 4 {

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
