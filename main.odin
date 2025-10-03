package honey

import "base:intrinsics"
@(require) import "core:fmt"
@(require) import "core:math"
import "core:math/linalg"
import "core:mem"
import "core:simd"
import "core:slice"

Sun_Direction := linalg.normalize(Vector3{1, 3, -2})

main :: proc() {

    fmt.printfln("has-hardware-simd: {}", simd.HAS_HARDWARE_SIMD)

    // Initialize window.
    initialize_window(SCREEN_W * FACTOR, SCREEN_H * FACTOR, "Honey Software Renderer")
    defer destroy_window()

    // ...
    assets.image1 = image_load("kenney.png")
    assets.image2 = image_load("kenney2.png")

    // ...
    teapot := parse_wavefront_mesh(#load("assets/teapot.obj", string), true)

    // Main loop.
    main_loop: for is_window_open() {

        // Populate image data for this frame.
        image_clear(screen, transmute(Color)(u32(0xFF181818)))
        slice.fill(screen_depth, 1.0) // clear depth buffer

        time: f32 = get_time()
        camera_position := Vector3{math.cos_f32(time / 3), 0, math.sin_f32(time / 5)} * 110.8
        camera_aspect := cast(f32)screen.size.x / cast(f32)screen.size.y
        camera_matrix :=
            linalg.matrix4_perspective_f32(math.PI / 2, camera_aspect, 0.1, 200.0) *
            linalg.matrix4_look_at_f32(camera_position, {0, 1.0, 0}, {0, 1, 0})

        render_mesh(teapot, assets.image1, camera_matrix)

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
    normal:   Vector3,
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
        b := mesh.vertices[mesh.indices[i + 2]]
        c := mesh.vertices[mesh.indices[i + 1]]

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

    // Map NDC -> Viewport in XY
    a_ndc = map_to_viewport(a_ndc)
    b_ndc = map_to_viewport(b_ndc)
    c_ndc = map_to_viewport(c_ndc)

    // TODO: Barycentric weights?

    // TODO: Fill triangle.
    // - Barycentric bbox?
    // - Scanline?

    // Render the triangle!
    if toggles.use_simd {
        rasterize_triangle_simd(a_ndc, b_ndc, c_ndc, a, b, c, image)
    } else {
        rasterize_triangle(a_ndc, b_ndc, c_ndc, a, b, c, image)
    }

    // }

    map_to_viewport :: proc(ndc: Vector4) -> Vector4 {

        pos := ((Vector2{ndc.x, -ndc.y} / ndc.w) + 1.0) / 2.0
        pos.x *= cast(f32)screen.size.x
        pos.y *= cast(f32)screen.size.y

        return {pos.x, pos.y, ndc.z, ndc.w}
    }

    vertex_shader :: proc(vertex: Vertex, transform: Matrix) -> Vector4 {
        return transform * Vector4{vertex.position.x, vertex.position.y, vertex.position.z, 1.0}
    }
}

rasterize_triangle :: proc(v0, v1, v2: Vector4, a, b, c: Vertex, image: Image) #no_bounds_check {

    screen_size := linalg.array_cast(screen.size, f32)

    // Find triangle bounds clamped to screen.
    triangle_min := linalg.clamp(linalg.min(v0.xy, v1.xy, v2.xy), 0, screen_size)
    triangle_max := linalg.clamp(linalg.max(v0.xy, v1.xy, v2.xy), 0, screen_size)

    ab12 := v1.xy - v2.xy
    ab20 := v2.xy - v0.xy
    ab01 := v0.xy - v1.xy

    for y in math.floor(triangle_min.y) ..< math.ceil(triangle_max.y) {
        for x in math.floor(triangle_min.x) ..< math.ceil(triangle_max.x) {

            p := Vector2{x, y}

            w0 := edge(v1.xy, ab12, p)
            w1 := edge(v2.xy, ab20, p)
            w2 := edge(v0.xy, ab01, p)

            if w0 < 0 || w1 < 0 || w2 < 0 {
                continue
            }

            wT := 1.0 / (w0 + w1 + w2)

            w0 *= wT
            w1 *= wT
            w2 *= wT

            ndc := interpolate(v0, v1, v2, w0, w1, w2)
            depth := ndc.z / ndc.w

            // ---- DEPTH TEST ----

            depth_ptr := raw_data(screen_depth)[(cast(int)y * screen.size.x) + cast(int)x:]
            if depth > depth_ptr[0] {
                continue // fragment hidden
            }

            // ---- INTERPOLATOR ---

            fragment_normal := linalg.normalize(interpolate(a.normal, b.normal, c.normal, w0, w1, w2))
            fragment_uv := interpolate(a.uv, b.uv, c.uv, w0, w1, w2)

            // ---- FRAGMENT SHADER BEGIN ---

            pixel := image_sample(image, fragment_uv)

            brightness := max(0.0, linalg.dot(fragment_normal, Sun_Direction))
            pixel = mix_color(0, pixel, 0.25 + (brightness * 0.75))

            // ---- FRAGMENT SHADER END ---

            pixel.a = 0xFF // no transparency

            // Write color
            image_set_pixel(screen, cast(int)x, cast(int)y, pixel)

            // Write depth
            depth_ptr[0] = depth
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
rasterize_triangle_simd :: proc(v0, v1, v2: Vector4, a, b, c: Vertex, image: Image) #no_bounds_check {

    screen_size := linalg.array_cast(screen.size, f32)

    // Find triangle bounds clamped to screen.
    triangle_min := linalg.clamp(linalg.min(v0.xy, v1.xy, v2.xy), 0, screen_size)
    triangle_max := linalg.clamp(linalg.max(v0.xy, v1.xy, v2.xy), 0, screen_size)

    // SIMD reads and writes need to be 16 byte aligned.
    align_min_x := cast(f32)mem.align_backward_int(cast(int)math.floor(triangle_min.x), 4)
    align_max_x := cast(f32)mem.align_forward_int(cast(int)math.ceil(triangle_max.x), 4)

    ab12 := v1.xy - v2.xy
    ab20 := v2.xy - v0.xy
    ab01 := v0.xy - v1.xy

    for y in math.floor(triangle_min.y) ..< math.ceil(triangle_max.y) {

        py := cast((#simd[4]f32))y

        for x := align_min_x; x < align_max_x; x += 4 {

            px: (#simd[4]f32) = {x + 0, x + 1, x + 2, x + 3}

            w0 := edge(v1.xy, ab12, px, py)
            w1 := edge(v2.xy, ab20, px, py)
            w2 := edge(v0.xy, ab01, px, py)

            write_mask := cast(#simd[4]u32)(0) // w0 >= 0 && w1 >= 0 && w2 >= 0
            write_mask = ~write_mask & simd.lanes_ge(w0, (#simd[4]f32)(0))
            write_mask = write_mask & simd.lanes_ge(w1, (#simd[4]f32)(0))
            write_mask = write_mask & simd.lanes_ge(w2, (#simd[4]f32)(0))

            if simd.reduce_or(write_mask) == 0 {
                continue // all fragments are outside the triangle
            }

            wT := 1.0 / (w0 + w1 + w2)

            w0 *= wT
            w1 *= wT
            w2 *= wT

            ndc_z := interpolate(v0.z, v1.z, v2.z, w0, w1, w2)
            ndc_w := interpolate(v0.w, v1.w, v2.w, w0, w1, w2)
            depth := ndc_z / ndc_w

            // ---- DEPTH TEST ----

            depth_ptr := cast(^#simd[4]f32)raw_data(screen_depth)[(cast(int)y * screen.size.x) + cast(int)x:]
            write_mask = write_mask & simd.lanes_lt(depth, depth_ptr^)

            if simd.reduce_or(write_mask) == 0 {
                continue // all fragments are hidden
            }

            // ---- INTERPOLATOR ---

            NX := interpolate(a.normal.x, b.normal.x, c.normal.x, w0, w1, w2)
            NY := interpolate(a.normal.y, b.normal.y, c.normal.y, w0, w1, w2)
            NZ := interpolate(a.normal.z, b.normal.z, c.normal.z, w0, w1, w2)

            U := interpolate(a.uv.x, b.uv.x, c.uv.x, w0, w1, w2)
            V := interpolate(a.uv.y, b.uv.y, c.uv.y, w0, w1, w2)

            // ---- FRAGMENT SHADER BEGIN ---

            R, G, B, A := convert_u32_to_f32_simd(image_sample_simd(image, U, V))

            brightness := 0.25 + (simd.max((#simd[4]f32)(0), dot(NX, NY, NZ, Sun_Direction)) * 0.75)

            R *= brightness
            G *= brightness
            B *= brightness

            pixels := convert_f32_to_u32_simd(R, G, B, A)

            // ---- FRAGMENT SHADER END ---

            // Write color.
            pixel_ptr := cast(^#simd[4]u32)image_get_pixel_ptr(screen, cast(int)x, cast(int)y)
            pixel_ptr^ = (pixels & write_mask) | (pixel_ptr^ &~ write_mask)

            // Write depth.
            depth_ptr^ = transmute(#simd[4]f32)((transmute(#simd[4]u32)depth & write_mask) |
                (transmute(#simd[4]u32)(depth_ptr^) &~ write_mask))
        }
    }

    edge :: #force_inline proc(a, ab: Vector2, cx, cy: #simd[4]f32) -> #simd[4]f32 {
        return ((a.y - cy) * ab.x) - ((a.x - cx) * ab.y)
    }

    interpolate :: proc(a, b, c, w0, w1, w2: #simd[4]f32) -> #simd[4]f32 {
        return (a * w0) + (b * w1) + (c * w2)
    }

    dot :: proc(ax, ay, az: #simd[4]f32, b: Vector3) -> #simd[4]f32 {
        return interpolate(ax, ay, az, b.x, b.y, b.z)
    }

    convert_f32_to_u32_simd :: proc(R, G, B, A: #simd[4]f32) -> #simd[4]u32 {
        Ri := cast(#simd[4]u32)(R * 0xFF)
        Gi := simd.shl(cast(#simd[4]u32)(G * 0xFF), 8)
        Bi := simd.shl(cast(#simd[4]u32)(B * 0xFF), 16)
        Ai := simd.shl(cast(#simd[4]u32)(A * 0xFF), 24)
        return Ri | Gi | Bi | Ai
    }

    convert_u32_to_f32_simd :: proc(color: #simd[4]u32) -> (R, G, B, A: #simd[4]f32) {
        R = (cast(#simd[4]f32)(color & 0xFF)) / 0xFF
        G = (cast(#simd[4]f32)(simd.shr(color, 8) & 0xFF)) / 0xFF
        B = (cast(#simd[4]f32)(simd.shr(color, 16) & 0xFF)) / 0xFF
        A = (cast(#simd[4]f32)(simd.shr(color, 24) & 0xFF)) / 0xFF
        return
    }
}
