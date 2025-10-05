package honey

import "base:intrinsics"
@(require) import "core:fmt"
@(require) import "core:math"
import "core:math/linalg"
import "core:mem"
import "core:simd"

Sun_Direction := linalg.normalize(Vector3{1, 3, -2})

latest_time: f32
time: f32

main :: proc() {

    // Initialize window.
    initialize_window(SCREEN_W * FACTOR, SCREEN_H * FACTOR, "Honey Software Renderer")
    defer destroy_window()

    // ...
    assets.image = image_load("assets/Nexo_Texture.png")

    // ...
    mesh := parse_wavefront_mesh(#load("assets/NexoITCH.obj", string), flip_uv = true)

    // Main loop.
    main_loop: for is_window_open() {

        // Populate image data for this frame.
        image_clear(screen, transmute(Color)(u32(0xFF181818)))
        mem.zero_slice(screen_depth) // zero clear, reversed-z

        time += toggles.pause ? 0 : get_time() - latest_time
        latest_time = get_time()

        camera_position := Vector3{math.cos_f32(time / 3), 2.5, math.sin_f32(time / 5)} * 3
        camera_aspect := cast(f32)screen.size.x / cast(f32)screen.size.y
        camera_matrix :=
            linalg.matrix4_perspective_f32(math.PI / 2, camera_aspect, 0.1, 200.0) *
            linalg.matrix4_look_at_f32(camera_position, {0, 7.0, 0}, {0, 1, 0})

        for y: f32 = 0.0; y < 30.0; y += 5.0 {
            for x: f32 = 0.0; x < 30.0; x += 5.0 {

                transform := linalg.matrix4_translate_f32({x, 0, y})

                render_mesh(mesh, assets.image, camera_matrix * transform)
            }
        }

        status := fmt.tprintf(
            "(Q) pause: {}\n(E) backface: {}\n(Z) use simd: {}\n(C) use integer: {}",
            toggles.pause,
            toggles.backface,
            toggles.use_simd,
            toggles.use_integer,
        )
        update_screen_pixels(status)

        // Exit when pressing escape.
        if is_key_pressed(.ESCAPE) {
            break main_loop
        }

        // Pause
        if is_key_pressed(.Q) {
            toggles.pause = !toggles.pause
        }

        // Backface culling
        if is_key_pressed(.E) {
            toggles.backface = !toggles.backface
        }

        // Toggle SIMD code paths.
        if is_key_pressed(.Z) {
            toggles.use_simd = !toggles.use_simd
        }

        // Toggle integer precision computations.
        if is_key_pressed(.C) {
            toggles.use_integer = !toggles.use_integer
        }
    }
}

SCREEN_W :: 800
SCREEN_H :: 500
FACTOR :: 2

assets: struct {
    image: Image,
}

toggles: struct {
    use_simd:    bool,
    use_integer: bool,
    backface:    bool,
    pause:       bool,
} = {
    use_simd    = true,
    use_integer = true,
    backface    = true,
    pause       = true,
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

    // Reference:
    // - https://github.com/kristoffer-dyrkorn/triangle-rasterizer/blob/main/9/triangle.js
    // - https://fgiesen.wordpress.com/2013/02/10/optimizing-the-basic-rasterizer/
    // - https://jtsorlinis.github.io/rendering-tutorial/
    // - https://github.com/gustavopezzi/triangle-rasterizer-int

    // Transform
    a_ndc := vertex_shader(a, transform)
    b_ndc := vertex_shader(b, transform)
    c_ndc := vertex_shader(c, transform)

    // Homogenize (perspective divide)
    a_ndc.xy = a_ndc.xy / a_ndc.w
    b_ndc.xy = b_ndc.xy / b_ndc.w
    c_ndc.xy = c_ndc.xy / c_ndc.w

    // Backface culling (counter-clockwise)
    if toggles.backface && signed_area(a_ndc.xy, b_ndc.xy, c_ndc.xy) < 0 {
        return
    }

    // TODO: Frustum plane clipping (near plane clipping at minimum)

    // 0 triangles (3 verts behind)
    // 1 triangles (3 verts in front OR 2 verts behind)
    // 2 triangles (2 verts in front)

    // parts := clip_triangle(a_ndc, b_ndc, c_ndc)
    // for a, b, c in parts {

    // Map NDC -> Viewport in XY
    a_ndc = map_to_viewport(a_ndc)
    b_ndc = map_to_viewport(b_ndc)
    c_ndc = map_to_viewport(c_ndc)

    // Find triangle bounds...
    triangle_min := linalg.array_cast(linalg.floor(linalg.min(a_ndc.xy, b_ndc.xy, c_ndc.xy)), int)
    triangle_max := linalg.array_cast(linalg.ceil(linalg.max(a_ndc.xy, b_ndc.xy, c_ndc.xy)), int)

    // ... clamp to screen.
    triangle_min = linalg.clamp(triangle_min, 0, screen.size)
    triangle_max = linalg.clamp(triangle_max, 0, screen.size)

    // TILE_SIZE :: 256 // power-of-2 and multiple of 16?

    // x_min := mem.align_backward_int(triangle_min.x, TILE_SIZE)
    // y_min := mem.align_backward_int(triangle_min.y, TILE_SIZE)
    // x_max := mem.align_forward_int(triangle_max.x, TILE_SIZE)
    // y_max := mem.align_forward_int(triangle_max.y, TILE_SIZE)

    // for y := y_min; y < y_max; y += TILE_SIZE {
    //     for x := x_min; x < x_max; x += TILE_SIZE {

    // tile_min := [2]int{x, y}
    // tile_max := linalg.min(tile_min + TILE_SIZE, screen.size)

    // ...clamped to screen.
    viewport_min := triangle_min // linalg.clamp(triangle_min, tile_min, tile_max)
    viewport_max := triangle_max // linalg.clamp(triangle_max, tile_min, tile_max)

    // Render the triangle!
    if toggles.use_simd {
        if toggles.use_integer {
            rasterize_triangle_simd_int(a_ndc, b_ndc, c_ndc, a, b, c, viewport_min, viewport_max, image)
        } else {
            rasterize_triangle_simd(a_ndc, b_ndc, c_ndc, a, b, c, viewport_min, viewport_max, image)
        }
    } else {
        if toggles.use_integer {
            rasterize_triangle_int(a_ndc, b_ndc, c_ndc, a, b, c, viewport_min, viewport_max, image)
        } else {
            rasterize_triangle(a_ndc, b_ndc, c_ndc, a, b, c, viewport_min, viewport_max, image)
        }
    }
    //     }
    // }

    // }

    map_to_viewport :: proc(ndc: Vector4) -> Vector4 {

        pos := (Vector2{ndc.x, ndc.y} + 1.0) / 2.0
        pos.x *= cast(f32)screen.size.x
        pos.y *= cast(f32)screen.size.y

        return {pos.x, pos.y, ndc.z, ndc.w}
    }

    vertex_shader :: proc(vertex: Vertex, transform: Matrix) -> Vector4 {
        return transform * Vector4{vertex.position.x, vertex.position.y, vertex.position.z, 1.0}
    }
}

@(private = "file")
rasterize_triangle :: proc(
    v0, v1, v2: Vector4,
    a, b, c: Vertex,
    viewport_min, viewport_max: [2]int,
    image: Image,
) #no_bounds_check {

    viewport_min := linalg.array_cast(viewport_min.xy, f32)
    viewport_max := linalg.array_cast(viewport_max.xy, f32)

    // Find triangle bounds clamped to screen.
    triangle_min := linalg.floor(linalg.clamp(linalg.min(v0.xy, v1.xy, v2.xy), viewport_min, viewport_max))
    triangle_max := linalg.ceil(linalg.clamp(linalg.max(v0.xy, v1.xy, v2.xy), viewport_min, viewport_max))

    // Barycentric interpolation starting points.
    w0_row := signed_area(v1.xy, v2.xy, triangle_min)
    w1_row := signed_area(v2.xy, v0.xy, triangle_min)
    w2_row := signed_area(v0.xy, v1.xy, triangle_min)

    inv_area := 1.0 / signed_area(v0.xy, v1.xy, v2.xy)

    v12 := v1 - v2
    v20 := v2 - v0
    v01 := v0 - v1

    for y in triangle_min.y ..< triangle_max.y {

        w0_inc := w0_row
        w1_inc := w1_row
        w2_inc := w2_row

        for x in triangle_min.x ..< triangle_max.x {

            w0 := w0_inc
            w1 := w1_inc
            w2 := w2_inc

            w0_inc += v12.y
            w1_inc += v20.y
            w2_inc += v01.y

            // TODO: bitwise or?
            // - https://github.com/kristoffer-dyrkorn/triangle-rasterizer/blob/main/9/triangle.js
            // - https://fgiesen.wordpress.com/2013/02/10/optimizing-the-basic-rasterizer/
            if w0 < 0 || w1 < 0 || w2 < 0 {
                continue
            }

            w0 *= inv_area
            w1 *= inv_area
            w2 *= inv_area

            ndc := interpolate(v0, v1, v2, w0, w1, w2)
            depth := ndc.w / ndc.z

            // ---- DEPTH TEST ----

            depth_ptr := raw_data(screen_depth)[(cast(int)y * screen.size.x) + cast(int)x:]

            if depth < depth_ptr[0] {
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

        w0_row -= v12.x
        w1_row -= v20.x
        w2_row -= v01.x
    }

    interpolate :: proc(a, b, c: $T, w0, w1, w2: f32) -> T {
        return (a * w0) + (b * w1) + (c * w2)
    }
}

@(private = "file")
rasterize_triangle_int :: proc(
    v0, v1, v2: Vector4,
    a, b, c: Vertex,
    triangle_min, triangle_max: [2]int,
    image: Image,
) #no_bounds_check {

    // Get triangle positions on the integer grid.
    p0 := linalg.array_cast(v0.xy, int)
    p1 := linalg.array_cast(v1.xy, int)
    p2 := linalg.array_cast(v2.xy, int)

    // Triangle is smaller than a pixel.
    // TODO: Should this just write a vertex "a" fragment?
    area := signed_area(p0, p1, p2)
    if area == 0 {
        return
    }

    // Precompuse inverse area to optimize interpolation.
    inv_area := 1.0 / cast(f32)area

    // Barycentric interpolation starting points.
    w_row: [3]int = {
        signed_area(p1, p2, triangle_min),
        signed_area(p2, p0, triangle_min),
        signed_area(p0, p1, triangle_min),
    }

    // Incremental barycentric interpolation step factors.
    delta_w_col: [3]int = {p1.y - p2.y, p2.y - p0.y, p0.y - p1.y}
    delta_w_row: [3]int = {p2.x - p1.x, p0.x - p2.x, p1.x - p0.x}

    for y in triangle_min.y ..< triangle_max.y {

        defer w_row += delta_w_row
        w_col := w_row

        for x in triangle_min.x ..< triangle_max.x {

            defer w_col += delta_w_col

            if (w_col[0] | w_col[1] | w_col[2]) < 0 {
                continue
            }

            w0 := cast(f32)w_col[0] * inv_area
            w1 := cast(f32)w_col[1] * inv_area
            w2 := cast(f32)w_col[2] * inv_area

            ndc := interpolate(v0, v1, v2, w0, w1, w2)
            depth := ndc.w / ndc.z

            // ---- DEPTH TEST ----

            depth_ptr := raw_data(screen_depth)[(y * screen.size.x) + x:]

            if depth < depth_ptr[0] {
                continue // fragment hidden
            }

            // ---- VERTEX INTERPOLATOR ---

            // TODO: Normalize?
            fragment_normal := interpolate(a.normal, b.normal, c.normal, w0, w1, w2)
            fragment_uv := interpolate(a.uv, b.uv, c.uv, w0, w1, w2)

            // ---- FRAGMENT SHADER BEGIN ---

            pixel := image_sample(image, fragment_uv)

            brightness := max(0.0, linalg.dot(fragment_normal, Sun_Direction))
            pixel = mix_color(0, pixel, 0.25 + (brightness * 0.75))

            // ---- FRAGMENT SHADER END ---

            pixel.a = 0xFF // no transparency

            // Write color
            image_set_pixel(screen, x, y, pixel)

            // Write depth
            depth_ptr[0] = depth
        }
    }

    interpolate :: proc(a, b, c: $T, w0, w1, w2: f32) -> T {
        return (a * w0) + (b * w1) + (c * w2)
    }
}

@(private = "file")
rasterize_triangle_simd :: proc(
    v0, v1, v2: Vector4,
    a, b, c: Vertex,
    viewport_min, viewport_max: [2]int,
    image: Image,
) #no_bounds_check {

    // Get viewport rect on the float grid.
    viewport_min := linalg.array_cast(viewport_min, f32)
    viewport_max := linalg.array_cast(viewport_max, f32)

    // Find triangle bounds clamped to screen.
    triangle_min := linalg.floor(linalg.clamp(linalg.min(v0.xy, v1.xy, v2.xy), viewport_min, viewport_max))
    triangle_max := linalg.ceil(linalg.clamp(linalg.max(v0.xy, v1.xy, v2.xy), viewport_min, viewport_max))

    // SIMD reads and writes need to be 16 byte aligned.
    align_min_x := cast(f32)mem.align_backward_int(cast(int)triangle_min.x, 4)
    align_max_x := cast(f32)mem.align_forward_int(cast(int)triangle_max.x, 4)

    // Since SIMD is aligned, we need to offset the incremental barycentric too.
    align_offset := align_min_x - triangle_min.x

    // Barycentric interpolation starting points.
    w0_row := signed_area(v1.xy, v2.xy, triangle_min)
    w1_row := signed_area(v2.xy, v0.xy, triangle_min)
    w2_row := signed_area(v0.xy, v1.xy, triangle_min)

    inv_area := 1.0 / signed_area(v0.xy, v1.xy, v2.xy)

    v12 := v1 - v2
    v20 := v2 - v0
    v01 := v0 - v1

    Step_Sizes :: (#simd[4]f32){0, 1, 2, 3}

    steps_v12y := v12.y * Step_Sizes
    steps_v20y := v20.y * Step_Sizes
    steps_v01y := v01.y * Step_Sizes

    for y in triangle_min.y ..< triangle_max.y {

        w0_inc := w0_row + (align_offset * v12.y)
        w1_inc := w1_row + (align_offset * v20.y)
        w2_inc := w2_row + (align_offset * v01.y)

        for x := align_min_x; x < align_max_x; x += 4 {

            w0 := w0_inc + steps_v12y
            w1 := w1_inc + steps_v20y
            w2 := w2_inc + steps_v01y

            w0_inc += v12.y * 4
            w1_inc += v20.y * 4
            w2_inc += v01.y * 4

            write_mask := cast(#simd[4]u32)(~u32(0))
            write_mask = write_mask & simd.lanes_ge(w0, (#simd[4]f32)(0))
            write_mask = write_mask & simd.lanes_ge(w1, (#simd[4]f32)(0))
            write_mask = write_mask & simd.lanes_ge(w2, (#simd[4]f32)(0))

            if simd.reduce_or(write_mask) == 0 {
                continue // all fragments are outside the triangle
            }

            w0 *= inv_area
            w1 *= inv_area
            w2 *= inv_area

            ndc_z := interpolate(v0.z, v1.z, v2.z, w0, w1, w2)
            ndc_w := interpolate(v0.w, v1.w, v2.w, w0, w1, w2)
            depth := ndc_w / ndc_z

            // ---- DEPTH TEST ----

            depth_ptr := cast(^#simd[4]f32)raw_data(screen_depth)[(cast(int)y * screen.size.x) + cast(int)x:]
            write_mask = write_mask & simd.lanes_gt(depth, depth_ptr^)

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

        w0_row -= v12.x
        w1_row -= v20.x
        w2_row -= v01.x
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

@(private = "file")
rasterize_triangle_simd_int :: proc(
    v0, v1, v2: Vector4,
    a, b, c: Vertex,
    viewport_min, viewport_max: [2]int,
    image: Image,
) #no_bounds_check {

    // Get triangle positions on the integer grid.
    p0 := linalg.array_cast(v0.xy, i32)
    p1 := linalg.array_cast(v1.xy, i32)
    p2 := linalg.array_cast(v2.xy, i32)

    // Get viewport rect on the integer grid.
    viewport_min := linalg.array_cast(viewport_min, i32)
    viewport_max := linalg.array_cast(viewport_max, i32)

    // Triangle is smaller than a pixel.
    // TODO: Should this just write a vertex "a" fragment?
    area := signed_area(p0, p1, p2)
    if area == 0 {
        return
    }

    // Find triangle bounds clamped to screen.
    triangle_min := linalg.clamp(linalg.min(p0, p1, p2), viewport_min, viewport_max)
    triangle_max := linalg.clamp(linalg.max(p0, p1, p2), viewport_min, viewport_max)

    // SIMD reads and writes need to be 16 byte aligned.
    align_min_x := cast(i32)mem.align_backward_int(cast(int)triangle_min.x, 4)
    align_max_x := cast(i32)mem.align_forward_int(cast(int)triangle_max.x, 4)

    // Since SIMD is aligned, we need to offset the incremental barycentric too.
    align_offset := align_min_x - triangle_min.x

    // Barycentric interpolation starting points.
    w0_row := signed_area(p1, p2, triangle_min)
    w1_row := signed_area(p2, p0, triangle_min)
    w2_row := signed_area(p0, p1, triangle_min)

    // Precompuse inverse area to optimize interpolation.
    inv_area := 1.0 / cast(f32)area

    p12 := p1 - p2
    p20 := p2 - p0
    p01 := p0 - p1

    Step_Sizes :: (#simd[4]i32){0, 1, 2, 3}

    steps_v12y := p12.y * Step_Sizes
    steps_v20y := p20.y * Step_Sizes
    steps_v01y := p01.y * Step_Sizes

    for y in triangle_min.y ..< triangle_max.y {

        w0_inc := w0_row + (align_offset * p12.y)
        w1_inc := w1_row + (align_offset * p20.y)
        w2_inc := w2_row + (align_offset * p01.y)

        defer {
            w0_row -= p12.x
            w1_row -= p20.x
            w2_row -= p01.x
        }

        for x := align_min_x; x < align_max_x; x += 4 {

            w0_steps := w0_inc + steps_v12y
            w1_steps := w1_inc + steps_v20y
            w2_steps := w2_inc + steps_v01y

            defer {
                w0_inc += p12.y * 4
                w1_inc += p20.y * 4
                w2_inc += p01.y * 4
            }

            write_mask := cast(#simd[4]u32)(0) // w0 >= 0 && w1 >= 0 && w2 >= 0
            write_mask = ~write_mask & simd.lanes_ge(w0_steps, (#simd[4]i32)(0))
            write_mask = write_mask & simd.lanes_ge(w1_steps, (#simd[4]i32)(0))
            write_mask = write_mask & simd.lanes_ge(w2_steps, (#simd[4]i32)(0))

            if simd.reduce_or(write_mask) == 0 {
                continue // all fragments are outside the triangle
            }

            w0 := (#simd[4]f32)(w0_steps) * inv_area
            w1 := (#simd[4]f32)(w1_steps) * inv_area
            w2 := (#simd[4]f32)(w2_steps) * inv_area

            ndc_z := interpolate(v0.z, v1.z, v2.z, w0, w1, w2)
            ndc_w := interpolate(v0.w, v1.w, v2.w, w0, w1, w2)
            depth := ndc_w / ndc_z

            // ---- DEPTH TEST ----

            depth_ptr := cast(^#simd[4]f32)raw_data(screen_depth)[(cast(int)y * screen.size.x) + cast(int)x:]
            write_mask = write_mask & simd.lanes_gt(depth, depth_ptr^)

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

@(private = "file")
signed_area :: #force_inline proc(a, b, c: [2]$V) -> V {
    return (b.x - a.x) * (c.y - a.y) - (b.y - a.y) * (c.x - a.x)
}
