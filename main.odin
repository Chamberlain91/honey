package honey

import "base:intrinsics"
@(require) import "core:fmt"
@(require) import "core:math"
import "core:math/linalg"
import "core:mem"
import "core:simd"
import "core:slice"

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
        slice.fill(screen_depth, 1.0) // clear depth buffer

        time += toggles.pause ? 0 : get_time() - latest_time
        latest_time = get_time()

        camera_position := Vector3{math.cos_f32(time / 3), 2.5, math.sin_f32(time / 5)} * 4
        camera_aspect := cast(f32)screen.size.x / cast(f32)screen.size.y
        camera_matrix :=
            linalg.matrix4_perspective_f32(math.PI / 2, camera_aspect, 0.1, 200.0) *
            linalg.matrix4_look_at_f32(camera_position, {0, 7.0, 0}, {0, 1, 0})

        render_mesh(mesh, assets.image, camera_matrix)

        status := fmt.tprintf(
            "pause: {}, backface: {}, use-simd: {}, use-split: {}",
            toggles.pause,
            toggles.backface,
            toggles.use_simd,
            toggles.use_split,
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

        // Toggle scanline triangle paths.
        if is_key_pressed(.X) {
            toggles.use_split = !toggles.use_split
        }
    }
}

SCREEN_W :: 960
SCREEN_H :: 540
FACTOR :: 2

assets: struct {
    image: Image,
}

toggles: struct {
    use_simd:  bool,
    use_split: bool,
    backface:  bool,
    pause:     bool,
} = {
    use_simd  = false,
    use_split = false,
    backface  = true,
    pause     = true,
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
    // -https://jtsorlinis.github.io/rendering-tutorial/

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

    // Render the triangle!
    if toggles.use_simd {
        if toggles.use_split {
            // TODO: rasterize_triangle_split_simd
        } else {
            rasterize_triangle_simd(a_ndc, b_ndc, c_ndc, a, b, c, image)
        }
    } else {
        if toggles.use_split {
            rasterize_triangle_split(a_ndc.xy, b_ndc.xy, c_ndc.xy)
        } else {
            rasterize_triangle(a_ndc, b_ndc, c_ndc, a, b, c, image)
        }
    }

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
rasterize_triangle_split :: proc(v0, v1, v2: Vector2) #no_bounds_check {

    // Reference:
    // - https://www.sunshine2k.de/coding/java/TriangleRasterization/TriangleRasterization.html

    // BOTTOM FLAT ONLY
    //
    //        # - 0
    //       # #
    //      #   #
    // 1 - ####### - 2
    //
    // TOP FLAT ONLY
    //
    // 0 - ####### - 1
    //      ##    #
    //        ##   #
    //          ##  #
    //            ## #
    //              ###
    //                ## - 2
    //
    // BOTH COMPUTE SPLIT
    //
    //        # - 0
    //       # #
    //      #   #
    // 1 - #-----# - 3
    //      ##    #
    //        ##   #
    //          ##  #
    //            ## #
    //              ###
    //                ## - 2

    v0 := linalg.array_cast(v0, int)
    v1 := linalg.array_cast(v1, int)
    v2 := linalg.array_cast(v2, int)

    // Sort small to high
    if v0.y > v1.y do swap(&v0, &v1)
    if v1.y > v2.y do swap(&v1, &v2)
    if v0.y > v1.y do swap(&v0, &v1)

    assert(v0.y <= v1.y)
    assert(v0.y <= v2.y)
    assert(v1.y <= v2.y)

    if v1.y == v2.y {

        fill_flat_bottom_triangle(v0, v1, v2)

    } else if v0.y == v1.y {

        fill_flat_top_triangle(v0, v1, v2)

    } else {

        t := inv_lerp(cast(f32)v0.y, cast(f32)v2.y, cast(f32)v1.y)
        v3 := [2]int{cast(int)lerp(cast(f32)v0.x, cast(f32)v2.x, t), v1.y}

        fill_flat_bottom_triangle(v0, v1, v3)
        fill_flat_top_triangle(v1, v3, v2)
    }

    fill_flat_bottom_triangle :: proc(v0, v1, v2: [2]int) {

        // Compute slopes.
        m0 := cast(f32)(v1.x - v0.x) / cast(f32)(v1.y - v0.y)
        m1 := cast(f32)(v2.x - v0.x) / cast(f32)(v2.y - v0.y)
        if m1 < m0 do swap(&m0, &m1) // ???

        x0 := cast(f32)v0.x
        x1 := cast(f32)v0.x

        for y := v0.y; y <= v1.y; y += 1 {

            for x := cast(int)x0; x <= cast(int)x1; x += 1 {
                if x >= 0 && x < screen.size.x && y >= 0 && y < screen.size.y {
                    image_set_pixel(screen, x, y, transmute(Color)u32(0xFF339933)) // RED
                }
            }

            x0 += m0
            x1 += m1
        }
    }

    fill_flat_top_triangle :: proc(v0, v1, v2: [2]int) {

        // Compute slopes.
        m0 := cast(f32)(v2.x - v0.x) / cast(f32)(v2.y - v0.y)
        m1 := cast(f32)(v2.x - v1.x) / cast(f32)(v2.y - v1.y)
        if m0 < m1 do swap(&m0, &m1) // ???

        x0 := cast(f32)v2.x
        x1 := cast(f32)v2.x

        for y := v2.y; y > v0.y; y -= 1 {

            for x := cast(int)x0; x <= cast(int)x1; x += 1 {
                if x >= 0 && x < screen.size.x && y >= 0 && y < screen.size.y {
                    image_set_pixel(screen, x, y, transmute(Color)u32(0xFF339933)) // GREEN
                }
            }

            x0 -= m0
            x1 -= m1
        }
    }

    swap :: proc(x, y: ^$V) {
        x^, y^ = y^, x^
    }

    lerp :: proc(a, b, t: f32) -> f32 {
        return a + (b - a) * t
    }

    inv_lerp :: proc(a, b, v: f32) -> f32 {
        return (v - a) / (b - a)
    }
}

@(private = "file")
rasterize_triangle :: proc(v0, v1, v2: Vector4, a, b, c: Vertex, image: Image) #no_bounds_check {

    screen_size := linalg.array_cast(screen.size, f32)

    triangle_min := linalg.floor(linalg.min(v0.xy, v1.xy, v2.xy))
    triangle_max := linalg.ceil(linalg.max(v0.xy, v1.xy, v2.xy))

    // Find triangle bounds clamped to screen.
    range_min := linalg.clamp(triangle_min, 0, screen_size)
    range_max := linalg.clamp(triangle_max, 0, screen_size)

    v01 := v0 - v1
    v12 := v1 - v2
    v20 := v2 - v0

    w0_row := signed_area(v1.xy, v2.xy, triangle_min)
    w1_row := signed_area(v2.xy, v0.xy, triangle_min)
    w2_row := signed_area(v0.xy, v1.xy, triangle_min)

    wT := 1.0 / signed_area(v0.xy, v1.xy, v2.xy)

    for y in range_min.y ..< range_max.y {

        w0_inc := w0_row
        w1_inc := w1_row
        w2_inc := w2_row

        for x in range_min.x ..< range_max.x {

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

        w0_row -= v12.x
        w1_row -= v20.x
        w2_row -= v01.x
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

@(private = "file")
signed_area :: #force_inline proc(a, b, c: [2]$V) -> V {
    return (b.x - a.x) * (c.y - a.y) - (b.y - a.y) * (c.x - a.x)
}

@(private = "file")
Triangle :: struct {
    positions: [3]Vector4,
    vertices:  [3]Vertex,
}
