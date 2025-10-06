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

    quad: Mesh(Vertex) = {
        vertices = {
            Vertex{position = {-30, 0, -30}, uv = {0, 0}, normal = {0, 1, 0}, color = 1.0},
            Vertex{position = {-30, 0, +30}, uv = {0, 1}, normal = {0, 1, 0}, color = 1.0},
            Vertex{position = {+30, 0, +30}, uv = {1, 1}, normal = {0, 1, 0}, color = 1.0},
            Vertex{position = {+30, 0, -30}, uv = {1, 0}, normal = {0, 1, 0}, color = 1.0},
        },
        indices  = {0, 2, 1, 0, 3, 2},
    }

    // Main loop.
    main_loop: for is_window_open() {

        // Populate image data for this frame.
        image_clear(screen, transmute(Color)(u32(0xFF181818)))
        mem.zero_slice(screen_depth) // clear depth (depth is -1/z form)

        time += toggles.pause ? 0 : get_time() - latest_time
        latest_time = get_time()

        camera_position := Vector3{math.cos_f32(time / 3), 1.5, math.sin_f32(time / 5)} * 5
        camera_aspect := cast(f32)screen.size.x / cast(f32)screen.size.y
        camera_matrix :=
            linalg.matrix4_perspective_f32(math.PI / 2, camera_aspect, 0.1, 200.0) *
            linalg.matrix4_look_at_f32(camera_position, {0, 7.0, 0}, {0, 1, 0})

        for y: f32 = -15; y <= 15; y += 5.0 {
            for x: f32 = -15; x <= 15; x += 5.0 {
                transform := linalg.matrix4_translate_f32({x, x * 0.5, y})
                render_mesh(mesh, assets.image, camera_matrix * transform)
            }
        }
        render_mesh(quad, assets.image, camera_matrix)

        status := fmt.tprintf(
            "(Q) pause: {}\n(E) backface: {}\n(Z) use simd: {}",
            toggles.pause,
            toggles.backface,
            toggles.use_simd,
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
    }
}

SCREEN_W :: 800
SCREEN_H :: 500
FACTOR :: 2

assets: struct {
    image: Image,
}

toggles: struct {
    use_simd: bool,
    backface: bool,
    pause:    bool,
} = {
    use_simd = false,
    backface = true,
    pause    = true,
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

    // Transform (World to NDC)
    tri_ndc: [3]Vector4 = {     //
        world_to_ndc(a, transform),
        world_to_ndc(b, transform),
        world_to_ndc(c, transform),
    }

    // Homogenize (perspective divide)
    for &ndc in tri_ndc {
        ndc.w = 1.0 / ndc.w
        ndc.xyz *= ndc.w
    }

    // Backface culling (counter-clockwise)
    // TODO: The backfaces aren't actually rendered properly when this branch is eliminated, so I need to figure out that bug.
    if toggles.backface && signed_area(tri_ndc[0].xy, tri_ndc[1].xy, tri_ndc[2].xy) < 0 {
        return
    }

    // TODO: Frustum plane clipping (near plane clipping at minimum)

    for &ndc in tri_ndc {

        // NEAR PLANE REJECTION
        if ndc.z < 0 do return

        // FAR PLANE REJECTION
        if ndc.z > 1 do return

        // Side planes
        if ndc.x < -1 do return // X- PLANE REJECTION
        if ndc.x > +1 do return // X+ PLANE REJECTION
        if ndc.y > +1 do return // Y+ PLANE REJECTION
        if ndc.y < -1 do return // Y- PLANE REJECTION
    }

    // 0 triangles (3 verts behind)
    // 1 triangles (3 verts in front OR 2 verts behind)
    // 2 triangles (2 verts in front)

    // parts := clip_triangle(a_ndc, b_ndc, c_ndc)
    // for a, b, c in parts {

    // Map XY to Viewport
    for &gl_Position in tri_ndc {
        gl_Position = map_to_viewport(gl_Position)
    }

    // Find triangle bounds...
    triangle_min := linalg.array_cast(linalg.floor(linalg.min(tri_ndc[0].xy, tri_ndc[1].xy, tri_ndc[2].xy)), int)
    triangle_max := linalg.array_cast(linalg.ceil(linalg.max(tri_ndc[0].xy, tri_ndc[1].xy, tri_ndc[2].xy)), int)

    // ... clamp to screen.
    triangle_min = linalg.clamp(triangle_min, 0, screen.size)
    triangle_max = linalg.clamp(triangle_max, 0, screen.size)

    // TILE_SIZE :: 64 // power-of-2 and multiple of 16?

    // x_min := mem.align_backward_int(triangle_min.x, TILE_SIZE)
    // y_min := mem.align_backward_int(triangle_min.y, TILE_SIZE)
    // x_max := mem.align_forward_int(triangle_max.x, TILE_SIZE)
    // y_max := mem.align_forward_int(triangle_max.y, TILE_SIZE)

    // for y := y_min; y < y_max; y += TILE_SIZE {
    //     for x := x_min; x < x_max; x += TILE_SIZE {

    //         tile_min := [2]int{x, y}
    //         tile_max := linalg.min(tile_min + (TILE_SIZE - 1), screen.size)

    // ...clamped to screen.
    v_min := triangle_min // linalg.clamp(triangle_min, tile_min, tile_max)
    v_max := triangle_max // linalg.clamp(triangle_max, tile_min, tile_max)

    // Render the triangle!
    if toggles.use_simd {
        rasterize_triangle_simd(tri_ndc[0], tri_ndc[1], tri_ndc[2], a, b, c, v_min, v_max, image)
    } else {
        rasterize_triangle(tri_ndc[0], tri_ndc[1], tri_ndc[2], a, b, c, v_min, v_max, image)
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

    world_to_ndc :: proc(vertex: Vertex, transform: Matrix) -> Vector4 {
        return transform * Vector4{vertex.position.x, vertex.position.y, vertex.position.z, 1.0}
    }
}

@(private = "file")
rasterize_triangle :: proc(
    v0, v1, v2: Vector4,
    a, b, c: Vertex,
    triangle_min, triangle_max: [2]int,
    image: Image,
) #no_bounds_check {

    // Precompuse inverse area to optimize interpolation.
    inv_area := 1.0 / signed_area(v0.xy, v1.xy, v2.xy)

    // Barycentric interpolation starting points.
    v_min := linalg.array_cast(triangle_min.xy, f32)
    weights_row: [3]f32 = {
        signed_area(v1.xy, v2.xy, v_min),
        signed_area(v2.xy, v0.xy, v_min),
        signed_area(v0.xy, v1.xy, v_min),
    }

    // Incremental barycentric interpolation step factors.
    delta_weights_col: [3]f32 = {v1.y - v2.y, v2.y - v0.y, v0.y - v1.y}
    delta_weights_row: [3]f32 = {v2.x - v1.x, v0.x - v2.x, v1.x - v0.x}

    for y in triangle_min.y ..< triangle_max.y {

        weights_col := weights_row
        weights_row += delta_weights_row // ???

        for x in triangle_min.x ..< triangle_max.x {

            weights := weights_col * inv_area
            weights_col += delta_weights_col // ???

            // Fragment is outside the triangle.
            if weights[0] < 0 || weights[1] < 0 || weights[2] < 0 {
                continue // fragment outside triangle
            }

            frag_w := interpolate(v0.w, v1.w, v2.w, weights)
            frag_z := interpolate(v0.z, v1.z, v2.z, weights) * frag_w

            // ---- DEPTH TEST ----

            depth_ptr := raw_data(screen_depth)[(y * screen.size.x) + x:]

            if frag_z < depth_ptr[0] {
                continue // fragment hidden
            }

            // Perspective correct interpolation weights.
            perspective := (weights * {v0.w, v1.w, v2.w}) / frag_w

            // ---- COMPUTE VARYING (INTERPOLATED) ATTRIBUTES ---

            fragment_normal := interpolate(a.normal, b.normal, c.normal, perspective)
            fragment_uv := interpolate(a.uv, b.uv, c.uv, perspective)

            // ---- FRAGMENT SHADER BEGIN ---

            pixel := image_sample(image, fragment_uv)

            brightness := max(0.0, linalg.dot(fragment_normal, Sun_Direction))
            pixel = mix_color(0, pixel, 0.25 + (brightness * 0.75))

            // ---- FRAGMENT SHADER END ---

            pixel.a = 0xFF // no transparency

            // Write color
            image_set_pixel(screen, x, y, pixel)

            // Write depth
            depth_ptr[0] = frag_z
        }
    }

    interpolate :: proc(a, b, c: $T, weights: [3]f32) -> T {
        return (a * weights[0]) + (b * weights[1]) + (c * weights[2])
    }
}

@(private = "file")
rasterize_triangle_simd :: proc(
    v0, v1, v2: Vector4,
    a, b, c: Vertex,
    triangle_min, triangle_max: [2]int,
    image: Image,
) #no_bounds_check {

    STEP_SIZES :: (#simd[4]f32){0, 1, 2, 3}

    // Precompuse inverse area to optimize interpolation.
    inv_area := 1.0 / signed_area(v0.xy, v1.xy, v2.xy)

    // SIMD reads and writes need to be 16 byte aligned.
    align_min_x := mem.align_backward_int(triangle_min.x, 4)
    align_max_x := mem.align_forward_int(triangle_max.x, 4)

    // Since SIMD is aligned, we need to offset the incremental barycentric too.
    align_offset := cast(f32)(align_min_x - triangle_min.x)

    v_min := linalg.array_cast(triangle_min.xy, f32)

    // Barycentric interpolation starting points.
    weights_row_0 := signed_area(v1.xy, v2.xy, v_min)
    weights_row_1 := signed_area(v2.xy, v0.xy, v_min)
    weights_row_2 := signed_area(v0.xy, v1.xy, v_min)

    v12 := v1 - v2
    v20 := v2 - v0
    v01 := v0 - v1

    steps_v12y := v12.y * STEP_SIZES
    steps_v20y := v20.y * STEP_SIZES
    steps_v01y := v01.y * STEP_SIZES

    for y in triangle_min.y ..< triangle_max.y {

        weights_col_0 := weights_row_0 + (align_offset * v12.y)
        weights_col_1 := weights_row_1 + (align_offset * v20.y)
        weights_col_2 := weights_row_2 + (align_offset * v01.y)

        weights_row_0 -= v12.x // ???
        weights_row_1 -= v20.x
        weights_row_2 -= v01.x

        for x := align_min_x; x < align_max_x; x += 4 {

            w0 := (weights_col_0 + steps_v12y) * inv_area
            w1 := (weights_col_1 + steps_v20y) * inv_area
            w2 := (weights_col_2 + steps_v01y) * inv_area

            weights_col_0 += v12.y * 4
            weights_col_1 += v20.y * 4
            weights_col_2 += v01.y * 4

            write_mask: #simd[4]u32 = ~u32(0)
            write_mask = write_mask & simd.lanes_ge(w0, 0)
            write_mask = write_mask & simd.lanes_ge(w1, 0)
            write_mask = write_mask & simd.lanes_ge(w2, 0)

            if simd.reduce_or(write_mask) == 0 {
                continue // all fragments outside triangle
            }

            frag_w := interpolate(v0.w, v1.w, v2.w, w0, w1, w2)
            frag_z := interpolate(v0.z, v1.z, v2.z, w0, w1, w2) * frag_w

            // ---- DEPTH TEST ----

            depth_ptr := cast(^#simd[4]f32)raw_data(screen_depth)[(y * screen.size.x) + x:]

            write_mask = write_mask & simd.lanes_gt(frag_z, depth_ptr^)
            if simd.reduce_or(write_mask) == 0 {
                continue // all fragments are hidden
            }

            // Perspective correct interpolation weights.
            w0 = (w0 * v0.w) / frag_w
            w1 = (w1 * v1.w) / frag_w
            w2 = (w2 * v2.w) / frag_w

            // ---- COMPUTE VARYING (INTERPOLATED) ATTRIBUTES ---

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
            pixel_ptr := cast(^#simd[4]u32)image_get_pixel_ptr(screen, x, y)
            pixel_ptr^ = (pixels & write_mask) | (pixel_ptr^ &~ write_mask)

            // Write depth.
            depth_ptr^ = transmute(#simd[4]f32)((transmute(#simd[4]u32)frag_z & write_mask) |
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
    device_coordinates: [4]Vector4,
    vertices:           [3]Vertex,
}
