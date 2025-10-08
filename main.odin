package honey

import "base:intrinsics"
import sa "core:container/small_array"
@(require) import "core:fmt"
@(require) import "core:math"
import la "core:math/linalg"
import "core:mem"
import "core:simd"
import "core:slice"

Sun_Direction := la.normalize(Vector3{1, 3, -2})

triangle_count: int

main :: proc() {

    // Initialize window.
    initialize_window(SCREEN_W * FACTOR, SCREEN_H * FACTOR, "Honey Software Renderer")
    defer destroy_window()

    // ...
    assets.image = image_load("assets/Nexo_Texture.png")

    // ...
    mesh := parse_wavefront_mesh(#load("assets/NexoITCH.obj", string), flip_uv = true)

    quad: Mesh = {
        vertices = {
            Vertex{position = {-30, 0, -30}, uv = {0, 0}, normal = {0, 1, 0}},
            Vertex{position = {+30, 0, -30}, uv = {0, 1}, normal = {0, 1, 0}},
            Vertex{position = {+30, 0, +30}, uv = {1, 1}, normal = {0, 1, 0}},
            Vertex{position = {-30, 0, +30}, uv = {1, 0}, normal = {0, 1, 0}},
        },
        indices  = {0, 1, 2, 0, 2, 3},
    }

    camera_position: Vector3
    camera_heading: f32
    camera_pitch: f32

    camera_aspect := cast(f32)screen.size.x / cast(f32)screen.size.y

    // Main loop.
    main_loop: for is_window_open() {

        defer set_mouse_position(get_window_size() / 2)

        // Populate image data for this frame.
        image_clear(screen, transmute(Color)(u32(0xFF181818))) // clear color
        slice.fill(screen_depth, 1.0) // clear depth

        camera_dir: Vector3 = {
            math.cos(camera_heading) * math.cos(camera_pitch),
            math.sin(camera_pitch),
            math.sin(camera_heading) * math.cos(camera_pitch),
        }

        camera_matrix :=
            la.matrix4_perspective_f32(math.PI / 2, camera_aspect, 0.1, 50.0) *
            la.matrix4_look_at_f32(camera_position, camera_position + camera_dir, {0, 1, 0})


        for y: f32 = -15; y <= 15; y += 5.0 {
            for x: f32 = -15; x <= 15; x += 5.0 {
                transform := la.matrix4_translate_f32({x, x * 0.5, y})
                render_mesh(mesh, assets.image, camera_matrix * transform)
            }
        }
        render_mesh(quad, assets.image, camera_matrix)

        status := fmt.tprintf(
            "(X) backface: {}\n(Z) use simd: {}\ntriangle count: {}",
            toggles.backface,
            toggles.use_simd,
            triangle_count,
        )
        update_screen_pixels(status)
        triangle_count = 0

        PITCH_LIMIT :: (math.PI / 2) * 0.8

        camera_heading += mouse_delta().x * 0.01
        camera_pitch -= mouse_delta().y * 0.01
        camera_pitch = clamp(camera_pitch, -PITCH_LIMIT, PITCH_LIMIT)

        if is_key_down(.W) {
            camera_position += camera_dir * get_delta_time() * 5
        }

        if is_key_down(.S) {
            camera_position -= camera_dir * get_delta_time() * 5
        }

        // Exit when pressing escape.
        if is_key_pressed(.ESCAPE) {
            break main_loop
        }

        // Toggle SIMD code paths.
        if is_key_pressed(.Z) {
            toggles.use_simd = !toggles.use_simd
        }

        // Backface culling
        if is_key_pressed(.X) {
            toggles.backface = !toggles.backface
        }
    }
}

SCREEN_W :: 960
SCREEN_H :: 540
FACTOR :: 1.5

assets: struct {
    image: Image,
}

toggles: struct {
    use_simd: bool,
    backface: bool,
} = {
    use_simd = true,
    backface = true,
}

// -----------------------------------------------------------------------------

Vertex :: struct {
    position: Vector3,
    normal:   Vector3,
    uv:       Vector2,
}

Mesh :: struct {
    vertices: []Vertex,
    indices:  []int,
}

render_mesh :: proc(mesh: Mesh, image: Image, transform: Matrix) #no_bounds_check {

    for i := 0; i < len(mesh.indices); i += 3 {

        a := mesh.vertices[mesh.indices[i + 0]]
        b := mesh.vertices[mesh.indices[i + 2]]
        c := mesh.vertices[mesh.indices[i + 1]]

        render_triangle(a, b, c, image, transform)
    }
}

render_triangle :: proc(v0, v1, v2: Vertex, image: Image, transform: Matrix) {

    // World -> Clip
    p0 := transform_vertex(v0, transform)
    p1 := transform_vertex(v1, transform)
    p2 := transform_vertex(v2, transform)

    // Backface culling. Compare normal to "view vector", negative indicates away from viewpoint.
    if toggles.backface && la.dot(la.cross(p1.xyz - p0.xyz, p2.xyz - p0.xyz), p0.xyz) < 0 {
        return
    }

    // Case 0 (no clipping, emit 1 triangle)
    //     +
    //    + +
    //   +   +
    //  +-----+
    // --------- near

    // Case 1 (emit 2 triangles)
    // +-------+
    //  +     +
    //   +---+ near
    //    + +
    //     +

    // Case 2 (emit 1 triangle)
    //     +
    //    + +
    //   +---+ near
    //  +     +
    // +-------+

    // Case 3 (no clipping, emit 0 triangle)
    // --------- near
    //     +
    //    + +
    //   +   +
    //  +-----+

    // Reject the triangle if it is completely out of view.
    if p0.x > +p0.w && p1.x > +p1.w && p2.x > +p2.w do return
    if p0.x < -p0.w && p1.x < -p1.w && p2.x < -p2.w do return
    if p0.y > +p0.w && p1.y > +p1.w && p2.y > +p2.w do return
    if p0.y < -p0.w && p1.y < -p1.w && p2.y < -p2.w do return
    if p0.z > +p0.w && p1.z > +p1.w && p2.z > +p2.w do return
    if p0.z < 0 && p1.z < 0 && p2.z < 0 do return

    // Clip the triangle if it intercepts the near z-plane.
    // This will generate 1 or 2 triangles to render.
    // Other clipping planes are handled by raster clipping.
    Triangle_Buffer :: sa.Small_Array(3, Triangle)
    triangles: Triangle_Buffer

    if p0.z < 0 {
        if p1.z < 0 {
            clip2({{p0, p1, p2}, {v0, v1, v2}}, &triangles)
        } else if p2.z < 0 {
            clip2({{p0, p2, p1}, {v0, v2, v1}}, &triangles)
        } else {
            clip1({{p0, p1, p2}, {v0, v1, v2}}, &triangles)
        }
    } else if p1.z < 0 {
        if p2.z < 0 {
            clip2({{p1, p2, p0}, {v1, v2, v0}}, &triangles)
        } else {
            clip1({{p1, p0, p2}, {v1, v0, v2}}, &triangles)
        }
    } else if p2.z < 0 {
        clip1({{p2, p0, p1}, {v2, v0, v1}}, &triangles)
    } else {
        // No clipping needed
        sa.append(&triangles, Triangle{{p0, p1, p2}, {v0, v1, v2}})
    }

    // Draw each triangle.
    for &triangle in sa.slice(&triangles) {

        // Map the triangle to the viewport (NDC -> Viewport).
        for &pos in triangle.positions {

            // Do the "perspective divide"
            pos.w = 1.0 / pos.w
            pos.xyz *= pos.w

            // NDC -> Viewport
            pos.xy = (pos.xy + 1.0) / 2.0
            pos.x *= cast(f32)screen.size.x
            pos.y *= cast(f32)screen.size.y
        }

        p0, p1, p2 = expand_values(triangle.positions)

        // Find triangle bounds...
        triangle_min := la.array_cast(la.floor(la.min(p0.xy, p1.xy, p2.xy)), int)
        triangle_max := la.array_cast(la.ceil(la.max(p0.xy, p1.xy, p2.xy)), int)

        // ... clamp to screen (x and y clip planes)
        triangle_min = la.clamp(triangle_min, 0, screen.size)
        triangle_max = la.clamp(triangle_max, 0, screen.size)

        // Render the triangle!
        if toggles.use_simd {
            rasterize_triangle_simd(triangle, triangle_min, triangle_max, image)
        } else {
            rasterize_triangle(triangle, triangle_min, triangle_max, image)
        }
    }

    // Count the number of triangles drawn.
    triangle_count += sa.len(triangles)

    transform_vertex :: proc(vertex: Vertex, transform: Matrix) -> (clip_position: Vector4) {
        clip_position.xyz = vertex.position
        clip_position.w = 1.0
        return transform * clip_position
    }

    clip1 :: proc(triangle: Triangle, output: ^Triangle_Buffer) {
        p0, p1, p2 := expand_values(triangle.positions) // assume 0 and 1 violates
        v0, v1, v2 := expand_values(triangle.vertices)

        alphaA := -p0.z / (p1.z - p0.z)
        alphaB := -p0.z / (p2.z - p0.z)

        pA := la.lerp(p0, p1, alphaA)
        pB := la.lerp(p0, p2, alphaB)

        vA := interpolate_vertex(v0, v1, alphaA)
        vB := interpolate_vertex(v0, v2, alphaB)

        sa.append(output, Triangle{{pA, p1, p2}, {vA, v1, v2}})
        sa.append(output, Triangle{{pB, pA, p2}, {vB, vA, v2}})
    }

    clip2 :: proc(triangle: Triangle, output: ^Triangle_Buffer) {
        p0, p1, p2 := expand_values(triangle.positions) // assume 0 violates
        v0, v1, v2 := expand_values(triangle.vertices)

        alpha0 := -p0.z / (p2.z - p0.z)
        alpha1 := -p1.z / (p2.z - p1.z)

        p0 = la.lerp(p0, p2, alpha0)
        p1 = la.lerp(p1, p2, alpha1)

        v0 = interpolate_vertex(v0, v2, alpha0)
        v1 = interpolate_vertex(v1, v2, alpha1)

        sa.append(output, Triangle{{p0, p1, p2}, {v0, v1, v2}})
    }

    interpolate_vertex :: proc(a, b: Vertex, t: f32) -> (c: Vertex) {
        c.position = la.lerp(a.position, b.position, t) // Not used as a interpolated value (can remove?)
        c.normal = la.lerp(a.normal, b.normal, t)
        c.uv = la.lerp(a.uv, b.uv, t)
        return
    }
}

@(private = "file")
rasterize_triangle :: proc(triangle: Triangle, triangle_min, triangle_max: [2]int, image: Image) #no_bounds_check {

    v0, v1, v2 := expand_values(triangle.positions)
    a, b, c := expand_values(triangle.vertices)

    // Precompuse inverse area to optimize interpolation.
    inv_area := 1.0 / signed_area(v0.xy, v1.xy, v2.xy)

    // Barycentric interpolation starting points.
    v_min := la.array_cast(triangle_min.xy, f32)
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

            // ---- DEPTH TEST ----

            depth_ptr := raw_data(screen_depth)[(y * screen.size.x) + x:]
            frag_z := interpolate(v0.z, v1.z, v2.z, weights)

            if frag_z > depth_ptr[0] {
                continue // fragment hidden
            }

            // Perspective correct interpolation weights.
            perspective := (weights * {v0.w, v1.w, v2.w}) / interpolate(v0.w, v1.w, v2.w, weights)

            // ---- COMPUTE VARYING (INTERPOLATED) ATTRIBUTES ---

            fragment_normal := interpolate(a.normal, b.normal, c.normal, perspective)
            fragment_uv := interpolate(a.uv, b.uv, c.uv, perspective)

            // ---- FRAGMENT SHADER BEGIN ---

            pixel := image_sample(image, fragment_uv)

            brightness := max(0.0, la.dot(fragment_normal, Sun_Direction))
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
    triangle: Triangle,
    triangle_min, triangle_max: [2]int,
    image: Image,
) #no_bounds_check {

    v0, v1, v2 := expand_values(triangle.positions)
    a, b, c := expand_values(triangle.vertices)

    STEP_SIZES :: (#simd[4]f32){0, 1, 2, 3}

    // Precompuse inverse area to optimize interpolation.
    inv_area := 1.0 / signed_area(v0.xy, v1.xy, v2.xy)

    // SIMD reads and writes need to be 16 byte aligned.
    align_min_x := mem.align_backward_int(triangle_min.x, 4)
    align_max_x := mem.align_forward_int(triangle_max.x, 4)

    // Since SIMD is aligned, we need to offset the incremental barycentric too.
    align_offset := cast(f32)(align_min_x - triangle_min.x)

    v_min := la.array_cast(triangle_min.xy, f32)

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

            // ---- DEPTH TEST ----

            depth_ptr := cast(^#simd[4]f32)raw_data(screen_depth)[(y * screen.size.x) + x:]
            frag_z := interpolate(v0.z, v1.z, v2.z, w0, w1, w2)

            write_mask = write_mask & simd.lanes_lt(frag_z, depth_ptr^)
            if simd.reduce_or(write_mask) == 0 {
                continue // all fragments are hidden
            }

            // Perspective correct interpolation weights.
            w_factor := 1.0 / interpolate(v0.w, v1.w, v2.w, w0, w1, w2)
            w0 = (w0 * v0.w) * w_factor
            w1 = (w1 * v1.w) * w_factor
            w2 = (w2 * v2.w) * w_factor

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
signed_area :: #force_inline proc(a, b, c: [2]f32) -> f32 {
    return (b.x - a.x) * (c.y - a.y) - (b.y - a.y) * (c.x - a.x)
}

@(private = "file")
Triangle :: struct {
    positions: [3]Vector4,
    vertices:  [3]Vertex,
}
