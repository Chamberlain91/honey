package honey

import "base:intrinsics"
import sa "core:container/small_array"
@(require) import "core:fmt"
@(require) import "core:math"
import la "core:math/linalg"
import "core:mem"
import "core:simd"
import "core:slice"

@(private)
SIMD_ALIGN :: 16 when simd.HAS_HARDWARE_SIMD else 4

DEFAULT_CLEAR_COLOR: Color : {0x18, 0x18, 0x18, 0xFF}

DEFAULT_CLEAR_DEPTH: f32 : 1.0

@(private)
_ctx: struct {
    debug_text:    string,
    framebuffer:   Framebuffer,
    renderer:      Renderer,
    toggles:       bit_set[Toggle],
    sun_direction: Vector3,
    stats:         struct {
        triangle_count: int,
    },
} = {
    toggles       = {.Backface_Culling},
    sun_direction = la.normalize(Vector3{1, 3, -2}),
}

@(private)
Framebuffer :: struct {
    color:   []Color,
    depth:   []f32,
    using _: struct #raw_union {
        using _: struct {
            width, height: int,
        },
        size:    [2]int,
    },
}

@(private)
allocate_framebuffer :: proc(width, height: int) -> Framebuffer {
    color := mem.make_aligned([]Color, width * height, SIMD_ALIGN) or_else panic("allocate_framebuffer failed.")
    depth := mem.make_aligned([]f32, width * height, SIMD_ALIGN) or_else panic("allocate_framebuffer failed.")
    return {color = color, depth = depth, width = width, height = height}
}

Vertex :: struct {
    position: Vector3,
    normal:   Vector3,
    uv:       Vector2,
}

Mesh :: struct {
    vertices: []Vertex,
    indices:  []int,
}

// Gets the size of the framebuffer (may differ from the window size based on initialization scale).
get_framebuffer_size :: proc() -> [2]int {
    return _ctx.framebuffer.size
}

// Gets the aspect ratio of the framebuffer.
get_framebuffer_aspect :: proc() -> f32 {
    size := get_framebuffer_size()
    return cast(f32)size.x / cast(f32)size.y
}

// Gets the triangle count since last call to `gfx_begin_rendering()`.
get_triangle_count :: proc() -> int {
    return _ctx.stats.triangle_count
}

// TODO: Better name
Toggle :: enum {
    Backface_Culling,
    Disable_SIMD,
}

// TODO: Better name
set_toggle :: proc(toggle: Toggle, state: bool) {
    if state {
        _ctx.toggles += {toggle}
    } else {
        _ctx.toggles -= {toggle}
    }
}

// TODO: Better name
get_toggle :: proc(toggle: Toggle) -> bool {
    return toggle in _ctx.toggles
}

// When `DEV_BUILD` is defined (e.g. debug builds) show this text as part of the overlay information.
set_debug_text :: proc(text: string) {
    _ctx.debug_text = text
}

// Begins rendering, clearing the framebuffer.
begin_rendering :: proc(color := DEFAULT_CLEAR_COLOR, depth: f32 = DEFAULT_CLEAR_DEPTH) {

    slice.fill(_ctx.framebuffer.color, color)
    slice.fill(_ctx.framebuffer.depth, depth)

    _ctx.stats = {}
}

// Ends rendering, flushing the framebuffer to the screen.
end_rendering :: proc() {
    renderer_flush(_ctx.renderer)
    flush_frame()
}

// Draws the specified mesh.
draw_mesh :: proc(mesh: Mesh, image: ^Image, transform: Matrix) #no_bounds_check {

    if len(mesh.indices) > 0 {

        for i := 0; i < len(mesh.indices); i += 3 {

            a := mesh.vertices[mesh.indices[i + 0]]
            b := mesh.vertices[mesh.indices[i + 2]]
            c := mesh.vertices[mesh.indices[i + 1]]

            draw_triangle(a, b, c, image, transform)
        }

    } else {

        for i := 0; i < len(mesh.vertices); i += 3 {

            a := mesh.vertices[i + 0]
            b := mesh.vertices[i + 2]
            c := mesh.vertices[i + 1]

            draw_triangle(a, b, c, image, transform)
        }

    }
}

// Draws the specified triangle.
draw_triangle :: proc(v0, v1, v2: Vertex, image: ^Image, transform: Matrix) #no_bounds_check {

    // World -> Clip
    p0 := transform_vertex(v0, transform)
    p1 := transform_vertex(v1, transform)
    p2 := transform_vertex(v2, transform)

    // Backface culling. Compare normal to "view vector", negative indicates away from viewpoint.
    if get_toggle(.Backface_Culling) && la.dot(la.cross(p1.xyz - p0.xyz, p2.xyz - p0.xyz), p0.xyz) < 0 {
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

    Triangle_Buffer :: sa.Small_Array(3, Triangle)

    // Clip the triangle if it intercepts the near z-plane.
    // This will generate 1 or 2 triangles to render.
    // Other clipping planes are handled by raster clipping.
    @(thread_local)
    triangles: Triangle_Buffer
    sa.clear(&triangles)

    // Reduces the vertex properties to only the ones needed for vertex interpolation.
    v0 := Triangle_Vertex{v0.normal, v0.uv}
    v1 := Triangle_Vertex{v1.normal, v1.uv}
    v2 := Triangle_Vertex{v2.normal, v2.uv}

    if p0.z < 0 {
        if p1.z < 0 {
            clip2({{p0, p1, p2}, {v0, v1, v2}, image}, &triangles)
        } else if p2.z < 0 {
            clip2({{p0, p2, p1}, {v0, v2, v1}, image}, &triangles)
        } else {
            clip1({{p0, p1, p2}, {v0, v1, v2}, image}, &triangles)
        }
    } else if p1.z < 0 {
        if p2.z < 0 {
            clip2({{p1, p2, p0}, {v1, v2, v0}, image}, &triangles)
        } else {
            clip1({{p1, p0, p2}, {v1, v0, v2}, image}, &triangles)
        }
    } else if p2.z < 0 {
        clip1({{p2, p0, p1}, {v2, v0, v1}, image}, &triangles)
    } else {
        // No clipping needed
        sa.append(&triangles, Triangle{{p0, p1, p2}, {v0, v1, v2}, image})
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
            pos.x *= cast(f32)_ctx.framebuffer.width
            pos.y *= cast(f32)_ctx.framebuffer.height
        }

        // Render the triangle!
        // rasterize_triangle(triangle, 0, _ctx.framebuffer.size - 1)

        // Submit triangle to the tiled region.
        renderer_append_triangle(_ctx.renderer, triangle)
    }

    // Count the number of triangles drawn.
    _ctx.stats.triangle_count += sa.len(triangles)

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

        sa.append(output, Triangle{{pA, p1, p2}, {vA, v1, v2}, triangle.image})
        sa.append(output, Triangle{{pB, pA, p2}, {vB, vA, v2}, triangle.image})
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

        sa.append(output, Triangle{{p0, p1, p2}, {v0, v1, v2}, triangle.image})
    }

    interpolate_vertex :: proc(a, b: Triangle_Vertex, t: f32) -> (c: Triangle_Vertex) {
        c.normal = la.lerp(a.normal, b.normal, t)
        c.uv = la.lerp(a.uv, b.uv, t)
        return
    }
}


@(private)
rasterize_triangle :: proc(triangle: Triangle, viewport_min, viewport_max: [2]int) #no_bounds_check {

    range_min, range_max := compute_triangle_bounds(triangle, viewport_min, viewport_max)

    // Render the triangle!
    if !get_toggle(.Disable_SIMD) && simd.HAS_HARDWARE_SIMD {
        rasterize_triangle_simd(triangle, range_min, range_max)
    } else {
        rasterize_triangle_normal(triangle, range_min, range_max)
    }
}

@(private = "file")
rasterize_triangle_normal :: proc(triangle: Triangle, triangle_min, triangle_max: [2]int) #no_bounds_check {

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

    for y in triangle_min.y ..= triangle_max.y {

        weights_col := weights_row
        weights_row += delta_weights_row // ???

        for x in triangle_min.x ..= triangle_max.x {

            weights := weights_col * inv_area
            weights_col += delta_weights_col // ???

            // Fragment is outside the triangle.
            if weights[0] < 0 || weights[1] < 0 || weights[2] < 0 {
                continue // fragment outside triangle
            }

            // ---- DEPTH TEST ----

            frag_index := compute_index(x, y, _ctx.framebuffer.width)
            frag_z := interpolate(v0.z, v1.z, v2.z, weights)

            depth_ptr := raw_data(_ctx.framebuffer.depth)[frag_index:]

            if frag_z > depth_ptr[0] {
                continue // fragment hidden
            }

            color_ptr := raw_data(_ctx.framebuffer.color)[frag_index:]

            // Perspective correct interpolation weights.
            perspective := (weights * {v0.w, v1.w, v2.w}) / interpolate(v0.w, v1.w, v2.w, weights)

            // ---- COMPUTE VARYING (INTERPOLATED) ATTRIBUTES ---

            fragment_normal := interpolate(a.normal, b.normal, c.normal, perspective)
            fragment_uv := interpolate(a.uv, b.uv, c.uv, perspective)

            // ---- FRAGMENT SHADER BEGIN ---

            pixel := image_sample(triangle.image^, fragment_uv)

            brightness := max(0.0, la.dot(fragment_normal, _ctx.sun_direction))
            pixel = mix_color(0, pixel, 0.25 + (brightness * 0.75))

            // ---- FRAGMENT SHADER END ---

            pixel.a = 0xFF // no transparency

            // Write to framebuffer.
            color_ptr[0], depth_ptr[0] = pixel, frag_z
        }
    }

    interpolate :: proc(a, b, c: $T, weights: [3]f32) -> T {
        return (a * weights[0]) + (b * weights[1]) + (c * weights[2])
    }
}

@(private = "file")
rasterize_triangle_simd :: proc(triangle: Triangle, triangle_min, triangle_max: Vector2i) #no_bounds_check {

    v0, v1, v2 := expand_values(triangle.positions)
    a, b, c := expand_values(triangle.vertices)

    STEP_SIZES :: (#simd[4]f32){0, 1, 2, 3}

    // Precompuse inverse area to optimize interpolation.
    inv_area := 1.0 / signed_area(v0.xy, v1.xy, v2.xy)

    // SIMD reads and writes need to be 16 byte aligned.
    align_min_x := mem.align_backward_int(triangle_min.x, 4)
    align_max_x := mem.align_backward_int(triangle_max.x, 4)

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

    for y in triangle_min.y ..= triangle_max.y {

        weights_col_0 := weights_row_0 + (align_offset * v12.y)
        weights_col_1 := weights_row_1 + (align_offset * v20.y)
        weights_col_2 := weights_row_2 + (align_offset * v01.y)

        weights_row_0 -= v12.x // ???
        weights_row_1 -= v20.x
        weights_row_2 -= v01.x

        for x := align_min_x; x <= align_max_x; x += 4 {

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

            frag_index := compute_index(x, y, _ctx.framebuffer.width)
            frag_z := interpolate(v0.z, v1.z, v2.z, w0, w1, w2)

            depth_ptr := cast(^#simd[4]f32)raw_data(_ctx.framebuffer.depth)[frag_index:]

            write_mask = write_mask & simd.lanes_lt(frag_z, depth_ptr^)
            if simd.reduce_or(write_mask) == 0 {
                continue // all fragments are hidden
            }

            // Perspective correct interpolation weights.
            w_factor := 1.0 / interpolate(v0.w, v1.w, v2.w, w0, w1, w2)
            w0 = (w0 * v0.w) * w_factor
            w1 = (w1 * v1.w) * w_factor
            w2 = (w2 * v2.w) * w_factor

            color_ptr := cast(^#simd[4]u32)raw_data(_ctx.framebuffer.color)[frag_index:]

            // ---- COMPUTE VARYING (INTERPOLATED) ATTRIBUTES ---

            NX := interpolate(a.normal.x, b.normal.x, c.normal.x, w0, w1, w2)
            NY := interpolate(a.normal.y, b.normal.y, c.normal.y, w0, w1, w2)
            NZ := interpolate(a.normal.z, b.normal.z, c.normal.z, w0, w1, w2)

            U := interpolate(a.uv.x, b.uv.x, c.uv.x, w0, w1, w2)
            V := interpolate(a.uv.y, b.uv.y, c.uv.y, w0, w1, w2)

            // ---- FRAGMENT SHADER BEGIN ---

            R, G, B, A := convert_u32_to_f32_simd(image_sample_simd(triangle.image^, U, V))

            brightness := 0.25 + (simd.max((#simd[4]f32)(0), dot(NX, NY, NZ, _ctx.sun_direction)) * 0.75)

            R *= brightness
            G *= brightness
            B *= brightness

            pixels := convert_f32_to_u32_simd(R, G, B, A)

            // ---- FRAGMENT SHADER END ---

            // Write color.
            color_ptr^ = (pixels & write_mask) | (color_ptr^ &~ write_mask)

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

@(private)
compute_triangle_bounds :: proc(triangle: Triangle, viewport_min, viewport_max: Vector2i) -> (r_min, r_max: Vector2i) {

    p0, p1, p2 := expand_values(triangle.positions)

    // Find triangle bounds...
    r_min = la.array_cast(la.min(p0.xy, p1.xy, p2.xy), int)
    r_max = la.array_cast(la.max(p0.xy, p1.xy, p2.xy), int)

    // Clamp rasterization bounds to the viewport.
    r_min = la.clamp(r_min, viewport_min, viewport_max)
    r_max = la.clamp(r_max, viewport_min, viewport_max)

    return
}

@(private)
compute_index :: #force_inline proc(x, y, width: int) -> int {
    return (y * width) + x
}

@(private = "file")
signed_area :: #force_inline proc(a, b, c: [2]f32) -> f32 {
    return (b.x - a.x) * (c.y - a.y) - (b.y - a.y) * (c.x - a.x)
}

@(private)
Triangle :: struct {
    positions: [3]Vector4,
    vertices:  [3]Triangle_Vertex,
    image:     ^Image,
}

@(private)
Triangle_Vertex :: struct {
    normal: Vector3,
    uv:     Vector2,
}
