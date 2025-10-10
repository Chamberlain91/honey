#+private
package honey

import la "core:math/linalg"
import "core:mem"
import "core:simd"

@(private)
SIMD_ALIGN :: 16 when simd.HAS_HARDWARE_SIMD else 4

Renderer :: struct {
    tile_size: int,
    tiles:     []Renderer_Tile,
    count:     Size,
}

Renderer_Tile :: struct {
    min, max:  Vector2i,
    triangles: [dynamic]Triangle,
}

renderer_create :: proc(tile_size: int = 64) -> (r: Renderer) {

    r.tile_size = tile_size
    r.count = ((_ctx.framebuffer.size + (tile_size - 1)) / tile_size)
    r.tiles = make([]Renderer_Tile, r.count.x * r.count.y)

    for y in 0 ..< r.count.y {
        for x in 0 ..< r.count.x {

            r_min := Vector2i{x, y} * tile_size
            r_max := la.min(r_min + tile_size, _ctx.framebuffer.size - 1)

            r.tiles[compute_index(x, y, r.count.x)] = Renderer_Tile {
                min = r_min,
                max = r_max,
            }
        }
    }

    return
}

// Submit the triangle to all tiles it touches.
renderer_append_triangle :: proc(renderer: Renderer, triangle: Triangle) #no_bounds_check {

    // Compute the rectangular range of overlapping tiles.
    co_min, co_max := compute_triangle_bounds(triangle, 0, _ctx.framebuffer.size - 1)
    co_min = (co_min) / renderer.tile_size
    co_max = (co_max + (renderer.tile_size - 1)) / renderer.tile_size

    for y in co_min.y ..< co_max.y {
        for x in co_min.x ..< co_max.x {
            tile := &renderer.tiles[compute_index(x, y, renderer.count.x)]
            append(&tile.triangles, triangle)
        }
    }
}

// Waits for this tile to finish rendering its triangles task list.
renderer_wait_tile :: proc(tile: ^Renderer_Tile) {

    // TODO: Actually make async
    for triangle in pop_safe(&tile.triangles) {
        rasterize_triangle(triangle, tile.min, tile.max)
    }
}

// Blocks to ensure all rendering of tiles have completed.
renderer_flush :: proc(renderer: Renderer) {

    for &tile in renderer.tiles {

        // Wait for the rendering thread to exit.
        renderer_wait_tile(&tile)

        // At this point, all triangles in the tile need to be processed.
        assert(len(tile.triangles) == 0)
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

@(private = "file")
signed_area :: #force_inline proc(a, b, c: [2]f32) -> f32 {
    return (b.x - a.x) * (c.y - a.y) - (b.y - a.y) * (c.x - a.x)
}
