#+private
package honey

import "base:runtime"
import "core:fmt"
import "core:math"
import la "core:math/linalg"
import "core:simd"
import "core:sync"
import "core:thread"
import "core:time"

@(private)
SIMD_ALIGN :: 16 when simd.HAS_HARDWARE_SIMD else 4

Renderer :: struct {
    // Size of the tile (in pixels).
    tile_size:       int,
    // List of tiles contained by the renderer.
    tiles:           []Raster_Tile,
    // Number of tiles in each axis.
    tile_count:      Size,
    // Triangles waiting to be dispatched (work list).
    triangles:       [dynamic]Triangle,
    // ...
    triangles_mutex: sync.Atomic_Mutex,
    // Wait group for dispatched tiles.
    wait_group:      sync.Wait_Group,
}

Raster_Tile :: struct {
    // Bounds of the tile (in pixels).
    min, max:  Vector2i,
    // Work list of the tile.
    triangles: [dynamic]^Triangle,
}

create_renderer :: proc(tile_size: int) -> (renderer: Renderer) {

    renderer.tile_size = tile_size
    renderer.tile_count = ((_ctx.framebuffer.size + (tile_size - 1)) / tile_size)
    renderer.tiles = make([]Raster_Tile, renderer.tile_count.x * renderer.tile_count.y)

    for y in 0 ..< renderer.tile_count.y {
        for x in 0 ..< renderer.tile_count.x {

            r_min := Vector2i{x, y} * tile_size
            r_max := la.min(r_min + tile_size, _ctx.framebuffer.size - 1)

            tile := &renderer.tiles[compute_index(x, y, renderer.tile_count.x)]

            tile^ = Raster_Tile {
                min = r_min,
                max = r_max,
            }
        }
    }

    fmt.printfln(
        "[INFO] Created {} raster tile groups ({}x{}).",
        len(renderer.tiles),
        expand_values(renderer.tile_count),
    )

    return
}

delete_renderer :: proc(renderer: Renderer) {

    for tile in renderer.tiles {
        delete(tile.triangles)
    }

    delete(renderer.triangles)
    delete(renderer.tiles)
}

flush_renderer :: proc(renderer: ^Renderer) {

    profile_scoped_event(#procedure)

    dispatch_start_time := time.now()

    // Submit triangle to overlapping tiles.
    profile_scope_begin(#procedure + ":submit_to_tiles")
    for &triangle in renderer.triangles {
        submit_to_tiles(renderer, &triangle)
    }
    profile_scope_end()

    profile_scope_begin(#procedure + ":launch")

    Rasterize_Task_Info :: struct {
        renderer: ^Renderer,
        tile:     ^Raster_Tile,
    }

    // Schedule rendering each tile.
    for &tile in renderer.tiles {
        if len(tile.triangles) == 0 do continue
        run_async(
            rasterize_tile_task,
            &renderer.wait_group,
            Rasterize_Task_Info {     //
                renderer = renderer,
                tile     = &tile,
            },
        )
    }

    profile_scope_end()
    profile_scope_begin(#procedure + ":wait")

    // Record the time since start of submit and dispatch of tiles.
    update_stat(&_ctx.stats.dispatch_duration, time.since(dispatch_start_time))

    rasterization_start_time := time.now()

    // Wait for all tiles to complete.
    sync.wait_group_wait(&renderer.wait_group)

    // Record the time spend waiting for rasterization tasks.
    update_stat(&_ctx.stats.rasterization_duration, time.since(rasterization_start_time))

    profile_scope_end()

    submit_to_tiles :: proc(renderer: ^Renderer, triangle: ^Triangle) #no_bounds_check {

        // Compute the rectangular range of overlapping tiles.
        co_min := (triangle.min) / renderer.tile_size
        co_max := (triangle.max + (renderer.tile_size - 1)) / renderer.tile_size

        for y in max(0, co_min.y) ..< min(co_max.y, renderer.tile_count.y) {
            for x in max(0, co_min.x) ..< min(co_max.x, renderer.tile_count.x) {
                tile := &renderer.tiles[compute_index(x, y, renderer.tile_count.x)]
                append(&tile.triangles, triangle)
            }
        }
    }

    rasterize_tile_task :: proc "contextless" (info: Rasterize_Task_Info) {

        profile_scoped_event(#procedure)

        for triangle in info.tile.triangles {
            rasterize_triangle(triangle, info.tile.min, info.tile.max)
        }

        clear(&info.tile.triangles)
    }
}

@(private)
rasterize_triangle :: proc "contextless" (triangle: ^Triangle, range_min, range_max: Vector2i) #no_bounds_check {

    // Clamp rasterization to range (preventing out-of-bounds access & raster "clipping planes")
    triangle_min, triangle_max := triangle.min, triangle.max
    triangle_min = la.clamp(triangle_min, range_min, range_max)
    triangle_max = la.clamp(triangle_max, range_min, range_max)

    // Render the triangle!
    if !get_toggle(.Disable_SIMD) && simd.HAS_HARDWARE_SIMD {
        rasterize_triangle_simd(triangle, triangle_min, triangle_max)
    } else {
        rasterize_triangle_normal(triangle, triangle_min, triangle_max)
    }
}

@(private = "file")
rasterize_triangle_normal :: proc "contextless" (
    triangle: ^Triangle,
    triangle_min, triangle_max: Vector2i,
) #no_bounds_check {

    v0, v1, v2 := expand_values(triangle.vertices)
    p0, p1, p2 := v0.position, v1.position, v2.position

    // Precompuse inverse area to optimize interpolation.
    inv_area := 1.0 / signed_area(p0.xy, p1.xy, p2.xy)

    // Computes mip level by approximation of triangle screen area rather than uv gradient.
    uv_size := la.array_cast(triangle.texture.mips[0].size, f32)
    lod_ratio := abs(signed_area(v0.uv * uv_size, v1.uv * uv_size, v2.uv * uv_size)) * inv_area
    lod := max(0.0, math.log2(lod_ratio) * 0.5)

    mip_level := cast(int)math.floor(lod + 0.5)
    mip_level = clamp(mip_level, 0, len(triangle.texture.mips) - 1)

    // Barycentric interpolation starting points.
    v_min := la.array_cast(triangle_min.xy, f32)
    weights_row := Vector3 {
        signed_area(p1.xy, p2.xy, v_min),
        signed_area(p2.xy, p0.xy, v_min),
        signed_area(p0.xy, p1.xy, v_min),
    }

    // Incremental barycentric interpolation step factors.
    delta_weights_col: Vector3 = {p1.y - p2.y, p2.y - p0.y, p0.y - p1.y}
    delta_weights_row: Vector3 = {p2.x - p1.x, p0.x - p2.x, p1.x - p0.x}

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
            frag_z := interpolate(p0.z, p1.z, p2.z, weights)

            depth_ptr := raw_data(_ctx.framebuffer.depth)[frag_index:]

            if frag_z > depth_ptr[0] {
                continue // fragment hidden
            }

            color_ptr := raw_data(_ctx.framebuffer.color)[frag_index:]

            // Perspective correct interpolation weights.
            perspective := (weights * {p0.w, p1.w, p2.w}) / interpolate(p0.w, p1.w, p2.w, weights)

            // ---- COMPUTE VARYING (INTERPOLATED) ATTRIBUTES ---

            fragment_normal := interpolate(v0.normal, v1.normal, v2.normal, perspective)
            fragment_uv := interpolate(v0.uv, v1.uv, v2.uv, perspective)

            // ---- FRAGMENT SHADER BEGIN ---

            pixel := texture_sample(triangle.texture^, mip_level, fragment_uv)
            if pixel.a == 0 {
                continue // alphg clipping (discard pixel)
            }

            brightness := max(0.0, la.dot(fragment_normal, _ctx.sun_direction))
            pixel = mix_color(0, pixel, 0.25 + (brightness * 0.75))

            // ---- FRAGMENT SHADER END ---

            pixel.a = 0xFF // no transparency

            // Write to framebuffer.
            color_ptr[0], depth_ptr[0] = pixel, frag_z
        }
    }

    interpolate :: proc "contextless" (a, b, c: $T, weights: Vector3) -> T {
        return (a * weights[0]) + (b * weights[1]) + (c * weights[2])
    }
}

@(private = "file")
rasterize_triangle_simd :: proc "contextless" (
    triangle: ^Triangle,
    triangle_min, triangle_max: Vector2i,
) #no_bounds_check {

    v0, v1, v2 := expand_values(triangle.vertices)
    p0, p1, p2 := v0.position, v1.position, v2.position

    STEP_SIZES: #simd[4]f32 : {0, 1, 2, 3}

    // Precompuse inverse area to optimize interpolation.
    inv_area := 1.0 / signed_area(p0.xy, p1.xy, p2.xy)

    // Computes mip level by approximation of triangle screen area rather than uv gradient.
    uv_size := la.array_cast(triangle.texture.mips[0].size, f32)
    lod_ratio := abs(signed_area(v0.uv * uv_size, v1.uv * uv_size, v2.uv * uv_size)) * inv_area
    lod := max(0.0, math.log2(lod_ratio) * 0.5)

    mip_level := cast(int)math.floor(lod + 0.5)
    mip_level = clamp(mip_level, 0, len(triangle.texture.mips) - 1)

    // SIMD reads and writes need to be 16 byte aligned.
    align_min_x := align_backward(triangle_min.x, 4)
    align_max_x := align_backward(triangle_max.x, 4)

    // Since SIMD is aligned, we need to offset the incremental barycentric too.
    align_offset := cast(f32)(align_min_x - triangle_min.x)

    v_min := la.array_cast(triangle_min.xy, f32)

    // Barycentric interpolation starting points.
    weights_row_0 := signed_area(p1.xy, p2.xy, v_min)
    weights_row_1 := signed_area(p2.xy, p0.xy, v_min)
    weights_row_2 := signed_area(p0.xy, p1.xy, v_min)

    p12 := p1 - p2
    p20 := p2 - p0
    p01 := p0 - p1

    steps_v12y := p12.y * STEP_SIZES
    steps_v20y := p20.y * STEP_SIZES
    steps_v01y := p01.y * STEP_SIZES

    for y in triangle_min.y ..= triangle_max.y {

        weights_col_0 := weights_row_0 + (align_offset * p12.y)
        weights_col_1 := weights_row_1 + (align_offset * p20.y)
        weights_col_2 := weights_row_2 + (align_offset * p01.y)

        weights_row_0 -= p12.x // ???
        weights_row_1 -= p20.x
        weights_row_2 -= p01.x

        for x := align_min_x; x <= align_max_x; x += 4 {

            w0 := (weights_col_0 + steps_v12y) * inv_area
            w1 := (weights_col_1 + steps_v20y) * inv_area
            w2 := (weights_col_2 + steps_v01y) * inv_area

            weights_col_0 += p12.y * 4
            weights_col_1 += p20.y * 4
            weights_col_2 += p01.y * 4

            write_mask: #simd[4]u32 = ~u32(0)
            write_mask = write_mask & simd.lanes_ge(w0, 0)
            write_mask = write_mask & simd.lanes_ge(w1, 0)
            write_mask = write_mask & simd.lanes_ge(w2, 0)

            if simd.reduce_or(write_mask) == 0 {
                continue // all fragments outside triangle
            }

            // ---- DEPTH TEST ----

            frag_index := compute_index(x, y, _ctx.framebuffer.width)
            frag_z := interpolate(p0.z, p1.z, p2.z, w0, w1, w2)

            depth_ptr := cast(^#simd[4]f32)raw_data(_ctx.framebuffer.depth)[frag_index:]

            write_mask = write_mask & simd.lanes_lt(frag_z, depth_ptr^)
            if simd.reduce_or(write_mask) == 0 {
                continue // all fragments are hidden
            }

            // Perspective correct interpolation weights.
            w_factor := 1.0 / interpolate(p0.w, p1.w, p2.w, w0, w1, w2)
            w0 = (w0 * p0.w) * w_factor
            w1 = (w1 * p1.w) * w_factor
            w2 = (w2 * p2.w) * w_factor

            color_ptr := cast(^#simd[4]u32)raw_data(_ctx.framebuffer.color)[frag_index:]

            // ---- COMPUTE VARYING (INTERPOLATED) ATTRIBUTES ---

            NX := interpolate(v0.normal.x, v1.normal.x, v2.normal.x, w0, w1, w2)
            NY := interpolate(v0.normal.y, v1.normal.y, v2.normal.y, w0, w1, w2)
            NZ := interpolate(v0.normal.z, v1.normal.z, v2.normal.z, w0, w1, w2)

            U := interpolate(v0.uv.x, v1.uv.x, v2.uv.x, w0, w1, w2)
            V := interpolate(v0.uv.y, v1.uv.y, v2.uv.y, w0, w1, w2)

            // ---- FRAGMENT SHADER BEGIN ---

            R, G, B, A := convert_u32_to_f32_simd(texture_sample_simd(triangle.texture^, mip_level, U, V))
            write_mask = write_mask & simd.lanes_ne(A, 0) // alpha clipping (discard pixel)

            brightness := 0.25 + (simd.max(cast(#simd[4]f32)0, dot(NX, NY, NZ, _ctx.sun_direction)) * 0.75)

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

    interpolate :: proc "contextless" (a, b, c, w0, w1, w2: #simd[4]f32) -> #simd[4]f32 {
        return (a * w0) + (b * w1) + (c * w2)
    }

    dot :: proc "contextless" (ax, ay, az: #simd[4]f32, b: Vector3) -> #simd[4]f32 {
        return interpolate(ax, ay, az, b.x, b.y, b.z)
    }

    convert_f32_to_u32_simd :: proc "contextless" (R, G, B, A: #simd[4]f32) -> #simd[4]u32 {
        Ri := cast(#simd[4]u32)(R * 0xFF)
        Gi := simd.shl(cast(#simd[4]u32)(G * 0xFF), 8)
        Bi := simd.shl(cast(#simd[4]u32)(B * 0xFF), 16)
        Ai := simd.shl(cast(#simd[4]u32)(A * 0xFF), 24)
        return Ri | Gi | Bi | Ai
    }

    convert_u32_to_f32_simd :: proc "contextless" (color: #simd[4]u32) -> (R, G, B, A: #simd[4]f32) {
        R = (cast(#simd[4]f32)(color & 0xFF)) / 0xFF
        G = (cast(#simd[4]f32)(simd.shr(color, 8) & 0xFF)) / 0xFF
        B = (cast(#simd[4]f32)(simd.shr(color, 16) & 0xFF)) / 0xFF
        A = (cast(#simd[4]f32)(simd.shr(color, 24) & 0xFF)) / 0xFF
        return
    }

    @(require_results)
    align_backward :: proc "contextless" (ptr, align: int) -> int {
        return ptr & ~(align - 1)
    }
}

@(private = "file")
signed_area :: proc "contextless" (a, b, c: [2]f32) -> f32 {
    return (b.x - a.x) * (c.y - a.y) - (b.y - a.y) * (c.x - a.x)
}

@(private)
run_async :: #force_inline proc(task: proc "contextless" (data: $D), group: ^sync.Wait_Group, data: D) {

    profile_scoped_event(#procedure)

    assert(group != nil)
    assert(task != nil)

    Async_Task_Info :: struct {
        procedure: proc "contextless" (_: D),
        data:      D,
        group:     ^sync.Wait_Group,
    }

    task_info := Async_Task_Info {
        procedure = task,
        data      = data,
        group     = group,
    }

    // This group will have 1 unit of work to do.
    sync.wait_group_add(group, 1)

    // Submit that unit of work.
    thread.pool_add_task(&_ctx.pool, context.allocator, do_task, new_clone(task_info, context.allocator))

    do_task :: proc(task: thread.Task) {

        // Execute the unit of work.
        info := cast(^Async_Task_Info)task.data
        info.procedure(info.data)

        // That unit of work is complete.
        sync.wait_group_done(info.group)

        // Free the task parameters.
        free(info)
    }
}
