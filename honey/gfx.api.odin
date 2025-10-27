package honey

import "base:intrinsics"
import "base:runtime"
import "core:fmt"
import "core:math"
import la "core:math/linalg"
import "core:mem"
import "core:slice"
import "core:sync"
import sysinfo "core:sys/info"
import "core:thread"
import "core:time"

DEFAULT_CLEAR_COLOR: Color : {0x18, 0x18, 0x18, 0xFF}

Vertex :: struct {
    position: Vector3,
    normal:   Vector3,
    uv:       Vector2,
}

Mesh :: struct {
    vertices: []Vertex,
    indices:  []int,
}

@(private)
thread_init :: proc() {

    fmt.printfln("[SYSTEM] OS: {}", sysinfo.os_version.as_string)
    fmt.printfln(
        "[SYSTEM] CPU: {} ({} physical, {} logical)",
        sysinfo.cpu.name,
        sysinfo.cpu.physical_cores,
        sysinfo.cpu.logical_cores,
    )
    fmt.printfln("[SYSTEM] RAM: {:M}", sysinfo.ram.total_ram)

    // We use N - 1 to give leeway for the main thread.
    thread.pool_init(&_ctx.pool, context.allocator, max(3, sysinfo.cpu.logical_cores - 1))
    thread.pool_start(&_ctx.pool)

    fmt.printfln("[INFO] Started thread pool with {} workers.", len(_ctx.pool.threads))
}

@(private)
thread_kill :: proc() {
    thread.pool_shutdown(&_ctx.pool)
    thread.pool_destroy(&_ctx.pool)
}

// Gets the size of the framebuffer (may differ from the window size based on initialization scale).
get_framebuffer_size :: proc() -> Vector2i {
    return _ctx.framebuffer.size
}

// Gets the aspect ratio of the framebuffer.
get_framebuffer_aspect :: proc() -> f32 {
    size := get_framebuffer_size()
    return cast(f32)size.x / cast(f32)size.y
}

// Gets the triangle count since last call to `begin_rendering()`.
get_triangle_count :: proc() -> int {
    return _ctx.stats.triangle_count
}

// Gets time spent projecting triangles to screen (in milliseconds).
get_vertex_duration :: proc() -> f64 {
    return _ctx.stats.vertex_duration
}

// Gets time spent rasterizing triangles (in milliseconds).
get_raster_duration :: proc() -> f64 {
    return _ctx.stats.rasterization_duration
}

// Gets time spent dispatching triangles to threads (in milliseconds).
get_dispatch_duration :: proc() -> f64 {
    return _ctx.stats.dispatch_duration
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
get_toggle :: proc "contextless" (toggle: Toggle) -> bool {
    return toggle in _ctx.toggles
}

// Begins rendering, clearing the framebuffer.
begin_rendering :: proc(color := DEFAULT_CLEAR_COLOR) {

    profile_scoped_event(#procedure)

    if profile_scoped_event(#procedure + ":color") {
        // TODO: Perhaps slice.zero (much faster)
        // slice.fill(_ctx.framebuffer.color, color)
        slice.zero(_ctx.framebuffer.color)
    }

    if profile_scoped_event(#procedure + ":depth") {
        // TODO: Perhaps slice.zero (much faster)
        slice.fill(_ctx.framebuffer.depth, 1.0)
    }

    // Each frame will have a new triangle list.
    clear(&_ctx.renderer.triangles)

    _ctx.stats.vertex_start_time = time.now()
    _ctx.stats.triangle_count = 0
}

// Ends rendering, flushing the framebuffer to the screen.
end_rendering :: proc() {

    profile_scoped_event(#procedure)

    // Wait for all triangles to be processed.
    sync.wait_group_wait(&_ctx.renderer.wait_group)

    // Vertex processing stage is now 'complete'.
    update_stat(&_ctx.stats.vertex_duration, time.since(_ctx.stats.vertex_start_time))

    // Count the number of triangles drawn.
    _ctx.stats.triangle_count += len(_ctx.renderer.triangles)

    // Process all triangles.
    flush_renderer(&_ctx.renderer)

    window_flush_content()
}

// Draws the specified model.
draw_model :: proc(model: Model, transform: Matrix) #no_bounds_check {
    for &x in soa_zip(mesh = model.meshes, image = model.textures) {
        draw_mesh_indexed(x.mesh, &x.image, transform)
    }
}

// Draws the specified mesh texture image.
draw_mesh_indexed :: proc(mesh: Mesh, image: ^Texture, transform: Matrix) #no_bounds_check {

    profile_scoped_event(#procedure)

    BATCH_SIZE :: 1024 * 3

    // Process batches of triangles in threads.
    for i := 0; i < len(mesh.indices); i += BATCH_SIZE {
        run_async(
            process_triangle_task,
            &_ctx.renderer.wait_group,
            Triangle_Task_Info {
                mesh = mesh,
                image = image,
                begin = i,
                end = min(i + BATCH_SIZE, len(mesh.indices) - 1),
                transform = transform,
            },
        )
    }

    Triangle_Task_Info :: struct {
        mesh:      Mesh,
        image:     ^Texture,
        begin:     int,
        end:       int,
        transform: Matrix,
    }

    process_triangle_task :: proc "contextless" (info: Triangle_Task_Info) {

        profile_scoped_event(#procedure)

        @(thread_local)
        batch: [dynamic]Triangle
        clear(&batch)

        for i := info.begin; i < info.end; i += 3 {
            a := compute_vertex(info.mesh.vertices[info.mesh.indices[i + 0]], info.transform)
            b := compute_vertex(info.mesh.vertices[info.mesh.indices[i + 2]], info.transform) // TODO: ???
            c := compute_vertex(info.mesh.vertices[info.mesh.indices[i + 1]], info.transform)
            process_triangle({a, b, c}, info.image, &batch)
        }

        // Submit triangle batch to rasterizer pending list.
        // TODO: Is it possible to safely assign these triangles directly to the raster tiles?
        //       Compute local clusters, then atomic lock and append?
        sync.guard(&_ctx.renderer.triangles_mutex)

        context = runtime.default_context()
        append(&_ctx.renderer.triangles, ..batch[:])
    }

    transform_vertex :: proc "contextless" (vertex: Vertex, transform: Matrix) -> (clip_position: Vector4) {
        clip_position.xyz = vertex.position
        clip_position.w = 1.0
        return transform * clip_position
    }

    compute_vertex :: proc "contextless" (vertex: Vertex, transform: Matrix) -> (out: VS_Out) {
        out = VS_Out {
            position = transform_vertex(vertex, transform),
            normal   = vertex.normal,
            uv       = vertex.uv,
        }
        return
    }
}

@(private)
process_triangle :: proc "contextless" (
    vertices: [3]VS_Out,
    image: ^Texture,
    output: ^[dynamic]Triangle,
) #no_bounds_check {

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

    p0, p1, p2 := vertices[0].position, vertices[1].position, vertices[2].position

    // Reject the triangle if it is completely out of view (beyond frustum).
    for axis in 0 ..< 3 do if check_outside_view(p0, p1, p2, axis) {
        return
    }

    is_clip0 := p0.z < 0.001
    is_clip1 := p1.z < 0.001
    is_clip2 := p2.z < 0.001

    switch int(is_clip0) + int(is_clip1) + int(is_clip2) {

    case 0:
        append_triangle(expand_values(vertices), image, output)

    case 1:
        i0 := is_clip0 ? 0 : is_clip1 ? 1 : 2
        i1 := (i0 + 1) % 3
        i2 := (i0 + 2) % 3

        c0, c1, c2 := vertices[i0], vertices[i1], vertices[i2]
        z0, z1, z2 := c0.position.z, c1.position.z, c2.position.z

        vA := interpolate_vertex(c0, c1, -z0 / (z1 - z0))
        vB := interpolate_vertex(c0, c2, -z0 / (z2 - z0))

        append_triangle(vB, vA, c2, image, output)
        append_triangle(vA, c1, c2, image, output)

    case 2:
        i0 := !is_clip0 ? 0 : !is_clip1 ? 1 : 2
        i1 := (i0 + 1) % 3
        i2 := (i0 + 2) % 3

        c0, c1, c2 := vertices[i0], vertices[i1], vertices[i2]
        z0, z1, z2 := c0.position.z, c1.position.z, c2.position.z

        vA := interpolate_vertex(c0, c1, -z0 / (z1 - z0))
        vB := interpolate_vertex(c0, c2, -z0 / (z2 - z0))

        append_triangle(vB, c0, vA, image, output)
    }

    append_triangle :: proc "contextless" (x0, x1, x2: VS_Out, image: ^Texture, output: ^[dynamic]Triangle) {

        v0, v1, v2 := map_to_viewport(x0), map_to_viewport(x1), map_to_viewport(x2)

        // Determine if this face is a backface (culled).
        is_backface := la.cross(v1.position.xy - v0.position.xy, v2.position.xy - v0.position.xy) < 0
        if get_toggle(.Backface_Culling) && is_backface {
            return
        }

        v_min, v_max := compute_triangle_bounds(v0, v1, v2)

        context = runtime.default_context()
        append(output, Triangle{{v0, v1, v2}, image, v_min, v_max})
    }

    map_to_viewport :: proc "contextless" (vertex: VS_Out) -> VS_Out {

        output := vertex

        // Perspective divide
        output.position.w = 1.0 / output.position.w
        output.position.xyz *= output.position.w

        // NDC -> Viewport
        output.position.xy = ((output.position.xy + 1.0) / 2.0) * _ctx.framebuffer.size_float

        return output
    }

    interpolate_vertex :: proc "contextless" (a, b: VS_Out, t: f32) -> (c: VS_Out) {
        c.position = la.lerp(a.position, b.position, t)
        c.normal = la.lerp(a.normal, b.normal, t)
        c.uv = la.lerp(a.uv, b.uv, t)
        return
    }

    check_outside_view :: proc "contextless" (p0, p1, p2: Vector4, AXIS: int) -> bool {
        if test(p0, p1, p2, AXIS, +1) do return true
        if test(p0, p1, p2, AXIS, -1) do return true
        return false

        test :: proc "contextless" (p0, p1, p2: Vector4, AXIS: int, $SIGN: f32) -> bool {
            return (p0[AXIS] * SIGN) > p0.w && (p1[AXIS] * SIGN) > p1.w && (p2[AXIS] * SIGN) > p2.w
        }
    }

    compute_triangle_bounds :: proc "contextless" (v0, v1, v2: VS_Out) -> (v_min, v_max: Vector2i) {

        p0 := la.array_cast(v0.position.xy, int)
        p1 := la.array_cast(v1.position.xy, int)
        p2 := la.array_cast(v2.position.xy, int)

        v_min = la.min(p0, p1, p2)
        v_max = la.max(p0, p1, p2)

        return
    }
}

// When `DEV_BUILD` is defined (e.g. debug builds) show this text as part of the overlay information.
set_debug_text :: proc(text: string) {
    _ctx.debug_text = text
}

@(private)
VS_Out :: struct {
    position: Vector4, // gl_Position
    normal:   Vector3,
    uv:       Vector2,
}

// FS_In
@(private)
Triangle :: struct {
    vertices: [3]VS_Out,
    texture:    ^Texture,
    min, max: Vector2i,
}

@(private)
_ctx: struct {
    debug_text:    string,
    framebuffer:   Framebuffer,
    renderer:      Renderer,
    toggles:       bit_set[Toggle],
    sun_direction: Vector3,
    stats:         struct {
        dispatch_duration:      f64,
        rasterization_duration: f64,
        vertex_start_time:      time.Time,
        vertex_duration:        f64,
        triangle_count:         int,
    },
    // vertex_cache:  [dynamic]VS_Out,
    pool:          thread.Pool,
} = {
    toggles       = {.Backface_Culling},
    sun_direction = la.normalize(Vector3{1, 3, -2}),
}

@(private)
Framebuffer :: struct {
    color:      []Color,
    depth:      []f32,
    using _:    struct #raw_union {
        using _: struct {
            width, height: int,
        },
        size:    Vector2i,
    },
    size_float: [2]f32,
}

@(private)
allocate_framebuffer :: proc(size: Vector2i) -> Framebuffer {
    color := mem.make_aligned([]Color, size.x * size.y, SIMD_ALIGN) or_else panic("allocate_framebuffer failed.")
    depth := mem.make_aligned([]f32, size.x * size.y, SIMD_ALIGN) or_else panic("allocate_framebuffer failed.")
    return {color = color, depth = depth, size = size, size_float = la.array_cast(size, f32)}
}

@(private)
delete_framebuffer :: proc(framebuffer: Framebuffer) {
    delete(framebuffer.color)
    delete(framebuffer.depth)
}

@(private)
compute_index :: proc "contextless" (x, y, width: int) -> int {
    return (y * width) + x
}

@(private)
update_stat :: proc(old_value: ^f64, duration: time.Duration, factor: f64 = 0.1) {
    old_value^ = math.lerp(old_value^, time.duration_milliseconds(duration), factor)
}
