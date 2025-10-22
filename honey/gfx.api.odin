package honey

import "base:intrinsics"
import "core:fmt"
import "core:math"
import la "core:math/linalg"
import "core:mem"
import "core:os"
import "core:slice"
import "core:thread"
import "core:time"

DEFAULT_CLEAR_COLOR: Color : {0x18, 0x18, 0x18, 0xFF}

DEFAULT_CLEAR_DEPTH: f32 : 1.0

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

    // We use N - 1 to give leeway for the main thread.
    thread.pool_init(&_ctx.pool, context.allocator, os.processor_core_count() - 1)
    thread.pool_start(&_ctx.pool)

    fmt.printfln("[INFO] Started thread pool with {} workers.", len(_ctx.pool.threads))
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

// Gets time spent projecting triangles to screen (in miliseconds).
get_vertex_duration :: proc() -> f64 {
    return _ctx.stats.vertex_duration
}

// Gets time spent rasterizing triangles (in miliseconds).
get_raster_duration :: proc() -> f64 {
    // Single-threaded mode doesn't have a dispatch step, so we actually measured rasterization.
    return get_toggle(.Multithreading) ? _ctx.stats.rasterization_duration : _ctx.stats.dispatch_duration
}

// Gets time spent dispatching triangles to threads (in miliseconds).
get_dispatch_duration :: proc() -> f64 {
    // Single-threaded mode doesn't have a dispatch step.
    return get_toggle(.Multithreading) ? _ctx.stats.dispatch_duration : 0
}

// TODO: Better name
Toggle :: enum {
    Backface_Culling,
    Multithreading,
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
begin_rendering :: proc(color := DEFAULT_CLEAR_COLOR, depth: f32 = DEFAULT_CLEAR_DEPTH) {

    slice.fill(_ctx.framebuffer.color, color)
    slice.fill(_ctx.framebuffer.depth, depth)

    // Each frame will have a new triangle list.
    clear(&_ctx.renderer.triangles)

    _ctx.stats.vertex_start_time = time.now()
    _ctx.stats.triangle_count = 0
}

// Ends rendering, flushing the framebuffer to the screen.
end_rendering :: proc() {

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
draw_mesh_indexed :: proc(mesh: Mesh, image: ^Image, transform: Matrix) #no_bounds_check {

    // Transform the entire mesh vertex set.
    resize(&_ctx.vertex_cache, len(mesh.vertices))
    for v, i in mesh.vertices {
        _ctx.vertex_cache[i] = VS_Out {
            position = transform_vertex(v, transform),
            normal   = v.normal,
            uv       = v.uv,
        }
    }

    // Process each triangle.
    for i := 0; i < len(mesh.indices); i += 3 {
        a := _ctx.vertex_cache[mesh.indices[i + 0]]
        b := _ctx.vertex_cache[mesh.indices[i + 2]]
        c := _ctx.vertex_cache[mesh.indices[i + 1]]
        process_triangle(a, b, c, image)
    }

    transform_vertex :: proc "contextless" (vertex: Vertex, transform: Matrix) -> (clip_position: Vector4) {
        clip_position.xyz = vertex.position
        clip_position.w = 1.0
        return transform * clip_position
    }
}

@(private)
process_triangle :: proc(v0, v1, v2: VS_Out, image: ^Image) #no_bounds_check {

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

    p0, p1, p2 := v0.position, v1.position, v2.position

    // Reject the triangle if it is completely out of view.
    if p0.x > +p0.w && p1.x > +p1.w && p2.x > +p2.w do return
    if p0.x < -p0.w && p1.x < -p1.w && p2.x < -p2.w do return
    if p0.y > +p0.w && p1.y > +p1.w && p2.y > +p2.w do return
    if p0.y < -p0.w && p1.y < -p1.w && p2.y < -p2.w do return
    if p0.z > +p0.w && p1.z > +p1.w && p2.z > +p2.w do return
    if p0.z < 0 && p1.z < 0 && p2.z < 0 do return

    is_clip0 := p0.z < 0.001
    is_clip1 := p1.z < 0.001
    is_clip2 := p2.z < 0.001

    vN := [3]VS_Out{v0, v1, v2}

    switch int(is_clip0) + int(is_clip1) + int(is_clip2) {

    case 0:
        append_triangle(v0, v1, v2, image)

    case 1:
        i0 := is_clip0 ? 0 : is_clip1 ? 1 : 2
        i1 := (i0 + 1) % 3
        i2 := (i0 + 2) % 3

        c0, c1, c2 := vN[i0], vN[i1], vN[i2]
        z0, z1, z2 := c0.position.z, c1.position.z, c2.position.z

        vA := interpolate_vertex(c0, c1, -z0 / (z1 - z0))
        vB := interpolate_vertex(c0, c2, -z0 / (z2 - z0))

        append_triangle(vB, vA, c2, image)
        append_triangle(vA, c1, c2, image)

    case 2:
        i0 := !is_clip0 ? 0 : !is_clip1 ? 1 : 2
        i1 := (i0 + 1) % 3
        i2 := (i0 + 2) % 3

        c0, c1, c2 := vN[i0], vN[i1], vN[i2]
        z0, z1, z2 := c0.position.z, c1.position.z, c2.position.z

        vA := interpolate_vertex(c0, c1, -z0 / (z1 - z0))
        vB := interpolate_vertex(c0, c2, -z0 / (z2 - z0))

        append_triangle(vB, c0, vA, image)
    }

    append_triangle :: proc(x0, x1, x2: VS_Out, image: ^Image) {

        v0, v1, v2 := map_to_viewport(x0), map_to_viewport(x1), map_to_viewport(x2)

        // Determine if this face is a backface (culled).
        is_backface := la.cross(v1.position.xy - v0.position.xy, v2.position.xy - v0.position.xy) < 0
        if get_toggle(.Backface_Culling) && is_backface {
            return
        }

        append(&_ctx.renderer.triangles, Triangle{{v0, v1, v2}, image})
    }

    map_to_viewport :: proc(vertex: VS_Out) -> (output: VS_Out) {

        output = vertex

        // Perspective divide
        output.position.w = 1.0 / vertex.position.w
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
    image:    ^Image,
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
    vertex_cache:  [dynamic]VS_Out,
    pool:          thread.Pool,
} = {
    toggles       = {.Backface_Culling, .Multithreading},
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
compute_index :: proc "contextless" (x, y, width: int) -> int {
    return (y * width) + x
}

@(private)
update_stat :: proc(old_value: ^f64, duration: time.Duration, factor: f64 = 0.1) {
    old_value^ = math.lerp(old_value^, time.duration_milliseconds(duration), factor)
}
