package honey

import "base:intrinsics"
import sa "core:container/small_array"
import "core:fmt"
import la "core:math/linalg"
import "core:mem"
import "core:mem/virtual"
import "core:os"
import "core:slice"
import "core:sync"
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

    fmt.printfln("Started thread pool with {} workers.", len(_ctx.pool.threads))
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

get_vertex_duration :: proc() -> time.Duration {
    return _ctx.stats.vertex_duration
}

get_raster_duration :: proc() -> time.Duration {
    return _ctx.stats.rasterization_duration
}

get_dispatch_duration :: proc() -> time.Duration {
    return _ctx.stats.dispatch_duration
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

    _ctx.stats = {
        vertex_start_time = time.now(),
    }

    // Each frame will have a new triangle list.
    clear(&_ctx.triangle_cache)
}

// Ends rendering, flushing the framebuffer to the screen.
end_rendering :: proc() {

    // Vertex processing stage is now 'complete'.
    _ctx.stats.vertex_duration = time.since(_ctx.stats.vertex_start_time)
    rasterization_start_time := time.now()

    // Count the number of triangles drawn.
    _ctx.stats.triangle_count += len(_ctx.triangle_cache)

    // Draw each clipped triangle.
    for &tri in &_ctx.triangle_cache {

        // Map the triangle to the viewport (NDC -> Viewport).
        for &v in tri.vertices {

            // Do the "perspective divide"
            v.position.w = 1.0 / v.position.w
            v.position.xyz *= v.position.w

            // NDC -> Viewport
            v.position.xy = (v.position.xy + 1.0) / 2.0
            v.position.x *= cast(f32)_ctx.framebuffer.width
            v.position.y *= cast(f32)_ctx.framebuffer.height
        }

        if get_toggle(.Multithreading) {

            // Submit triangle to the tiled region.
            renderer_append_triangle(&_ctx.renderer, &tri)

        } else {

            // Render the triangle, directly.
            rasterize_triangle(&tri, compute_triangle_bounds(tri, 0, _ctx.framebuffer.size - 1))
        }
    }

    _ctx.stats.rasterization_duration = time.since(rasterization_start_time)

    dispatch_rasterization_tasks(&_ctx.renderer)

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

    // We need to clear the vertex cache for each mesh, since the transform will be different.
    clear(&_ctx.vertex_cache)

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
    reserve(&_ctx.triangle_cache, len(_ctx.triangle_cache) + (len(mesh.indices) / 3))
    for i := 0; i < len(mesh.indices); i += 3 {
        a := &_ctx.vertex_cache[mesh.indices[i + 0]]
        b := &_ctx.vertex_cache[mesh.indices[i + 2]]
        c := &_ctx.vertex_cache[mesh.indices[i + 1]]
        process_triangle(a, b, c, image)
    }

    transform_vertex :: proc "contextless" (vertex: Vertex, transform: Matrix) -> (clip_position: Vector4) {
        clip_position.xyz = vertex.position
        clip_position.w = 1.0
        return transform * clip_position
    }
}

@(private)
process_triangle :: proc(v0, v1, v2: ^VS_Out, image: ^Image) #no_bounds_check {

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

    // Backface culling. Compare normal to "view vector", negative indicates away from viewpoint.
    if get_toggle(.Backface_Culling) && la.dot(la.cross(p1.xyz - p0.xyz, p2.xyz - p0.xyz), p0.xyz) < 0 {
        return
    }

    // Reject the triangle if it is completely out of view.
    if p0.x > +p0.w && p1.x > +p1.w && p2.x > +p2.w do return
    if p0.x < -p0.w && p1.x < -p1.w && p2.x < -p2.w do return
    if p0.y > +p0.w && p1.y > +p1.w && p2.y > +p2.w do return
    if p0.y < -p0.w && p1.y < -p1.w && p2.y < -p2.w do return
    if p0.z > +p0.w && p1.z > +p1.w && p2.z > +p2.w do return
    if p0.z < 0 && p1.z < 0 && p2.z < 0 do return

    if p0.z < 0 {
        if p1.z < 0 {
            clip2({{v0^, v1^, v2^}, image})
        } else if p2.z < 0 {
            clip2({{v0^, v2^, v1^}, image})
        } else {
            clip1({{v0^, v1^, v2^}, image})
        }
    } else if p1.z < 0 {
        if p2.z < 0 {
            clip2({{v1^, v2^, v0^}, image})
        } else {
            clip1({{v1^, v0^, v2^}, image})
        }
    } else if p2.z < 0 {
        clip1({{v2^, v0^, v1^}, image})
    } else {
        // No clipping needed
        append(&_ctx.triangle_cache, Triangle{{v0^, v1^, v2^}, image})
    }

    clip1 :: #force_inline proc(triangle: Triangle) {

        p0, p1, p2 := triangle.vertices[0].position, triangle.vertices[1].position, triangle.vertices[2].position
        v0, v1, v2 := expand_values(triangle.vertices)

        alphaA := -p0.z / (p1.z - p0.z)
        alphaB := -p0.z / (p2.z - p0.z)

        vA := interpolate_vertex(v0, v1, alphaA)
        vB := interpolate_vertex(v0, v2, alphaB)

        append(&_ctx.triangle_cache, Triangle{{vA, v1, v2}, triangle.image})
        append(&_ctx.triangle_cache, Triangle{{vB, vA, v2}, triangle.image})
    }

    clip2 :: #force_inline proc(triangle: Triangle) {

        p0, p1, p2 := triangle.vertices[0].position, triangle.vertices[1].position, triangle.vertices[2].position
        v0, v1, v2 := expand_values(triangle.vertices)

        alpha0 := -p0.z / (p2.z - p0.z)
        alpha1 := -p1.z / (p2.z - p1.z)

        v0 = interpolate_vertex(v0, v2, alpha0)
        v1 = interpolate_vertex(v1, v2, alpha1)

        append(&_ctx.triangle_cache, Triangle{{v0, v1, v2}, triangle.image})
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
    debug_text:     string,
    framebuffer:    Framebuffer,
    renderer:       Renderer,
    toggles:        bit_set[Toggle],
    sun_direction:  Vector3,
    stats:          struct {
        dispatch_duration:      time.Duration,
        rasterization_duration: time.Duration,
        vertex_start_time:      time.Time,
        vertex_duration:        time.Duration,
        triangle_count:         int,
    },
    vertex_cache:   [dynamic]VS_Out,
    triangle_cache: [dynamic]Triangle,
    pool:           thread.Pool,
    wait_group:     sync.Wait_Group,
} = {
    toggles       = {.Backface_Culling, .Multithreading},
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

@(private)
compute_triangle_bounds :: proc "contextless" (
    triangle: Triangle,
    viewport_min, viewport_max: Vector2i,
) -> (
    r_min, r_max: Vector2i,
) {

    p0, p1, p2 := triangle.vertices[0].position, triangle.vertices[1].position, triangle.vertices[2].position

    // Find triangle bounds...
    r_min = la.array_cast(la.min(p0.xy, p1.xy, p2.xy), int)
    r_max = la.array_cast(la.max(p0.xy, p1.xy, p2.xy), int)

    // Clamp rasterization bounds to the viewport.
    r_min = la.clamp(r_min, viewport_min, viewport_max)
    r_max = la.clamp(r_max, viewport_min, viewport_max)

    return
}

@(private)
compute_index :: #force_inline proc "contextless" (x, y, width: int) -> int {
    return (y * width) + x
}
