package honey

import "base:intrinsics"
import "core:fmt"
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

    fmt.printfln("Started thread pool with {} workers.", len(_ctx.pool.threads))
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
    clear(&_ctx.renderer.triangles)
}

// Ends rendering, flushing the framebuffer to the screen.
end_rendering :: proc() {

    // Vertex processing stage is now 'complete'.
    _ctx.stats.vertex_duration = time.since(_ctx.stats.vertex_start_time)

    // Count the number of triangles drawn.
    _ctx.stats.triangle_count += len(_ctx.renderer.triangles)

    // Process all triangles.
    flush_renderer(&_ctx.renderer)

    // Single-threaded mode doesn't have a dispatch step, so we actually measured rasterization.
    if !get_toggle(.Multithreading) {
        _ctx.stats.rasterization_duration = _ctx.stats.dispatch_duration
        _ctx.stats.dispatch_duration = 0
    }

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

    edge0 := p1.xyz - p0.xyz
    edge1 := p2.xyz - p0.xyz

    // Backface culling. Compare normal to "view vector", negative indicates away from viewpoint.
    if get_toggle(.Backface_Culling) && la.dot(la.cross(edge0, edge1), p0.xyz) < 0 {
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
            clip2(v0, v1, v2, image)
        } else if p2.z < 0 {
            clip2(v0, v2, v1, image)
        } else {
            clip1(v0, v1, v2, image)
        }
    } else if p1.z < 0 {
        if p2.z < 0 {
            clip2(v1, v2, v0, image)
        } else {
            clip1(v1, v0, v2, image)
        }
    } else if p2.z < 0 {
        clip1(v2, v0, v1, image)
    } else {
        // No clipping needed.
        append_triangle(v0, v1, v2, image)
    }

    clip1 :: proc(v0, v1, v2: VS_Out, image: ^Image) {
        z0, z1, z2 := v0.position.z, v1.position.z, v2.position.z

        vA := interpolate_vertex(v0, v1, -z0 / (z1 - z0))
        vB := interpolate_vertex(v0, v2, -z0 / (z2 - z0))

        append_triangle(vA, v1, v2, image)
        append_triangle(vB, vA, v2, image)
    }

    clip2 :: proc(v0, v1, v2: VS_Out, image: ^Image) {
        z0, z1, z2 := v0.position.z, v1.position.z, v2.position.z

        vA := interpolate_vertex(v0, v2, -z0 / (z2 - z0))
        vB := interpolate_vertex(v1, v2, -z1 / (z2 - z1))

        append_triangle(vA, vB, v2, image)
    }

    append_triangle :: proc(v0, v1, v2: VS_Out, image: ^Image) {
        append(
            &_ctx.renderer.triangles,
            Triangle{{map_to_viewport(v0), map_to_viewport(v1), map_to_viewport(v2)}, image},
        )
    }

    map_to_viewport :: proc(vertex: VS_Out) -> VS_Out {
        vertex := vertex

        // Perspective divide
        vertex.position.w = 1.0 / vertex.position.w
        vertex.position.xyz *= vertex.position.w

        // NDC -> Viewport
        vertex.position.xy = ((vertex.position.xy + 1.0) / 2.0) * _ctx.framebuffer.sizef

        return vertex
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
        dispatch_duration:      time.Duration,
        rasterization_duration: time.Duration,
        vertex_start_time:      time.Time,
        vertex_duration:        time.Duration,
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
    color:   []Color,
    depth:   []f32,
    using _: struct #raw_union {
        using _: struct {
            width, height: int,
        },
        size:    Vector2i,
    },
    sizef:   [2]f32,
}

@(private)
allocate_framebuffer :: proc(size: Vector2i) -> Framebuffer {
    color := mem.make_aligned([]Color, size.x * size.y, SIMD_ALIGN) or_else panic("allocate_framebuffer failed.")
    depth := mem.make_aligned([]f32, size.x * size.y, SIMD_ALIGN) or_else panic("allocate_framebuffer failed.")
    return {color = color, depth = depth, size = size, sizef = la.array_cast(size, f32)}
}

@(private)
compute_index :: proc "contextless" (x, y, width: int) -> int {
    return (y * width) + x
}
