package honey_app

import "./honey"
import "core:fmt"
import "core:math"
import la "core:math/linalg"

RESOLUTION_FACTOR :: 4 // 4 = 1080p
SCREEN_W :: 480 * RESOLUTION_FACTOR
SCREEN_H :: 270 * RESOLUTION_FACTOR

PITCH_LIMIT :: (math.PI / 2) * 0.9

main :: proc() {

    // Initialize window.
    honey.initalize(SCREEN_W, SCREEN_H, "Honey Software Renderer", scale = 4 / RESOLUTION_FACTOR, target_fps = -1)
    defer honey.shutdown()

    // Load the character mesh.
    model := honey.load_wavefront_model("assets/sponza.obj")
    character := honey.load_wavefront_model("assets/NexoITCH.obj")

    v_min, v_max: honey.Vector3 = max(f32), min(f32)
    for mesh in model.meshes {
        for &v in mesh.vertices {
            v.position *= 0.01 // scale down sponza.obj
            v_min = la.min(v.position, v_min)
            v_max = la.max(v.position, v_max)
        }
    }
    fmt.printfln("Model ranges from: {} to {} (size {})", v_min, v_max, v_max - v_min)

    // default position within sponza.obj
    camera_position: honey.Vector3 = {-10.8, 0.57, 1.0}
    camera_heading: f32 = -0.56
    camera_pitch: f32 = 0.419

    // Main loop.
    main_loop: for honey.is_window_open() {
        defer free_all(context.temp_allocator)

        // Show cursor when holding Left ALT.
        honey.set_cursor_visible(honey.is_key_down(.LEFT_ALT))

        // Only grab the cursor when its hidden and window has focus.
        defer if !honey.is_cursor_visible() && honey.is_window_focused() {
            honey.set_mouse_position(honey.window_size() / 2)
        }

        camera_dir: honey.Vector3 = {
            math.cos(camera_heading) * math.cos(camera_pitch),
            math.sin(camera_pitch),
            math.sin(camera_heading) * math.cos(camera_pitch),
        }
        camera_right := la.cross(camera_dir, honey.Vector3{0, 1, 0})
        camera_up := la.cross(camera_right, camera_dir)

        camera_matrix :=
            la.matrix4_perspective_f32(math.PI / 2, honey.get_framebuffer_aspect(), 0.1, 60.0) *
            la.matrix4_look_at_f32(camera_position, camera_position + camera_dir, {0, 1, 0})

        status := fmt.tprintf(
            "(X) backface culling: {}\n(Z) simd rasterization: {}\n(C) multithreading: {}\ntriangle count: {}\nvertex: {}\ndispatch: {}\nraster: {}",
            honey.get_toggle(.Backface_Culling),
            !honey.get_toggle(.Disable_SIMD),
            honey.get_toggle(.Multithreading),
            honey.get_triangle_count(),
            honey.get_vertex_duration(),
            honey.get_dispatch_duration(),
            honey.get_raster_duration(),
        )
        honey.set_debug_text(status)

        honey.begin_rendering()
        {
            honey.draw_model(character, camera_matrix)
            honey.draw_model(model, camera_matrix)
        }
        honey.end_rendering()

        if !honey.is_cursor_visible() {
            camera_heading += honey.mouse_delta().x * 0.01
            camera_pitch -= honey.mouse_delta().y * 0.01
            camera_pitch = clamp(camera_pitch, -PITCH_LIMIT, PITCH_LIMIT)
        }

        move: honey.Vector3
        if honey.is_key_down(.W) do move += camera_dir
        if honey.is_key_down(.A) do move -= camera_right
        if honey.is_key_down(.S) do move -= camera_dir
        if honey.is_key_down(.D) do move += camera_right
        if honey.is_key_down(.Q) do move -= camera_up
        if honey.is_key_down(.E) do move += camera_up

        speed := (1.0 + cast(f32)cast(int)(honey.is_key_down(.LEFT_SHIFT))) * honey.delta_time() * 5
        camera_position += la.normalize0(move) * speed

        // Exit when pressing escape.
        if honey.is_key_pressed(.ESCAPE) {
            break main_loop
        }

        // Toggle SIMD code paths.
        if honey.is_key_pressed(.Z) {
            honey.set_toggle(.Disable_SIMD, !honey.get_toggle(.Disable_SIMD))
        }

        // Backface culling
        if honey.is_key_pressed(.X) {
            honey.set_toggle(.Backface_Culling, !honey.get_toggle(.Backface_Culling))
        }

        // Multithreading
        if honey.is_key_pressed(.C) {
            honey.set_toggle(.Multithreading, !honey.get_toggle(.Multithreading))
        }
    }
}
