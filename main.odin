package honey_app

import "./honey"
import "base:intrinsics"
import "core:fmt"
import "core:math"
import la "core:math/linalg"

RESOLUTION_FACTOR :: 2 // 4 = 1080p
SCREEN_W :: 480 * RESOLUTION_FACTOR
SCREEN_H :: 270 * RESOLUTION_FACTOR

PITCH_LIMIT :: (math.PI / 2) * 0.9

main :: proc() {

    // Initialize window.
    honey.initalize(SCREEN_W, SCREEN_H, "Honey Software Renderer", scale = 4 / RESOLUTION_FACTOR)
    defer honey.shutdown()

    // Load the character mesh.
    character_image := honey.image_load("assets/Nexo_Texture.png")
    character_mesh := honey.parse_wavefront_mesh(#load("assets/NexoITCH.obj", string), flip_uv = true)

    floor_quad: honey.Mesh = {
        vertices = {
            honey.Vertex{position = {-30, 0, -30}, uv = {0, 0}, normal = {0, 1, 0}},
            honey.Vertex{position = {+30, 0, -30}, uv = {0, 1}, normal = {0, 1, 0}},
            honey.Vertex{position = {+30, 0, +30}, uv = {1, 1}, normal = {0, 1, 0}},
            honey.Vertex{position = {-30, 0, +30}, uv = {1, 0}, normal = {0, 1, 0}},
        },
        indices  = {0, 1, 2, 0, 2, 3},
    }

    camera_position: honey.Vector3 = {-15, 15, 15}
    camera_heading: f32 = -0.69
    camera_pitch: f32 = -0.61

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

        camera_matrix :=
            la.matrix4_perspective_f32(math.PI / 2, honey.get_framebuffer_aspect(), 0.1, 60.0) *
            la.matrix4_look_at_f32(camera_position, camera_position + camera_dir, {0, 1, 0})

        honey.begin_rendering()
        {
            // Draw the floor quad.
            honey.draw_mesh(floor_quad, &character_image, camera_matrix)

            // Draw an array of character models.
            for y: f32 = -15; y <= 15; y += 5.0 {
                for x: f32 = -15; x <= 15; x += 5.0 {
                    transform := la.matrix4_translate_f32({x, x * 0.5, y})
                    honey.draw_mesh(character_mesh, &character_image, camera_matrix * transform)
                }
            }
        }
        status := fmt.tprintf(
            "(X) backface culling: {}\n(Z) simd rasterization: {}\n(C) multithreading: {}\ntriangle count: {}",
            honey.get_toggle(.Backface_Culling),
            !honey.get_toggle(.Disable_SIMD),
            honey.get_toggle(.Multithreading),
            honey.get_triangle_count(),
        )
        honey.set_debug_text(status)
        honey.end_rendering()

        if !honey.is_cursor_visible() {
            camera_heading += honey.mouse_delta().x * 0.01
            camera_pitch -= honey.mouse_delta().y * 0.01
            camera_pitch = clamp(camera_pitch, -PITCH_LIMIT, PITCH_LIMIT)
        }

        if honey.is_key_down(.W) {
            camera_position += camera_dir * honey.delta_time() * 5
        }

        if honey.is_key_down(.S) {
            camera_position -= camera_dir * honey.delta_time() * 5
        }

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
