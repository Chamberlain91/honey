package honey

import "core:c"
import "core:fmt"
import "core:strings"
import ray "vendor:raylib"

DEV_BUILD :: #config(DEV_BUILD, ODIN_DEBUG)

WHITE :: ray.WHITE
RED :: ray.RED
GREEN :: ray.GREEN
BLUE :: ray.BLUE
CYAN :: Color{0x0, 0xFF, 0xFF, 0xFF}
ORANGE :: Color{0xFF, 0x7F, 0x0, 0xFF}
YELLOW :: ray.YELLOW
MAGENTA :: ray.MAGENTA

@(private = "file")
texture_: ray.Texture2D

// Initialize the window and resources needed to display graphics and handle user input.
// - `width`, `height` Size of the framebuffer.
// - `scale` Used to scale up the specified `width` and `height` when creating the window.
// - `target_fps` Set to `0` to match the primary monitor. Set to `negative` to unlock framerate. Set `positive` to lock framerate to specified number.
initalize :: proc(width, height: int, title: string, scale: f32 = 1.0, target_fps: int = 0) {

    ray.SetTraceLogLevel(.WARNING)

    title := strings.clone_to_cstring(title, context.temp_allocator)
    ray.InitWindow(cast(c.int)(cast(f32)width * scale), cast(c.int)(cast(f32)height * scale), title)

    // Match monitor FPS.
    if target_fps == 0 {
        ray.SetTargetFPS(ray.GetMonitorRefreshRate(ray.GetCurrentMonitor()))
    } else if target_fps > 0 {
        ray.SetTargetFPS(cast(c.int)target_fps)
    }

    // Construct screen image.
    {
        img := ray.GenImageColor(cast(c.int)width, cast(c.int)height, ray.RED)
        defer ray.UnloadImage(img)

        _ctx.framebuffer = allocate_framebuffer({width, height})
        _ctx.renderer = create_renderer(100)
        thread_init()

        texture_ = ray.LoadTextureFromImage(img)
    }
}

shutdown :: proc() {
    ray.UnloadTexture(texture_)
    ray.CloseWindow()
}

// Determines if the window still wants to be open.
is_window_open :: proc() -> bool {
    return !ray.WindowShouldClose()
}

// Close the window, likely terminating the main loop.
quit :: proc() {
    ray.CloseWindow()
}

// Gets an absolute time in seconds.
time :: proc() -> f32 {
    return cast(f32)ray.GetTime()
}

// Gets the relative time in seconds since last frame.
delta_time :: proc() -> f32 {
    return ray.GetFrameTime()
}

// Gets the size of the window (may differ from the framebuffer size based on initialization scale).
window_size :: proc() -> Vector2i {
    return {cast(int)ray.GetScreenWidth(), cast(int)ray.GetScreenHeight()}
}

@(private)
window_flush_content :: proc() {

    // Update texture with image contents.
    ray.UpdateTexture(texture_, raw_data(_ctx.framebuffer.color))

    // Flush texture to the screen
    // Draw the texture flipped to that 0,0 image coordinates is bottom-left.
    ray.BeginDrawing()

    ray.ClearBackground(ray.BLACK)
    ray.DrawTexturePro(
        texture_,
        {0, 0, cast(f32)texture_.width, cast(f32)-texture_.height},
        {0, 0, cast(f32)ray.GetScreenWidth(), cast(f32)ray.GetScreenHeight()},
        {0, 0},
        0,
        ray.WHITE,
    )

    when DEV_BUILD {
        status := fmt.ctprintf(
            "{} x {} | Frame time: {:.2f} ms ({} fps)\n{}",
            expand_values(_ctx.framebuffer.size),
            1000.0 / cast(f32)ray.GetFPS(),
            ray.GetFPS(),
            _ctx.debug_text,
        )
        ray.DrawText(status, 10, 10, 20, ray.WHITE)
    }

    ray.EndDrawing()
}

Vector2 :: [2]f32
Vector3 :: [3]f32
Vector4 :: [4]f32

Vector2i :: [2]int
Vector3i :: [3]int
Vector4i :: [4]int

Size :: Vector2i

Matrix :: matrix[4, 4]f32

Color :: ray.Color

mix_color :: proc "contextless" (a, b: Color, t: f32) -> Color {
    return ray.ColorLerp(a, b, t)
}

Key :: distinct ray.KeyboardKey

// Determine if the key was just pressed this frame.
is_key_pressed :: proc(key: Key) -> bool {
    return ray.IsKeyPressed(auto_cast key)
}

// Determine if the key was just released this frame.
is_key_released :: proc(key: Key) -> bool {
    return ray.IsKeyReleased(auto_cast key)
}

// Determine if the key is held.
is_key_down :: proc(key: Key) -> bool {
    return ray.IsKeyDown(auto_cast key)
}

Mouse_Button :: distinct ray.MouseButton

// Gets the current mouse position.
mouse_position :: proc() -> Vector2 {
    return ray.GetMousePosition()
}

// Sets the current mouse position.
set_mouse_position :: proc(position: Vector2i) {
    ray.SetMousePosition(auto_cast position.x, auto_cast position.y)
}

// Show or hide the cursor.
set_cursor_visible :: proc(enable: bool) {
    if enable do ray.ShowCursor()
    else do ray.HideCursor()
}

// Determines if the window has focus.
is_window_focused :: proc() -> bool {
    return ray.IsWindowFocused()
}

// Determines if the cursor is visible.
is_cursor_visible :: proc() -> bool {
    return !ray.IsCursorHidden()
}

// Gets the mouse delta position (difference since last frame).
mouse_delta :: proc() -> Vector2 {
    return ray.GetMouseDelta()
}

// Determines if the mouse button was just pressed this frame.
mouse_pressed :: proc(button: Mouse_Button) -> bool {
    return ray.IsMouseButtonPressed(auto_cast button)
}

// Determines if the mouse button was just released this frame.
mouse_released :: proc(button: Mouse_Button) -> bool {
    return ray.IsMouseButtonReleased(auto_cast button)
}

// Determines if the mouse button is held.
mouse_down :: proc(button: Mouse_Button) -> bool {
    return ray.IsMouseButtonDown(auto_cast button)
}

// TODO: Gamepad

@(private)
panic_on_error :: proc(err: $E, loc := #caller_location) {
    when E == bool {
        if !err {
            fmt.panicf("[ERROR] Encountered OK: {}", err, loc = loc)
        }
    } else {
        if err != nil {
            fmt.panicf("[ERROR] Encountered error: {}", err, loc = loc)
        }
    }
}
