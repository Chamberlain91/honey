package honey

import "core:bytes"
import "core:c"
import "core:fmt"
import "core:image/png"
import "core:mem"
import "core:simd"
import "core:slice"
import "core:strings"
import ray "vendor:raylib"

DEV_BUILD :: #config(DEV_BUILD, ODIN_DEBUG)

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

        _ctx.framebuffer = allocate_framebuffer(width, height)

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
window_size :: proc() -> [2]int {
    return {cast(int)ray.GetScreenWidth(), cast(int)ray.GetScreenHeight()}
}

@(private)
flush_frame :: proc() {

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

mix_color :: proc(a, b: Color, t: f32) -> Color {
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

// Determine if the cursor is visible.
is_cursor_visible :: proc() -> bool {
    return !ray.IsCursorHidden()
}

// Gets the mouse delta position (difference since last frame).
mouse_delta :: proc() -> Vector2 {
    return ray.GetMouseDelta()
}

// Determine if the mouse button was just pressed this frame.
mouse_pressed :: proc(button: Mouse_Button) -> bool {
    return ray.IsMouseButtonPressed(auto_cast button)
}

// Determine if the mouse button was just released this frame.
mouse_released :: proc(button: Mouse_Button) -> bool {
    return ray.IsMouseButtonReleased(auto_cast button)
}

// Determine if the mouse button is held.
mouse_down :: proc(button: Mouse_Button) -> bool {
    return ray.IsMouseButtonDown(auto_cast button)
}

// TODO: Gamepad

// -----------------------------------------------------------------------------

Image :: struct {
    data: []Color,
    size: [2]int,
    simd: struct {
        begin: #simd[4]uintptr,
        end:   #simd[4]uintptr,
    },
}

/// Loads and image from the specified path.
image_load :: proc(path: string) -> Image {

    image, _ := png.load_from_file(path)
    defer png.destroy(image)

    data := cast(^Color)raw_data(bytes.buffer_to_bytes(&image.pixels))
    pixels := slice.from_ptr(data, image.width * image.height)

    fmt.printfln("Loaded image: {}x{} with {} pixels", image.width, image.height, len(pixels))

    return image_clone_aligned(pixels, image.width, image.height)

    image_clone_aligned :: proc(pixels: []Color, w, h: int) -> Image {

        aligned_memory, err := mem.make_aligned([]Color, len(pixels), 16)
        if err != .None {
            fmt.panicf("Unable to allocate aligned image memory: {}", err)
        }

        assert(len(pixels) == len(aligned_memory))
        copy(aligned_memory, pixels)

        image := Image {
            data = aligned_memory,
            size = {w, h},
            simd = {     //
                begin = cast(uintptr)raw_data(aligned_memory),
                end   = cast(uintptr)raw_data(aligned_memory) + cast(uintptr)(len(pixels) * size_of(Color)),
            },
        }

        return image
    }
}

// TODO: image_load_bytes, image_load_path, image_load group

// TODO: Document
image_clear :: proc(image: Image, color: Color) {
    slice.fill(image.data, color)
}

// TODO: Document
image_set_pixel :: #force_inline proc(image: Image, x, y: int, color: Color) #no_bounds_check {
    image_get_pixel_ptr(image, x, y)[0] = color
}

// TODO: Document
image_get_pixel :: #force_inline proc(image: Image, x, y: int) -> Color #no_bounds_check {
    return image_get_pixel_ptr(image, x, y)[0]
}

// TODO: Document
image_get_pixel_ptr :: #force_inline proc(image: Image, x, y: int) -> [^]Color #no_bounds_check {
    return raw_data(image.data)[(y * image.size.x) + x:]
}

// Nearest sample an image.
@(private)
image_sample :: proc(image: Image, uv: [2]f32) -> Color #no_bounds_check {

    x := cast(int)(uv.x * cast(f32)(image.size.x - 1))
    y := cast(int)(uv.y * cast(f32)(image.size.y - 1))

    // Ensure pointers valid to access.
    offset := clamp((y * image.size.x) + x, 0, len(image.data))
    return raw_data(image.data)[offset:][0]
}

// Nearest sample an image using SIMD operations.
@(private)
image_sample_simd :: proc(image: Image, U, V: #simd[4]f32) -> #simd[4]u32 #no_bounds_check {

    U, V := U, V

    U = simd.clamp(U, 0, 1)
    V = simd.clamp(V, 0, 1)

    // Computes the offset within the image data.
    xs := cast(#simd[4]i32)(U * cast(f32)(image.size.x - 1))
    ys := cast(#simd[4]i32)(V * cast(f32)(image.size.y - 1))
    offsets := (ys * cast(i32)image.size.x) + xs

    // Read the requested image data.
    // TODO: https://github.com/odin-lang/Odin/issues/5737
    addrs := image.simd.begin + cast(#simd[4]uintptr)(offsets * size_of(Color))
    // Ensure pointers valid to access.
    addrs = simd.clamp(addrs, image.simd.begin, image.simd.end)

    return simd.gather(cast(#simd[4]rawptr)addrs, (#simd[4]u32)(0), (#simd[4]u32)(1))
}
