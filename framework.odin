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

screen: Image
screen_depth: []f32

@(private = "file")
texture_: ray.Texture2D

initialize_window :: proc(width, height: int, title: string) {

    ray.SetTraceLogLevel(.WARNING)

    title := strings.clone_to_cstring(title, context.temp_allocator)
    ray.InitWindow(cast(c.int)width, cast(c.int)height, title)

    // Match monitor FPS.
    // ray.SetTargetFPS(ray.GetMonitorRefreshRate(ray.GetCurrentMonitor()))

    // Construct screen image.
    {
        img := ray.GenImageColor(SCREEN_W, SCREEN_H, ray.RED)
        defer ray.UnloadImage(img)

        screen = image_clone_aligned((cast([^]Color)img.data)[0:SCREEN_W * SCREEN_H], SCREEN_W, SCREEN_H)
        screen_depth = mem.make_aligned([]f32, SCREEN_W * SCREEN_H, 16) or_else panic("Allocation failed.")

        texture_ = ray.LoadTextureFromImage(img)
    }
}

destroy_window :: proc() {
    ray.UnloadTexture(texture_)
    ray.CloseWindow()
}

is_window_open :: proc() -> bool {
    return !ray.WindowShouldClose()
}

quit :: proc() {
    ray.CloseWindow()
}

get_time :: proc() -> f32 {
    return cast(f32)ray.GetTime()
}

update_screen_pixels :: proc(info: string) {

    // Update texture with image contents.
    ray.UpdateTexture(texture_, raw_data(screen.data))

    status := fmt.ctprintf(
        "{} x {} | Frame time: {:.2f} ms ({} fps)\n{}",
        expand_values(screen.size),
        1000.0 / cast(f32)ray.GetFPS(),
        ray.GetFPS(),
        info,
    )

    // Flush texture to the screen.
    ray.BeginDrawing()
    ray.ClearBackground(ray.BLACK)
    ray.DrawTextureEx(texture_, {0, 0}, 0, FACTOR, ray.WHITE)
    ray.DrawText(status, 10, 10, 20, ray.WHITE)
    ray.EndDrawing()
}

// ----

Vector2 :: [2]f32
Vector3 :: [3]f32
Vector4 :: [4]f32

Matrix :: matrix[4, 4]f32

// ----

Color :: ray.Color

mix_color :: proc(a, b: Color, t: f32) -> Color {
    return ray.ColorLerp(a, b, t)
}

// ----

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

// -----------------------------------------------------------------------------

Image :: struct {
    data: []Color,
    size: [2]int,
    simd: struct {
        begin: #simd[4]uintptr,
        end:   #simd[4]uintptr,
    },
}

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

image_load :: proc(path: string) -> Image {

    image, _ := png.load_from_file(path)
    defer png.destroy(image)

    data := cast(^Color)raw_data(bytes.buffer_to_bytes(&image.pixels))
    pixels := slice.from_ptr(data, image.width * image.height)

    fmt.printfln("Loaded image: {}x{} with {} pixels", image.width, image.height, len(pixels))

    return image_clone_aligned(pixels, image.width, image.height)
}

image_clear :: proc(image: Image, color: Color) {
    for y in 0 ..< image.size.y {
        for x in 0 ..< image.size.x {
            image_set_pixel(image, x, y, color)
        }
    }
}

image_set_pixel :: #force_inline proc(image: Image, x, y: int, color: Color) #no_bounds_check {
    image_get_pixel_ptr(image, x, y)[0] = color
}

image_get_pixel :: #force_inline proc(image: Image, x, y: int) -> Color #no_bounds_check {
    return image_get_pixel_ptr(image, x, y)[0]
}

image_get_pixel_ptr :: #force_inline proc(image: Image, x, y: int) -> [^]Color #no_bounds_check {
    return raw_data(image.data)[(y * image.size.x) + x:]
}

// Nearest sample an image.
image_sample :: proc(image: Image, uv: [2]f32) -> Color #no_bounds_check {

    x := cast(int)(uv.x * cast(f32)(image.size.x - 1))
    y := cast(int)(uv.y * cast(f32)(image.size.y - 1))

    // Ensure pointers valid to access.
    offset := clamp((y * image.size.x) + x, 0, len(image.data))
    return raw_data(image.data)[offset:][0]
}

// Nearest sample an image using SIMD operations.
image_sample_simd :: proc(image: Image, U, V: #simd[4]f32) -> #simd[4]u32 #no_bounds_check {

    U, V := U, V

    U = simd.clamp(U, 0, 1)
    V = simd.clamp(V, 0, 1)

    // Computes the offset within the image data.
    xs := cast(#simd[4]i32)(U * cast(f32)(image.size.x - 1))
    ys := cast(#simd[4]i32)(V * cast(f32)(image.size.y - 1))
    offsets := (ys * cast(i32)image.size.x) + xs

    // Read the requested image data.
    addrs := image.simd.begin + cast(#simd[4]uintptr)(offsets * size_of(Color))
    // Ensure pointers valid to access.
    addrs = simd.clamp(addrs, image.simd.begin, image.simd.end)

    return simd.gather(cast(#simd[4]rawptr)addrs, (#simd[4]u32)(0), (#simd[4]u32)(1))
}
