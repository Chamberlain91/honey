package honey

import ray "vendor:raylib"

SCREEN_W :: 480
SCREEN_H :: 320
FACTOR :: 2

main :: proc() {

    // Initialize window.
    ray.InitWindow(SCREEN_W * FACTOR, SCREEN_H * FACTOR, "Honey Software Renderer")
    defer ray.CloseWindow()

    // Match monitor FPS.
    ray.SetTargetFPS(ray.GetMonitorRefreshRate(ray.GetCurrentMonitor()))

    // Construct image.
    image := ray.GenImageColor(SCREEN_W, SCREEN_H, ray.RED)
    defer ray.UnloadImage(image)

    // Construct texture from image.
    texture := ray.LoadTextureFromImage(image)
    defer ray.UnloadTexture(texture)

    screen = {
        pixels = cast([^]Color)image.data,
        width  = cast(int)image.width,
        height = cast(int)image.height,
    }

    // Main loop.
    main_loop: for !ray.WindowShouldClose() {

        // Populate image data for this frame.
        for y in 0 ..< screen.height {
            for x in 0 ..< screen.width {

                r := cast(u8)(cast(f32)x / cast(f32)image.width * 0xFF)
                g := cast(u8)(cast(f32)y / cast(f32)image.height * 0xFF)

                set_pixel(x, y, Color{r, g, 0, 0xFF})
            }
        }

        // Update texture with image contents.
        ray.UpdateTexture(texture, image.data)

        // Flush texture to the screen.
        ray.BeginDrawing()
        ray.DrawTextureEx(texture, {0, 0}, 0, FACTOR, ray.WHITE)
        ray.DrawFPS(10, 10)
        ray.EndDrawing()

        // Exit when pressing escape.
        if ray.IsKeyPressed(.ESCAPE) {
            break main_loop
        }
    }
}

Color :: ray.Color

screen: struct {
    pixels:        [^]Color,
    width, height: int,
}

set_pixel :: #force_inline proc(x, y: int, color: Color) #no_bounds_check {
    screen.pixels[(y * screen.width) + x] = color
}

get_pixel :: #force_inline proc(x, y: int) -> Color #no_bounds_check {
    return screen.pixels[(y * screen.width) + x]
}
