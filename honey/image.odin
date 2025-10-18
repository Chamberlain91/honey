package honey

import "core:bytes"
import "core:fmt"
import "core:image/png"
import "core:mem"
import "core:simd"
import "core:slice"

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

    fmt.printfln("[INFO] Reading image: {}", path)

    image, image_err := png.load_from_file(path, png.Options{.alpha_add_if_missing})
    panic_on_error(image_err)
    defer png.destroy(image)

    data := cast(^Color)raw_data(bytes.buffer_to_bytes(&image.pixels))
    pixels := slice.from_ptr(data, image.width * image.height)

    return image_clone_aligned(pixels, image.width, image.height)
}

@(private)
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
image_sample :: proc "contextless" (image: Image, uv: [2]f32) -> Color #no_bounds_check {

    x := cast(int)(uv.x * cast(f32)(image.size.x - 1))
    y := cast(int)(uv.y * cast(f32)(image.size.y - 1))

    x = wrap(x, image.size.x)
    y = wrap(y, image.size.y)

    // Ensure pointers valid to access.
    offset := clamp((y * image.size.x) + x, 0, len(image.data))
    return raw_data(image.data)[offset:][0]

    wrap :: proc "contextless" (x, n: int) -> int {
        return ((x % n) + n) % n
    }
}

// Nearest sample an image using SIMD operations.
@(private)
image_sample_simd :: proc "contextless" (image: Image, U, V: #simd[4]f32) -> #simd[4]u32 #no_bounds_check {

    U, V := U, V

    // Computes the offset within the image data.
    xs := cast(#simd[4]i32)(wrap(U) * cast(f32)(image.size.x - 1))
    ys := cast(#simd[4]i32)(wrap(V) * cast(f32)(image.size.y - 1))
    offsets := (ys * cast(i32)image.size.x) + xs

    // Read the requested image data.
    // TODO: https://github.com/odin-lang/Odin/issues/5737
    addrs := image.simd.begin + cast(#simd[4]uintptr)(offsets * size_of(Color))
    // Ensure pointers valid to access.
    addrs = simd.clamp(addrs, image.simd.begin, image.simd.end)

    return simd.gather(cast(#simd[4]rawptr)addrs, (#simd[4]u32)(0), (#simd[4]u32)(1))

    wrap :: proc "contextless" (x: #simd[4]f32) -> #simd[4]f32 {

        // frac := x - floo(x)
        // addo := (frac < 0) ? 1 : 0
        // return frac + addo

        frac := x - simd.floor(x)
        add_one := transmute(#simd[4]f32)(simd.lanes_lt(frac, 0) & transmute(u32)f32(1.0))
        return frac + add_one
    }
}
