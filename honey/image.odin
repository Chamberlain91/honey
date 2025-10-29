package honey

import "core:bytes"
import "core:fmt"
import "core:image/png"
import "core:math"
import la "core:math/linalg"
import "core:mem"
import "core:simd"
import "core:slice"

Image :: struct {
    data: []Color,
    size: [2]int,
}

Texture :: struct {
    mips: []Image,
}

/// Loads and image from the specified path.
image_load_bytes :: proc(data: []byte) -> Image {

    fmt.printfln("[INFO] Reading image from {} bytes", len(data))

    image, image_err := png.load_from_bytes(data, png.Options{.alpha_add_if_missing})
    panic_on_error(image_err)
    defer png.destroy(image)

    pixels := slice.reinterpret([]Color, bytes.buffer_to_bytes(&image.pixels))
    return image_clone_aligned(pixels, image.width, image.height)
}

/// Loads and image from the specified path.
image_load_path :: proc(_path: string) -> Image {

    path, path_err := normalize_path_slash(_path, context.temp_allocator)
    panic_on_error(path_err)

    fmt.printfln("[INFO] Reading image: {}", path)

    image, image_err := png.load_from_file(path, png.Options{.alpha_add_if_missing})
    panic_on_error(image_err)
    defer png.destroy(image)

    pixels := slice.reinterpret([]Color, bytes.buffer_to_bytes(&image.pixels))
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
    }

    return image
}

to_texture :: proc(image: Image) -> Texture {

    assert(image.size.x > 0)
    assert(image.size.y > 0)
    assert(image.data != nil)

    // TODO: Actually compute mip maps

    image := image

    num_mips := 1 + cast(int)math.log2_f32(cast(f32)min(image.size.x, image.size.y))

    texture := Texture {
        mips = make([]Image, num_mips),
    }

    // Top level is norma image.
    texture.mips[0] = image

    // Compute lower mips by downsampling each time.
    for i in 1 ..< len(texture.mips) {

        // Allocate half size image.
        half_memory, half_memory_ok := mem.make_aligned([]Color, len(image.data) / 4, 16)
        panic_on_error(half_memory_ok)
        mip_image := Image {
            data = half_memory,
            size = image.size / 2,
        }

        for y in 0 ..< mip_image.size.y {
            for x in 0 ..< mip_image.size.x {

                sample_coord := [2]int{x, y} * 2
                samples: [4]Color = {
                    image_get_pixel(image, expand_values(sample_coord + {0, 0})),
                    image_get_pixel(image, expand_values(sample_coord + {1, 0})),
                    image_get_pixel(image, expand_values(sample_coord + {1, 1})),
                    image_get_pixel(image, expand_values(sample_coord + {0, 1})),
                }

                average: [4]int
                for sample_index in 0 ..< 4 {
                    for component in 0 ..< 4 {
                        average[component] += cast(int)samples[sample_index][component]
                    }
                }
                average /= 4

                image_set_pixel(mip_image, x, y, cast(Color)la.array_cast(average, u8))
            }
        }

        // Store half image and swap with reference image.
        texture.mips[i] = mip_image
        image = mip_image
    }

    return texture
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

@(private)
texture_sample :: proc "contextless" (texture: Texture, level: int, uv: [2]f32) -> Color #no_bounds_check {
    return image_sample(texture.mips[level], uv)
}

// Nearest sample an image using SIMD operations.
@(private)
image_sample_simd :: proc "contextless" (image: Image, U, V: #simd[4]f32) -> #simd[4]u32 #no_bounds_check {

    // Computes the offset within the image data.
    xs := cast(#simd[4]i32)(wrap(U) * cast(f32)image.size.x)
    ys := cast(#simd[4]i32)(wrap(V) * cast(f32)image.size.y)

    // Computes the addresses to gather image data from.
    byte_offsets := cast(#simd[4]uintptr)(to_offset(image, xs, ys) * size_of(Color))
    begin := cast(#simd[4]uintptr)raw_data(image.data)
    ptrs := transmute(#simd[4]rawptr)(begin + byte_offsets)

    // Read the requested image data.
    return simd.gather(ptrs, cast(#simd[4]u32)0, cast(#simd[4]bool)true)

    wrap :: proc "contextless" (x: #simd[4]f32) -> #simd[4]f32 {
        frac := x - simd.floor(x)
        add_one := transmute(#simd[4]f32)(simd.lanes_lt(frac, 0) & transmute(u32)f32(1.0))
        return frac + add_one
    }

    to_offset :: proc "contextless" (image: Image, xs, ys: #simd[4]i32) -> #simd[4]i32 {
        offset := (ys * cast(i32)image.size.x) + xs
        return simd.clamp(offset, 0, cast(#simd[4]i32)len(image.data))
    }
}

@(private)
texture_sample_simd :: proc "contextless" (
    texture: Texture,
    level: int,
    U, V: #simd[4]f32,
) -> #simd[4]u32 #no_bounds_check {
    return image_sample_simd(texture.mips[level], U, V)
}
