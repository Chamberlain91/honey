#+private
package honey

import la "core:math/linalg"

Renderer :: struct {
    tile_size: int,
    tiles:     []Renderer_Tile,
    count:     Size,
}

Renderer_Tile :: struct {
    min, max:  Vector2i,
    triangles: [dynamic]Triangle,
}

renderer_create :: proc(tile_size: int = 64) -> (r: Renderer) {

    r.tile_size = tile_size
    r.count = ((_ctx.framebuffer.size + (tile_size - 1)) / tile_size)
    r.tiles = make([]Renderer_Tile, r.count.x * r.count.y)

    for y in 0 ..< r.count.y {
        for x in 0 ..< r.count.x {

            r_min := Vector2i{x, y} * tile_size
            r_max := la.min(r_min + tile_size, _ctx.framebuffer.size - 1)

            r.tiles[compute_index(x, y, r.count.x)] = Renderer_Tile {
                min = r_min,
                max = r_max,
            }
        }
    }

    return
}

// Submit the triangle to all tiles it touches.
renderer_append_triangle :: proc(renderer: Renderer, triangle: Triangle) #no_bounds_check {

    // Compute the rectangular range of overlapping tiles.
    co_min, co_max := compute_triangle_bounds(triangle, 0, _ctx.framebuffer.size - 1)
    co_min = (co_min) / renderer.tile_size
    co_max = (co_max + (renderer.tile_size - 1)) / renderer.tile_size

    for y in co_min.y ..< co_max.y {
        for x in co_min.x ..< co_max.x {
            tile := &renderer.tiles[compute_index(x, y, renderer.count.x)]
            append(&tile.triangles, triangle)
        }
    }
}

// Waits for this tile to finish rendering its triangles task list.
renderer_wait_tile :: proc(tile: ^Renderer_Tile) {

    // TODO: Actually make async
    for triangle in pop_safe(&tile.triangles) {
        rasterize_triangle(triangle, tile.min, tile.max)
    }
}

// Blocks to ensure all rendering of tiles have completed.
renderer_flush :: proc(renderer: Renderer) {

    for &tile in renderer.tiles {

        // Wait for the rendering thread to exit.
        renderer_wait_tile(&tile)

        // At this point, all triangles in the tile need to be processed.
        assert(len(tile.triangles) == 0)
    }
}
