package honey

import sa "core:container/small_array"
import "core:strconv"
import "core:strings"

// Parse a Wavefront `*.obj` model format. This is rudementary!
// This does not consider `*.mtl` files nor submeshes, smoothing groups, or other features.
parse_wavefront_mesh :: proc(text: string, counter_clockwise := true, flip_uv := false) -> Mesh {

    vertices: [dynamic]Vertex
    indices: [dynamic]int

    positions: [dynamic]Vector3
    defer delete(positions)

    normals: [dynamic]Vector3
    defer delete(normals)

    uvs: [dynamic]Vector2
    defer delete(uvs)

    lookup: map[Vertex]int
    defer delete(lookup)

    Face :: struct {
        position_index: int,
        texture_index:  int,
        normal_index:   int,
    }

    text_it := text
    for line in strings.split_lines_iterator(&text_it) {

        parts: [dynamic]string
        defer delete(parts)

        line_it := line
        for part in strings.split_iterator(&line_it, " ") {
            (len(part) > 0) or_continue
            append(&parts, part)
        }

        // Skip empty lines.
        if len(parts) == 0 do continue

        switch parts[0] {
        case "v":
            x := strconv.parse_f32(parts[1]) or_else 0
            y := strconv.parse_f32(parts[2]) or_else 0
            z := strconv.parse_f32(parts[3]) or_else 0
            append(&positions, Vector3{x, y, z})
        case "vn":
            x := strconv.parse_f32(parts[1]) or_else 0
            y := strconv.parse_f32(parts[2]) or_else 0
            z := strconv.parse_f32(parts[3]) or_else 0
            append(&normals, Vector3{x, y, z})
        case "vt":
            x := strconv.parse_f32(parts[1]) or_else 0
            y := strconv.parse_f32(parts[2]) or_else 0
            append(&uvs, Vector2{x, flip_uv ? 1.0 - y : y})
        case "f":
            // f v1 ...
            // f v1/vt1 ...
            // f v1/vt1/vn1 ...
            // f v1//vn1 ...

            faces: sa.Small_Array(10, Face)

            // Parse face information.
            for p in parts[1:] {

                fs: sa.Small_Array(3, int)

                p := p // parse each number
                for index_str in strings.split_by_byte_iterator(&p, '/') {
                    attrib_index := strconv.parse_int(index_str) or_else 0
                    sa.append(&fs, attrib_index - 1) // *.obj files are 1 indexed
                }

                face: Face
                switch sa.len(fs) {
                case 1: face.position_index = sa.get(fs, 0)
                case 2:
                    face.position_index = sa.get(fs, 0)
                    face.texture_index = sa.get(fs, 1)
                case 3:
                    face.position_index = sa.get(fs, 0)
                    face.texture_index = sa.get(fs, 1) // could be -1
                    face.normal_index = sa.get(fs, 2)
                case: unreachable()
                }
                sa.append(&faces, face)
            }

            // Triangle fan
            for i in 1 ..< sa.len(faces) - 1 {

                f0 := sa.get(faces, 0)
                f1 := sa.get(faces, counter_clockwise ? i + 1 : i)
                f2 := sa.get(faces, counter_clockwise ? i : i + 1)

                p0 := positions[f0.position_index]
                p1 := positions[f1.position_index]
                p2 := positions[f2.position_index]

                u0 := (f0.texture_index >= 0) ? uvs[f0.texture_index] : {}
                u1 := (f1.texture_index >= 0) ? uvs[f1.texture_index] : {}
                u2 := (f2.texture_index >= 0) ? uvs[f2.texture_index] : {}

                n0 := (f0.normal_index >= 0) ? normals[f0.normal_index] : {}
                n1 := (f1.normal_index >= 0) ? normals[f1.normal_index] : {}
                n2 := (f2.normal_index >= 0) ? normals[f2.normal_index] : {}

                verts := [3]Vertex {
                    {position = p0, normal = n0, uv = u0},
                    {position = p1, normal = n1, uv = u1},
                    {position = p2, normal = n2, uv = u2},
                }

                for vertex in verts {
                    index, ok := lookup[vertex]
                    if !ok {
                        index = len(vertices)
                        append(&vertices, vertex)
                        lookup[vertex] = index
                    }
                    append(&indices, index)
                }
            }
        }
    }

    mesh := Mesh {
        vertices = vertices[:],
        indices  = indices[:],
    }
    return mesh
}
