package honey

import sa "core:container/small_array"
import "core:fmt"
import "core:mem"
import "core:os"
import "core:path/filepath"
import "core:slice"
import "core:strconv"
import "core:strings"

Model :: struct {
    textures: []Image,
    meshes:   []Mesh,
}

destroy_model :: proc(model: Model) {
    for m in model.meshes {
        delete(m.vertices)
        delete(m.indices)
    }
    for m in model.textures {
        delete(m.data)
    }
}

// Parse a Wavefront `*.obj` model format. This is rudementary!
// This does not consider `*.mtl` files nor submeshes, smoothing groups, or other features.
load_wavefront_model :: proc(path: string, counter_clockwise := true, flip_uv := false) -> Model {

    positions: [dynamic]Vector3
    defer delete(positions)

    normals: [dynamic]Vector3
    defer delete(normals)

    uvs: [dynamic]Vector2
    defer delete(uvs)

    lookup: map[Vertex]int
    defer delete(lookup)

    texture_lookup: map[string]int
    defer delete(texture_lookup)
    textures: []Image

    builders: []Mesh_Builder
    defer delete(builders)

    Face :: struct {
        position_index: int,
        texture_index:  int,
        normal_index:   int,
    }

    dir, file := filepath.split(path)

    fmt.printfln("[INFO] Reading wavefront model: {}", path)

    data := os.read_entire_file(path) or_else fmt.panicf("Could not read model file: {}", path)
    defer delete(data)

    material: ^Image
    builder: ^Mesh_Builder

    text_it := cast(string)data
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

        case "mtllib":
            // Loads the referenced material library.
            matlib_path, matlib_path_err := filepath.join({dir, parts[1]}, context.temp_allocator)
            panic_on_error(matlib_path_err)
            textures, texture_lookup = load_wavefront_materials(matlib_path)
            builders = make([]Mesh_Builder, len(textures))

        case "usemtl":
            if index, ok := texture_lookup[parts[1]]; ok {
                material = &textures[index]
                builder = &builders[index]
            } else {
                fmt.eprintfln("- Missing material '{}'", parts[1])
            }

        // PARSING POSITIONS
        case "v":
            x := strconv.parse_f32(parts[1]) or_else 0
            y := strconv.parse_f32(parts[2]) or_else 0
            z := strconv.parse_f32(parts[3]) or_else 0
            append(&positions, Vector3{x, y, z})

        // PARSING NORMALS
        case "vn":
            x := strconv.parse_f32(parts[1]) or_else 0
            y := strconv.parse_f32(parts[2]) or_else 0
            z := strconv.parse_f32(parts[3]) or_else 0
            append(&normals, Vector3{x, y, z})

        // PARSING TEXTURE COORDINATES
        case "vt":
            x := strconv.parse_f32(parts[1]) or_else 0
            y := strconv.parse_f32(parts[2]) or_else 0
            append(&uvs, Vector2{x, flip_uv ? 1.0 - y : y})

        // PARSING FACES
        case "f":
            // Faces may be formatted in one of the following ways:
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
                case 1:
                    face.position_index = sa.get(fs, 0)
                case 2:
                    face.position_index = sa.get(fs, 0)
                    face.texture_index = sa.get(fs, 1)
                case 3:
                    face.position_index = sa.get(fs, 0)
                    face.texture_index = sa.get(fs, 1) // could be -1
                    face.normal_index = sa.get(fs, 2)
                case:
                    unreachable()
                }
                sa.append(&faces, face)
            }

            // We split n-gons via triangle fan pattern.
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
                        index = len(builder.vertices)
                        append(&builder.vertices, vertex)
                        lookup[vertex] = index
                    }
                    append(&builder.indices, index)
                }
            }
        }
    }

    // Compile meshes from builders associated with each texture.
    meshes := make([]Mesh, len(texture_lookup))
    for &mesh, index in meshes {
        mesh = to_mesh(builders[index])
    }

    model := Model {
        textures = textures[:],
        meshes   = meshes[:],
    }

    return model

    Mesh_Builder :: struct {
        vertices: [dynamic]Vertex,
        indices:  [dynamic]int,
    }

    to_mesh :: proc(builder: Mesh_Builder, allocator := context.allocator) -> Mesh {
        mesh := Mesh {
            vertices = builder.vertices[:],
            indices  = builder.indices[:],
        }
        return mesh
    }
}

@(private)
load_wavefront_materials :: proc(path: string) -> (textures: []Image, textures_lookup: map[string]int) {

    dir, _ := filepath.split(path)

    fmt.printfln("[INFO] Reading wavefront material library: {}", path)

    data, data_ok := os.read_entire_file(path)
    panic_on_error(data_ok)
    defer delete(data)

    materials: map[string]string
    defer delete(materials)

    name: string

    text_it := cast(string)data
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
        case "newmtl":
            name = strings.clone(parts[1], context.temp_allocator)
            materials[name] = ""
        case "map_Kd":
            materials[name] = filepath.join({dir, parts[1]}, context.temp_allocator)
        }
    }

    textures = make([]Image, len(materials))

    index: int
    for material_name, material_path in materials {
        if material_path == "" {
            fmt.eprintfln("[WARN] Material '{}' did not have a texture, defaulting to 1x1 white.", material_name)
            textures[index] = image_clone_aligned({WHITE}, 1, 1)
        } else {
            textures[index] = image_load(material_path)
        }
        textures_lookup[material_name] = index
        index += 1
    }

    return
}
