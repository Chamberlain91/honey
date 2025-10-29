package honey

import sa "core:container/small_array"
import "core:fmt"
import la "core:math/linalg"
import os "core:os/os2"
import "core:path/filepath"
import "core:strconv"
import "core:strings"
import "vendor:cgltf"

Model :: struct {
    textures: []Texture,
    meshes:   []Mesh,
}

delete_model :: proc(model: Model) {
    for mesh in model.meshes {
        delete(mesh.vertices)
        delete(mesh.indices)
    }
    delete(model.meshes)
    for tex in model.textures {
        for img in tex.mips {
            delete(img.data)
        }
        delete(tex.mips)
    }
    delete(model.textures)
}

// Parse a Wavefront `*.obj` model format. This is rudementary!
// This does not consider `*.mtl` files nor submeshes, smoothing groups, or other features.
load_wavefront_model :: proc(path: string, flip_winding := false, scale: f32 = 1.0, flip_uv := true) -> Model {

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
    textures: []Texture

    builders: []Mesh_Builder
    defer delete(builders)

    Face :: struct {
        position_index: int,
        texture_index:  int,
        normal_index:   int,
    }

    dir, file := filepath.split(path)

    fmt.printfln("[INFO] Reading wavefront model: {}", path)

    data, data_ok := os.read_entire_file(path, context.allocator)
    panic_on_error(data_ok)
    defer delete(data)

    material: ^Texture
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
            matlib_path, matlib_path_err := normalize_path_slash(
                filepath.join({dir, parts[1]}, context.temp_allocator),
                context.temp_allocator,
            )
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
            append(&positions, Vector3{x, y, z} * scale)

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
                f1 := sa.get(faces, flip_winding ? i + 1 : i)
                f2 := sa.get(faces, flip_winding ? i : i + 1)

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

    debug_model_stats(model, file)

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

    @(disabled = !DEV_BUILD)
    debug_model_stats :: proc(model: Model, name: string) {

        v_min, v_max: Vector3 = max(f32), min(f32)
        for mesh in model.meshes {
            for v in mesh.vertices {
                v_min = la.min(v.position, v_min)
                v_max = la.max(v.position, v_max)
            }
        }

        fmt.printfln("[INFO] Model '{}'", name)
        fmt.printfln("[INFO] - min: {: 7.3f}", v_min)
        fmt.printfln("[INFO] - max: {: 7.3f}", v_max)
    }

    load_wavefront_materials :: proc(path: string) -> (textures: []Texture, textures_lookup: map[string]int) {

        dir, _ := filepath.split(path)

        fmt.printfln("[INFO] Reading wavefront material library: {}", path)

        data, data_ok := os.read_entire_file(path, context.allocator)
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
                materials[name] = normalize_path_slash(
                    filepath.join({dir, parts[1]}, context.temp_allocator),
                    context.temp_allocator,
                )
            }
        }

        textures = make([]Texture, len(materials))

        index: int
        for material_name, material_path in materials {
            if material_path == "" {
                fmt.eprintfln("[WARN] Material '{}' did not have a texture, defaulting to 1x1 white.", material_name)
                textures[index] = to_texture(image_clone_aligned({WHITE}, 1, 1))
            } else {
                textures[index] = to_texture(image_load_path(material_path))
            }
            textures_lookup[material_name] = index
            index += 1
        }

        return
    }
}

load_gltf_model :: proc(path: string, scale: f32 = 1.0) -> Model {

    path_ := strings.clone_to_cstring(path, context.temp_allocator)

    options: cgltf.options
    data, data_err := cgltf.parse_file(options, path_)
    panic_on_error(data_err)
    defer cgltf.free(data)

    panic_on_error(cgltf.load_buffers(options, data, path_))

    // fmt.printfln("- asset: {}", data.asset)
    // fmt.printfln("- type: {}", data.file_type)
    // fmt.printfln("- {} meshes", len(data.meshes))
    // fmt.printfln("- {} animations", len(data.animations))
    // fmt.printfln("- {} textures", len(data.textures))
    // fmt.printfln("- {} materials", len(data.materials))

    out_meshes: [dynamic]Mesh
    out_textures: [dynamic]Texture

    // fmt.printfln("- mesh: {}", mesh)
    for mesh in data.meshes {

        // fmt.printfln("- primitives: {}", mesh.primitives)
        for primitive in mesh.primitives {
            assert(primitive.type == .triangles)

            // Find primary attributes.
            positions, positions_ok := find_accessor(primitive, .position)
            normals, normals_ok := find_accessor(primitive, .normal)
            uvs, uvs_ok := find_accessor(primitive, .texcoord)

            panic_on_error(positions_ok)
            panic_on_error(normals_ok)
            panic_on_error(uvs_ok)

            // Find skin attributes.
            skin_joints, skin_joints_ok := find_accessor(primitive, .joints)
            skin_weights, skin_weights_ok := find_accessor(primitive, .weights)

            panic_on_error(skin_joints_ok == skin_weights_ok)

            // TODO: Support skinned meshes
            _, _ = skin_joints, skin_weights

            // ---

            ensure(positions.data.component_type == .r_32f)
            ensure(positions.data.type == .vec3)
            ensure(!positions.data.normalized)

            ensure(normals.data.component_type == .r_32f)
            ensure(normals.data.type == .vec3)
            ensure(!normals.data.normalized)

            ensure(uvs.data.component_type == .r_32f)
            ensure(uvs.data.type == .vec2)
            ensure(!uvs.data.normalized)

            indices: [dynamic]int
            for i_index in 0 ..< primitive.indices.count {
                index := cgltf.accessor_read_index(primitive.indices, i_index)
                append(&indices, cast(int)index)
            }

            vertices: [dynamic]Vertex
            for index in 0 ..< positions.data.count {
                vertex: Vertex
                panic_on_error(cgltf.accessor_read_float(positions.data, index, raw_data(vertex.position[:]), 3))
                panic_on_error(cgltf.accessor_read_float(normals.data, index, raw_data(vertex.normal[:]), 3))
                panic_on_error(cgltf.accessor_read_float(uvs.data, index, raw_data(vertex.uv[:]), 2))

                vertex.position *= scale

                append(&vertices, vertex)
            }

            // primitive.material.alpha_mode
            // primitive.material.alpha_cutoff
            // primitive.material.double_sided
            // primitive.material.unlit
            // primitive.material.emissive_texture
            // primitive.material.emissive_strength
            // primitive.material.emissive_factor

            pbr_color_texture := primitive.material.pbr_metallic_roughness.base_color_texture

            texture := pbr_color_texture.texture
            image := texture.image_

            // TODO: Incorporate
            sampler := texture.sampler
            // sampler.wrap_t == .repeat
            // sampler.wrap_s
            // sampler.min_filter
            // sampler.mag_filter
            _ = sampler

            // TODO: Cache load by material image pointer
            image_bytes := cgltf.buffer_view_data(image.buffer_view)[:image.buffer_view.size]
            png_image := image_load_bytes(image_bytes)
            out_texture := to_texture(png_image)

            out_mesh := Mesh {
                indices  = indices[:],
                vertices = vertices[:],
            }
            append(&out_meshes, out_mesh)
            append(&out_textures, out_texture)
        }
    }

    model := Model {
        meshes   = out_meshes[:],
        textures = out_textures[:],
    }

    return model

    find_accessor :: proc(primitive: cgltf.primitive, type: cgltf.attribute_type) -> (^cgltf.attribute, bool) {
        for &attr in primitive.attributes {
            if attr.type == type do return &attr, true
        }
        return nil, false
    }
}
