"
Generates the source code for a vertex shader that uses
  the graphics service's `screen_triangle` and `screen_quad`.

You can add your own transformation to the vertex position by passing code
  which manipulates the screen coordinate `pos4`.
Afterwards the `vec2 uv` is generated from `pos4`, and you can modify it by passing more code.
"
make_vertex_shader_blit(modify_pos4 = "", modify_uv = "") = """
    in vec2 vIn_corner;
    out vec2 vOut_uv;
    void main() {
        vec4 pos4 = vec4(vIn_corner, 0.5, 1);
        $modify_pos4;
        gl_Position = pos4;

        vec2 uv = 0.5 + (0.5 * pos4.xy);
        $modify_uv;
        vOut_uv = uv;
    }
"""

"
A Context service which defines a bunch of useful GL resources:

 * `screen_triangle` : A 1-triangle mesh with 2D positions in NDC-space.
                       When drawn, it will perfectly cover the entire screen,
                          making it easy to spin up post-processing effects.
                       The UV coordinates can be calculated from the XY positions
                          (or from gl_FragCoord).
 * `screen_quad` : A 2-triangle mesh describing a square, with 2D coordinates
                in the range (-1, -1) to (+1, +1).
            This _can_ be used for post-processing effects, but it's less efficient
                than `screen_triangle`, for technical reasons.
 * `blit` : A simple shader to render a 2D texture (e.x. copy a Target to the screen).
            Refer to `simple_blit()`.
 * `empty_mesh` : A mesh with no vertex data, for dispatching entirely procedural geometry.
                  Has the 'points' PrimitiveType.
"
@bp_service BasicGraphics(force_unique) begin
    screen_triangle::Mesh
    screen_quad::Mesh
    blit::Program
    empty_mesh::Mesh

    # The master list of things this service owns and needs to clean up.
    references::Set{AbstractResource}


    INIT() = begin
        screen_tri_poses = Buffer(false, map(Vec{2, Int8}, [
            (-1, -1),
            (3, -1),
            (-1, 3)
        ]))
        screen_tri = Mesh(PrimitiveTypes.triangle_strip,
                        [ VertexDataSource(screen_tri_poses, sizeof(Vec{2, Int8})) ],
                        [ VertexAttribute(1, 0x0, VSInput_FVector(Vec2{Int8}, false)) ])

        quad_poses = Buffer(false, map(Vec{2, Int8}, [
            (-1, -1),
            (1, -1),
            (-1, 1),
            (1, 1)
        ]))
        screen_quad = Mesh(PrimitiveTypes.triangle_strip,
                           [ VertexDataSource(quad_poses, sizeof(Vec{2, Int8})) ],
                           [ VertexAttribute(1, 0x0, VSInput_FVector(Vec2{Int8}, false)) ])

        blit = GL.bp_glsl_str("""
            uniform mat3 u_mesh_transform = mat3(1, 0, 0,
                                                 0, 1, 0,
                                                 0, 0, 1);
            uniform sampler2D u_tex;
            uniform mat4 u_color_map = mat4(1, 0, 0, 0,
                                            0, 1, 0, 0,
                                            0, 0, 1, 0,
                                            0, 0, 0, 1);
            uniform float u_curve = 1.0;

            #START_VERTEX
            $(make_vertex_shader_blit("pos4.xyz = u_mesh_transform * vec3(pos4.xy, 1.0);    pos4.xy /= pos4.z;   pos4.z = 0.5"))

            #START_FRAGMENT
            in vec2 vOut_uv;
            out vec4 vOut_color;
            void main() {
                vOut_color = pow(u_color_map * texture(u_tex, vOut_uv),
                                 vec4(u_curve, u_curve, u_curve, u_curve));
            }
        """)

        empty_mesh = GL.Mesh(GL.PrimitiveTypes.point,
                             GL.VertexDataSource[ ],
                             GL.VertexAttribute[ ])

        return new(
            screen_tri, screen_quad, blit, empty_mesh,
            Set(AbstractResource[
                screen_tri_poses, screen_tri,
                quad_poses, screen_quad,
                blit,
                empty_mesh
            ])
        )
    end
    SHUTDOWN(service, is_context_closing::Bool) = begin
        if !is_context_closing
            close.(c_basic_graphics.references)
        end
    end

    "
    Renders the given texure, using the given screen_quad transform
        and the given color transform on the texture's pixels.
    "
    function simple_blit(service,
                         tex::Union{Texture, View},
                         ;
                         quad_transform::fmat3x3 = m_identityf(3, 3),
                         color_transform::fmat4x4 = m_identityf(4, 4),
                         output_curve::Float32 = 1.0f0,
                         disable_depth_test::Bool = true,
                         manage_tex_view::Bool = true
                        )
        context = get_context()

        tex_view = (tex isa View) ? tex : get_view(tex)
        wrap_tex_activation::Bool = false
        if manage_tex_view && !tex_view.is_active
            wrap_tex_activation = true
            view_activate(tex_view)
        end

        set_uniform(service.blit, "u_tex", tex_view)
        set_uniform(service.blit, "u_mesh_transform", quad_transform)
        set_uniform(service.blit, "u_color_map", color_transform)
        set_uniform(service.blit, "u_curve", output_curve)

        old_depth_test = context.state.depth_test
        if disable_depth_test
            set_depth_test(context, ValueTests.pass)
        end

        # If drawing full-screen, use the more efficient triangle.
        # If transforming it, use the more precise, intuitive screen_quad.
        render_mesh((quad_transform == m_identityf(3, 3)) ?
                        service.screen_triangle :
                        service.screen_quad,
                    service.blit)

        if wrap_tex_activation
            view_deactivate(tex_view)
        end

        set_depth_test(context, old_depth_test)
    end
end

export Service_BasicGraphics, service_BasicGraphics,
       service_BasicGraphics_init, service_BasicGraphics_shutdown,
       simple_blit, make_vertex_shader_blit