import GLFW
using ModernGLbp
include("util.jl")

# Check the name/type of the GLENUM constants.
function test_gl_enum(i::Integer, name::Symbol, expected_type::DataType = GLenum)
    e = GLENUM(i)
    if e.name != name
        error("GLENUM(", i, ") should be :", name, " but is :", e.name)
    end
    if typeof(e.number) != expected_type
        error(e, " should be a ", expected_type, ", but it's a ", typeof(e.number))
    end
end
test_gl_enum(0x82EE, :GL_VERTICES_SUBMITTED)
test_gl_enum(0x000080000, :GL_QUERY_BUFFER_BARRIER_BIT, GLbitfield)
test_gl_enum(Int(1), :GL_TRUE)
test_gl_enum(Int8(1), :GL_TRUE)
test_gl_enum(UInt(1), :GL_TRUE)
test_gl_enum(UInt8(1), :GL_TRUE)
test_gl_enum(Int(0), :GL_FALSE)
test_gl_enum(Int8(0), :GL_FALSE)
test_gl_enum(UInt(0), :GL_FALSE)
test_gl_enum(UInt8(0), :GL_FALSE)


is_ci() = lowercase(get(ENV, "CI", "false")) == "true"

if !is_ci() # only do test if not CI... this is for automated testing environments which fail for OpenGL stuff, but I'd like to test if at least including works

    GLFW.Init()
    # OS X-specific GLFW hints to initialize the correct version of OpenGL
    wh = 600
    # Create a windowed mode window and its OpenGL context
    window = GLFW.CreateWindow(wh, wh, "OpenGL Example")
    # Make the window's context current
    GLFW.MakeContextCurrent(window)
    GLFW.ShowWindow(window)
    GLFW.SetWindowSize(window, wh, wh) # Seems to be necessary to guarantee that window > 0

    glViewport(0, 0, wh, wh)

    println(createcontextinfo())
    # The data for our triangle
    data = GLfloat[
        0.0, 0.5,
        0.5, -0.5,
        -0.5,-0.5
    ]
    # Generate a vertex array and array buffer for our data
    vao = glGenVertexArray()
    glBindVertexArray(vao)
    vbo = glGenBuffer()
    glBindBuffer(GL_ARRAY_BUFFER, vbo)
    glBufferData(GL_ARRAY_BUFFER, sizeof(data), data, GL_STATIC_DRAW)
    # Create and initialize shaders
    const vsh = """
    $(get_glsl_version_string())
    in vec2 position;
    void main() {
        gl_Position = vec4(position, 0.0, 1.0);
    }
    """
    const fsh = """
    $(get_glsl_version_string())
    out vec4 outColor;
    void main() {
        outColor = vec4(1.0, 1.0, 1.0, 1.0);
    }
    """
    vertexShader = createShader(vsh, GL_VERTEX_SHADER)
    fragmentShader = createShader(fsh, GL_FRAGMENT_SHADER)
    program = createShaderProgram(vertexShader, fragmentShader)
    glUseProgram(program)
    positionAttribute = glGetAttribLocation(program, "position");
    glEnableVertexAttribArray(positionAttribute)
    glVertexAttribPointer(positionAttribute, 2, GL_FLOAT, false, 0, C_NULL)
    # Loop until the user closes the window
    for i=1:500
    	# Pulse the background blue
    	glClearColor(0.0, 0.0, 0.5 * (1 + sin(i * 0.02)), 1.0)
    	glClear(GL_COLOR_BUFFER_BIT)
    	# Draw our triangle
    	glDrawArrays(GL_TRIANGLES, 0, 3)
    	# Swap front and back buffers
    	GLFW.SwapBuffers(window)
    	# Poll for and process events
    	GLFW.PollEvents()
    end
    GLFW.Terminate()

end
