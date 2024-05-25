# BplusApp.GL.BUFFER_LAYOUT_MACRO_DEBUG_STREAM = stdout

@std140 struct A
    f::Float32 # 1-4
    b::Bool # 5-8
    # Pad 9-16
    vf::v3f # 17 - 28
    i::Int32 # 29 - 32
    vi::v2i # 33 - 40
    # Pad 41-48
    bs::StaticBlockArray{25, Bool} # 49 - 448
    m::@Mat(3, 2, Float64) # 449 - 496
    # Struct alignment: 16
end

# For debugging, make a very clear to-string function for A.
# show() is tested and should not be changed.
function pretty_print(io::IO, a::A; indentation="")
    println(io, "A(")
        println(io, indentation, "\tf  = ", a.f)
        println(io, indentation, "\tb  = ", a.b)
        println(io, indentation, "\tvf = ", a.vf)
        println(io, indentation, "\ti  = ", a.i)
        println(io, indentation, "\tvi = ", a.vi)
        println(io, indentation, "\tbs = ...")
        println(io, indentation, "\tm = ", a.m)
    print(io, indentation, ")")
end

# Test struct A:
@bp_test_no_allocations(block_byte_size(A),
                        496,
                        "Struct 'A' in std140 layout is ", block_byte_size(A), " bytes")
@bp_test_no_allocations(block_alignment(A), 16)
@bp_test_no_allocations(block_padding_size(A), 16, "Struct 'A' in std140 layout")
function check_A_field(name::Symbol, type::Type, first_byte::Int)
    @bp_test_no_allocations(type <: block_property_type(A, Val(name)), true,
                            "Type of A's field '$name' should be $type but was $(block_property_type(A, Val(name)))")
    @bp_test_no_allocations(BplusApp.GL.block_property_first_byte(A, Val(name)), first_byte,
                            "First byte of A's field '$name'")
end
check_A_field(:f, Float32, 1)
check_A_field(:b, Bool, 5)
check_A_field(:vf, v3f, 17)
check_A_field(:i, Int32, 29)
check_A_field(:vi, v2i, 33)
check_A_field(:bs, StaticBlockArray{25, Bool, BplusApp.GL.OglBlock_std140}, 49)
check_A_field(:m, @Mat(3, 2, Float64), 449)
@bp_test_no_allocations(propertynames(A), (:f, :b, :vf, :i, :vi, :bs, :m))
@bp_test_no_allocations(block_property_types(A), (Float32, Bool, v3f, Int32, v2i, StaticBlockArray{25, Bool}, @Mat(3, 2, Float64)))
const A1_ARRAY = [(i%3)==0 for i in 1:25]
a1_args() = (@f32(1.4), true, v3f(4.4, 5.5, 6.6), 4, v2i(-3, -200),
             A1_ARRAY,
             @Mat(3, 2, Float64)(1, 2, 3, 4, 5, 6))
make_a_1() = A(a1_args()...)
@bp_check(make_a_1() isa A{Vector{UInt8}})
@bp_test_no_allocations(getproperty.(Ref(make_a_1()), propertynames(A)),
                        a1_args())
let sprinted = sprint(show, make_a_1()),
    sprinted_args = sprint.(Ref(show), a1_args())
  @bp_check(sprinted == "A($(join(sprinted_args, ", ")))",
            "Actual value: ", sprinted, "\n")
end

# This struct has only 3 floats, so it should have 12 bytes (padded to 16) and 16-byte alignment.
@std140 struct B
    f1::Float32
    f2::Float32
    f3::Float32
end
@bp_test_no_allocations(block_byte_size(B), 16, "@std140 struct B")
@bp_test_no_allocations(block_padding_size(B), 4, "@std140 struct B")
@bp_test_no_allocations(block_alignment(B), 16, "@std140 struct B")
function check_B_field(name::Symbol, type::Type, first_byte::Int)
    @bp_test_no_allocations(block_property_type(B, Val(name)), type, "Type of B's field '$name'")
    @bp_test_no_allocations(BplusApp.GL.block_property_first_byte(B, Val(name)), first_byte,
                            "First byte of B's field '$name'")
end
check_B_field(:f1, Float32, 1)
check_B_field(:f2, Float32, 5)
check_B_field(:f3, Float32, 9)
@bp_test_no_allocations(propertynames(B), (:f1, :f2, :f3))
@bp_test_no_allocations(block_property_types(B), (Float32, Float32, Float32))
const TEST_B_ARGS = Float32.((1, 2, 3))
const TEST_B = B(1, 2, 3)
@bp_check(TEST_B isa B{Vector{UInt8}})
@bp_test_no_allocations(getproperty.(Ref(TEST_B), propertynames(B)),
                        TEST_B_ARGS)
let sprinted = sprint(show, TEST_B),
    sprinted_args = sprint.(Ref(show), TEST_B_ARGS)
  @bp_check(sprinted == "B($(join(sprinted_args, ", ")))",
            "Actual value: ", sprinted, "\n")
end


# C tests nested structs and some other types.
@std140 struct C
    b::B # 1-16
    as::StaticBlockArray{3, A} # 17 - 1504
    f::Float32 # 1505 - 1508
    # Pad 12 bytes, 1509 - 1520
    bs::StaticBlockArray{500, B} # 1521 - 9520
    a::A # 9521 - 10016
    m64::@Mat(4, 3, Float64) # v3d, size 24, alignment 32
                             # Total size 128
                             # 10017 - 10144
    m32s::StaticBlockArray{11, @Mat(2, 4, Float32)} # v4f, size 16, alignment 16
                                                    # Total size 22*16=352
                                                    # 10145 - 10496
    # Struct alignment: 32
end
@bp_test_no_allocations(block_byte_size(C), 10496)
@bp_test_no_allocations(block_padding_size(C), 12)
@bp_test_no_allocations(block_alignment(C), 32)
function check_C_field(name::Symbol, type::Type, first_byte::Int)
    @bp_test_no_allocations(type <: block_property_type(C, Val(name)), true,
                            "Type of C's field '$name' should be $type but was $(block_property_type(C, Val(name)))")
    @bp_test_no_allocations(BplusApp.GL.block_property_first_byte(C, Val(name)), first_byte,
                            "First byte of C's field '$name'")
end
check_C_field(:b, B, 1)
check_C_field(:as, StaticBlockArray{3, A, BplusApp.GL.OglBlock_std140}, 17)
check_C_field(:f, Float32, 1505)
check_C_field(:bs, StaticBlockArray{500, B, BplusApp.GL.OglBlock_std140}, 1521)
check_C_field(:a, A, 9521)
check_C_field(:m64, dmat4x3, 10017)
check_C_field(:m32s, StaticBlockArray{11, fmat2x4, BplusApp.GL.OglBlock_std140}, 10145)
@bp_test_no_allocations(propertynames(C), (:b, :as, :f, :bs, :a, :m64, :m32s))
@bp_test_no_allocations(block_property_types(C), (B, StaticBlockArray{3, A}, Float32, StaticBlockArray{500, B}, A, dmat4x3, StaticBlockArray{11, fmat2x4}))
const TEST_C_ARGS = (
    B(3, 4, 5),
    [A(
        i*0.5f0,
        (i%5)==0,
        v3f(i/2.0f0, i*30.0f0, -i),
        -i*10,
        v2i(i, i+1),
        [((j*i)%4)==0 for j in 1:25],
        dmat3x2(i-1, i-2, i-3, i-4, i-5, i-6)
    ) for i in 1:3],
    20.02f0,
    [B(i * -3.4, i, i + 7.53) for i in 1:500],
    A(
        23.231f0,
        false,
        v3f(10, 22, 13),
        111111,
        v2u(899, 888),
        [((i%10) == 1) for i in 1:25],
        dmat3x2(
            81, 82, 83,
            944, 05, 96
        )
    ),
    dmat4x3(
        111, 112, 113, 114,
        222, 223, 224, 225,
        335, 366, 454, 666
    ),
    [fmat2x4(i-10, i-100, i-1000, i-10000, i+11, i+111, i+1111, i+11111) for i in 1:11]
)
const TEST_C = C(TEST_C_ARGS...)
@bp_check(TEST_C isa C{Vector{UInt8}})
for (f, T, expected, actual) in zip(propertynames(C), block_property_types(C), TEST_C_ARGS, getproperty.(Ref(TEST_C), propertynames(C)))
    @bp_check(expected == actual,
              "C.", f, "::", T, " should be:\n", expected, "\n\n  but is\n", actual)
end
@bp_test_no_allocations(getproperty.(Ref(TEST_C), propertynames(C)),
                        TEST_C_ARGS)
# Testing show() on a struct this big is impractical.


# Test the generated GLSL code and that the layout matches up with OpenGL,
#    by writing a compute shader that reads/writes the data.
bp_gl_context( v2i(300, 300), "std140 test with compute shader";
                vsync=VsyncModes.on,
                debug_mode=true
             ) do context::Context
    shader = BplusApp.GL.bp_glsl_str("""
        #START_COMPUTE
        layout(local_size_x = 1) in;

        struct A {
            $(glsl_decl(A))
        };
        struct B {
            $(glsl_decl(B))
        };

        layout(std140, binding = 0) uniform Cin {
            $(glsl_decl(C))
        } u_in;
        layout(std140, binding = 0) buffer Cout {
            $(glsl_decl(C))
        } u_out;

        void main() {
            $((
                "u_out.$f = u_in.$f;"
                 for f in propertynames(C)
            )...)
        }
    """, debug_out = stderr)

    # Send the data to one GPU buffer.
    c_cpu = TEST_C
    c_in_gpu = Buffer(true, c_cpu)
    set_uniform_block(c_in_gpu, 1)

    # Set up a second GPU buffer to receive it.
    c_out_gpu = Buffer(true, C())
    set_shader_storage_block(c_out_gpu, 1)

    # Run the shader which copies the data.
    dispatch_compute_groups(shader, one(v3i))
    gl_catch_up_before(MemoryActions.buffer_download_or_upload)

    # Download and test the results.
    c_cpu_bytes = Vector{UInt8}(undef, block_byte_size(C))
    get_buffer_data(c_out_gpu, c_cpu_bytes)
    c_cpu_2 = C{Vector{UInt8}}(c_cpu_bytes)
    @bp_test_no_allocations(c_cpu, c_cpu_2)
end