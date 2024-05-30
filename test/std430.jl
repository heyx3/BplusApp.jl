# BplusApp.GL.BUFFER_LAYOUT_MACRO_DEBUG_STREAM = stdout

@std430 struct A
    f::Float32 # 1-4
    b::Bool # 5-8
    # Pad 9-16
    vf::v3f # 17 - 28
    i::Int32 # 29 - 32
    vi::v2i # 33 - 40
    bs::StaticBlockArray{25, Bool} # 41 - 140
    # Pad 141-144
    m::@Mat(3, 2, Float64) # 145 - 192
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
@bp_test_no_allocations(block_alignment(A), 16)
@bp_test_no_allocations(block_byte_size(A), 192)
@bp_test_no_allocations(block_padding_size(A), 12)
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
check_A_field(:bs, StaticBlockArray{25, Bool, BplusApp.GL.OglBlock_std430}, 41)
check_A_field(:m, @Mat(3, 2, Float64), 145)
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

# This struct has only 3 floats, so it should have 12 bytes and 4-byte alignment.
@std430 struct B
    f1::Float32
    f2::Float32
    f3::Float32
end
@bp_test_no_allocations(block_byte_size(B), 12)
@bp_test_no_allocations(block_padding_size(B), 0)
@bp_test_no_allocations(block_alignment(B), 4)
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
@std430 struct C
    b::B # 1-12
    # Pad 4, to 13 - 16
    as::StaticBlockArray{3, A} # 17 - 592
    f::Float32 # 593 - 596
    bs::StaticBlockArray{500, B} # 597 - 6596
    # Pad 12, to 6597-6608
    a::A # 6609 - 6800
    # Pad 16, to 6801 - 6816
    m64::@Mat(4, 3, Float64) # v3d, size 24, alignment 32
                             # Total size 128
                             # 6817 - 6944
    m32s::StaticBlockArray{11, @Mat(2, 4, Float32)} # v4f, size 16, alignment 16
                                                    # Total size 22*16=352
                                                    # 6945 - 7296
    # Struct alignment: 32
end
@bp_test_no_allocations(block_alignment(C), 32)
@bp_test_no_allocations(block_padding_size(C), 32)
@bp_test_no_allocations(block_byte_size(C), 7296)
function check_C_field(name::Symbol, type::Type, first_byte::Int)
    @bp_test_no_allocations(type <: block_property_type(C, Val(name)), true,
                            "Type of C's field '$name' should be $type but was $(block_property_type(C, Val(name)))")
    @bp_test_no_allocations(BplusApp.GL.block_property_first_byte(C, Val(name)), first_byte,
                            "First byte of C's field '$name'")
end
check_C_field(:b, B, 1)
check_C_field(:as, StaticBlockArray{3, A, BplusApp.GL.OglBlock_std430}, 17)
check_C_field(:f, Float32, 593)
check_C_field(:bs, StaticBlockArray{500, B, BplusApp.GL.OglBlock_std430}, 597)
check_C_field(:a, A, 6609)
check_C_field(:m64, dmat4x3, 6817)
check_C_field(:m32s, StaticBlockArray{11, fmat2x4, BplusApp.GL.OglBlock_std430}, 6945)
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
function compare_blocks(block_expected::T, block_actual::T, T_name) where {T<:AbstractOglBlock}
    for f in propertynames(T)
        expected = getproperty(block_expected, f)
        actual = getproperty(block_actual, f)
        @bp_check(typeof(expected) == typeof(actual),
                  T_name, ".", f, ": Type: ", typeof(expected), " vs ", typeof(actual))
        if (expected isa AbstractArray) && !isa(expected, Vec)
            @bp_check(length(expected) == length(actual),
                      T_name, ".", f, ": Count: ", length(expected), " vs ", length(actual))
            for (i, el_expected, el_actual) in zip(1:length(expected), expected, actual)
                @bp_check(el_expected == el_actual,
                          T_name, ".", f, "[", i, "]: ",
                            el_expected, " vs ", el_actual)
            end
        else
            @bp_check(expected == actual,
                      T_name, ".", f, ": ", expected, " vs ", actual)
        end
    end
end
function run_block_compute_test(test_value::Union{AbstractOglBlock, BlockArray},
                                type_name, shader_src)
    BlockType = block_simple_type(typeof(test_value))

    gpu_in = Buffer(true, test_value)
    gpu_out = Buffer(true, BlockType)
    set_storage_block(gpu_in, 1)
    set_storage_block(gpu_out, 2)

    shader = BplusApp.GL.bp_glsl_str(shader_src)#, debug_out = stderr)
    dispatch_compute_groups(shader, one(v3i))
    gl_catch_up_before(MemoryActions.ALL) #TODO: Switch back to 'buffer_download_or_upload'

    cpu_expected = test_value
    cpu_actual = get_buffer_data(gpu_out, BlockType)
    compare_blocks(cpu_expected, cpu_actual, type_name)
    @bp_test_no_allocations(cpu_expected, cpu_actual)
end
bp_gl_context( v2i(300, 300), "std430 test with compute shader";
                vsync=VsyncModes.on,
                debug_mode=true
             ) do context::Context
    # Test B, the simplest struct, first.
    # Test C, the most complex struct, last.
    #TODO: Test BlockArray{A}, BlockArray{B}, and BlockArray{C} as well
    #TODO: Test passing true for the 'recommend_storage_on_cpu' flag

    run_block_compute_test(TEST_B, :B, """
        #START_COMPUTE
        layout(local_size_x = 1) in;

        layout(std430, binding = 0) buffer Bin {
            $(glsl_decl(B))
        } u_in;
        layout(std430, binding = 1) buffer Bout {
            $(glsl_decl(B))
        } u_out;

        void main() {
            $((
                "u_out.$f = u_in.$f;"
                 for f in propertynames(B)
            )...)
        }
    """)

    run_block_compute_test(make_a_1(), :A, """
        #START_COMPUTE
        layout(local_size_x = 1) in;

        layout(std430, binding = 0) buffer Ain {
            $(glsl_decl(A))
        } u_in;
        layout(std430, binding = 1) buffer Aout {
            $(glsl_decl(A))
        } u_out;

        void main() {
            $((
                "u_out.$f = u_in.$f;"
                 for f in propertynames(A)
            )...)
        }
    """)

    run_block_compute_test(TEST_C, :C, """
        #START_COMPUTE
        layout(local_size_x = 1) in;

        struct A {
            $(glsl_decl(A))
        };
        struct B {
            $(glsl_decl(B))
        };

        layout(std430, binding = 0) buffer Cin {
            $(glsl_decl(C))
        } u_in;
        layout(std430, binding = 1) buffer Cout {
            $(glsl_decl(C))
        } u_out;

        void main() {
            $((
                "u_out.$f = u_in.$f;"
                 for f in propertynames(C)
            )...)
        }
    """)
end