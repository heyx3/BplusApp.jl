# BplusApp.GL.BUFFER_LAYOUT_MACRO_DEBUG_STREAM = stdout

@std140 struct A
    f::Float32 # 1-4
    b::Bool # 5-8
    vf::v3f # 17 - 28
    i::Int32 # 29 - 32
    vi::v2i # 33 - 40
    # Pad 41-48
    bs::NTuple{25, Bool} # 49 - 448
    m::@Mat(3, 2, Float64) # 449 - 496
    # Struct alignment: 16
end

# For debugging, make a very clear print() for A.
function Base.print(io::IO, a::A; indentation="")
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
@bp_test_no_allocations(propertynames(A), (:f, :b, :vf, :i, :vi, :bs, :m))
@bp_test_no_allocations(block_property_types(A), (Float32, Bool, v3f, Int32, v2i, NTuple{25, Bool}, @Mat(3, 2, Float64)))
function check_A_field(name::Symbol, type::Type, first_byte::Int)
    @bp_test_no_allocations(block_property_type(A, Val(name)), type, "Type of A's field '$name'")
    @bp_test_no_allocations(BplusApp.GL.block_property_first_byte(A, Val(name)), first_byte,
                            "First byte of A's field '$name'")
end
check_A_field(:f, Float32, 1)
check_A_field(:b, Bool, 5)
check_A_field(:vf, v3f, 17)
check_A_field(:i, Int32, 29)
check_A_field(:vi, v2i, 33)
check_A_field(:bs, NTuple{25, Bool}, 49)
check_A_field(:m, @Mat(3, 2, Float64), 449)
a1_args() = (@f32(1.4), true, v3f(4.4, 5.5, 6.6), 4, v2i(-3, -200),
             ntuple(i -> (i%3)==0, Val(25)),
             @Mat(3, 2, Float64)(1, 2, 3, 4, 5, 6))
make_a_1() = A(a1_args()...)
@bp_test_no_allocations(typeof(make_a_1()), A{Vector{UInt8}})
@bp_test_no_allocations(getproperty.(Ref(make_a_1()), propertynames(A)),
                        a1_args())
let sprinted = sprint(show, make_a_1()),
    sprinted_args = sprint.(Ref(show), a1_args())
  @bp_check(sprinted == "A($(join(sprinted_args, ", ")))",
            "Actual value: ", sprinted, "\n")
end
@bp_test_no_allocations(block_alignment(A), 16)
#TODO: Test padding byte count
#TODO: Test bool array
#TODO: Test matrix


@std140 struct B
    a::A # 32B
    m::fmat4 # 64B = 96
    i::Int32 # 4B = 100
    # Pad 12B to align with dvec2 (16B): 112
    d::dmat3x2 # 48B = 160
    bs::NTuple{6, Bool} # 96B = 256
    backup_as::NTuple{10, A} # 320B = 576
end

# For debugging, make a very clear print() for B.
function Base.print(io::IO, b::B; indentation="")
    println(io, "B(")
        print(io, indentation, "\t", "a = ")
            print(io, b.a; indentation=indentation*"\t")
            println(io)
        println(io, indentation, "\t", "m = ", b.m)
        println(io, indentation, "\t", "i = ", b.i)
        println(io, indentation, "\t", "d = ", b.d)
        println(io, indentation, "\t", "bs = [")
        for bool in b.bs
            println(io, indentation, "\t\t", bool)
        end
        println(io, indentation, "]")
        println(io, indentation, "\t", "backup_as = [")
        for a in b.backup_as
            print(io, indentation, "\t\t")
            print(io, a; indentation=indentation * "\t\t")
            println(io)
        end
        println(io, indentation, "]")
    print(io, indentation, ")")
end

# Test struct B:
@bp_test_no_allocations(sizeof(B), 576)
@bp_test_no_allocations(propertynames(B), (:a, :m, :i, :d, :bs, :backup_as),
                        propertynames(B))
@bp_test_no_allocations(BplusApp.GL.property_types(B), (A, fmat4, Int32, dmat3x2, NTuple{6, Bool}, NTuple{10, A}))
Random.rand(::Type{A})::A = A(rand(Float32), rand(Bool), rand(v4f))
Random.seed!(0x57483829)
let in_data = (
                   rand(A),
                   fmat4(4.1, 4.2, 4.3, 4.4,
                         4.5, 4.6, 4.7, 4.8,
                         4.9, 4.01, 4.02, 4.03,
                         4.04, 4.05, 4.06, 4.07),
                   Int32(-666),
                   dmat3x2(7.7, 7.8, 7.9,
                           8.7, 8.8, 8.9),
                   ntuple(i -> rand(Bool), 6),
                   ntuple(i -> rand(A), 10)
              ),
    make_b = () -> B(in_data...)
    @bp_test_no_allocations(make_b(), make_b(),
                            "Constructor should create an equal value without allocation")
    let b = Ref(make_b())
        @bp_test_no_allocations(getproperty.(b, propertynames(B)),
                                in_data,
                                "Reading all properties: ",
                                    getproperty.(b, propertynames(B)))
    end
end
function check_B_field(name::Symbol, type::Type, offset::Int)
    @bp_test_no_allocations(BplusApp.GL.property_type(B, name), type,
                            "Field B.", name)
    @bp_test_no_allocations(BplusApp.GL.property_offset(B, name), offset,
                            "Field B.", name)
end
check_B_field(:m, fmat4, 32)
check_B_field(:d, dmat3x2, 112)
check_B_field(:bs, NTuple{6, Bool}, 160)
check_B_field(:backup_as, NTuple{10, A}, 256)
@bp_test_no_allocations(BplusApp.GL.block_alignment(B), 16)


@std140 struct C
    bool_vec::Vec{2, Bool} # 8B
    # Pad 8B to align with struct B (16B): 16
    b::B # 576B = 592
    f1::Float32 # 596
    # Pad 12B to align with v4f: 608
    array1::NTuple{10, Vec{2, Bool}} # alignment is v4f
                                     # 160B = 768
    array2::NTuple{5, fmat3x2} # alignment is v2f => v4f
                               # element stride is v4f*3 = 48
                               # 240B = 1008
    # No padding needed to align with 16B base alignment
end

# For debugging, make a very clear print() for C.
function Base.print(io::IO, c::C; indentation="")
    println(io, "C(")
        println(io, indentation, "\t", "bool_vec = ", c.bool_vec)
        print(io, indentation, "\t", "b = ")
            print(io, c.b; indentation=indentation*"\t")
            println(io)
        println(io, indentation, "\t", "f1 = ", c.f1)
        println(io, indentation, "\t", "array1 = [")
        for v in c.array1
            println(io, indentation, "\t\t", v)
        end
        println(io, indentation, "]")
        println(io, indentation, "\t", "array2 = [")
        for m in c.array2
            println(io, indentation, "\t\t", m)
        end
        println(io, indentation, "]")
    print(io, indentation, ")")
end

# Test struct C:
@bp_test_no_allocations(sizeof(C), 1008)
@bp_test_no_allocations(propertynames(C), (:bool_vec, :b, :f1, :array1, :array2))
@bp_test_no_allocations(BplusApp.GL.property_types(C), (Vec{2, Bool}, B, Float32, NTuple{10, Vec{2, Bool}}, NTuple{5, fmat3x2}))
Random.rand(::Type{B})::B = B(
    rand(A),
    rand(fmat4),
    rand(Int32),
    rand(dmat3x2),
    ntuple(i -> rand(Bool), 6),
    ntuple(i -> rand(A), 10)
)
Random.seed!(0xbafacada)
let in_data = (
                  v2b(false, true),
                  rand(B),
                  -1.2f0,
                  ntuple(i -> Vec(rand(Bool), rand(Bool)),
                         Val(10)),
                  ntuple(i -> rand(fmat3x2), Val(5))
              )
    make_c = () -> C(in_data...)
    @bp_test_no_allocations(make_c(), make_c(),
                            "Constructor should create an equal value without allocation")
    let c = Ref(make_c())
        get_props = () -> getproperty.(c, propertynames(C))
        for (prop, expected) in zip(propertynames(C), in_data)
            @bp_check(getproperty(c[], prop) == expected,
                      "Property ", prop, " should be:\n\t",
                        expected, "\n",
                      " but was:\n\t", getproperty(c[], prop))
        end
        @bp_test_no_allocations(typeof.(get_props()), typeof.(in_data),
                                "Property types")
        @bp_test_no_allocations(get_props(), in_data,
                                "Property values")
    end
end
@inline function check_C_field(name::Symbol, type::Type, offset::Int)
    @bp_test_no_allocations(BplusApp.GL.property_type(C, name), type,
                            "Property C.", name)
    @bp_test_no_allocations(BplusApp.GL.property_offset(C, name), offset,
                            "Property C.", name)
end
check_C_field(:bool_vec, Vec{2, Bool}, 0)
check_C_field(:b, B, 16)
check_C_field(:f1, Float32, 592)
check_C_field(:array1, NTuple{10, Vec{2, Bool}}, 608)
check_C_field(:array2, NTuple{5, fmat3x2}, 768)
@bp_test_no_allocations(BplusApp.GL.block_alignment(C), 16)

#TODO: Test BplusApp.GL.glsl_decl() in both std140 and std430
