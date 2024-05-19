# Use the macros @std140 and @std430 to automatically pad your struct to match its GPU layout.
# The struct that is generated holds a byte array on the heap
#    and offers properties to get/set fields by updating this byte array.
# It can also be automatically converted to a pointer for C calls/GPU uploads.

"
Some kind of bitstype data, laid out in a way that OpenGL/GLSL can understand.
While the structs themselves are immutable, they are backed by a mutable array
    so you can set their properties.

To pass it into a C function, wrap it with a `Ref()` call.
To create an array of your block type `T`, construct `BlockArray` with either
    `BlockArray{T}(bytes::AbstractVector{UInt8})` or
    `BlockArray{T}(count::Int)`.
"
abstract type AbstractOglBlock end
abstract type OglBlock_std140 <: AbstractOglBlock end
abstract type OglBlock_std430 <: AbstractOglBlock end
#TODO: Dynamically support both 140 and 430 for each block type

"Gets the layout type of a struct (or struct type). An example return value is `OglBlock_std140`"
block_mode(a::AbstractOglBlock) = block_mode(typeof(a))
block_mode(::Type{<:OglBlock_std140}) = OglBlock_std140
block_mode(::Type{<:OglBlock_std430}) = OglBlock_std430


#  Interface:  #

"
Gets the total byte-size of the given struct (or array property), including padding.
Use this instead of `sizeof()`.
"
block_byte_size(x::AbstractOglBlock) = block_byte_size(typeof(x))
block_byte_size(T::Type{<:AbstractOglBlock}) = error("Not implemented: ", T)

"Gets the amount of padding in the given struct, in bytes"
padding_size(b::AbstractOglBlock) = padding_size(typeof(b))
padding_size(T::Type{<:AbstractOglBlock}) = error("Not implemented: ", T)

block_alignment(b::AbstractOglBlock) = block_alignment(typeof(b))
block_alignment(T::Type{<:AbstractOglBlock}) = error("Not implemented: ", T)

"Gets the bytes of a block, as a mutable array of `UInt8`"
block_byte_array(b::AbstractOglBlock)::AbstractVector{UInt8} = getfield(b, :buffer)

"Returns the type of a property"
@inline property_type(b::AbstractOglBlock, name::Symbol) = property_type(typeof(b), Val(name))
@inline property_type(T::Type{<:AbstractOglBlock}, name::Symbol) = property_type(T, Val(name))
@inline property_type(T::Type{<:AbstractOglBlock}, Name::Val) = error(T, " has no property '", val_type(Name))

property_types(b::AbstractOglBlock) = property_types(typeof(b))
property_types(T::Type{<:AbstractOglBlock}) = error(T, " didn't implement ", property_types)

"Returns the byte offset to a property"
@inline property_offset(b::AbstractOglBlock, name::Symbol) = property_offset(typeof(b), name)
@inline property_offset(T::Type{<:AbstractOglBlock}, name::Symbol) = property_offset(T, Val(name))
@inline property_offset(T::Type{<:AbstractOglBlock}, Name::Val) = error(T, " has no property '", val_type(Name), "'")


"Parameters for a declaration of an OpenGL block"
@kwdef struct GLSLBlockDecl
    # Optional name that the block's fields will be nested within for GLSL code.
    glsl_name::Optional{AbstractString} = nothing
    # The official name of the block, outside of GLSL code (which use 'glsl_name').
    open_gl_name::AbstractString

    # The type of block. Usually "uniform" or "buffer".
    type::AbstractString

    # Extra arguments to the "layout(...)" section.
    layout_qualifiers::AbstractString = ""
    # Optional final block element representing a dynamically-sized array.
    final_array_field::Optional{AbstractString} = nothing

    # Maps nested structs to their corresponding name in GLSL.
    # If a type isn't in this lookup, its Julia name is carried into GLSL.
    type_names::Dict{Type, String} = Dict{Type, String}()
end

"Gets a GLSL string declaring the given block type"
glsl_decl(b::AbstractOglBlock, params::GLSLBlockDecl = GLSLBlockDecl()) = glsl_decl(typeof(b), params)
glsl_decl(T::Type{<:AbstractOglBlock}, params::GLSLBlockDecl = GLSLBlockDecl()) = begin
    layout_std = if T <: OglBlock_std140
                     "std140"
                 elseif T <: OglBlock_std430
                     "std430"
                 else
                     error("Unknown subtype: ", T)
                 end
    return "layout($layout_std, $(params.layout_qualifiers)) $(params.type) $(params.open_gl_name)
    {
        $((
            "$(glsl_type_decl(block_property_type(T, f), string(f), params.type_names)) $f;"
                for f in propertynames(T)
        )...)
    } $(exists(params.glsl_name) ? params.glsl_name : "") ;"
end

glsl_type_decl(::Type{Bool}, name::String, struct_lookup::Dict{Type, String}) = "bool $name;"
glsl_type_decl(::Type{Int32}, name::String, struct_lookup::Dict{Type, String}) = "int $name;"
glsl_type_decl(::Type{Int64}, name::String, struct_lookup::Dict{Type, String}) = "int64 $name;"
glsl_type_decl(::Type{UInt32}, name::String, struct_lookup::Dict{Type, String}) = "uint $name;"
glsl_type_decl(::Type{UInt64}, name::String, struct_lookup::Dict{Type, String}) = "uint64 $name;"
glsl_type_decl(::Type{Float32}, name::String, struct_lookup::Dict{Type, String}) = "float $name;"
glsl_type_decl(::Type{Float64}, name::String, struct_lookup::Dict{Type, String}) = "double $name;"
glsl_type_decl(::Type{Vec{N, Bool}}, name::String, struct_lookup::Dict{Type, String}) where {N} = "bvec$N $name;"
glsl_type_decl(::Type{Vec{N, Int32}}, name::String, struct_lookup::Dict{Type, String}) where {N} = "ivec$N $name;"
glsl_type_decl(::Type{Vec{N, UInt32}}, name::String, struct_lookup::Dict{Type, String}) where {N} = "uvec$N $name;"
glsl_type_decl(::Type{Vec{N, Float32}}, name::String, struct_lookup::Dict{Type, String}) where {N} = "vec$N $name;"
glsl_type_decl(::Type{Vec{N, Float64}}, name::String, struct_lookup::Dict{Type, String}) where {N} = "dvec$N $name;"
glsl_type_decl(::Type{<:Mat{C, R, Float32}}, name::String, struct_lookup::Dict{Type, String}) where {C, R} = "mat$Cx$R $name;"
glsl_type_decl(::Type{<:Mat{C, R, Float64}}, name::String, struct_lookup::Dict{Type, String}) where {C, R} = "dmat$Cx$R $name;"
glsl_type_decl(T::Type{<:AbstractOglBlock}, name::String, struct_lookup::Dict{Type, String}) = get(struct_lookup, T, string(T))
glsl_type_decl(::Type{NTuple{N, T}}, name::String, struct_lookup::Dict{Type, String}) where {N, T} = "$(glsl_type_decl(T, struct_lookup)) $name[$N]"



#  Internals:  #


Base.propertynames(x::AbstractOglBlock) = propertynames(typeof(x))
Base.Ref(a::AbstractOglBlock) = Ref(block_byte_array(a), 1)

@inline Base.getproperty(a::AbstractOglBlock, name::Symbol) = let Name = Val(name)
    block_property_get(
        a, Name,
        block_property_type(typeof(a), Name)
    )
end
@inline Base.setproperty!(a::AbstractOglBlock, name::Symbol, value) = let Name = Val(name)
    block_property_set(
        a, Name,
        block_property_type(typeof(a), Name),
        value
    )
end


#  Block Arrays:  #

"
A facade for a (mutable) array of items within a buffer block.

The size is not specified at compile-time,
    because some GPU arrays can be sized dynamically (ones at the tail end of the buffer memory).
"
struct BlockArray{T, TMode<:AbstractOglBlock, TArray<:AbstractVector{UInt8}} <: AbstractVector{T}
    buffer::TArray # Use same field name as the blocks themselves, for convenience
end

"Constructs the block-array with an existing byte array"
BlockArray{T, TMode}(a::AbstractVector{UInt8}) where {T, TMode           } = BlockArray{T, TMode        , typeof(a)}(a)
BlockArray{T       }(a::AbstractVector{UInt8}) where {T<:AbstractOglBlock} = BlockArray{T, block_mode(T), typeof(a)}(a)
"Constructs the block-array with an element count"
BlockArray{T, TMode}(count::Int) where {T, TMode           } = BlockArray{T, TMode        , Vector{UInt8}}(Vector{UInt8}(undef, sizeof(T) * count))
BlockArray{T       }(count::Int) where {T<:AbstractOglBlock} = BlockArray{T, block_mode(T), Vector{UInt8}}(Vector{UInt8}(undef, sizeof(T) * count))

"Returns the array's associated OglBlock type, OglBlock_std140 or OglBlock_std430"
block_array_mode(a::BlockArray{T, TMode}) where {T, TMode} = TMode

# AbstractVector stuff:
Base.eltype(a::BlockArray{T}) where {T} = T
Base.eltype(::Type{<:BlockArray{T}}) where {T} = T
@inline Base.size(a::BlockArray{T}) where {T} = size(block_byte_array(a)) .÷ sizeof(T)
block_array_stride(::Type{<:BlockArray{T}}) where {T} = sizeof(T)
Base.Ref(a::BlockArray{T}, i::Integer = 1) where {T} = Ref(block_byte_array(a), i * sizeof(T))
@inline Base.getindex(a::BlockArray, i::Integer) = block_index_get(a, i, eltype(a))
@inline Base.setindex!(a::BlockArray, v, i::Integer) = block_index_set(a, i, eltype(a), v)
println("#TODO: Slicing BlockArray")

"Gets the mutable byte array underlying this buffer"
block_byte_array(b::Union{AbstractOglBlock, BlockArray})::AbstractVector{UInt8} = getfield(b, :buffer)
@inline block_byte_size(x::BlockArray) = length(block_byte_array(x))


#  Property/element getters and setters:  #

# Block properties each have a type and a byte offset in the larger buffer.
# Array properties (like `float myFloats[3]`) are represented by the type `NTuple`.
block_property_type(block_type, Name)::Type = error(block_type, " has no property '", val_type(Name), "'")
block_property_first_byte(block_type, Name)::Int = error(block_type, " has no property '", val_type(Name), "'")
# Add helpful error messages for specific cases.
block_property_type(b::AbstractOglBlock, name) = error("You should pass in a type, not an instance")
block_property_first_byte(b::AbstractOglBlock, name) = error("You should pass in a type, not an instance")

# You can get and set the properties of a buffer block.
block_property_get(block, name, TReturn) = error(
    typeof(block), " has no property '",
    val_type(name), "' of type ", TReturn
)
block_property_set(block, name, TReturn, value) = error(
    typeof(block), " has no property '", val_type(name), "' of type ", TReturn,
    ", or it can't be set to a ", typeof(value)
)

# You can get and set elements of a buffer array.
block_index_get(block, i, TReturn) = error(typeof(block), " cannot be indexed to get a ", TReturn)
block_index_set(block, i, TReturn, value) = error(typeof(block), " cannot be indexed to set a ", TReturn, " to a ", typeof(value))


# Normal bitstype getters/setters.
error("#TODO: Start from here, updating the older getters/setters")
@inline block_property_get(a::AbstractOglBlock, Name::Val, ::Type{TReturn}) where {TReturn} = reinterpret_from_bytes(
    block_byte_array(a),
    TReturn,
    block_property_first_byte(typeof(a), Name)
)
@inline block_property_set(a::AbstractOglBlock, Name::Val, ::Type{TReturn}, value) where {TReturn} = reinterpret_to_bytes(
    convert(TReturn, value),
    block_byte_array(a),
    block_property_first_byte(typeof(a), Name)
)
@inline block_index_get(a::BlockArray, i::Integer, ::Type{TReturn}) where {TReturn} = reinterpret_from_bytes(
    block_byte_array(a),
    TReturn,
    1 + (block_array_element_stride(TReturn, block_array_mode(a)) * (i - 1))
)
@inline block_index_set(a::BlockArray, i::Integer, ::Type{TReturn}, value) where {TReturn} = reinterpret_to_bytes(
    convert(TReturn, value),
    block_byte_array(a),
    1 + (block_array_element_stride(TReturn, block_array_mode(a)) * (i - 1))
)

# Bools are stored in a block as UInt32.
@inline block_property_get(a::AbstractOglBlock, Name::Val, ::Type{Bool}) = !iszero(block_property_get(
    a,
    Name,
    UInt32
))
@inline block_property_set(a::AbstractOglBlock, Name::Val, ::Type{Bool}, value) = block_property_set(
    a, Name,
    UInt32, convert(Bool, value) ? one(UInt32) : zero(UInt32)
)
@inline block_index_get(a::BlockArray, i::Integer, ::Type{Bool}) = !iszero(block_index_get(
    a, i, UInt32
))
@inline block_index_set(a::BlockArray, i::Integer, ::Type{Bool}, value) = block_index_set(
    a, i,
    UInt32, convert(Bool, value) ? one(UInt32) : zero(UInt32)
)

# Bool vectors in a block are stored as UInt32 vectors.
@inline block_property_get(a::AbstractOglBlock, Name::Val, ::Type{VecB{N}}) where {N} = map(f->!iszero(f), block_property_get(
    a,
    Name,
    VecU{N}
))
@inline block_property_set(a::AbstractOglBlock, Name::Val, ::Type{VecB{N}}, value) where {N} = block_property_set(
    a, Name,
    UInt32,
    map(f -> convert(Bool, f) ? one(UInt32) : zero(UInt32), value)
)
@inline block_index_get(a::BlockArray, i::Integer, ::Type{VecB{N}}) where {N} = map(f ->!iszero(f), block_index_get(
    a, i, VecU{N}
))
@inline block_index_set(a::BlockArray, i::Integer, ::Type{Bool}, value) = block_index_set(
    a, i,
    UInt32,
    map(f -> convert(Bool, f) ? one(UInt32) : zero(UInt32), value)
)

# Matrix properties are stored in columns, like an array of vectors.
function block_property_get(a::AbstractOglBlock, Name::Val, ::Type{<:Mat{C, R, F}}) where {C, R, F}
    arr = block_property_get(a, Name, NTuple{C, Vec{R, F}})
    column_vectors = Iterators.flatten(arr)
    elements_by_column = Iterators.flatten(column_vectors)
    return @Mat(C, R, F)(elements_by_column...)
end
function block_property_set(a::AbstractOglBlock, Name::Val, ::Type{<:Mat{C, R, F}}, value) where {C, R, F}
    arr = block_property_get(a, Name, NTuple{C, Vec{R, F}})
    for col in 1:C
        arr[col] = convert(Vec{R, F}, value[:, col])
    end
end
function block_index_get(a::BlockArray, i::Integer, T::Type{<:Mat{C, R, F}}) where {C, R, F}
    bN = block_array_element_stride(T, block_array_mode(a))
    b1 = 1 + (bN * (i - 1))
    a_view = @view block_byte_array(a)[b1:(b1+bN-1)]
    arr = BlockArray{Vec{R, F}, block_array_mode(a)}(a_view)

    column_vectors = Iterators.flatten(arr)
    elements_by_column = Iterators.flatten(column_vectors)
    return @Mat(C, R, F)(elements_by_column...)
end
function block_index_set(a::BlockArray, i::Integer, T::Type{<:Mat{C, R, F}}, value) where {C, R, F}
    bN = block_array_element_stride(T, block_array_mode(a))
    b1 = 1 + (bN * (i - 1))
    a_view = @view block_byte_array(a)[b1:(b1+bN-1)]
    arr = BlockArray{Vec{R, F}, block_array_mode(a)}(a_view)

    for col in 1:C
        arr[col] = convert(Vec{R, F}, value[:, col])
    end
end

# Implementations of getter/setter for a property that is an array:
@inline function block_property_get(a::AbstractOglBlock, Name::Val, ::Type{<:NTuple{N, T}}) where {N, T}
    b1 = block_property_first_byte(typeof(a), Name)
    bN = N * block_field_size(T, block_mode(a))
    bEnd = b1 + bN - 1
    return BlockArray{T, block_mode(a)}(@view(block_byte_array(a)[b1:bEnd]))
end
@inline function block_property_set(a::AbstractOglBlock, Name::Val, ::Type{<:NTuple{N, T}},
                                    value::Union{ConstVector{T}, AbstractVector{T}}) where {T}
    @bp_gl_assert(length(value) == N,
                  "Expected to set ", typeof(a), ".", val_type(Name), " to an array of ",
                    N, " elements, but got ", length(value), " elements instead")
    b1 = block_property_first_byte(typeof(a), Name)
    bN = N * block_field_size(T, block_mode(a))
    bEnd = b1 + bN - 1
    reinterpret_to_bytes(value, @view(block_byte_array(a)[b1:bEnd]))
end

# Implementations of getter/setter for a block property:
@inline function block_property_get(a::AbstractOglBlock, Name::Val, T::Type{<:AbstractOglBlock})
    b1 = block_property_first_byte(typeof(a), Name)
    bN = block_byte_size(T)
    bEnd = b1 + bN - 1
    arr = @view block_byte_array(a)[b1:bEnd]
    return T{typeof(arr)}(arr)
end
@inline function block_property_set(a::AbstractOglBlock, Name::Val, ::Type{T}, value::T) where {T<:AbstractOglBlock}
    b1 = block_property_first_byte(typeof(a), Name)
    bN = block_byte_size(T)
    bEnd = b1 + bN - 1
    block_byte_array(a)[b1:bEnd] = block_byte_array(value)
end

# Implementations of getter/setter for an array element that's an OglBlock:
@inline function block_index_get(a::BlockArray{T}, i::Integer, ::Type{T}) where {T<:AbstractOglBlock}
    b1 = 1 + (i * block_byte_size(T))
    bN = block_byte_size(T)
    bEnd = b1 + bN - 1
    arr = @view block_byte_array(a)[b1:bEnd]
    return T{typeof(arr)}(arr)
end
@inline function block_index_set(a::BlockArray{T}, i::Integer, ::Type{T}, value::T) where {T<:AbstractOglBlock}
    b1 = 1 + (i * block_byte_size(T))
    bN = block_byte_size(T)
    bEnd = b1 + bN - 1
    block_byte_array(a)[b1:bEnd] = block_byte_array(value)
end


#= A new block struct (call it `s::S`) must implement the following:
    * Base.propertynames(::Type{S})
    * block_byte_size(::Type{S})
    * block_property_type(::Type{<:S}, ::Val)
    * block_property_first_byte(::Type{<:S}, ::Val)
    * Internal constructor that takes an `AbstractVector{UInt8}`
    * External constructor that takes all the fields and uses a `Vector{UInt8}` buffer
=#


#  OpenGL Spec implementation  #

# Note that in this implementation, NTuple is used to represent static arrays.

function block_field_alignment end
function block_field_size end

function block_array_element_alignment end
function block_array_element_stride end


block_field_alignment(T::Type{<:AbstractOglBlock}, mode::Type{<:AbstractOglBlock}) = block_alignment(T)
# Most scalars/vectors translate to the GPU trivially.
# However bools are 4 bytes.
block_field_alignment(T::Type{<:ScalarBits}      , mode::Type{<:AbstractOglBlock}) = (T == Bool) ? sizeof(UInt32) : sizeof(T)
block_field_alignment( ::Type{Vec{N, T}}         , mode::Type{<:AbstractOglBlock}) where {N, T} =
    if N == 3
        block_field_alignment(Vec{4, T}, mode)
    elseif T == Bool
        block_field_alignment(Vec{N, UInt32}, mode)
    else
        N * sizeof(T)
    end
# AbstractOglBlock's are already sized appropriately.
# Array fields are based on their elements.
block_field_alignment( ::Type{<:NTuple{N, T}}    , mode::Type{<:AbstractOglBlock}) where {N, T} = block_array_element_alignment(T, mode)
# Matrices are like an array of column vectors.
block_field_alignment( ::Type{<:Mat{C, R, F}}    , mode::Type{<:AbstractOglBlock}) where {C, R, F} =
    block_field_alignment(NTuple{C, Vec{R, F}}, mode)

block_field_size(T::Type                    , mode::Type{<:AbstractOglBlock})                 = block_field_alignment(T, mode)
block_field_size(T::Type{<:AbstractOglBlock}, mode::Type{<:AbstractOglBlock})                 = block_byte_size(T)
block_field_size( ::Type{<:Mat{C, R, F}}    , mode::Type{<:AbstractOglBlock}) where {C, R, F} = block_field_size(NTuple{C, Vec{R, F}}, mode)
block_field_size( ::Type{<:NTuple{N, T}}    , mode::Type{<:AbstractOglBlock}) where {N, T   } = N * block_field_alignment(NTuple{N, T}, mode)

# An array element's alignment is equal to its alignment as a field,
#    if in std140 then rounded up to a multiple of v4f.
block_array_element_alignment(T, mode)                    = block_field_alignment(T, mode)
block_array_element_alignment(T, ::Type{OglBlock_std140}) = round_up_to_multiple(block_field_alignment(T, mode), sizeof(v4f))

block_array_element_stride(T                    , mode)              = block_array_element_alignment(T, mode)
block_array_element_stride(T::Type{<:Mat{C}}    , mode) where {C}    = C * block_array_element_alignment(T, mode)
block_array_element_stride( ::Type{NTuple{N, T}}, mode) where {N, T} = N * block_array_element_stride(T, mode)


function Base.:(==)(a::T, b::T)::Bool where {T<:AbstractOglBlock}
    return all(Base.:(==).(
        getproperty.(Ref(a), propertynames(T)),
        getproperty.(Ref(b), propertynames(T))
    ))
end

function Base.hash(a::T, h::UInt)::UInt where {T<:AbstractOglBlock}
    return hash(
        getproperty.(Ref(a), propertynames(T)),
        h
    )
end

function Base.show(io::IO, a::AbstractOglBlock)
    print(io, Base.typename(typeof(a)).name, '(')
    for (i, prop) in enumerate(propertynames(a))
        if i > 1
            print(io, ", ")
        end
        show(io, getproperty(a, prop))
    end
    print(io, ')')
end


#TODO: Support some kind of annotation for row-major matrices.
"""
Generates an immutable struct using OpenGL-friendly types,
    whose byte layout exactly follows the std140 standard in shader blocks.

Sample usage:
````
@std140 struct MyInnerUniformBlock
    f::Float32
    bools::vb4
    position_array::NTuple{12, v3f}
end

@std140 struct MyOuterUniformBlock
    i::Int32 # Be careful; 'Int' in Julia means Int64
    items::NTuple{5, MyInnerUniformBlock}
    b::Bool
end

const MY_UBO_DATA = MyOuterUniformBlock(
    3,
    ntuple(i -> zero(MyInnerUniformBlock), 5),
    true
)
println("i is: ", MY_UBO_DATA.i)

const MUTABLE_UBO = Ref(MyInnerUniformBlock(3.5,
                                            vb4(false, false, true, true),
                                            ntuple(i -> zero(v3f))))
MUTABLE_UBO.f = 3.4f0
println("f is: ", MUTABLE_UBO[].f)
````
"""
macro std140(struct_expr)
    return block_struct_impl(struct_expr, :std140, __module__)
end
"""
Generates an immutable struct using OpenGL-friendly types,
    whose byte layout exactly follows the std430 standard in shader blocks.

Sample usage:
````
@std430 struct MyInnerShaderStorageBlock
    f::Float32
    bools::vb4
    position_array::NTuple{12, v3f}
end

@std430 struct MyOuterShaderStorageBlock
    i::Int32 # Be careful; 'Int' in Julia means Int64
    items::NTuple{5, MyInnerShaderStorageBlock}
    b::Bool
end

const MY_SSBO_DATA = MyOuterShaderStorageBlock(
    3,
    ntuple(i -> zero(MyInnerShaderStorageBlock), 5),
    true
)
println("i is: ", MY_SSBO_DATA.i)

const MUTABLE_SSBO = Ref(MyInnerShaderStorageBlock(3.5,
                                                   vb4(false, false, true, true),
                                                   ntuple(i -> zero(v3f))))
MUTABLE_SSBO.f = 3.4f0
println("f is: ", MUTABLE_SSBO[].f)
````
"""
macro std430(struct_expr)
    return block_struct_impl(struct_expr, :std430, __module__)
end
function block_struct_impl(struct_expr, mode::Symbol, invoking_module::Module)
    if !Base.is_expr(struct_expr, :struct)
        error("Expected struct block, got: ", struct_expr)
    elseif struct_expr.args[1]
        error("UBO struct cannot be mutable; wrap it in a Ref if you want that!")
    end

    mode_switch(std140, std430) = if mode == :std140
                                      std140()
                                  elseif mode == :std430
                                      std430()
                                  else
                                      error("Unexpected mode: ", mode)
                                  end
    base_type::Type{<:AbstractOglBlock} = mode_switch(() -> OglBlock_std140,
                                                      () -> OglBlock_std430)

    SCALAR_TYPES = Union{Scalar32, Scalar64, Bool}
    VECTOR_TYPES = Union{(
        Vec{n, t}
          for (n, t) in Iterators.product(1:4, union_types(SCALAR_TYPES))
    )...}
    MATRIX_TYPES = Union{(
        @Mat(c, r, f)
          for (c, r, f) in Iterators.product(1:4, 1:4, (Float32, Float64))
    )...}
    NON_ARRAY_TYPES = Union{SCALAR_TYPES, VECTOR_TYPES, MATRIX_TYPES, base_type}
    ARRAY_TYPES = ConstVector{<:NON_ARRAY_TYPES}

    # Parse the header.
    (is_mutable_from_cpu::Bool, struct_name, body::Expr) = struct_expr.args
    if !isa(struct_name, Symbol)
        error(mode, " struct has invalid name: '", struct_name, "'")
    elseif is_mutable_from_cpu
        error(mode, " struct '", struct_name, "' must not be mutable")
    end

    # Parse the body.
    lines = [line for line in body.args if !isa(line, LineNumberNode)]
    function check_field_errors(field_name, T, is_within_array::Bool = false)
        if T <: Vec
            if !(T <: VECTOR_TYPES)
                error("Invalid vector count or type in ", field_name, ": ", T)
            elseif T <: Vec3
                error("Problem with field ", field_name,
                        ": 3D vectors are not padded correctly by every graphics driver, ",
                        "and their size is almost always padded out to 4D anyway, so just use ",
                        "a 4D vector")
            end
        elseif T <: Mat
            if !(T <: MATRIX_TYPES)
                error("Invalid matrix size or component type in ", field_name, ": ", T)
            elseif (T <: Mat{C, 3} where {C})
                error("Problem with field ", field_name,
                        ": 3D vectors (and by extension, 3-row matrices) are not padded correctly ",
                        "by every graphics driver. Their size gets padded out to 4D anyway, ",
                        "so just use a 4-row matrix.")
            end
        elseif T <: NTuple
            if is_within_array
                error("No nested arrays allowed, for simplicity. Flatten field ", field_name)
            end
            check_field_errors(field_name, eltype(T), true)
        elseif isstructtype(T) # Note that NTuple is a 'struct type', so
                               #    we have to handle the NTuple case first
            if !(T <: base_type)
                error("Non-", mode, " struct referenced by ", field_name, ": ", T)
            end
        elseif T <: SCALAR_TYPES
            # Nothing to check
        else
            error("Unexpected type in field ", field_name, ": ", T)
        end
    end
    field_definitions = map(lines) do line
        if !Base.is_expr(line, :(::))
            error("Expected only field declarations ('a::B'). Got: ", line)
        end

        (field_name, field_type) = line.args
        if !isa(field_name, Symbol)
            error("Name of the field should be a simple token. Got: '", field_name, "'")
        end
        field_type = invoking_module.eval(field_type)
        if !isa(field_type, Type)
            error("Expected a concrete type for the field's value. Got: ", field_type)
        end

        check_field_errors(field_name, field_type)

        return (field_name, field_type)
    end

    # Figure out padding and field offsets.
    total_byte_size::Int = 0
    total_padding_bytes::Int = 0
    max_field_alignment::Int = 0
    property_offsets = Vector{Int}()
    function align_next_field(alignment, record_offset = true)
        max_field_alignment = max(max_field_alignment, alignment)

        missing_bytes = (alignment - (total_byte_size % alignment))
        if missing_bytes < alignment # Only if not already aligned
            total_byte_size += missing_bytes
            total_padding_bytes += missing_bytes
        end

        if record_offset
            push!(property_offsets, total_byte_size)
        end
    end
    for (field_name, field_type) in field_definitions
        if field_type == Bool
            align_next_field(4)
            total_byte_size += 4
        elseif field_type <: VecT{Bool}
            n_components = length(field_type)
            byte_size = 4 * n_components
            alignment = 4 * (1, 2, 4, 4)[n_components]

            align_next_field(alignment)
            total_byte_size += byte_size
        elseif field_type <: ScalarBits
            byte_size = sizeof(field_type)
            alignment = byte_size

            align_next_field(alignment)
            total_byte_size += byte_size
        elseif field_type <: Vec
            n_components = length(field_type)
            component_type = eltype(field_type)
            byte_size = sizeof(component_type) * n_components
            alignment = sizeof(component_type) * (1, 2, 4, 4)[n_components]

            align_next_field(alignment)
            total_byte_size += byte_size
        elseif field_type <: Mat
            (C, R, F) = mat_params(field_type)

            column_alignment = sizeof(F) * (1, 2, 4, 4)[R]
            column_alignment = mode_switch(
                () -> round_up_to_multiple(column_alignment, sizeof(v4f)),
                () -> column_alignment
            )

            byte_size = C * column_alignment
            alignment = column_alignment

            align_next_field(alignment)
            total_byte_size += byte_size
        elseif field_type <: base_type
            byte_size = sizeof(field_type)
            alignment = block_alignment(field_type)

            align_next_field(alignment)
            total_byte_size += byte_size
        elseif field_type <: NTuple
            array_length = tuple_length(field_type)
            element_type = eltype(field_type)

            (element_alignment, element_stride) =
                if element_type <: SCALAR_TYPES
                    size = (element_type == Bool) ? 4 : sizeof(element_type)
                    mode_switch(
                        () -> (size = round_up_to_multiple(size, sizeof(v4f))),
                        () -> nothing
                    )
                    (size, size)
                elseif element_type <: Vec
                    component_size = (eltype(element_type) == Bool) ?
                                            4 :
                                            sizeof(eltype(element_type))
                    vec_size = (1, 2, 4, 4)[length(element_type)] * component_size
                    mode_switch(
                        () -> (vec_size = round_up_to_multiple(vec_size, sizeof(v4f))),
                        () -> nothing
                    )
                    (vec_size, vec_size)
                elseif element_type <: Mat
                    (C, R, F) = mat_params(element_type)
                    column_size = sizeof(F) * (1, 2, 4, 4)[R]
                    mode_switch(
                        () -> (column_size = round_up_to_multiple(column_size, sizeof(v4f))),
                        () -> nothing
                    )
                    (column_size, column_size * C)
                elseif element_type <: base_type
                    # Any @std140/@std430 struct will already be padded to the right size,
                    #    but the padding logic is still here for completeness.
                    (a, s) = (block_alignment(element_type), sizeof(element_type))
                    mode_switch(
                        () -> (round_up_to_multiple(a, sizeof(v4f)),
                               round_up_to_multiple(s, sizeof(v4f))),
                        () -> (a, s)
                    )
                else
                    error("Unhandled case: ", element_type)
                end

            align_next_field(element_alignment)
            total_byte_size += element_stride * array_length
        else
            error("Unhandled: ", field_type)
        end
    end

    # Struct alignment is the largest field alignment, then in std140 rounded up to a vec4 alignment.
    struct_alignment = mode_switch(
        () -> round_up_to_multiple(max_field_alignment, sizeof(v4f)),
        () -> max_field_alignment
    )

    # Add padding to the struct to match its alignment.
    # Note that this inadvertently modifies 'max_field_alignment',
    #    but that variable isn't used past this point.
    align_next_field(struct_alignment, false)

    # Generate the final code.
    struct_name_str = string(struct_name)
    struct_name = esc(struct_name)
    (property_names, property_types) = unzip(field_definitions, 2)
    property_functions = map(zip(field_definitions, property_offsets)) do ((name, type), offset)
        compile_time_name = :( Val{$(QuoteNode(name))} )
        return quote
            $(@__MODULE__).property_offset(::Type{$struct_name}, ::$compile_time_name) = $offset
            $(@__MODULE__).property_type(::Type{$struct_name}, ::$compile_time_name) = $type

            $(esc(:Base)).getproperty(a::$struct_name, ::$compile_time_name) =
                $(block_macro_get_property_body(struct_name, name, offset, type, mode))
            $(esc(:Base)).setproperty!(r::Ref{$struct_name}, ::$compile_time_name, value) =
                $(block_macro_set_property_body(struct_name, name, offset, type, mode))
        end
    end
    return quote
        Core.@__doc__ struct $struct_name <: $base_type
            var"raw bytes"::NTuple{$total_byte_size, UInt8}
        end
        @bp_check(sizeof($struct_name) == $total_byte_size,
                  $struct_name_str, " should be ", $total_byte_size,
                    " bytes but it was changed by Julia to ", sizeof($struct_name))

        Base.propertynames(::Type{$struct_name}) = tuple($(QuoteNode.(property_names)...))
        $(@__MODULE__).property_types(::Type{$struct_name}) = tuple($(property_types...))
        $(property_functions...)

        $(@__MODULE__).padding_size(::Type{$struct_name}) = $total_padding_bytes
        $(@__MODULE__).block_alignment(::Type{$struct_name}) = $struct_alignment

        $struct_name($(esc.(property_names)...)) = $(@__MODULE__).construct_block($struct_name, $(esc.(property_names)...))
    end
end

export @std140, @std430
       padding_size, block_byte_array,
       glsl_decl, GLSLBlockDecl