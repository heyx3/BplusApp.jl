"
Some kind of view into a texture's data, which can be passed into a shader
    as a simple 64-bit integer handle, or one of the opaque sampler types.
Assumes the OpenGL extension `ARB_bindless_texture`.
This is much easier to manage than the old-school texture units and binding.
You can even pass an array of unrelated textures, as an array of uint64 (or uvec2)!

You should never create these yourself; they're generated by their owning Texture.

IMPORTANT: you must `view_activate()` an instance before it's used in a shader,
    and it should be `view_deactivate()`-d when not in use
    to free up GPU resources for other render passes.
Using an inactive handle leads to Undefined Behavior (a.k.a. wacky crashes).

The lifetime of this resource is managed by its owning Texture,
which is why this type does not inherit from `AbstractResource`.
"
mutable struct View
    handle::Ptr_View
    owner::AbstractResource # Can't type it as `Texture`, because that type doesn't exist yet

    is_active::Bool

    is_sampling::Bool # True if this is a 'Texture' view,
                      #   false if this is an 'Image' view.
end

"
Tells the GPU to activate this handle so it can be used in a shader.
Simple Views are activated with an access type (read, write, read_write).
"
function view_activate(view::View, simple_access::E_ImageAccessModes = ImageAccessModes.read_write)
    # It's not clear at all in the standard what happens if you make an ImageView resident
    #    more than once, with different access modes.
    # So I just forbid duplicate activations altogether.
    @bp_check(!view.is_active, "Can't activate a view twice!")
    @bp_check(get_ogl_handle(view.owner) != Ptr_Texture(),
              "This view's owning texture has been destroyed")
    if !view.is_active
        view.is_active = true
        if view.is_sampling
            glMakeTextureHandleResidentARB(view.handle)
        else
            glMakeImageHandleResidentARB(view.handle, simple_access)
        end
    end
end

"Tells the GPU to deactivate this handle, potentially freeing up resources for other data to be loaded."
function view_deactivate(view::View)
    # Ideally we should check that the owning texture hasn't been destroyed, as with `view_activate()`,
    #    but in practice it's not as important and I feel that
    #    it will lead to a lot of initialization-order pain.
    if view.is_active
        view.is_active = false
        if view.is_sampling
            glMakeTextureHandleNonResidentARB(view.handle)
        else
            glMakeImageHandleNonResidentARB(view.handle)
        end
    end
end

get_ogl_handle(view::View) = view.handle

# Unfortunately, OpenGL allows implementations to re-use the handles of destroyed views;
#    otherwise, I'd use that for hashing/equality.

export View, view_activate, view_deactivate


##################
##  Parameters  ##
##################

"
The parameters defining a 'simple' View.

Note that mip levels start at 1, not 0, to reflect Julia's 1-based indexing convention.

The 'layer' field allows you to pick a single layer of a 3D or cubemap texture,
    causing the view to act like a 2D texture.

The 'apparent_format' field changes how the texture is interpreted in the shader;
    it defaults to the texture's actual format.
"
Base.@kwdef struct SimpleViewParams
    mip_level::Int = 1
    layer::Optional{Int} = nothing
    apparent_format::Optional{TexFormat} = nothing
end

"
Maps every possible format for a simple texture view to its type-name in the shader.

Comes from this reference: https://registry.khronos.org/OpenGL-Refpages/gl4/html/glBindImageTexture.xhtml#Description    
"
const LEGAL_APPARENT_SIMPLEVIEW_FORMATS = Dict{Union{SimpleFormat, E_SpecialFormats}, String}(
    # Float/uint/int textures can be rgba/rg/r and 8/16/32 bits.
    # With the exception of 8-bit floats.
    begin
        formats = [ FormatTypes.float => "f",
                    FormatTypes.uint => "ui",
                    FormatTypes.int => "i" ]
        components = [ SimpleFormatComponents.RGBA => "rgba",
                       SimpleFormatComponents.RG => "rg",
                       SimpleFormatComponents.R => "r" ]
        bit_depths = [ SimpleFormatBitDepths.B32 => "32",
                       SimpleFormatBitDepths.B16 => "16",
                       SimpleFormatBitDepths.B8 => "8" ]
        everything = Iterators.product(formats, components, bit_depths)
        filtered = Iterators.filter(everything) do ((format, format_str),
                                                    (component, comp_str),
                                                    (bit_depth, bit_str), )
            (format != FormatTypes.float) || (bit_depths != SimpleFormatBitDepths.B8)
        end
        constructed = Iterators.map(filtered) do ((format, format_str),
                                                  (component, comp_str),
                                                  (bit_depth, bit_str), )
            SimpleFormat(format, component, bit_depth) => string(comp_str, bit_str, format_str)
        end
        constructed
    end...,

    # Normalized uint/int textures can be rgba/rg/r and 8/16 bits.
    begin
        formats = [ FormatTypes.normalized_uint => "",
                    FormatTypes.normalized_int => "_snorm" ]
        components = [ SimpleFormatComponents.RGBA => "rgba",
                       SimpleFormatComponents.RG => "rg",
                       SimpleFormatComponents.R => "r" ]
        bit_depths = [ SimpleFormatBitDepths.B16 => "16",
                       SimpleFormatBitDepths.B8 => "8" ]
        everything = Iterators.product(formats, components, bit_depths)
        constructed = Iterators.map(everything) do ((format, format_str),
                                                  (component, comp_str),
                                                  (bit_depth, bit_str), )
            SimpleFormat(format, component, bit_depth) => string(comp_str, bit_str, format_str)
        end
        constructed
    end...,

    # A few special formats are supported.
    SpecialFormats.rgb_tiny_ufloats => "r11f_g11f_b10f",
    SpecialFormats.rgb10_a2_uint => "rgb10_a2ui",
    SpecialFormats.rgb10_a2 => "rgb10_a2",
)

"
Gets whether a texture of format `src` can appear in a shader
    as a 'simple view' texture of format `dest`
"
function is_format_compatible_in_tex_simpleview(src::TexFormat, dest::TexFormat)
    return haskey(LEGAL_APPARENT_SIMPLEVIEW_FORMATS, dest) &&
           get_pixel_bit_size(src) == get_pixel_bit_size(dest)
end

"The parameters that uniquely define a texture's view."
const ViewParams = Union{Optional{TexSampler}, SimpleViewParams}

export SimpleViewParams, ViewParams,
       LEGAL_APPARENT_SIMPLEVIEW_FORMATS, is_format_compatible_in_tex_simpleview


#####################
##   Constructors  ##
#####################

"
Creates a new sampled view from the given texture and optional sampler.
If no sampler is given, the texture's default sampler settings are used.

IMPORTANT: users shouldn't ever be creating these by hand;
    they should come from the Texture interfae.
"
function View(owner::AbstractResource, sampler::Optional{TexSampler})
    local handle::gl_type(Ptr_View)
    if exists(sampler)
        handle = glGetTextureSamplerHandleARB(get_ogl_handle(owner), get_sampler(sampler))
    else
        handle = glGetTextureHandleARB(get_ogl_handle(owner))
    end

    # Create the instance, and register it with the View-Debugger.
    instance = View(Ptr_View(handle), owner,
                    glIsTextureHandleResidentARB(handle),
                    true)
    service_ViewDebugging_add_view(instance.handle, instance)

    return instance
end

"
Creates a new simple view on a texture.

IMPORTANT: users shouldn't ever be creating these by hand;
    they should come from the Texture interface.
"
function View(owner::AbstractResource, owner_format::TexFormat, params::SimpleViewParams)
    apparent_format = isnothing(params.apparent_format) ? owner_format : params.apparent_format
    @bp_check(is_format_compatible_in_tex_simpleview(owner_format, apparent_format),
              "Simple View with format ", apparent_format, " is not legal ",
                "with a texture of true format ", owner_format)
    handle::gl_type(Ptr_View) = glGetImageHandleARB(
        get_ogl_handle(owner),
        params.mip_level - 1,
        exists(params.layer) ? GL_FALSE : GL_TRUE,
        isnothing(params.layer) ? 0 : params.layer-1,
        get_ogl_enum(apparent_format)
    )

    # Create the instance, and register it with the View-Debugger.
    instance = View(Ptr_View(handle), owner,
                    glIsImageHandleResidentARB(handle),
                    false)
    service_ViewDebugging_add_view(instance.handle, instance)

    return instance
end