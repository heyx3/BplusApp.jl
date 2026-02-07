#################################
##    Helpers for GUI functions

"Dear ImGUI colors can be a U32 hex code, or RGB[A] floats between 0 and 1"
const GuiColor = Union{UInt32, gVec4, Vec3, Vec4}
gui_color_to_im_color(u::UInt32) = u
gui_color_to_im_color(v::gVec4) = v
gui_color_to_im_color(v::Vec3) = gVec4(v..., 1)
gui_color_to_im_color(v::Vec4) = gVec4(v...)

export GuiColor


###########################################
##    Push->Lambda->Pop GUI state helpers

"
Nests some GUI code within a window.
Extra arguments get passed directly into `CImGui.Begin()`.
Returns the output of your code block, or `nothing` if the UI was culled.
"
function gui_window(to_do, args...; kw_args...)::Optional
    is_open = CImGui.Begin(args..., kw_args...)
    try
        if is_open
            return to_do()
        end
    finally
        CImGui.End()
    end

    return nothing
end

"
Executes some GUI code with a different item width.

See: https://pixtur.github.io/mkdocs-for-imgui/site/api-imgui/ImGui--Dear-ImGui-end-user/#PushItemWidth

* >0.0f: width in pixels
* <0.0f: align xx pixels to the right of window (so -1.0f always align width to the right side)
* ==0.0f: default to ~⅔ of windows width
"
function gui_with_item_width(to_do, width::Real; unchanged::Bool=false)
    unchanged && return to_do()
    CImGui.PushItemWidth(convert(Float32, width))
    try
        return to_do()
    finally
        CImGui.PopItemWidth()
    end
end


function gui_with_indentation(to_do, width::Optional{Real} = nothing
                              ; unchanged::Bool=false)
    unchanged && return to_do()
    CImGui.Indent(@optional(exists(width), width))
    try
        return to_do()
    finally
        CImGui.Unindent()
    end
end

"
Adds a tooltip to the previous widget/group,
  conditionally-executing your lambda to fill in the tooltip's GUI.

Returns `nothing` if the tooltip isn't open; otherwise returns `Some(x)`
  where `x` is the returned value from your lambda.
"
function gui_tooltip(to_do; skip::Bool = false)::Optional{Some}
    skip && return nothing
    if CImGui.IsItemHovered(CImGui.LibCImGui.ImGuiHoveredFlags_AllowWhenDisabled)
       CImGui.BeginTooltip()
        try
            return Some(to_do())
        finally
            CImGui.EndTooltip()
        end
    end

    return nothing
end
"
Adds a plaintext tooltip to the previous widget/group, optionally using text wrapping,
  and returns whether the tooltip was open.
"
function gui_tooltip(label::String,
                     wrap_size::Real = CImGui.GetFontSize() * 40
                     ; unchanged::Bool = false)::Bool
    unchanged && return false
    return exists(gui_tooltip() do
        gui_with_text_wrap(wrap_size) do
            CImGui.TextUnformatted(label)
        end
    end)
end

function gui_with_style(to_do, var::CImGui.LibCImGui.ImGuiStyleVar,
                               value::Union{Real, Vec2, gVec2, Tuple{Any, Any}}
                               ; unchanged::Bool = false)
    unchanged && return to_do()
    CImGui.PushStyleVar(var,
        if value isa Real
            value
        elseif value isa Vec2
            value.data
        elseif value isa Tuple
            convert.(Ref(Float32), value)
        else
            error("Unhandled: ", typeof(value))
        end
    )
    try
        to_do()
    finally
        CImGui.PopStyleVar()
    end
end
"Color can be provided as a U32 hex code, or 3-4 floats between 0 and 1"
function gui_with_style(to_do, color_idx::CImGui.LibCImGui.ImGuiCol_,
                               color::Union{UInt32, Vec3, Vec4, gVec4}
                               ; unchanged::Bool = false)
    unchanged && return to_do()
    CImGui.PushStyleColor(color_idx,
        if color isa UInt32
            color
        elseif color isa Vec3
            gVec4(v4f(color..., 1)...)
        elseif color isa Vec4
            gVec4(convert(v4f, color)...)
        elseif color isa gVec4
            color
        else
            error("Unhandled: ", typeof(color))
        end
    )
    try
        to_do()
    finally
        CImGui.PopStyleColor()
    end
end

function gui_with_padding(to_do, padding...; unchanged::Bool = false)
    unchanged && return to_do()
    CImGui.PushStyleVar(CImGui.ImGuiStyleVar_WindowPadding, padding...)
    try
        return to_do()
    finally
        CImGui.PopStyleVar()
    end
end

"
Sets the text wrapping amount (i.e the horizontal postion where it wraps).

NaN or negative values disable wrapping.
Inf or 0 makes it wrap at the very end of the content area.
"
function gui_with_text_wrap(to_do, wrapping::Real=Inf; unchanged::Bool = false)
    unchanged && return to_do()
    CImGui.PushTextWrapPos(
        if isnan(wrapping)
            -1.0f0
        elseif isinf(wrapping)
            0.0f0
        else
            convert(Float32, wrapping)
        end
    )
    try
        to_do()
    finally
        CImGui.PopTextWrapPos()
    end
end

function gui_with_clip_rect(to_do,
                            rect::Box2Df, intersect_with_current_rect::Bool,
                            draw_list::Optional{Ptr{LibCImGui.ImDrawList}} = nothing
                            #TODO: Param for using world space (call different CImGui func)
                            ; unchanged::Bool = false)
    unchanged && return to_do()
    CImGui.PushClipRect(@optional(exists(draw_list), draw_list),
                        gVec2(min_inclusive(rect)...),
                        gVec2(max_exclusive(rect)...),
                        intersect_with_current_rect)
    try
        return to_do()
    finally
        CImGui.PopClipRect()
    end
end

function gui_add_font_from_memory_ttf(bytes::AbstractVector{UInt8},
                                      sizes_pixels::AbstractVector{<:Real},
                                      oversampling::v2i = v2i(1, 1)
                                     )::AbstractVector{Ptr{CImGui.LibCImGui.ImFont}}
    config = CImGui.ImFontConfig_ImFontConfig()
    unsafe_store!(config.OversampleH, Cint(oversampling.x))
    unsafe_store!(config.OversampleV, Cint(oversampling.y))

    unsafe_store!(config.FontDataOwnedByAtlas, false)
    fonts = CImGui.AddFontFromMemoryTTF.(
        Ref(unsafe_load(CImGui.GetIO().Fonts)),
        Ref(bytes), length(bytes),
        sizes_pixels, Ref(config)
    )

    CImGui.ImFontConfig_destroy(config)
    return fonts
end

function gui_with_font(to_do, font::Ptr; unchanged::Bool = false)
    unchanged && return to_do()
    CImGui.PushFont(font)
    try
        return to_do()
    finally
        CImGui.PopFont()
    end
end
function gui_with_font(to_do, font_idx::Integer; unchanged::Bool = false)
    unchanged && return to_do()
    font_singleton = unsafe_load(CImGui.GetIO().Fonts)
    font_list::CImGui.ImVector_ImFontPtr = unsafe_load(font_singleton.Fonts)
    font::Ptr{CImGui.ImFont} = unsafe_load(font_list.Data, font_idx)
    return gui_with_font(to_do, font, unchanged=false)
end

"
Executes some GUI code without allowing the user to tab to different widgets
  (so tabs get inserted into text editors).
"
function gui_with_unescaped_tabbing(to_do; unchanged::Bool = false)
    unchanged && return to_do()
    CImGui.PushAllowKeyboardFocus(false)
    try
        return to_do()
    finally
        CImGui.PopAllowKeyboardFocus()
    end
end

"
Executes some GUI code with the given ID data (ptr, String, Integer, or begin + end Strings)
  pushed onto the stack.
"
function gui_with_nested_id(to_do, values...; unchanged::Bool = false)
    unchanged && return to_do()
    CImGui.PushID(values...)
    try
        return to_do()
    finally
        CImGui.PopID()
    end
end

"
Executes some GUI within a fold (what Dear ImGUI calls a 'tree node').
If it's open, returns the output of your lambda; otherwise returns `nothing`.
"
function gui_within_fold(to_do, label; unchanged::Bool = false)
    unchanged && return to_do()
    is_visible::Bool = CImGui.TreeNode(label)
    if is_visible
        try
            return to_do()
        finally
            CImGui.TreePop()
        end
    end

    return nothing
end

"
Groups widgets together for placement within larger layouts
    (such as a vertical group within a horizontal line).
"
function gui_within_group(to_do; unchanged::Bool = false)
    unchanged && return to_do()
    CImGui.BeginGroup()
    try
        return to_do()
    finally
        CImGui.EndGroup()
    end
end

"""
Defines a set of tabs, and the contents underneath each.
Within this block you can create new tab views as follows:

````
gui_tab_views("Tab1") do
    # GUI code for the tab's contents
end
gui_tab_views("Tab2") do
    # GUI code for the other tab's contents
end
````
"""
function gui_tab_views(to_do, label, flags = CImGui.LibCImGui.ImGuiTabBarFlags_None
                       ; unchanged::Bool = false)
    unchanged && return to_do()
    if CImGui.BeginTabBar(label, flags)
        try
            return to_do()
        finally
            CImGui.EndTabBar()
        end
    else
        return nothing
    end
end
"Defines one tab, as part of a tab bar (see `gui_tab_views()`). Returns whether the tab view was open."
function gui_tab_item(to_do, label, flags = CImGui.LibCImGui.ImGuiTabItemFlags_None
                      ; unchanged::Bool = false)
    if unchanged
        to_do()
        return true
    end
    if CImGui.BeginTabItem(label, C_NULL, flags)
        try
            to_do()
            return true
        finally
            CImGui.EndTabItem()
        end
    else
        return false
    end
end

"
Groups a GUI together into a smaller window.
Returns the output of 'to_do()', or `nothing` if the window is closed.
"
function gui_within_child_window(to_do, id, size, flags=0; unchanged::Bool = false)::Optional
    unchanged && return to_do()

    if id isa String
        return gui_within_child_window(to_do, CImGui.GetID(id), size, flags)
    end

    is_open = CImGui.BeginChildFrame(id, size, flags)
    try
        if is_open
            return to_do()
        end
    finally
        CImGui.EndChildFrame()
    end

    return nothing
end

export gui_with_item_width, gui_with_indentation, gui_with_clip_rect, gui_with_padding, gui_with_text_wrap,
       gui_with_unescaped_tabbing, gui_with_style, gui_with_font, gui_with_nested_id, gui_tooltip,
       gui_window, gui_within_fold, gui_within_group, gui_tab_views, gui_tab_item, gui_within_child_window,
       gui_add_font_from_memory_ttf
#aa


##  Custom editors  ##

"Edits a vector with spherical coordinates; returns its new value."
function gui_spherical_vector( label, vec::v3f
                               ;
                               stays_normalized::Bool = false,
                               fallback_yaw::Ref{Float32} = Ref(zero(Float32))
                             )::v3f
    radius = vlength(vec)
    vec /= radius

    yawpitch = Float32.((atan(vec.y, vec.x), acos(vec.z)))
    # Keep the editor stable when it reaches a straight-up or straight-down vector.
    if yawpitch[2] in (Float32(-π), Float32(π))
        yawpitch = (fallback_yaw[], yawpitch[2])
    else
        fallback_yaw[] = yawpitch[1]
    end

    if stays_normalized
        # Remove floating-point error in the previous length calculation.
        radius = one(Float32)
    else
        # Provide an editor for the radius.
        @c CImGui.InputFloat("$label Length", &radius)
        CImGui.SameLine()
    end

    # Show two sliders, but with different ranges so we can't use CImGui.SliderFloat2().
    yaw, pitch = yawpitch
    content_width = CImGui.GetContentRegionAvailWidth()
    CImGui.SetNextItemWidth(content_width * 0.4)
    @c CImGui.SliderFloat("##Yaw", &yaw, -π, π)
    CImGui.SameLine()
    CImGui.SetNextItemWidth(content_width * 0.4)
    gui_with_nested_id("Slider 2") do
        @c CImGui.SliderFloat(label, &pitch, 0, π - 0.00001)
    end
    yawpitch = (yaw, pitch)

    # Convert back to cartesian coordinates.
    pitch_sincos = sincos(yawpitch[2])
    vec_2d = v2f(sincos(yawpitch[1])).yx * pitch_sincos[1]
    vec_z = pitch_sincos[2]
    return radius * vappend(vec_2d, vec_z)
end

export gui_spherical_vector


##  Small helper functions  ##

"
Sizes the next CImGui window in terms of a percentage of the actual window's size.

Optionally adds a pixel-space border, padding it inwards.
Optionally clamps the pixel size (by moving the max corner).
"
function gui_next_window_space(uv_space::Box2Df,
                               min_and_max_pixel_border::v2i = zero(v2i),
                               min_only_pixel_border::v2i = zero(v2i)
                               ;
                               min_pixel_size::v2i = zero(v2i),
                               max_pixel_size::v2i = -one(v2i),
                               window_size::v2i = get_window_size(get_context()))
    # Remove the padding from the reported window size.
    window_size -= min_and_max_pixel_border * v2i(2, 2)
    window_size -= min_only_pixel_border
    w_size::v2f = window_size * size(uv_space)
    w_size = clamp(
        w_size,
        min_pixel_size,
        vselect(max_pixel_size, typemax(v2i), max_pixel_size < 0)
    )

    # Add the padding to the calculated position.
    pos::v2f = (window_size * min_inclusive(uv_space)) +
               convert(v2f, min_only_pixel_border + min_and_max_pixel_border)

    CImGui.SetNextWindowPos(gVec2(pos...))
    CImGui.SetNextWindowSize(gVec2(w_size...))
end
"Sizes the next CImGui window in terms of a pixel rectangle"
function gui_next_window_space(pixel_space::Box2Di; window_size::v2i = get_window_size(get_context()))
    CImGui.SetNextWindowPos(gVec2(min_inclusive(pixel_space)...))
    CImGui.SetNextWindowSize(gVec2(size(pixel_space...)))
end

export gui_next_window_space



##############################
##    GUI drawing helpers

# Standard locations where primitives can be drawn with Dear ImGUI.
@bp_enum(GuiDrawingCanvas,
    current_window,
    background, foreground
)
get_gui_drawing_canvas(c::E_GuiDrawingCanvas)::Ptr{CImGui.ImDrawList} =
    if c == GuiDrawingCanvas.current_window
        CImGui.GetWindowDrawList()
    elseif c == GuiDrawingCanvas.background
        CImGui.GetBackgroundDrawList()
    elseif c == GuiDrawingCanvas.foreground
        CImGui.GetForegroundDrawList()
    else
        error("Unhandled: ", c)
    end

const GuiDrawingDestination = Union{E_GuiDrawingCanvas, Ptr{CImGui.ImDrawList}}
get_gui_drawing_canvas(ptr::Ptr{CImGui.ImDrawList}) = ptr

struct GuiDrawBorder
    border::GuiColor
end
struct GuiDrawFilled
    fill::GuiColor
end
const GuiDrawSimpleColorType = Union{GuiDrawBorder, GuiDrawFilled}

struct GuiDrawMultiColor
    top_left::GuiColor
    top_right::GuiColor
    bottom_left::GuiColor
    bottom_right::GuiColor
end
const GuiDrawColorType = Union{GuiDrawSimpleColorType, GuiDrawMultiColor}


"
Drawing coordinates (e.g. `TArea=Box2Df` for drawing boxes) that are relative
  to Dear ImGUI's current layout position (a.k.a. its Cursor pos).

You can also ask to emit a Dummy widget
  from the cursor to the bottom/right of the drawn object.
"
struct GuiDrawCursorRelative{TArea}
    area::TArea
    emit_dummy::v2b

    GuiDrawCursorRelative{TArea}(area, emit_dummy::Union{Bool, Vec2}) where {TArea} = new{TArea}(
        convert(TArea, area),
        if emit_dummy isa Bool
            v2b(emit_dummy, emit_dummy)
        else
            convert(v2b, emit_dummy)
        end
    )
    GuiDrawCursorRelative(area, emit_dummy::Union{Bool, Vec2}) = new{typeof(area)}(
        area,
        if emit_dummy isa Bool
            v2b(emit_dummy, emit_dummy)
        else
            convert(v2b, emit_dummy)
        end
    )
end
"Absolute or relative coordinates for some drawing operation"
const GuiDrawCoords{TArea} = Union{TArea, GuiDrawCursorRelative{TArea}}


function gui_get_draw_clip_area(canvas::GuiDrawingDestination = GuiDrawingCanvas.current_window)::Box2Df
    draw_list = get_gui_drawing_canvas(canvas)
    return Box2Df(
        min=convert(v2f, CImGui.GetClipRectMin(draw_list)),
        max=convert(v2f, CImGui.GetClipRectMax(draw_list))
    )
end

function gui_draw_line(coords::GuiDrawCoords{NTuple{2, v2f}},
                       color::GuiColor,
                       canvas::GuiDrawingDestination = GuiDrawingCanvas.current_window
                       ;
                       thickness::Float64 = 1.0)
    draw_list = get_gui_drawing_canvas(canvas)

    # Get absolute draw coordinates.
    cursor_pos = convert(v2f, CImGui.GetCursorScreenPos())
    (final_coords, is_relative)::Tuple{NTuple{2, v2f}, Bool} =
        if coords isa GuiDrawCursorRelative
            (coords.area, true)
        else
            (coords, false)
        end
    if is_relative
        final_coords = final_coords .+ convert(v2f, cursor_pos)
    end
    g_final_coords = convert.(Ref(gVec2), final_coords)

    CImGui.AddLine(draw_list, g_final_coords..., gui_color_to_im_color(color), thickness)

    # If desired, insert a dummy widget to cover this shape.
    if is_relative
        CImGui.Dummy((
            coords.emit_dummy * (max(final_coords...) - cursor_pos)
        )...)
    end
end
function gui_draw_rect(coords::GuiDrawCoords{Box2Df},
                       color::GuiDrawColorType
                       ;
                       canvas::GuiDrawingDestination = GuiDrawingCanvas.current_window,
                       # Border Thickness is only used if drawing with `GuiDrawBorder`.
                       border_thickness::Float64 = 1.0,
                       # Corner rounding is not used with MultiColor.
                       corner_roundedness::Float64 = 0.0,
                       corners_to_round::CImGui.LibCImGui.ImDrawFlags_ = CImGui.LibCImGui.ImDrawFlags_RoundCornersAll)
    draw_list = get_gui_drawing_canvas(canvas)

    # Get absolute draw coordinates.
    cursor_pos = convert(v2f, CImGui.GetCursorScreenPos())
    (area::Box2Df, is_relative::Bool) =
        if coords isa GuiDrawCursorRelative
            (coords.area, true)
        else
            (coords, false)
        end
    rect_corners::NTuple{2, v2f} = (min_inclusive(area), max_inclusive(area))
    if is_relative
        rect_corners = map(c-> c+cursor_pos, rect_corners)
    end
    g_rect_corners = convert.(Ref(gVec2), rect_corners)

    # Get the color/fill type, and dispatch.
    if color isa GuiDrawBorder
        CImGui.AddRect(draw_list, g_rect_corners..., gui_color_to_im_color(color.border),
                       corner_roundedness, corners_to_round, border_thickness)
    elseif color isa GuiDrawFilled
        CImGui.AddRectFilled(draw_list, g_rect_corners..., gui_color_to_im_color(color.fill),
                             corner_roundedness, corners_to_round)
    elseif color isa GuiDrawMultiColor
        CImGui.AddRectFilledMultiColor(draw_list, g_rect_corners...,
                                       gui_color_to_im_color.((color.top_left, color.top_right, color.bottom_left, color.bottom_right)))
    else
        error("Unhandled: ", typeof(color))
    end

    # If desired, insert a dummy widget to cover this shape.
    if is_relative
        CImGui.Dummy((
            coords.emit_dummy * (max(rect_corners...) - cursor_pos)
        )...)
    end
end
"Draws an arbitrary 4-sided shape"
function gui_draw_quad(coords::GuiDrawCoords{NTuple{4, v2f}},
                       color::GuiDrawSimpleColorType
                       ;
                       canvas::GuiDrawingDestination = GuiDrawingCanvas.current_window,
                       # Border Thickness is only used if drawing with `GuiDrawBorder`.
                       border_thickness::Float64 = 1.0)
    draw_list = get_gui_drawing_canvas(canvas)

    # Get absolute draw coordinates.
    cursor_pos = convert(v2f, CImGui.GetCursorScreenPos())
    (corners::NTuple{4, v2f}, is_relative::Bool) =
        if coords isa GuiDrawCursorRelative
            (coords.area, true)
        else
            (coords, false)
        end
    if is_relative
        corners = map(c-> c+cursor_pos, corners)
    end
    g_corners = convert.(Ref(gVec2), corners)

    # Get the color/fill type, and dispatch.
    if color isa GuiDrawBorder
        CImGui.AddQuad(draw_list, g_corners..., gui_color_to_im_color(color.border),
                       corner_roundedness, corners_to_round, border_thickness)
    elseif color isa GuiDrawFilled
        CImGui.AddQuadFilled(draw_list, g_corners..., gui_color_to_im_color(color.fill),
                             corner_roundedness, corners_to_round)
    else
        error("Unhandled: ", typeof(color))
    end

    # If desired, insert a dummy widget to cover this shape.
    if is_relative
        CImGui.Dummy((
            coords.emit_dummy * (max(corners...) - cursor_pos)
        )...)
    end
end
#TODO: All the other draw-list primitives

export gui_draw_line, gui_draw_rect, gui_draw_quad,
       gui_get_draw_clip_area,
       GuiDrawingCanvas, E_GuiDrawingCanvas,
       GuiDrawBorder, GuiDrawFilled, GuiDrawMultiColor,
       GuiDrawCursorRelative