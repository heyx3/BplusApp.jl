##  Buttons  ##

struct JoystickButtonID
    i::Int
end
StructTypes.StructType(::Type{JoystickButtonID}) = StructTypes.NumberType
StructTypes.numbertype(::Type{JoystickButtonID}) = Int
Int(j::JoystickButtonID) = j.i


"Some kind of binary input that GLFW tracks"
#NOTE: Make sure this has no overlap with `AxisID`!
const ButtonID = Union{GLFW.Key, GLFW.MouseButton, Tuple{GLFW.Joystick, JoystickButtonID}}
const SerializedButtonID = @SerializedUnion(union_types(ButtonID)...)

export JoystickButtonID, ButtonID, SerializedButtonID


##  Axes  ##

@bp_enum(MouseAxes,
    x, y, scroll_x, scroll_y
)

struct JoystickAxisID
    i::Int
end
StructTypes.StructType(::Type{JoystickAxisID}) = StructTypes.NumberType
StructTypes.numbertype(::Type{JoystickAxisID}) = Int
Int(j::JoystickAxisID) = j.i

"Maps a key up/down to a value range"
struct ButtonAsAxis
    id::ButtonID
    released::Float32
    pressed::Float32
end
ButtonAsAxis(id::ButtonID) = ButtonAsAxis(id, 0, 1)
ButtonAsAxis_Reflected(id::ButtonID) = ButtonAsAxis(id, 1, 0)
ButtonAsAxis_Negative(id::ButtonID) = ButtonAsAxis(id, 0, -1)

"A `StructTypes`-friendly representation of `ButtonAsAxis`"
struct SerializedButtonAsAxis
    id::SerializedButtonID
    released::Float32
    pressed::Float32
    SerializedButtonAsAxis(b::ButtonAsAxis) = new(SerializedButtonID(b.id), b.released, b.pressed)
end
StructTypes.StructType(::Type{ButtonAsAxis}) = StructTypes.CustomStruct()
StructTypes.lower(b::ButtonAsAxis) = SerializedButtonAsAxis(b)
StructTypes.lowertype(::Type{ButtonAsAxis}) = SerializedButtonAsAxis
StructTypes.StructType(::Type{SerializedButtonAsAxis}) = StructTypes.UnorderedStruct()

"Some kind of continuous input that GLFW tracks"
#NOTE: Make sure this has no overlap with `ButtonID`!
const AxisID = Union{E_MouseAxes,
                     Tuple{GLFW.Joystick, JoystickAxisID},
                     ButtonAsAxis, Vector{ButtonAsAxis}}
const SerializedAxisID = @SerializedUnion(union_types(AxisID)...)

export MouseAxes, E_MouseAxes, JoystickAxisID, AxisID,
       ButtonAsAxis, ButtonAsAxis_Reflected, ButtonAsAxis_Negative