"The graphics/input/GUI part of the B+ library"
module BplusApp

using BplusCore
@using_bplus_core

include("ModernGL_fork/src/ModernGLbp.jl")

include("GL/GL.jl")
export GL
include("Input/Input.jl")
export Input
include("GUI/GUI.jl")
export GUI

# Extras built on top of the main three modules
using .GL, .Input, .GUI
include("basic_graphics_service.jl")
include("game_loop.jl")


# Helper macro to import all BplusApp stuff.
const MODULES = tuple(:GL, :Input, :GUI, :ModernGLbp)
const MODULES_USING_STATEMENTS = [:( using BplusApp.$m ) for m in MODULES]

"Imports all App B+ modules"
macro using_bplus_app()
    return quote $(MODULES_USING_STATEMENTS...) end
end
export @using_bplus_app


end # module BplusApp
