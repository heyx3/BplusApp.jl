"The graphics/input/GUI part of the B+ library"
module BplusApp

using BplusCore
@using_bplus_core

include("ModernGL_fork/ModernGLbp.jl")

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


"Imports all App B+ modules"
macro using_bplus_app()
    return quote
        using BplusCore; @using_bplus_core
        using BplusApp, BplusApp.GL, BplusApp.Input, BplusApp.GUI
    end
end
export @using_bplus_app


end # module BplusApp
