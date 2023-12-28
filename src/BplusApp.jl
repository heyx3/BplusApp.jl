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


"Imports all App B+ modules"
macro using_bplus_app()
    return :( using BplusApp.GL, BplusApp.Input, BplusApp.GUI )
end
export @using_bplus_app


end # module BplusApp
