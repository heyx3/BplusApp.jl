module Input

#TODO: Fix serialization, and add a serialization unit test

# External dependencies
using Setfield
using GLFW, StructTypes, MacroTools

# B+ dependencies
using BplusCore; @using_bplus_core
using ..GL

@make_toggleable_asserts bp_input_

@decentralized_module_init

include("mappings.jl")
include("inputs.jl")
include("service.jl")

end # module