PROJECT_DIR = joinpath(@__DIR__, "..")

# Make sure the test is always running in the same directory and within the same project.
using Pkg
cd(PROJECT_DIR)
Pkg.activate(".")

using BplusCore; @using_bplus_core
using BplusApp; @using_bplus_app

# Enable asserts for B+ App.
BplusApp.GL.bp_gl_asserts_enabled() = true
BplusApp.Input.bp_input_asserts_enabled() = true
BplusApp.GUI.bp_gui_asserts_enabled() = true

# Execute the tests.
const TEST_HEADER_EXTRA = quote
    using Dates, Setfield, InteractiveUtils
    using JSON3, CSyntax, StructTypes
    using CImGui, GLFW

    # Sadly, the macros to auto-import B+ do not work right in here.
    using BplusCore, BplusApp
    for use in (BplusCore.MODULES_USING_STATEMENTS...,
                BplusApp.MODULES_USING_STATEMENTS...)
        eval(use)
    end

    "Checks for OpenGL error messages, and prints less-severe messages"
    function check_gl_logs(context::String)
        logs = pull_gl_logs()
        for log in logs
            msg = sprint(show, log)
            if log.severity in (DebugEventSeverities.high, DebugEventSeverities.medium)
                @warn "$context. $msg"
                println("Stacktrace:\n--------------------")
                display(stacktrace())
                println("-------------------\n\n")
            elseif log.severity == DebugEventSeverities.low
                @warn "$context. $msg"
            elseif log.severity == DebugEventSeverities.none
                @info "$context. $msg"
            else
                @error "Message, UNEXPECTED SEVERITY $(log.severity): $msg"
            end
        end
    end
end
include_string(@__MODULE__, BplusCore.TEST_RUNNER_CODE,
               joinpath(pathof(BplusCore), "..", "test_runner.jl"))