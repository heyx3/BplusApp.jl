# bp_gl_context( v2i(200, 200), "Input tests";
#                vsync=VsyncModes.on,
#                debug_mode=true
#              ) do context::
#     create_button("B1", ButtonInput(GLFW.KEY_DOWN, ButtonModes.down))
#     create_button("B2", ButtonInput(GLFW.MouseButton, ButtonModes.up))
# end