The graphics, input, and GUI packages for [B+](https://github.com/heyx3/B-plus). Refer to the main repo for documentation.

The modules contained here are:

* GL
* Input
* GUI

There is also a fork of [ModernGL](https://github.com/JuliaGL/ModernGL.jl) in here, called `ModernGLbp`, which adds ARB extensions and some UX improvements. This fork is unfortunately kept as a copy rather than something like a git submodule, because I don't know if Julia's package build system (Yggdrasil) can handle git submodules. However, `ModernGLbp` is also maintained as a true fork [here on Github](https://github.com/heyx3/ModernGL.jl), and the local copy should exactly match this one.

If you only want this package (and BplusCore), without the rest of B+, you can add this package directly to your project and then do `using BplusApp; @using_bplus_app` to import all the above.