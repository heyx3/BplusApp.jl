The graphics, input, and GUI packages for [B+](https://github.com/heyx3/B-plus). Refer to the main repo for documentation.

The modules contained here are:

* GL
* Input
* GUI
* ModernGL fork (see below for explanation)

If you only want this package, without the rest of B+, you can add `BplusApp` to your project and then do `using BplusApp; @using_bplus_app` to import all modules.

## ModernGL fork

This project keeps a fork of [ModernGL](https://github.com/JuliaGL/ModernGL.jl), called `ModernGLbp`, which adds ARB extensions and some UX improvements.

It is technically kept as a copy rather than a git submodule, because I don't know if Yggdrasil (Julia's package build system) can handle git submodules. However, `ModernGLbp` is also maintained as a true fork [here on Github](https://github.com/heyx3/ModernGL.jl), and if you're working on `BplusApp` you can quite easily treat this in-repo copy like a git submodule, with the following steps:

1. Clone the ModernGL fork somewhere else.
2. Copy *.git/* from that cloned repo, into this repo at *BplusApp/src/ModernGL_fork/*

This project's *.gitignore* file is configured to ignore the inner *.git/* folder.
The forked code will belong to both repos, BplusApp and ModernGL.
`cd` into *BplusApp/src/ModernGL_fork/* to work within ModernGL, and move outside there to work within BplusApp.