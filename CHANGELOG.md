# v0.2.0

* Add GUI helpers:
  * `gui_tab_views()`
  * `gui_tab_item()`
* Add `convert_pixel()` to translate `ImageIO` data into B+ terms
* Fix bug when using `View` instead of `Texture` in CImGui/GuiService
* Allow getting the handle of a `View` with `get_ogl_handle(v)`
* **[Breaking]** Fix how `@std140` and `@std430` work
  * They must be mutable to avoid Julia JIT dying on crazy immutable types like `NTuple{16400, UInt8}`
  * Now acts like a nested series of mutable structs, but is actually one contiguous buffer owned by the outermost struct

# v0.1.1

* Made OpenGL extension loading more flexible.
* Allowed for Intel integrated GPU support by making shader_int64 optional!

# v0.1.0

Initial version