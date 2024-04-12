# v0.1.2

* Add GUI helpers:
  * `gui_tab_views()`
  * `gui_tab_item()`
* Add `convert_pixel()` to translate `ImageIO` data into B+ terms
* Fix bug when using `View` instead of `Texture` in CImGui/GuiService
* Allow getting the handle of a `View` with `get_ogl_handle(v)`
* Catch more user errors:
  * Trying to make a `@std140` or `@std430` struct mutable

# v0.1.1

* Made OpenGL extension loading more flexible.
* Allowed for Intel integrated GPU support by making shader_int64 optional!

# v0.1.0

Initial version