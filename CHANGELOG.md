# v0.3.0

\[none, just syncing with BplusCore's updates]

# v0.2.1

* Fix numerous bugs within `GL`
* Cosmetic/debug-time improvements
* GUI helper functions:
  * Add more flexible parameters
  * Fix API bugs
* `service_Input_reset()` to reset the service to a blank slate

# v0.2.0

* Add GUI helpers:
  * `gui_tab_views()`
  * `gui_tab_item()`
* Add `convert_pixel()` to translate `ImageIO` data into B+ terms
* Fix bug when using `View` instead of `Texture` in CImGui/GuiService
* Allow getting the handle of a `View` with `get_ogl_handle(v)`
* **[Breaking]** Improve the getting and setting of Buffer data
* **[Breaking]** Completely rewrite `@std140` and `@std430`
  * They must be mutable to avoid Julia JIT dying on crazy immutable types like `NTuple{16400, UInt8}`
  * Now they are backed by a mutable byte array rather than an `NTuple` of bytes
    * Nested structs and static-arrays are views of the outermost struct's byte array

# v0.1.1

* Made OpenGL extension loading more flexible.
* Allowed for Intel integrated GPU support by making shader_int64 optional!

# v0.1.0

Initial version