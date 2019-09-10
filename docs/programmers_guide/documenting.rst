***********************
Documenting your module
***********************

All modules should be documented, so that the users are aware of all
its capabilities.

There are several levels of documentation:


* Tooltips
  It is recommended that all new preferences and as much of the GUI as
  possible be documented through tooltips. This is the only help that
  most users will read.

  Tooltips are easily added directly with gtk+: Just call
  `Gtk.Widget.Set_Tooltip_Text` or `Gtk.Widget.Set_Tooltip_Markup`

* extended documentation
  Extended documentation should be written in HTML.
  See the GNAT Studio user's guide on how to make new documentation available to
  users.


