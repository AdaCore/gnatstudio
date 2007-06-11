with Gtk.Widget; use Gtk.Widget;
with Gtk.Container; use Gtk.Container;

package RAD.GB_Widget is

   procedure Children_Foreach
     (Widget   : access Gtk_Widget_Record'Class;
      Callback : Forall_Function);
   --  Call the given callback for each child of a widget.
   --  It gets round some of the quirks of the different versions of GTK,
   --  and descends menus as well.

   function Is_GB_Widget
     (Widget : access Gtk_Widget_Record'Class) return Boolean;
   --  Return True if Widget is a widget handled (vampirized) by RAD.

end RAD.GB_Widget;
