with System; use System;
with Glib; use Glib;
with Gdk.Event; use Gdk.Event;
with Gdk.Types; use Gdk.Types;
with Gtk.Accel_Group; use Gtk.Accel_Group;
with Gtk.Object; use Gtk.Object;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Style; use Gtk.Style;
with Gtk.Widget; use Gtk.Widget;

package body Variable_Editor_Pkg.Callbacks is

   use Gtk.Arguments;

   --------------------
   -- On_Add_Clicked --
   --------------------

   procedure On_Add_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      null;
   end On_Add_Clicked;

   ----------------------
   -- On_Close_Clicked --
   ----------------------

   procedure On_Close_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      null;
   end On_Close_Clicked;

end Variable_Editor_Pkg.Callbacks;
