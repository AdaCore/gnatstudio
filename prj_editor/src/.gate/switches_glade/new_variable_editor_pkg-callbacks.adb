with System; use System;
with Glib; use Glib;
with Gdk.Event; use Gdk.Event;
with Gdk.Types; use Gdk.Types;
with Gtk.Accel_Group; use Gtk.Accel_Group;
with Gtk.Object; use Gtk.Object;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Style; use Gtk.Style;
with Gtk.Widget; use Gtk.Widget;

package body New_Variable_Editor_Pkg.Callbacks is

   use Gtk.Arguments;

   --------------------------------
   -- On_Get_Environment_Toggled --
   --------------------------------

   procedure On_Get_Environment_Toggled
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      null;
   end On_Get_Environment_Toggled;

   -------------------------------
   -- On_Typed_Variable_Toggled --
   -------------------------------

   procedure On_Typed_Variable_Toggled
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      null;
   end On_Typed_Variable_Toggled;

   ----------------------------
   -- On_Concatenate_Clicked --
   ----------------------------

   procedure On_Concatenate_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      null;
   end On_Concatenate_Clicked;

   --------------------
   -- On_Add_Clicked --
   --------------------

   procedure On_Add_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      null;
   end On_Add_Clicked;

   -----------------------
   -- On_Cancel_Clicked --
   -----------------------

   procedure On_Cancel_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      null;
   end On_Cancel_Clicked;

end New_Variable_Editor_Pkg.Callbacks;
