--  with System; use System;
--  with Glib; use Glib;
--  with Gdk.Event; use Gdk.Event;
--  with Gdk.Types; use Gdk.Types;
--  with Gtk.Accel_Group; use Gtk.Accel_Group;
--  with Gtk.Object; use Gtk.Object;
--  with Gtk.Enums; use Gtk.Enums;
--  with Gtk.Style; use Gtk.Style;
with Gtk.Widget; use Gtk.Widget;
with Variable_Editors; use Variable_Editors;

package body Variable_Editor_Pkg.Callbacks is

--     use Gtk.Arguments;

   --------------------
   -- On_Add_Clicked --
   --------------------

   procedure On_Add_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
      Edit : New_Var_Edit;
   begin
      Gtk_New (Edit);
      Show_All (Edit);
   end On_Add_Clicked;

   ----------------------
   -- On_Close_Clicked --
   ----------------------

   procedure On_Close_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      --  ??? Should reset the current values for the variables
      null;
   end On_Close_Clicked;

end Variable_Editor_Pkg.Callbacks;
