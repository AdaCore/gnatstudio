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
with Gtk.Main;

package body Variable_Editor_Pkg.Callbacks is

--     use Gtk.Arguments;

   --------------------
   -- On_Add_Clicked --
   --------------------

   procedure On_Add_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
      Editor : Variable_Edit := Variable_Edit (Object);
      Edit : New_Var_Edit;
   begin
      Gtk_New (Edit, Editor, Scenario_Variable_Only => True);
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
      Gtk.Main.Main_Quit;
   end On_Close_Clicked;

end Variable_Editor_Pkg.Callbacks;
