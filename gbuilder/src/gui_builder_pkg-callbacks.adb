with System; use System;
with Glib; use Glib;
with Gdk.Event; use Gdk.Event;
with Gdk.Types; use Gdk.Types;
with Gtk.Accel_Group; use Gtk.Accel_Group;
with Gtk.Object; use Gtk.Object;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Style; use Gtk.Style;
with Gtk.Widget; use Gtk.Widget;

package body Gui_Builder_Pkg.Callbacks is

   use Gtk.Arguments;

   ---------------------------------
   -- On_Gui_Builder_Delete_Event --
   ---------------------------------

   function On_Gui_Builder_Delete_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean
   is
      Arg1 : Gdk_Event := To_Event (Params, 1);
   begin
      return False;
   end On_Gui_Builder_Delete_Event;

   -----------------------
   -- On_Quit1_Activate --
   -----------------------

   procedure On_Quit1_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Quit1_Activate;

   -----------------------
   -- On_Edit1_Activate --
   -----------------------

   procedure On_Edit1_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Edit1_Activate;

   ---------------------------
   -- On_Settings1_Activate --
   ---------------------------

   procedure On_Settings1_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Settings1_Activate;

   -----------------------
   -- On_Help1_Activate --
   -----------------------

   procedure On_Help1_Activate
     (Object : access Gtk_Menu_Item_Record'Class)
   is
   begin
      null;
   end On_Help1_Activate;

end Gui_Builder_Pkg.Callbacks;
