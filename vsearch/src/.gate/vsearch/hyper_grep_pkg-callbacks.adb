with System; use System;
with Glib; use Glib;
with Gdk.Event; use Gdk.Event;
with Gdk.Types; use Gdk.Types;
with Gtk.Accel_Group; use Gtk.Accel_Group;
with Gtk.Object; use Gtk.Object;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Style; use Gtk.Style;
with Gtk.Widget; use Gtk.Widget;

package body Hyper_Grep_Pkg.Callbacks is

   use Gtk.Arguments;

   --------------------------------
   -- On_Hyper_Grep_Delete_Event --
   --------------------------------

   function On_Hyper_Grep_Delete_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean
   is
      Arg1 : Gdk_Event := To_Event (Params, 1);
   begin
      return False;
   end On_Hyper_Grep_Delete_Event;

   ------------------------------
   -- On_Browse_Button_Clicked --
   ------------------------------

   procedure On_Browse_Button_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
   begin
      null;
   end On_Browse_Button_Clicked;

   -----------------------------------
   -- On_Only_Project_Check_Toggled --
   -----------------------------------

   procedure On_Only_Project_Check_Toggled
     (Object : access Gtk_Check_Button_Record'Class)
   is
   begin
      null;
   end On_Only_Project_Check_Toggled;

   -----------------------------
   -- On_Start_Button_Clicked --
   -----------------------------

   procedure On_Start_Button_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
   begin
      null;
   end On_Start_Button_Clicked;

   ----------------------------
   -- On_Stop_Button_Clicked --
   ----------------------------

   procedure On_Stop_Button_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
   begin
      null;
   end On_Stop_Button_Clicked;

   -----------------------------
   -- On_Close_Button_Clicked --
   -----------------------------

   procedure On_Close_Button_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
   begin
      null;
   end On_Close_Button_Clicked;

end Hyper_Grep_Pkg.Callbacks;
