with System; use System;
with Glib; use Glib;
with Gdk.Event; use Gdk.Event;
with Gdk.Types; use Gdk.Types;
with Gtk.Accel_Group; use Gtk.Accel_Group;
with Gtk.Object; use Gtk.Object;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Style; use Gtk.Style;
with Gtk.Widget; use Gtk.Widget;

package body Open_Session_Pkg.Callbacks is

   use Gtk.Arguments;

   --------------------------
   -- On_List_Select_Child --
   --------------------------

   procedure On_List_Select_Child
     (Object : access Gtk_List_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      Arg1 : Gtk_Widget := Gtk_Widget (To_Object (Params, 1));
   begin
      null;
   end On_List_Select_Child;

   ---------------------------
   -- On_Select_All_Clicked --
   ---------------------------

   procedure On_Select_All_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
   begin
      null;
   end On_Select_All_Clicked;

   -----------------------------
   -- On_Unselect_All_Clicked --
   -----------------------------

   procedure On_Unselect_All_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
   begin
      null;
   end On_Unselect_All_Clicked;

   --------------------------
   -- On_Ok_Button_Clicked --
   --------------------------

   procedure On_Ok_Button_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
   begin
      null;
   end On_Ok_Button_Clicked;

   ------------------------------
   -- On_Cancel_Button_Clicked --
   ------------------------------

   procedure On_Cancel_Button_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
   begin
      null;
   end On_Cancel_Button_Clicked;

end Open_Session_Pkg.Callbacks;
