with System; use System;
with Glib; use Glib;
with Gdk.Event; use Gdk.Event;
with Gdk.Types; use Gdk.Types;
with Gtk.Accel_Group; use Gtk.Accel_Group;
with Gtk.Object; use Gtk.Object;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Style; use Gtk.Style;
with Gtk.Widget; use Gtk.Widget;

package body Explorer_Window_Pkg.Callbacks is

   use Gtk.Arguments;

   -------------------------------------
   -- On_Explorer_Window_Delete_Event --
   -------------------------------------

   function On_Explorer_Window_Delete_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean
   is
      Arg1 : Gdk_Event := To_Event (Params, 1);
   begin
      return False;
   end On_Explorer_Window_Delete_Event;

   -------------------------
   -- On_Clist_Select_Row --
   -------------------------

   procedure On_Clist_Select_Row
     (Object : access Gtk_Clist_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      Arg1 : Gint := To_Gint (Params, 1);
      Arg2 : Gint := To_Gint (Params, 2);
      Arg3 : Gdk_Event := To_Event (Params, 3);
   begin
      null;
   end On_Clist_Select_Row;

   -------------------
   -- On_Ok_Clicked --
   -------------------

   procedure On_Ok_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
   begin
      null;
   end On_Ok_Clicked;

   -----------------------
   -- On_Cancel_Clicked --
   -----------------------

   procedure On_Cancel_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
   begin
      null;
   end On_Cancel_Clicked;

end Explorer_Window_Pkg.Callbacks;
