with System; use System;
with Glib; use Glib;
with Gdk.Event; use Gdk.Event;
with Gdk.Types; use Gdk.Types;
with Gtk.Accel_Group; use Gtk.Accel_Group;
with Gtk.Object; use Gtk.Object;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Style; use Gtk.Style;
with Gtk.Widget; use Gtk.Widget;

package body Builder_Pkg.Callbacks is

   use Gtk.Arguments;

   -----------------------------
   -- On_Builder_Delete_Event --
   -----------------------------

   function On_Builder_Delete_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean
   is
      Arg1 : Gdk_Event := To_Event (Params, 1);
   begin
      return False;
   end On_Builder_Delete_Event;

   ----------------------
   -- On_Build_Clicked --
   ----------------------

   procedure On_Build_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
   begin
      null;
   end On_Build_Clicked;

   ---------------------
   -- On_Quit_Clicked --
   ---------------------

   procedure On_Quit_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
   begin
      null;
   end On_Quit_Clicked;

end Builder_Pkg.Callbacks;
