with System; use System;
with Glib; use Glib;
with Gdk.Event; use Gdk.Event;
with Gdk.Types; use Gdk.Types;
with Gtk.Accel_Group; use Gtk.Accel_Group;
with Gtk.Object; use Gtk.Object;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Style; use Gtk.Style;
with Gtk.Widget; use Gtk.Widget;

package body Open_Program_Pkg.Callbacks is

   use Gtk.Arguments;

   -----------------------------
   -- On_Radio_Button_Toggled --
   -----------------------------

   procedure On_Radio_Button_Toggled
     (Object : access Gtk_Radio_Button_Record'Class)
   is
   begin
      null;
   end On_Radio_Button_Toggled;

   ----------------------------
   -- On_Open_Button_Clicked --
   ----------------------------

   procedure On_Open_Button_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
   begin
      null;
   end On_Open_Button_Clicked;

   ------------------------
   -- On_Ok_Open_Clicked --
   ------------------------

   procedure On_Ok_Open_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
   begin
      null;
   end On_Ok_Open_Clicked;

   ----------------------------
   -- On_Cancel_Open_Clicked --
   ----------------------------

   procedure On_Cancel_Open_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
   begin
      null;
   end On_Cancel_Open_Clicked;

   --------------------------
   -- On_Help_Open_Clicked --
   --------------------------

   procedure On_Help_Open_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
   begin
      null;
   end On_Help_Open_Clicked;

end Open_Program_Pkg.Callbacks;
