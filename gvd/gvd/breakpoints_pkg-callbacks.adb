with System; use System;
with Glib; use Glib;
with Gdk.Event; use Gdk.Event;
with Gdk.Types; use Gdk.Types;
with Gtk.Accel_Group; use Gtk.Accel_Group;
with Gtk.Object; use Gtk.Object;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Style; use Gtk.Style;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Main; use Gtk.Main;
with Advanced_Breakpoint_Pkg; use Advanced_Breakpoint_Pkg;

package body Breakpoints_Pkg.Callbacks is

   use Gtk.Arguments;

   ----------------------------
   -- On_Advanced_Bp_Clicked --
   ----------------------------

   procedure On_Advanced_Bp_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
      Descriptor : Advanced_Breakpoint_Descriptor;
   begin
      Advanced_Breakpoint_Editor (Descriptor);
      Free (Descriptor);
   end On_Advanced_Bp_Clicked;

   -----------------------
   -- On_Add_Bp_Clicked --
   -----------------------

   procedure On_Add_Bp_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
   begin
      null;
   end On_Add_Bp_Clicked;

   --------------------------
   -- On_Remove_Bp_Clicked --
   --------------------------

   procedure On_Remove_Bp_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
   begin
      null;
   end On_Remove_Bp_Clicked;

   ------------------------------
   -- On_Remove_All_Bp_Clicked --
   ------------------------------

   procedure On_Remove_All_Bp_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
   begin
      null;
   end On_Remove_All_Bp_Clicked;

   ----------------------
   -- On_Ok_Bp_Clicked --
   ----------------------

   procedure On_Ok_Bp_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
   begin
      Main_Quit;
   end On_Ok_Bp_Clicked;

   --------------------------
   -- On_Cancel_Bp_Clicked --
   --------------------------

   procedure On_Cancel_Bp_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
   begin
      Main_Quit;
   end On_Cancel_Bp_Clicked;

end Breakpoints_Pkg.Callbacks;
