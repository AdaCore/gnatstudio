with System; use System;
with Glib; use Glib;
with Gdk.Event; use Gdk.Event;
with Gdk.Types; use Gdk.Types;
with Gtk.Accel_Group; use Gtk.Accel_Group;
with Gtk.Object; use Gtk.Object;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Style; use Gtk.Style;
with Gtk.Widget; use Gtk.Widget;

package body Breakpoints_Pkg.Callbacks is

   use Gtk.Arguments;

   ----------------------------------
   -- On_Location_Selected_Toggled --
   ----------------------------------

   procedure On_Location_Selected_Toggled
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      null;
   end On_Location_Selected_Toggled;

   -----------------------------------
   -- On_Subprogam_Selected_Toggled --
   -----------------------------------

   procedure On_Subprogam_Selected_Toggled
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      null;
   end On_Subprogam_Selected_Toggled;

   ---------------------------------
   -- On_Address_Selected_Toggled --
   ---------------------------------

   procedure On_Address_Selected_Toggled
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      null;
   end On_Address_Selected_Toggled;

   --------------------------------
   -- On_Regexp_Selected_Toggled --
   --------------------------------

   procedure On_Regexp_Selected_Toggled
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      null;
   end On_Regexp_Selected_Toggled;

   -----------------------------
   -- On_Add_Location_Clicked --
   -----------------------------

   procedure On_Add_Location_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      null;
   end On_Add_Location_Clicked;

   ----------------------------------
   -- On_Advanced_Location_Clicked --
   ----------------------------------

   procedure On_Advanced_Location_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      null;
   end On_Advanced_Location_Clicked;

   -------------------------------
   -- On_Add_Watchpoint_Clicked --
   -------------------------------

   procedure On_Add_Watchpoint_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      null;
   end On_Add_Watchpoint_Clicked;

   ------------------------------------
   -- On_Advanced_Watchpoint_Clicked --
   ------------------------------------

   procedure On_Advanced_Watchpoint_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      null;
   end On_Advanced_Watchpoint_Clicked;

   ------------------------------
   -- On_Add_Exception_Clicked --
   ------------------------------

   procedure On_Add_Exception_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      null;
   end On_Add_Exception_Clicked;

   -----------------------------------
   -- On_Advanced_Exception_Clicked --
   -----------------------------------

   procedure On_Advanced_Exception_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      null;
   end On_Advanced_Exception_Clicked;

   -----------------------
   -- On_Remove_Clicked --
   -----------------------

   procedure On_Remove_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      null;
   end On_Remove_Clicked;

   ---------------------
   -- On_View_Clicked --
   ---------------------

   procedure On_View_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      null;
   end On_View_Clicked;

   ----------------------
   -- On_Ok_Bp_Clicked --
   ----------------------

   procedure On_Ok_Bp_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      null;
   end On_Ok_Bp_Clicked;

end Breakpoints_Pkg.Callbacks;
