with System; use System;
with Glib; use Glib;
with Gdk.Event; use Gdk.Event;
with Gdk.Types; use Gdk.Types;
with Gtk.Accel_Group; use Gtk.Accel_Group;
with Gtk.Object; use Gtk.Object;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Style; use Gtk.Style;
with Gtk.Widget; use Gtk.Widget;

package body Codefix_Window_Pkg.Callbacks is

   use Gtk.Arguments;

   ------------------------------------
   -- On_Codefix_Window_Delete_Event --
   ------------------------------------

   function On_Codefix_Window_Delete_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean
   is
      Arg1 : Gdk_Event := To_Event (Params, 1);
   begin
      return False;
   end On_Codefix_Window_Delete_Event;

   --------------------------
   -- On_Fix_Entry_Changed --
   --------------------------

   procedure On_Fix_Entry_Changed
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      null;
   end On_Fix_Entry_Changed;

   --------------------------------
   -- On_Skip_Correction_Clicked --
   --------------------------------

   procedure On_Skip_Correction_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      null;
   end On_Skip_Correction_Clicked;

   ----------------------------------
   -- On_Accept_Correction_Clicked --
   ----------------------------------

   procedure On_Accept_Correction_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      null;
   end On_Accept_Correction_Clicked;

   -------------------------------------
   -- On_Skip_All_Corrections_Clicked --
   -------------------------------------

   procedure On_Skip_All_Corrections_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      null;
   end On_Skip_All_Corrections_Clicked;

   ---------------------------------------
   -- On_Accept_All_Corrections_Clicked --
   ---------------------------------------

   procedure On_Accept_All_Corrections_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      null;
   end On_Accept_All_Corrections_Clicked;

   ---------------------
   -- On_Undo_Clicked --
   ---------------------

   procedure On_Undo_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      null;
   end On_Undo_Clicked;

   ------------------------
   -- On_Refresh_Clicked --
   ------------------------

   procedure On_Refresh_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      null;
   end On_Refresh_Clicked;

   -------------------------------
   -- On_Cancel_Changes_Clicked --
   -------------------------------

   procedure On_Cancel_Changes_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      null;
   end On_Cancel_Changes_Clicked;

   ------------------------------
   -- On_Apply_Changes_Clicked --
   ------------------------------

   procedure On_Apply_Changes_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      null;
   end On_Apply_Changes_Clicked;

end Codefix_Window_Pkg.Callbacks;
