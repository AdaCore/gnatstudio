with System; use System;
with Glib; use Glib;
with Gdk.Event; use Gdk.Event;
with Gdk.Types; use Gdk.Types;
with Gtk.Accel_Group; use Gtk.Accel_Group;
with Gtk.Object; use Gtk.Object;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Style; use Gtk.Style;
with Gtk.Widget; use Gtk.Widget;

package body Main_Debug_Window_Pkg.Callbacks is

   use Gtk.Arguments;

   ---------------------------------------
   -- On_Main_Debug_Window_Delete_Event --
   ---------------------------------------

   function On_Main_Debug_Window_Delete_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean
   is
      Arg1 : Gdk_Event := To_Event (Params, 1);
   begin
      return False;
   end On_Main_Debug_Window_Delete_Event;

   ------------------------------
   -- On_Run1_Toolbar_Activate --
   ------------------------------

   procedure On_Run1_Toolbar_Activate
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
   begin
      null;
   end On_Run1_Toolbar_Activate;

   ------------------------
   -- On_Start1_Activate --
   ------------------------

   procedure On_Start1_Activate
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
   begin
      null;
   end On_Start1_Activate;

   -----------------------
   -- On_Step1_Activate --
   -----------------------

   procedure On_Step1_Activate
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
   begin
      null;
   end On_Step1_Activate;

   -----------------------------------
   -- On_Step_Instruction1_Activate --
   -----------------------------------

   procedure On_Step_Instruction1_Activate
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
   begin
      null;
   end On_Step_Instruction1_Activate;

   -----------------------
   -- On_Next1_Activate --
   -----------------------

   procedure On_Next1_Activate
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
   begin
      null;
   end On_Next1_Activate;

   -----------------------------------
   -- On_Next_Instruction1_Activate --
   -----------------------------------

   procedure On_Next_Instruction1_Activate
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
   begin
      null;
   end On_Next_Instruction1_Activate;

   -------------------------
   -- On_Finish1_Activate --
   -------------------------

   procedure On_Finish1_Activate
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
   begin
      null;
   end On_Finish1_Activate;

   ---------------------------
   -- On_Continue1_Activate --
   ---------------------------

   procedure On_Continue1_Activate
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
   begin
      null;
   end On_Continue1_Activate;

   ---------------------
   -- On_Up1_Activate --
   ---------------------

   procedure On_Up1_Activate
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
   begin
      null;
   end On_Up1_Activate;

   -----------------------
   -- On_Down1_Activate --
   -----------------------

   procedure On_Down1_Activate
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
   begin
      null;
   end On_Down1_Activate;

   ----------------------------
   -- On_Interrupt1_Activate --
   ----------------------------

   procedure On_Interrupt1_Activate
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
   begin
      null;
   end On_Interrupt1_Activate;

   -------------------------------------
   -- On_Process_Notebook_Switch_Page --
   -------------------------------------

   procedure On_Process_Notebook_Switch_Page
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      Arg1 : Address := To_Address (Params, 1);
      Arg2 : Guint := To_Guint (Params, 2);
   begin
      null;
   end On_Process_Notebook_Switch_Page;

end Main_Debug_Window_Pkg.Callbacks;
