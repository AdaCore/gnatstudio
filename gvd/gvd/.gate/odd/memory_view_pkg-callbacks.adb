with System; use System;
with Glib; use Glib;
with Gdk.Event; use Gdk.Event;
with Gdk.Types; use Gdk.Types;
with Gtk.Accel_Group; use Gtk.Accel_Group;
with Gtk.Object; use Gtk.Object;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Style; use Gtk.Style;
with Gtk.Widget; use Gtk.Widget;

package body Memory_View_Pkg.Callbacks is

   use Gtk.Arguments;

   ---------------------------------
   -- On_Memory_View_Delete_Event --
   ---------------------------------

   function On_Memory_View_Delete_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean
   is
      Arg1 : Gdk_Event := To_Event (Params, 1);
   begin
      return False;
   end On_Memory_View_Delete_Event;

   ----------------------------------
   -- On_Memory_View_Size_Allocate --
   ----------------------------------

   procedure On_Memory_View_Size_Allocate
     (Object : access Gtk_Window_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      Arg1 : Address := To_Address (Params, 1);
   begin
      null;
   end On_Memory_View_Size_Allocate;

   -------------------------------
   -- On_Address_Entry_Activate --
   -------------------------------

   procedure On_Address_Entry_Activate
     (Object : access Gtk_Entry_Record'Class)
   is
   begin
      null;
   end On_Address_Entry_Activate;

   -----------------------------
   -- On_Address_View_Clicked --
   -----------------------------

   procedure On_Address_View_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
   begin
      null;
   end On_Address_View_Clicked;

   ---------------------------
   -- On_Size_Entry_Changed --
   ---------------------------

   procedure On_Size_Entry_Changed
     (Object : access Gtk_Entry_Record'Class)
   is
   begin
      null;
   end On_Size_Entry_Changed;

   ---------------------------
   -- On_Data_Entry_Changed --
   ---------------------------

   procedure On_Data_Entry_Changed
     (Object : access Gtk_Entry_Record'Class)
   is
   begin
      null;
   end On_Data_Entry_Changed;

   ---------------------------
   -- On_Show_Ascii_Toggled --
   ---------------------------

   procedure On_Show_Ascii_Toggled
     (Object : access Gtk_Check_Button_Record'Class)
   is
   begin
      null;
   end On_Show_Ascii_Toggled;

   ---------------------
   -- On_Pgup_Clicked --
   ---------------------

   procedure On_Pgup_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
   begin
      null;
   end On_Pgup_Clicked;

   ---------------------
   -- On_Pgdn_Clicked --
   ---------------------

   procedure On_Pgdn_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
   begin
      null;
   end On_Pgdn_Clicked;

   -----------------------------
   -- On_View_Key_Press_Event --
   -----------------------------

   function On_View_Key_Press_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean
   is
      Arg1 : Gdk_Event := To_Event (Params, 1);
   begin
      return False;
   end On_View_Key_Press_Event;

   -------------------------
   -- On_View_Move_Cursor --
   -------------------------

   procedure On_View_Move_Cursor
     (Object : access Gtk_Text_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      Arg1 : Gint := To_Gint (Params, 1);
      Arg2 : Gint := To_Gint (Params, 2);
   begin
      null;
   end On_View_Move_Cursor;

   ----------------------------------
   -- On_View_Button_Release_Event --
   ----------------------------------

   function On_View_Button_Release_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean
   is
      Arg1 : Gdk_Event := To_Event (Params, 1);
   begin
      return False;
   end On_View_Button_Release_Event;

   --------------------------------
   -- On_View_Button_Press_Event --
   --------------------------------

   function On_View_Button_Press_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean
   is
      Arg1 : Gdk_Event := To_Event (Params, 1);
   begin
      return False;
   end On_View_Button_Press_Event;

   ----------------------
   -- On_Reset_Clicked --
   ----------------------

   procedure On_Reset_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
   begin
      null;
   end On_Reset_Clicked;

   -----------------------
   -- On_Submit_Clicked --
   -----------------------

   procedure On_Submit_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
   begin
      null;
   end On_Submit_Clicked;

   -----------------------
   -- On_Cancel_Clicked --
   -----------------------

   procedure On_Cancel_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
   begin
      null;
   end On_Cancel_Clicked;

end Memory_View_Pkg.Callbacks;
