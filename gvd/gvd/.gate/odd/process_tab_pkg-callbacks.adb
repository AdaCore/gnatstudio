with System; use System;
with Glib; use Glib;
with Gdk.Event; use Gdk.Event;
with Gdk.Types; use Gdk.Types;
with Gtk.Accel_Group; use Gtk.Accel_Group;
with Gtk.Object; use Gtk.Object;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Style; use Gtk.Style;
with Gtk.Widget; use Gtk.Widget;

package body Process_Tab_Pkg.Callbacks is

   use Gtk.Arguments;

   ---------------------------------
   -- On_Process_Tab_Delete_Event --
   ---------------------------------

   function On_Process_Tab_Delete_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean
   is
      Arg1 : Gdk_Event := To_Event (Params, 1);
   begin
      return False;
   end On_Process_Tab_Delete_Event;

   --------------------------------
   -- On_Data_Paned_Delete_Event --
   --------------------------------

   function On_Data_Paned_Delete_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean
   is
      Arg1 : Gdk_Event := To_Event (Params, 1);
   begin
      return False;
   end On_Data_Paned_Delete_Event;

   ------------------------------
   -- On_Stack_List_Select_Row --
   ------------------------------

   procedure On_Stack_List_Select_Row
     (Object : access Gtk_Clist_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      Arg1 : Gint := To_Gint (Params, 1);
      Arg2 : Gint := To_Gint (Params, 2);
      Arg3 : Gdk_Event := To_Event (Params, 3);
   begin
      null;
   end On_Stack_List_Select_Row;

   --------------------------------------
   -- On_Stack_List_Button_Press_Event --
   --------------------------------------

   function On_Stack_List_Button_Press_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean
   is
      Arg1 : Gdk_Event := To_Event (Params, 1);
   begin
      return False;
   end On_Stack_List_Button_Press_Event;

   ---------------------------------
   -- On_Editor_Text_Delete_Event --
   ---------------------------------

   function On_Editor_Text_Delete_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean
   is
      Arg1 : Gdk_Event := To_Event (Params, 1);
   begin
      return False;
   end On_Editor_Text_Delete_Event;

   --------------------------------------------
   -- On_Command_Scrolledwindow_Delete_Event --
   --------------------------------------------

   function On_Command_Scrolledwindow_Delete_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean
   is
      Arg1 : Gdk_Event := To_Event (Params, 1);
   begin
      return False;
   end On_Command_Scrolledwindow_Delete_Event;

   ----------------------------------
   -- On_Debugger_Text_Insert_Text --
   ----------------------------------

   procedure On_Debugger_Text_Insert_Text
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      Arg1 : String := To_String (Params, 1);
      Arg2 : Gint := To_Gint (Params, 2);
      Arg3 : Address := To_Address (Params, 3);
   begin
      null;
   end On_Debugger_Text_Insert_Text;

   ----------------------------------
   -- On_Debugger_Text_Delete_Text --
   ----------------------------------

   procedure On_Debugger_Text_Delete_Text
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      Arg1 : Gint := To_Gint (Params, 1);
      Arg2 : Gint := To_Gint (Params, 2);
   begin
      null;
   end On_Debugger_Text_Delete_Text;

   --------------------------------------
   -- On_Debugger_Text_Key_Press_Event --
   --------------------------------------

   function On_Debugger_Text_Key_Press_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean
   is
      Arg1 : Gdk_Event := To_Event (Params, 1);
   begin
      return False;
   end On_Debugger_Text_Key_Press_Event;

   ---------------------------------
   -- On_Debugger_Text_Grab_Focus --
   ---------------------------------

   procedure On_Debugger_Text_Grab_Focus
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      null;
   end On_Debugger_Text_Grab_Focus;

end Process_Tab_Pkg.Callbacks;
