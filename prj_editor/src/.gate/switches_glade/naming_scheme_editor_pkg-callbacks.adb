with System; use System;
with Glib; use Glib;
with Gdk.Event; use Gdk.Event;
with Gdk.Types; use Gdk.Types;
with Gtk.Accel_Group; use Gtk.Accel_Group;
with Gtk.Object; use Gtk.Object;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Style; use Gtk.Style;
with Gtk.Widget; use Gtk.Widget;

package body Naming_Scheme_Editor_Pkg.Callbacks is

   use Gtk.Arguments;

   --------------------------------
   -- On_Standard_Scheme_Changed --
   --------------------------------

   procedure On_Standard_Scheme_Changed
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      null;
   end On_Standard_Scheme_Changed;

   ----------------
   -- Customized --
   ----------------

   procedure Customized
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      null;
   end Customized;

   -----------------------------------
   -- On_Exceptions_List_Select_Row --
   -----------------------------------

   procedure On_Exceptions_List_Select_Row
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      Arg1 : Gint := To_Gint (Params, 1);
      Arg2 : Gint := To_Gint (Params, 2);
      Arg3 : Gdk_Event := To_Event (Params, 3);
   begin
      null;
   end On_Exceptions_List_Select_Row;

   ---------------------------------------
   -- On_Exception_List_Key_Press_Event --
   ---------------------------------------

   function On_Exception_List_Key_Press_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean
   is
      Arg1 : Gdk_Event := To_Event (Params, 1);
   begin
      return False;
   end On_Exception_List_Key_Press_Event;

   -----------------------
   -- On_Update_Clicked --
   -----------------------

   procedure On_Update_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      null;
   end On_Update_Clicked;

   ----------------------------------------
   -- On_Unit_Name_Entry_Key_Press_Event --
   ----------------------------------------

   function On_Unit_Name_Entry_Key_Press_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean
   is
      Arg1 : Gdk_Event := To_Event (Params, 1);
   begin
      return False;
   end On_Unit_Name_Entry_Key_Press_Event;

   --------------------------------------------
   -- On_Spec_Filename_Entry_Key_Press_Event --
   --------------------------------------------

   function On_Spec_Filename_Entry_Key_Press_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean
   is
      Arg1 : Gdk_Event := To_Event (Params, 1);
   begin
      return False;
   end On_Spec_Filename_Entry_Key_Press_Event;

   --------------------------------------------
   -- On_Body_Filename_Entry_Key_Press_Event --
   --------------------------------------------

   function On_Body_Filename_Entry_Key_Press_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean
   is
      Arg1 : Gdk_Event := To_Event (Params, 1);
   begin
      return False;
   end On_Body_Filename_Entry_Key_Press_Event;

end Naming_Scheme_Editor_Pkg.Callbacks;
