with System; use System;
with Glib; use Glib;
with Gdk.Event; use Gdk.Event;
with Gdk.Types; use Gdk.Types;
with Gtk.Accel_Group; use Gtk.Accel_Group;
with Gtk.Object; use Gtk.Object;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Style; use Gtk.Style;
with Gtk.Widget; use Gtk.Widget;

package body Make_Suite_Window_Pkg.Callbacks is

   use Gtk.Arguments;

   ---------------------------------------
   -- On_Make_Suite_Window_Delete_Event --
   ---------------------------------------

   function On_Make_Suite_Window_Delete_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean
   is
      Arg1 : Gdk_Event := To_Event (Params, 1);
   begin
      return False;
   end On_Make_Suite_Window_Delete_Event;

   --------------------
   -- On_Add_Clicked --
   --------------------

   procedure On_Add_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
   begin
      null;
   end On_Add_Clicked;

   -----------------------
   -- On_Remove_Clicked --
   -----------------------

   procedure On_Remove_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
   begin
      null;
   end On_Remove_Clicked;

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

   ---------------------
   -- On_Help_Clicked --
   ---------------------

   procedure On_Help_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
   begin
      null;
   end On_Help_Clicked;

end Make_Suite_Window_Pkg.Callbacks;
