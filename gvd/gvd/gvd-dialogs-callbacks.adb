with System; use System;
with Glib; use Glib;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Main; use Gtk.Main;
with GNAT.IO; use GNAT.IO;

package body Task_Dialog_Pkg.Callbacks is

   use Gtk.Arguments;

   -----------------------------
   -- On_Task_List_Select_Row --
   -----------------------------

   procedure On_Task_List_Select_Row
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      Arg1 : Gint := To_Gint (Params, 1);
      Arg2 : Gint := To_Gint (Params, 2);
   begin
      Put_Line ("Selected Row is" & Gint'Image (Arg1));
      Put_Line ("Selected Column is" & Gint'Image (Arg2));
   end On_Task_List_Select_Row;

   -----------------------------
   -- On_Close_Button_Clicked --
   -----------------------------

   procedure On_Close_Button_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
   begin
      Hide (Get_Toplevel (Object));
   end On_Close_Button_Clicked;

end Task_Dialog_Pkg.Callbacks;
