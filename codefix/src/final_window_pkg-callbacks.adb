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

with Codefix.Errors_Manager; use Codefix.Errors_Manager;

package body Final_Window_Pkg.Callbacks is

   use Gtk.Arguments;

   ---------------------------------
   -- On_Final_Validation_Clicked --
   ---------------------------------

   procedure On_Final_Validation_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
      Final_Window : Final_Window_Access := Final_Window_Access (Object);
      Success      : Boolean;
   begin
      Update
        (Final_Window.Graphic_Codefix.Corrector,
         Success,
         Final_Window.Graphic_Codefix.Current_Text,
         null);

      Gtk.Main.Main_Quit;
   end On_Final_Validation_Clicked;

   -----------------------------
   -- On_Final_Cancel_Clicked --
   -----------------------------

   procedure On_Final_Cancel_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
   begin
      Gtk.Main.Main_Quit;
   end On_Final_Cancel_Clicked;

end Final_Window_Pkg.Callbacks;
