with Ada.Text_IO; use Ada.Text_IO;

with System; use System;
with Glib; use Glib;
with Gdk.Event; use Gdk.Event;
with Gdk.Types; use Gdk.Types;
with Gtk.Accel_Group; use Gtk.Accel_Group;
with Gtk.Object; use Gtk.Object;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Style; use Gtk.Style;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Label; use Gtk.Label;
with Gtk.Text; use Gtk.Text;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Enums; use Gtk.Enums; use Gtk.Enums.String_List;
with Gtk.Combo; use Gtk.Combo;
with Gtk.List; use Gtk.List;
with Gtk.Main; use Gtk.Main;

with Codefix; use Codefix;
with Codefix.Text_Manager; use Codefix.Text_Manager;
with Codefix.Errors_Manager; use Codefix.Errors_Manager;
with Codefix.Errors_Parser; use Codefix.Errors_Parser;
with Codefix.Formal_Errors; use Codefix.Formal_Errors;
with Codefix.File_Io; use Codefix.File_Io;
with Codefix.Text_Navigators;
use Codefix.Formal_Errors.Extract_List;

with Codefix.Graphic_Codefix_Pkg; use Codefix.Graphic_Codefix_Pkg;
with Gen_Proposition_Pkg; use Gen_Proposition_Pkg;

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
      Gtk.Main.Main_Quit;
      return False;
   end On_Codefix_Window_Delete_Event;

   --------------------------
   -- On_Fix_Entry_Changed --
   --------------------------

   procedure On_Fix_Entry_Changed
     (Object : access Gtk_Widget_Record'Class)
   is
      Graphic_Codefix : Graphic_Codefix_Access :=
        Graphic_Codefix_Access (Object);

   begin
      if Get_Text (Graphic_Codefix.Fix_Entry) /= "" then
         Set_Page
           (Graphic_Codefix.Choices_Proposed,
            Get_Nth_Solution (Graphic_Codefix) - 1);
      end if;
   end On_Fix_Entry_Changed;

   --------------------------------
   -- On_Skip_Correction_Clicked --
   --------------------------------

   procedure On_Skip_Correction_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
      Graphic_Codefix : Graphic_Codefix_Access :=
        Graphic_Codefix_Access (Object);
   begin
      Load_Next_Error (Graphic_Codefix);
   end On_Skip_Correction_Clicked;

   ----------------------------------
   -- On_Accept_Correction_Clicked --
   ----------------------------------

   procedure On_Accept_Correction_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
      Graphic_Codefix : Graphic_Codefix_Access :=
        Graphic_Codefix_Access (Object);
   begin
      Validate
        (Graphic_Codefix.Corrector,
         Graphic_Codefix.Current_Error,
         Integer (Get_Current_Page (Graphic_Codefix.Choices_Proposed) + 1));

      Load_Next_Error (Graphic_Codefix);
   end On_Accept_Correction_Clicked;

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
