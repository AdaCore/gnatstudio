with System; use System;
with Glib; use Glib;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Frame; use Gtk.Frame;
with Gtk.Notebook; use Gtk.Notebook;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Main; use Gtk.Main;
with Gtkada.Code_Editors; use Gtkada.Code_Editors;
with Odd_Intl; use Odd_Intl;
with Odd.Pixmaps; use Odd.Pixmaps;
with Odd.Process; use Odd.Process;
with Debugger; use Debugger;

package body Task_Dialog_Pkg.Callbacks is

   use Odd;
   use Gtk.Arguments;

   -----------------------------
   -- On_Task_List_Select_Row --
   -----------------------------

   procedure On_Task_List_Select_Row
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      --  Since lists start at 0, increment the value.
      Thread   : Gint := To_Gint (Params, 1) + 1;
      Frame    : Gtk_Frame;
      Editor   : Code_Editor;

      --  Get the process notebook from the main window which is associated
      --  with the task dialog (toplevel (object)).

      Notebook : Gtk_Notebook := Task_Dialog_Access
        (Get_Toplevel (Object)).Main_Window.Process_Notebook;

      --  Get the current page in the process notebook.

      Process  : Debugger_Process_Tab :=
        Process_User_Data.Get (Get_Nth_Page
          (Notebook, Get_Current_Page (Notebook)));
      Label    : Gtk_Label;

   begin
      Gtk_New (Frame);
      Gtk_New (Label, -"Thread" & Gint'Image (Thread));
      Append_Page (Process.Thread_Notebook, Frame, Label);
      Gtk_New_Hbox (Editor, False, 0);
      Add (Frame, Editor);
      Show_All (Frame);
      Set_Page (Process.Thread_Notebook, -1);
      Configure
        (Editor, Editor_Font, Editor_Font_Size,
         dot_xpm, arrow_xpm,
         Comments_Color    => Comments_Color,
         Strings_Color     => Strings_Color,
         Keywords_Color    => Keywords_Color,
         Show_Line_Numbers => Editor_Show_Line_Nums);
      Thread_Switch (Process.Debugger, Natural (Thread));
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
