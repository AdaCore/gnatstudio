with System; use System;
with Glib; use Glib;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Frame; use Gtk.Frame;
with Gtk.Notebook; use Gtk.Notebook;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Main; use Gtk.Main;
with Odd_Intl; use Odd_Intl;
with Odd.Pixmaps; use Odd.Pixmaps;
with Odd.Process; use Odd.Process;
with Debugger; use Debugger;
with Gtkada.Code_Editors; use Gtkada.Code_Editors;

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
      Thread        : Gint := To_Gint (Params, 1) + 1;
      Frame         : Gtk_Frame;

      --  Get the process notebook from the main window which is associated
      --  with the task dialog (toplevel (object)).

      Notebook      : Gtk_Notebook := Task_Dialog_Access
        (Get_Toplevel (Object)).Main_Window.Process_Notebook;

      --  Get the current page in the process notebook.

      Process       : Debugger_Process_Tab :=
        Process_User_Data.Get (Get_Nth_Page
          (Notebook, Get_Current_Page (Notebook)));
      Label         : Gtk_Label;
      Child         : Page_List.Glist;
      Page          : Gint;
      Notebook_Page : Gtk_Notebook_Page;

      use Page_List;

   begin
      Child := Get_Children (Process.Thread_Notebook);
      Page := 0;

      while Child /= Null_List loop
         Notebook_Page := Get_Data (Child);

         if Get (Gtk_Label (Get_Tab_Label (Notebook_Page))) =
           -"Thread" & Gint'Image (Thread)
         then
            --  The desired thread has already a page associated with it.

            Reparent (Process.Editor_Text, Get_Child (Notebook_Page));
            Set_Page (Process.Thread_Notebook, Page);
            Thread_Switch (Process.Debugger, Natural (Thread));

            return;
         end if;

         Child := Next (Child);
         Page := Page + 1;
      end loop;

      Gtk_New (Frame);
      Gtk_New (Label, -"Thread" & Gint'Image (Thread));
      Append_Page (Process.Thread_Notebook, Frame, Label);
      Reparent (Process.Editor_Text, Frame);
      Show (Frame);
      Set_Page (Process.Thread_Notebook, -1);
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
