-----------------------------------------------------------------------
--                 Odd - The Other Display Debugger                  --
--                                                                   --
--                         Copyright (C) 2000                        --
--                 Emmanuel Briot and Arnaud Charlet                 --
--                                                                   --
-- Odd is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with System; use System;
with Glib; use Glib;
with Gtk.Label; use Gtk.Label;
with Gtk.Frame; use Gtk.Frame;
with Gtk.Notebook; use Gtk.Notebook;
with Gtk.Widget; use Gtk.Widget;
with Odd_Intl; use Odd_Intl;
with Odd.Process; use Odd.Process;
with Debugger; use Debugger;
with Odd.Code_Editors; use Odd.Code_Editors;
with Main_Debug_Window_Pkg; use Main_Debug_Window_Pkg;
with Process_Proxies; use Process_Proxies;

package body Odd.Dialogs.Callbacks is

   use Odd;
   use Gtk.Arguments;

   ----------------------------------
   -- On_Backtrace_List_Select_Row --
   ----------------------------------

   procedure On_Backtrace_List_Select_Row
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      Frame       : Gint         := To_Gint (Params, 1) + 1;

      --  Get the process notebook from the main window which is associated
      --  with the task dialog (toplevel (object)).

      Main_Window : Gtk_Window   :=
        Backtrace_Dialog_Access (Get_Toplevel (Object)).Main_Window;
      Notebook    : Gtk_Notebook :=
        Main_Debug_Window_Access (Main_Window).Process_Notebook;

      --  Get the current page in the process notebook.

      Process     : Debugger_Process_Tab :=
        Process_User_Data.Get (Get_Nth_Page
          (Notebook, Get_Current_Page (Notebook)));

   begin
      Stack_Frame (Process.Debugger, Positive (Frame), True);
   end On_Backtrace_List_Select_Row;

   ----------------------------------
   -- On_Backtrace_Process_Stopped --
   ----------------------------------

   procedure On_Backtrace_Process_Stopped
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      Bt       : Backtrace_Array (1 .. Max_Frame);
      Len      : Natural;
      Dialog   : Backtrace_Dialog_Access :=
        Debugger_Process_Tab (Object).Window.Backtrace_Dialog;
      Process  : Process_Proxy_Access;
      Internal : Boolean;

   begin
      if Visible_Is_Set (Dialog) then
         Process := Get_Process (Debugger_Process_Tab (Object).Debugger);
         Internal := Is_Internal_Command (Process);
         Set_Internal_Command (Process, True);
         Backtrace (Debugger_Process_Tab (Object).Debugger, Bt, Len);
         Set_Internal_Command (Process, Internal);
         Update (Dialog, Bt (1 .. Len));
         Free  (Bt (1 .. Len));
      end if;
   end On_Backtrace_Process_Stopped;

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

      Notebook      : Gtk_Notebook :=
        Main_Debug_Window_Access (Task_Dialog_Access
        (Get_Toplevel (Object)).Main_Window).Process_Notebook;

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
            --  Note that Set_Page will take care of the actual thread
            --  switching.

            Reparent (Process.Editor_Text, Get_Child (Notebook_Page));
            Set_Page (Process.Thread_Notebook, Page);
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
   -- On_Task_Process_Stopped --
   -----------------------------

   procedure On_Task_Process_Stopped
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      Dialog   : Task_Dialog_Access :=
        Debugger_Process_Tab (Object).Window.Task_Dialog;
      Process  : Process_Proxy_Access;
      Internal : Boolean;

   begin
      if Visible_Is_Set (Dialog) then
         Process := Get_Process (Debugger_Process_Tab (Object).Debugger);
         Internal := Is_Internal_Command (Process);
         Set_Internal_Command (Process, True);

         declare
            Info : Thread_Information_Array :=
              Info_Threads (Debugger_Process_Tab (Object).Debugger);
         begin
            Update (Dialog, Info);
            Free (Info);
         end;

         Set_Internal_Command (Process, Internal);
      end if;
   end On_Task_Process_Stopped;

   -----------------------------
   -- On_Close_Button_Clicked --
   -----------------------------

   procedure On_Close_Button_Clicked
     (Object : access Gtk_Button_Record'Class) is
   begin
      Hide (Get_Toplevel (Object));
   end On_Close_Button_Clicked;

   ---------------------------------
   -- On_Question_List_Select_Row --
   ---------------------------------

   procedure On_Question_List_Select_Row
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      Row         : Gint := To_Gint (Params, 1);
      Dialog      : Question_Dialog_Access :=
        Question_Dialog_Access (Get_Toplevel (Object));
   begin
      Send (Dialog.Debugger,
            Get_Text (Dialog.List, Row, 0),
            Display => True,
            Empty_Buffer => False,
            Wait_For_Prompt => False);

      --  This dialog is destroyed, not simply hidden, since it has to
      --  be recreated from scratch every time anyway.
      Destroy (Dialog);
   end On_Question_List_Select_Row;

   -------------------------------
   -- On_Question_Close_Clicked --
   -------------------------------

   procedure On_Question_Close_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
      Dialog      : Question_Dialog_Access :=
        Question_Dialog_Access (Get_Toplevel (Object));
   begin
      --  Send the interrupt signal to the debugger, so that it does not keep
      --  waiting for user input.
      Interrupt (Dialog.Debugger);

      --  Destroy the dialog, since we will have to recreate it anyway.
      Destroy (Dialog);
   end On_Question_Close_Clicked;

end Odd.Dialogs.Callbacks;
