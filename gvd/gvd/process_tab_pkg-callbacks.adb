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
with Gtk.Widget; use Gtk.Widget;
with Gtk.Handlers; use Gtk.Handlers;
with Unchecked_Conversion;
with Odd.Process; use Odd.Process;
with Gdk.Types.Keysyms;  use Gdk.Types.Keysyms;
with Gdk.Event;   use Gdk.Event;
with Odd_Intl; use Odd_Intl;
with Main_Debug_Window_Pkg; use Main_Debug_Window_Pkg;
with Debugger; use Debugger;

with Ada.Tags; use Ada.Tags;
with Ada.Text_IO; use Ada.Text_IO;

package body Process_Tab_Pkg.Callbacks is

   use Gtk.Arguments;
   use String_History;

   ------------------------------------
   -- On_Thread_Notebook_Switch_Page --
   ------------------------------------

   procedure On_Thread_Notebook_Switch_Page
     (Object : access Gtk_Notebook_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      Arg1 : Address := To_Address (Params, 1);
      Arg2 : Guint := To_Guint (Params, 2);

      function To_Notebook_Page is new
        Unchecked_Conversion (Address, Gtk_Notebook_Page);

      Notebook      : Gtk_Notebook;
      Process       : Debugger_Process_Tab;
      Notebook_Page : constant Gtk_Notebook_Page := To_Notebook_Page (Arg1);
      Thread        : Natural;
      Widget        : Gtk_Widget := Get_Toplevel (Object);

   begin
      if Widget'Tag /= Main_Debug_Window_Record'Tag then
         --  This means that we are still creating the notebook.
         return;
      end if;

      Notebook := Main_Debug_Window_Access (Widget).Process_Notebook;
      Process  := Process_User_Data.Get (Get_Nth_Page
        (Notebook, Get_Current_Page (Notebook)));

      if Arg2 = 0 then
         --  Need to find the current thread ???
         return;
      end if;

      declare
         Label : constant String :=
           Get (Gtk_Label (Get_Tab_Label (Notebook_Page)));
         Thread_String : constant String := -"Thread";

      begin
         Thread := Natural'Value
           (Label (Label'First + Thread_String'Length .. Label'Last));
      end;

      Reparent (Process.Editor_Text, Get_Child (Notebook_Page));
      Handler_Block (Object, Process.Notebook_Handler_Id);
      Set_Page (Process.Thread_Notebook, Gint (Arg2));
      Thread_Switch (Process.Debugger, Thread);
      Handler_Unblock (Object, Process.Notebook_Handler_Id);
   end On_Thread_Notebook_Switch_Page;

   ----------------------------------
   -- On_Debugger_Text_Insert_Text --
   ----------------------------------

   procedure On_Debugger_Text_Insert_Text
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      Arg1 : String := To_String (Params, 1);
      --  Arg2 : Gint := To_Gint (Params, 2);
      Position : Address := To_Address (Params, 3);

      Top  : Debugger_Process_Tab := Debugger_Process_Tab (Object);

      type Guint_Ptr is access all Guint;
      function To_Guint_Ptr is new Unchecked_Conversion (Address, Guint_Ptr);
      use Odd.Process;

   begin
      if To_Guint_Ptr (Position).all < Top.Edit_Pos then
         Emit_Stop_By_Name (Top.Debugger_Text, "insert_text");
      else
         if Arg1 (Arg1'First) = ASCII.LF then
            declare
               S : String :=
                 Get_Chars (Top.Debugger_Text, Gint (Top.Edit_Pos));
            begin
               --  If the command is empty, then we simply reexecute the last
               --  user command. Note that, with gdb, we can't simply send
               --  LF, since some internal commands might have been executed
               --  in the middle.

               if S'Length = 0 then
                  begin
                     Move_To_Previous (Top.Command_History);
                     Text_Output_Handler
                       (Top, Get_Current (Top.Command_History) & ASCII.LF,
                        Is_Command => True);
                     Process_User_Command
                       (Top, Get_Current (Top.Command_History));
                  exception
                     --  No previous command => do nothing
                     when No_Such_Item =>
                        null;
                  end;

               else
                  --  Insert the newline character after the user's command.
                  Text_Output_Handler (Top, "" & ASCII.LF);

                  --  Process the command.
                  Process_User_Command (Top, S);
               end if;

               --  Move the cursor after the output of the command.
               Top.Edit_Pos := Get_Length (Top.Debugger_Text);
               Set_Position (Top.Debugger_Text, Gint (Top.Edit_Pos));

               --  Stop propagating this event.
               Emit_Stop_By_Name (Top.Debugger_Text, "insert_text");
            end;
         end if;
      end if;
   end On_Debugger_Text_Insert_Text;

  ----------------------------------
   -- On_Debugger_Text_Delete_Text --
   ----------------------------------

   procedure On_Debugger_Text_Delete_Text
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      --  Arg1 : Gint := To_Gint (Params, 1);
      Arg2 : Gint := To_Gint (Params, 2);

      Top  : Debugger_Process_Tab := Debugger_Process_Tab (Object);

   begin
      if Arg2 <= Gint (Top.Edit_Pos) then
         Emit_Stop_By_Name (Top.Debugger_Text, "delete_text");
      end if;
   end On_Debugger_Text_Delete_Text;

   --------------------------------------
   -- On_Debugger_Text_Key_Press_Event --
   --------------------------------------

   function On_Debugger_Text_Key_Press_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean
   is
      Arg1 : Gdk_Event := To_Event (Params, 1);

      Top  : Debugger_Process_Tab := Debugger_Process_Tab (Object);
      use type Gdk.Types.Gdk_Key_Type;

   begin
      if Get_Key_Val (Arg1) = Gdk_Up
        or else Get_Key_Val (Arg1) = Gdk_Down
      then
         Delete_Text
           (Top.Debugger_Text,
            Gint (Top.Edit_Pos),
            Gint (Get_Length (Top.Debugger_Text)));
         begin
            if Get_Key_Val (Arg1) = Gdk_Up then
               Move_To_Previous (Top.Command_History);
            else
               Move_To_Next (Top.Command_History);
            end if;

            Set_Point (Top.Debugger_Text, Top.Edit_Pos);
            Text_Output_Handler
              (Top, Get_Current (Top.Command_History), Is_Command => True);
            Set_Position
              (Top.Debugger_Text, Gint (Get_Length (Top.Debugger_Text)));
         exception
            when No_Such_Item =>
               null;
         end;
         Emit_Stop_By_Name (Top.Debugger_Text, "key_press_event");
      end if;

      return True;
   end On_Debugger_Text_Key_Press_Event;

end Process_Tab_Pkg.Callbacks;
