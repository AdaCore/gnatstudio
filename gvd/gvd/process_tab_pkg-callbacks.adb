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
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Tags; use Ada.Tags;
with Ada.Text_IO; use Ada.Text_IO;

package body Process_Tab_Pkg.Callbacks is

   use Gtk.Arguments;
   use String_History;

   procedure Move_Until_Match ( History : in out History_List;
                                S : in String;
                                D : in Direction;
                                Index : out Integer;
                                Found : out Boolean);
   --  Scan the history to find an entry which begins like S.
   --  Index indicates the number of characters found beyond that pattern.

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
      Thread_Switch (Process.Debugger, Thread, Display => True);
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
      Arg2 : Gint := To_Gint (Params, 2);
      Position : Address := To_Address (Params, 3);

      Top  : Debugger_Process_Tab := Debugger_Process_Tab (Object);

      type Guint_Ptr is access all Guint;
      function To_Guint_Ptr is new Unchecked_Conversion (Address, Guint_Ptr);
      use Odd.Process;

   begin

      Wind (Top.Command_History, Forward);

      if To_Guint_Ptr (Position).all < Top.Edit_Pos then

         --  Move the cursor back to the end of the window, so that the text
         --  is correctly inserted. This is more user friendly that simply
         --  forbidding any key.

         if Is_Graphic (Arg1 (Arg1'First)) then
            Text_Output_Handler
              (Top, Arg1 (Arg1'First .. Arg1'First + Integer (Arg2) - 1),
               Is_Command => True);
            Set_Position
              (Top.Debugger_Text, Gint (Get_Length (Top.Debugger_Text)));
         end if;

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
                     Process_User_Command
                       (Top, Get_Current (Top.Command_History),
                        Output_Command => True);
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


   ----------------------
   -- Move_Until_Match --
   ----------------------

   procedure Move_Until_Match ( History : in out History_List;
                                S : in String;
                                D : in Direction;
                                Index : out Integer;
                                Found : out Boolean) is
      Counter : Integer := 0;
   begin
      Found := False;
      loop
         if D = Forward then
            Move_To_Next (History);
         else
            Move_To_Previous (History);
         end if;

         declare
            Data : constant String := Get_Current (History);
         begin
            if S'Length <= Data'Length
              and then S = Data (Data'First .. Data'First + S'Length - 1)
            then
               Found := True;
               Index := Data'Length - S'Length;
               return;
            end if;
            Counter := Counter + 1;
         end;
      end loop;

   exception
      when No_Such_Item =>
         for J in 1 .. Counter loop
            if D = Forward then
               Move_To_Previous (History);
            else
               Move_To_Next (History);
            end if;
         end loop;

         Index := Get_Current (History)'Length - S'Length;
   end Move_Until_Match;

   --------------------------------------
   -- On_Debugger_Text_Key_Press_Event --
   --------------------------------------

   function On_Debugger_Text_Key_Press_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean
   is
      Index : Integer;
      Found : Boolean;
      D : Direction;
      Arg1  : Gdk_Event := To_Event (Params, 1);
      Top   : Debugger_Process_Tab := Debugger_Process_Tab (Object);
      use type Gdk.Types.Gdk_Key_Type;

   begin
      if Get_Key_Val (Arg1) = GDK_Up
        or else Get_Key_Val (Arg1) = GDK_Down
      then
         if Get_Key_Val (Arg1) = GDK_Up then
            D := Backward;
         else
            D := Forward;
         end if;
         if Get_Has_Selection (Top.Debugger_Text) then
            Move_Until_Match
              (Top.Command_History,
               Get_Chars
               (Top.Debugger_Text,
                Gint (Top.Edit_Pos),
                Gint (Get_Selection_Start_Pos (Top.Debugger_Text))),
               D, Index, Found);
         else
            Move_Until_Match
              (Top.Command_History,
               Get_Chars (Top.Debugger_Text,
                          Gint (Top.Edit_Pos),
                          Get_Position (Top.Debugger_Text)),
               D, Index, Found);
         end if;
         if Found then
            Delete_Text
              (Top.Debugger_Text,
               Gint (Top.Edit_Pos),
               Gint (Get_Length (Top.Debugger_Text)));

            Set_Point (Top.Debugger_Text, Top.Edit_Pos);
            Text_Output_Handler
              (Top, Get_Current (Top.Command_History), Is_Command => True);
            Select_Region
              (Top.Debugger_Text,
               Gint (Get_Length (Top.Debugger_Text) - Guint (Index) ));
         else
            Delete_Text
              (Top.Debugger_Text,
               Gint (Get_Selection_Start_Pos (Top.Debugger_Text)),
               Gint (Get_Selection_End_Pos (Top.Debugger_Text)));
         end if;
         Emit_Stop_By_Name (Top.Debugger_Text, "key_press_event");
      end if;
      return True;
   exception
      when No_Such_Item => return False;
   end On_Debugger_Text_Key_Press_Event;

end Process_Tab_Pkg.Callbacks;
