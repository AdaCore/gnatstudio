-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                      Copyright (C) 2000-2001                      --
--                              ACT-Europe                           --
--                                                                   --
-- GVD is free  software;  you can redistribute it and/or modify  it --
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
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Unchecked_Conversion;

with Glib;                  use Glib;
with Gdk.Types;             use Gdk.Types;
with Gtk.Widget;            use Gtk.Widget;
with Gtk.Handlers;          use Gtk.Handlers;
with Gtk.Editable;          use Gtk.Editable;
with Gtk.Notebook;          use Gtk.Notebook;
with Gdk.Types.Keysyms;     use Gdk.Types.Keysyms;
with Gdk.Event;             use Gdk.Event;
with Gdk.Color;             use Gdk.Color;
with Gtk.Menu;              use Gtk.Menu;

with GVD.Preferences;       use GVD.Preferences;
with GVD.Process;           use GVD.Process;
with Main_Debug_Window_Pkg; use Main_Debug_Window_Pkg;
with Debugger;              use Debugger;
with Process_Proxies;       use Process_Proxies;
with Basic_Types;           use Basic_Types;
with GVD.Types;             use GVD.Types;
with String_Utils;          use String_Utils;

package body Process_Tab_Pkg.Callbacks is

   use Gtk.Arguments;
   use String_History;

   procedure Move_Until_Match
     (History : in out History_List;
      S : in String;
      D : in Direction;
      Index : out Integer;
      Found : out Boolean);
   --  Scan the history to find an entry which begins like S.
   --  Index indicates the number of characters found beyond that pattern.

   ---------------------------------
   -- On_Process_Tab_Delete_Event --
   ---------------------------------

   function On_Process_Tab_Delete_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean
   is
   begin
      Hide (Get_Toplevel (Object));
      return True;
   end On_Process_Tab_Delete_Event;

   ------------------------------
   -- On_Stack_List_Select_Row --
   ------------------------------

   procedure On_Stack_List_Select_Row
     (Object : access Gtk_Clist_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      Frame     : Gint := To_Gint (Params, 1) + 1;
      Top_Level : constant Gtk_Window := Gtk_Window (Get_Toplevel (Object));
      Process   : Debugger_Process_Tab;
      Notebook  : Gtk_Notebook;
      Box       : Gtk_Box;
      Page      : Gint;

   begin
      if Get_Pref (Separate_Data) then
         Box      := Process_Tab_Access (Top_Level).Process_Hbox;
         Notebook := Gtk_Notebook (Get_Parent (Box));
         Page     := Page_Num (Notebook, Box);
      else
         Notebook := Main_Debug_Window_Access (Top_Level).Process_Notebook;
         Page     := Get_Current_Page (Notebook);
      end if;

      Process := Process_User_Data.Get (Get_Nth_Page (Notebook, Page));
      Stack_Frame (Process.Debugger, Positive (Frame), GVD.Types.Visible);
   end On_Stack_List_Select_Row;

   --------------------------------------
   -- On_Stack_List_Button_Press_Event --
   --------------------------------------

   function On_Stack_List_Button_Press_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean
   is
      Arg1    : Gdk_Event := To_Event (Params, 1);
      Process : constant Debugger_Process_Tab :=
        Debugger_Process_Tab (Object);

   begin
      if Get_Button (Arg1) = 3
        and then Get_Event_Type (Arg1) = Button_Press
      then
         Popup (Call_Stack_Contextual_Menu (Process),
                Button        => Gdk.Event.Get_Button (Arg1),
                Activate_Time => Gdk.Event.Get_Time (Arg1));
         Emit_Stop_By_Name (Process.Stack_List, "button_press_event");
         return True;
      end if;
      return False;
   end On_Stack_List_Button_Press_Event;

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

      Top  : constant Debugger_Process_Tab := Debugger_Process_Tab (Object);

      type Guint_Ptr is access all Guint;
      function To_Guint_Ptr is new
        Ada.Unchecked_Conversion (Address, Guint_Ptr);

      use GVD.Process;

   begin
      if To_Guint_Ptr (Position).all < Top.Edit_Pos then
         --  Move the cursor back to the end of the window, so that the text
         --  is correctly inserted. This is more user friendly that simply
         --  forbidding any key.

         if Is_Graphic (Arg1 (Arg1'First)) then
            Output_Text
              (Top, Arg1 (Arg1'First .. Arg1'First + Integer (Arg2) - 1),
               Is_Command => True);
            Set_Position
              (Top.Debugger_Text, Gint (Get_Length (Top.Debugger_Text)));
         end if;

         Emit_Stop_By_Name (Top.Debugger_Text, "insert_text");

      else
         if Arg1 (Arg1'First) = ASCII.LF then
            declare
               S : constant String :=
                 Get_Chars (Top.Debugger_Text, Gint (Top.Edit_Pos));
            begin
               --  If the command is empty, then we simply reexecute the last
               --  user command. Note that, with gdb, we can't simply send
               --  LF, since some internal commands might have been executed
               --  in the middle.

               Wind (Top.Window.Command_History, Forward);

               if S'Length = 0 then
                  begin
                     Find_Match (Top.Window.Command_History,
                                 Natural (Get_Num (Top)),
                                 Backward);
                     Process_User_Command
                       (Top, Get_Current
                        (Top.Window.Command_History).Command.all,
                        Output_Command => True,
                        Mode => User);
                  exception
                     --  No previous command => do nothing
                     when No_Such_Item =>
                        null;
                  end;

               else
                  --  Insert the newline character after the user's command.
                  Output_Text (Top, "" & ASCII.LF);

                  --  Process the command.
                  Process_User_Command (Top, S, Mode => User);
               end if;

               --  Move the cursor after the output of the command.
               if Get_Process (Top.Debugger) /= null then
                  Top.Edit_Pos := Get_Length (Top.Debugger_Text);
                  Set_Position (Top.Debugger_Text, Gint (Top.Edit_Pos));
               end if;

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
      Arg1 : Gint := To_Gint (Params, 1);
      Arg2 : Gint := To_Gint (Params, 2);
      Top  : constant Debugger_Process_Tab := Debugger_Process_Tab (Object);

   begin
      if Arg2 <= Gint (Top.Edit_Pos) then
         Emit_Stop_By_Name (Top.Debugger_Text, "delete_text");
      elsif Arg1 < Gint (Top.Edit_Pos) then
         Delete_Text (Top.Debugger_Text, Gint (Top.Edit_Pos), Arg2);
      end if;
   end On_Debugger_Text_Delete_Text;

   ----------------------
   -- Move_Until_Match --
   ----------------------

   procedure Move_Until_Match
     (History : in out History_List;
      S       : in String;
      D       : in Direction;
      Index   : out Integer;
      Found   : out Boolean)
   is
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
            Data : constant String := Get_Current (History).Command.all;
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

         Index := Get_Current (History).Command.all'Length - S'Length;
   end Move_Until_Match;

   --------------------------------------
   -- On_Debugger_Text_Key_Press_Event --
   --------------------------------------

   function On_Debugger_Text_Key_Press_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean
   is
      Arg1  : Gdk_Event := To_Event (Params, 1);
      Top   : Debugger_Process_Tab := Debugger_Process_Tab (Object);
      use type Gdk.Types.Gdk_Key_Type;

      procedure Output (Text : String);
      --  Insert Text in using Top.Debugger_Text and Text_Font

      procedure Output (Text : String) is
      begin
         Insert
           (Top.Debugger_Text, Top.Debugger_Text_Font,
            Black (Get_System), Null_Color, Text);
      end Output;

   begin
      case Get_Key_Val (Arg1) is
         when GDK_Up | GDK_Down =>
            Emit_Stop_By_Name (Top.Debugger_Text, "key_press_event");

            declare
               D : Direction;
            begin
               if Get_Key_Val (Arg1) = GDK_Up then
                  D := Backward;
               else
                  D := Forward;
               end if;

               Find_Match
                 (Top.Window.Command_History, Integer (Get_Num (Top)), D);
               Delete_Text
                 (Top.Debugger_Text,
                  Gint (Top.Edit_Pos),
                  Gint (Get_Length (Top.Debugger_Text)));
               Output_Text
                 (Top, Get_Current (Top.Window.Command_History).Command.all,
                  Is_Command => True);
               Set_Position
                 (Gtk_Editable (Top.Debugger_Text),
                  Gint (Get_Length (Top.Debugger_Text)));

            exception
               when No_Such_Item =>
                  if D = Forward then
                     Delete_Text
                       (Top.Debugger_Text,
                        Gint (Top.Edit_Pos),
                        Gint (Get_Length (Top.Debugger_Text)));
                  end if;

                  return True;
            end;

         when GDK_Tab =>
            Emit_Stop_By_Name (Top.Debugger_Text, "key_press_event");

            declare
               C      : constant String :=
                 Get_Chars (Top.Debugger_Text, Gint (Top.Edit_Pos));
               S      : String_Array := Complete (Top.Debugger, C);
               Max    : Integer := 0;
               Min    : Integer := 0;
               Prefix : Integer;
               Width  : constant Integer := 100;
               --  Width of the console window, in number of characters;
               Num   : Integer;

            begin
               if S'First > S'Last then
                  null;
               elsif S'First = S'Last then
                  declare
                     New_Command : constant String := S (S'First).all;
                     Dummy       : Boolean;
                  begin
                     Dummy :=
                       Backward_Delete (Top.Debugger_Text, Guint (C'Length));
                     Output_Text (Top, New_Command & " ", Is_Command => True);
                     Set_Position
                       (Top.Debugger_Text,
                        Get_Position (Top.Debugger_Text) +
                          New_Command'Length + 1);
                  end;

               else
                  --  Find the lengths of the longest and shortest
                  --  words in the list;

                  Min := S (S'First)'Length;

                  for J in S'Range loop
                     if S (J)'Length > Max then
                        Max := S (J)'Length;
                     end if;

                     if S (J)'Length < Min then
                        Min := S (J)'Length;
                     end if;
                  end loop;

                  --  Compute number of words to display per line.
                  Num := Width / (Max + 2);

                  --  Find the common prefix in all the words.
                  Prefix := 0;

                  declare
                     Prefix_Found : Boolean := True;
                     J            : Integer;
                  begin
                     while Prefix <= Min and then Prefix_Found loop
                        Prefix := Prefix + 1;
                        J := S'First;

                        while J <= S'Last and then Prefix_Found loop
                           if S (J) (S (J)'First + Prefix - 1)
                             = S (S'First) (S (S'First)'First + Prefix - 1)
                           then
                              J := J + 1;
                           else
                              Prefix := Prefix - 1;
                              Prefix_Found := False;
                           end if;
                        end loop;
                     end loop;
                  end;

                  --  Print the list of possibilities.
                  Freeze (Top.Debugger_Text);
                  Output ((1 => ASCII.LF));

                  for J in S'Range loop
                     if (J mod Num) = 0 then
                        Output ((1 => ASCII.LF));
                     end if;

                     Output (S (J).all);

                     for K in S (J)'Length .. Max + 2 loop
                        Output (" ");
                     end loop;
                  end loop;

                  Output ((1 => ASCII.LF));
                  Thaw (Top.Debugger_Text);

                  --  Display the prompt and the common prefix.
                  Display_Prompt (Top.Debugger);

                  declare
                     Common_Pref : Integer := C'Last;
                  begin
                     Skip_To_Char (C, Common_Pref, ' ', -1);
                     Common_Pref := Common_Pref - C'First;
                     Output_Text
                       (Top,
                        C (C'First .. C'First + Common_Pref),
                        Is_Command => True);
                     Set_Position
                       (Top.Debugger_Text,
                        Get_Position (Top.Debugger_Text) +
                          Gint (Common_Pref) + 1);
                  end;

                  Output_Text
                    (Top,
                     S (S'First)
                       (S (S'First)'First .. S (S'First)'First + Prefix - 1),
                     Is_Command => True);
                  Set_Position
                    (Top.Debugger_Text,
                     Get_Position (Top.Debugger_Text) + Gint (Prefix));
               end if;

               Free (S);
            end;

         when others =>
            null;
      end case;

      return True;
   end On_Debugger_Text_Key_Press_Event;

   ---------------------------------
   -- On_Debugger_Text_Grab_Focus --
   ---------------------------------

   procedure On_Debugger_Text_Grab_Focus
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      Wind (Debugger_Process_Tab (Object).Window.Command_History, Forward);
   end On_Debugger_Text_Grab_Focus;

end Process_Tab_Pkg.Callbacks;
