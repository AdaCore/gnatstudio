-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2003                       --
--                             AdaCore                               --
--                                                                   --
-- GPS is free  software; you can  redistribute it and/or modify  it --
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

--  This package provides the implementation for a general interactive console.

with Gdk.Color;   use Gdk.Color;

with Gtk.Enums;
with Gtk.Main;
with Gtk.Text_Buffer;
with Gtk.Text_View;
with Gtk.Text_Mark;

with Gtk.Text_Tag;
with Gtk.Scrolled_Window;

with GNAT.OS_Lib;
with Basic_Types;       use Basic_Types;
with Histories;
with GUI_Utils;
with System;

with Pango.Font;

package Interactive_Consoles is

   type Interactive_Console_Record is new
     Gtk.Scrolled_Window.Gtk_Scrolled_Window_Record with private;
   type Interactive_Console is
     access all Interactive_Console_Record'Class;

   type Command_Handler is access
     function
       (Console   : access Interactive_Console_Record'Class;
        Input     : String;
        User_Data : System.Address)
        return String;

   type Interrupt_Handler is access function
     (Console   : access Interactive_Console_Record'Class;
      User_Data : System.Address) return Boolean;
   --  Called when ctrl-c is pressed in an interactive console.
   --  Should return true if the process was interrupted, False if nothing was
   --  done.

   procedure Gtk_New
     (Console      : out Interactive_Console;
      Prompt       : String;
      Handler      : Command_Handler;
      User_Data    : System.Address;
      Font         : Pango.Font.Pango_Font_Description;
      History_List : Histories.History;
      Key          : Histories.History_Key;
      Highlight    : Gdk_Color := Null_Color;
      Wrap_Mode    : Gtk.Enums.Gtk_Wrap_Mode := Gtk.Enums.Wrap_None;
      Empty_Equals_Repeat : Boolean := False);
   --  Create a new console.
   --  History_List and Key are used to handle the history of commands entered
   --  by the user in the interactive window. No history is provided if
   --  History_List is null
   --  If Empty_Equals_Repeat is True, an empty command will execute the last
   --  command.

   procedure Initialize
     (Console   : access Interactive_Console_Record'Class;
      Prompt    : String;
      Handler   : Command_Handler;
      User_Data : System.Address;
      Font      : Pango.Font.Pango_Font_Description;
      History_List : Histories.History;
      Key          : Histories.History_Key;
      Highlight    : Gdk_Color := Null_Color;
      Wrap_Mode : Gtk.Enums.Gtk_Wrap_Mode;
      Empty_Equals_Repeat : Boolean := False);
   --  Internal initialization function.

   procedure Insert
     (Console        : access Interactive_Console_Record;
      Text           : String;
      Add_LF         : Boolean := True;
      Highlight      : Boolean := False;
      Add_To_History : Boolean := False);
   --  Suspend the user insertion and insert Text as an information message.
   --  If Add_To_History is True, the inserted text will be inserted in the
   --  user command history.

   function Read
     (Console    : access Interactive_Console_Record;
      Whole_Line : Boolean) return String;
   --  Read the characters currently available in the console.
   --  If Whole_Line is true, then this call is blocking until a newline
   --  character has been read

   procedure Clear (Console : access Interactive_Console_Record);
   --  Clear all the text in the Console.

   procedure Enable_Prompt_Display
     (Console : access Interactive_Console_Record;
      Enable  : Boolean);
   --  When Enable is False, the Prompt is not redisplayed automatically
   --  and the console is uneditable by the user.

   function Is_Editable
     (Console : access Interactive_Console_Record) return Boolean;
   --  Return whether the console is user editable, or read-only.

   procedure Display_Prompt
     (Console : access Interactive_Console_Record'Class);
   --  Displays the prompt at the end of the current text.

   procedure Set_Prompt
     (Console : access Interactive_Console_Record;
      Prompt  : String);
   --  Dynamically change the prompt. This doesn't impact the currently
   --  displayed prompt.

   procedure Set_Command_Handler
     (Console   : access Interactive_Console_Record'Class;
      Handler   : Command_Handler;
      User_Data : System.Address);
   --  Reset the command handler for the console

   procedure Set_Completion_Handler
     (Console : access Interactive_Console_Record'Class;
      Handler : GUI_Utils.Completion_Handler);
   --  Set Handler as the default completion handler for the console.
   --  User_Data passed to Handler is the same one that is passed to the
   --  Command_Handler.

   procedure Set_Interrupt_Handler
     (Console : access Interactive_Console_Record'Class;
      Handler : Interrupt_Handler);
   --  Set Handler to be called when ctrl-c is pressed in the interactive
   --  console.

   procedure Set_Highlight_Color
     (Console : access Interactive_Console_Record'Class;
      Color   : Gdk_Color);
   --  Set the color used for highlighting tags.

   function Get_Chars
     (Console : access Interactive_Console_Record)
      return String;
   --  Return the contents of the console window.

   function Get_History
     (Console : access Interactive_Console_Record)
      return GNAT.OS_Lib.String_List_Access;
   --  Return the command history.

   function Get_View
     (Console : access Interactive_Console_Record)
      return Gtk.Text_View.Gtk_Text_View;
   --  Return the text view.

private

   type Interactive_Console_Record is new
     Gtk.Scrolled_Window.Gtk_Scrolled_Window_Record
   with record
      Handler    : Command_Handler;
      Completion : GUI_Utils.Completion_Handler;
      Interrupt  : Interrupt_Handler;
      User_Data  : System.Address;

      Buffer : Gtk.Text_Buffer.Gtk_Text_Buffer;
      View   : Gtk.Text_View.Gtk_Text_View;

      Prompt : String_Access;
      --  The prompt to be displayed.

      Prompt_Mark : Gtk.Text_Mark.Gtk_Text_Mark;
      --  The position after which the user can insert text.

      Insert_Mark : Gtk.Text_Mark.Gtk_Text_Mark;
      --  The insert position.

      Internal_Insert : Boolean := False;
      --  Whether internal functions are writing in the console buffer.
      --  Prevents recursion in the handler for "insert_text".

      Uneditable_Tag : Gtk.Text_Tag.Gtk_Text_Tag;
      --  A tag indicating uneditable text.

      Prompt_Tag : Gtk.Text_Tag.Gtk_Text_Tag;
      --  A tag used to distinguish the prompt from the rest of the text.

      Highlight_Tag : Gtk.Text_Tag.Gtk_Text_Tag;
      --  A tag used to distinguish highlighted text.

      External_Messages_Tag : Gtk.Text_Tag.Gtk_Text_Tag;
      --  A tag used to identify messages that were generated by GPS but not
      --  by a user comment.

      Button_Press : Boolean := False;
      --  Whether a mouse button is currently pressed.
      --  Used to enable selection in places where the cursor is not
      --  allowed.

      User_Input : String_Access;
      --  The text that was entered at the prompt but was never validated.
      --  Used when commands are issued by menus, etc, replacing the user
      --  input.

      Input_Blocked : Boolean := False;
      --  Set to True means that the console is in uneditable mode.

      Message_Was_Displayed : Boolean := False;
      --  Indicate that a message was displayed, ie the last input point is
      --  not a prompt.

      History : Histories.History;
      Key     : GNAT.OS_Lib.String_Access;
      --  The command history. The most recent commands are at the
      --  beginning.

      Current_Position : Integer := -1;
      --  The current position when browsing the command history.

      Idle_Id : Gtk.Main.Idle_Handler_Id := 0;
      Idle_Registered : Boolean := False;
      --  The handler for idle callbacks.

      Empty_Equals_Repeat : Boolean := True;
      --  Whether an empty command should be equivalent to repeating the
      --  last command.

      Highlight    : Gdk_Color := Null_Color;
      --  The color used for highlighting.

      Waiting_For_Input : Boolean := False;
      --  Whether the console is blocked in a call to readline()
   end record;

end Interactive_Consoles;
