-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
--                            ACT-Europe                             --
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

--  This package provides the implementation for the GPS interactive console.

with Glib.Object; use Glib.Object;

with Gdk.Color;   use Gdk.Color;

with Gtk.Main;
with Gtk.Text_Buffer;
with Gtk.Text_View;
with Gtk.Text_Mark;

with Gtk.Text_Tag;
with Gtk.Scrolled_Window;

with Basic_Types;       use Basic_Types;
with String_List_Utils; use String_List_Utils;

with Pango.Font;

package Glide_Interactive_Consoles is

   type Glide_Interactive_Console_Record is new
     Gtk.Scrolled_Window.Gtk_Scrolled_Window_Record with private;
   type Glide_Interactive_Console is
     access all Glide_Interactive_Console_Record'Class;

   type Command_Handler is access
     function
       (Input     : in String;
        User_Data : access GObject_Record'Class)
        return String;

   type Completion_Handler is access
     function
       (Input     : in String;
        User_Data : access GObject_Record'Class)
        return String_List.List;
   --  This function should return a list of adequate elements that all
   --  begin with Input, or a list containing only Input.

   procedure Gtk_New
     (Console   : out Glide_Interactive_Console;
      Prompt    : String;
      Handler   : Command_Handler;
      User_Data : GObject;
      Font      : Pango.Font.Pango_Font_Description);
   --  Create a new console for glide.

   procedure Initialize
     (Console   : access Glide_Interactive_Console_Record'Class;
      Prompt    : String;
      Handler   : Command_Handler;
      User_Data : GObject;
      Font      : Pango.Font.Pango_Font_Description);
   --  Internal initialization function.

   procedure Insert
     (Console   : access Glide_Interactive_Console_Record;
      Text      : String;
      Add_LF    : Boolean := True;
      Highlight : Boolean := False);
   --  Suspend the user insertion and insert Text as an information message.

   procedure Clear (Console : access Glide_Interactive_Console_Record);
   --  Clear all the text in the Console.

   procedure Enable_Prompt_Display
     (Console : access Glide_Interactive_Console_Record;
      Enable  : Boolean);
   --  When Enable is False, the Prompt is not redisplayed automatically
   --  and the console is uneditable by the user.

   procedure Display_Prompt
     (Console : access Glide_Interactive_Console_Record'Class);
   --  Displays the prompt at the end of the current text.

   procedure Set_Completion_Handler
     (Console : access Glide_Interactive_Console_Record'Class;
      Handler : Completion_Handler);
   --  Set Handler as the default completion handler for the console.
   --  User_Data passed to Handler is the same one that is passed to the
   --  Command_Handler.

   procedure Set_Highlight_Color
     (Console : access Glide_Interactive_Console_Record'Class;
      Color   : Gdk_Color);
   --  Set the color used for highlighting tags.

private

   use String_List;

   type Glide_Interactive_Console_Record is new
     Gtk.Scrolled_Window.Gtk_Scrolled_Window_Record
   with record
      Handler    : Command_Handler;
      Completion : Completion_Handler;
      User_Data  : GObject;

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

      History : List;
      --  The command history. The most recent commands are at the
      --  beginning.

      Current_Position : List_Node;
      --  The current position when browsing the command history.

      Idle_Id : Gtk.Main.Idle_Handler_Id := 0;
      Idle_Registered : Boolean := False;
      --  The handler for idle callbacks.
   end record;

end Glide_Interactive_Consoles;
