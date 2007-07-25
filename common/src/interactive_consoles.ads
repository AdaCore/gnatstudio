-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2007, AdaCore              --
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

with Glib;
with Gtk.Enums;
with Gtk.Main;
with Gtk.Text_Buffer;
with Gtk.Text_View;
with Gtk.Text_Mark;

with Gtk.Text_Tag;
with Gtk.Scrolled_Window;

with GNAT.Expect;
with GNAT.Regpat;
with GNAT.Scripts;
with GNAT.Strings;
with Histories;
with GUI_Utils;
with String_List_Utils;
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
        User_Data : System.Address) return String;

   type Interrupt_Handler is access function
     (Console   : access Interactive_Console_Record'Class;
      User_Data : System.Address) return Boolean;
   --  Called when ctrl-c is pressed in an interactive console.
   --  Should return true if the process was interrupted, False if nothing was
   --  done.

   function Default_Command_Handler
     (Console   : access Interactive_Console_Record'Class;
      Input     : String;
      User_Data : System.Address) return String;
   --  A command handler that executes Input into the scripting language
   --  associated with Console

   function Default_Completion_Handler
     (Input     : String;
      User_Data : System.Address)
      return String_List_Utils.String_List.List;
   --  The default completion handler for a console, which queries the
   --  associated scripting language.
   --  When called, User_Data will be the Console itself, and not the one
   --  passed to Gtk_New

   function Default_Interrupt_Handler
     (Console   : access Interactive_Console_Record'Class;
      User_Data : System.Address) return Boolean;
   --  The default interrupt handler for a console, which requests the
   --  associated scripting language to interrupt the current command.

   procedure Gtk_New
     (Console             : out Interactive_Console;
      Prompt              : String;
      Handler             : Command_Handler := Default_Command_Handler'Access;
      User_Data           : System.Address;
      Font                : Pango.Font.Pango_Font_Description;
      History_List        : Histories.History;
      Key                 : Histories.History_Key;
      Highlight           : Gdk_Color := Null_Color;
      Wrap_Mode           : Gtk.Enums.Gtk_Wrap_Mode := Gtk.Enums.Wrap_None;
      Empty_Equals_Repeat : Boolean := False);
   --  Create a new console.
   --  History_List and Key are used to handle the history of commands entered
   --  by the user in the interactive window. No history is provided if
   --  History_List is null
   --  If Empty_Equals_Repeat is True, an empty command will execute the last
   --  command.

   procedure Initialize
     (Console             : access Interactive_Console_Record'Class;
      Prompt              : String;
      Handler             : Command_Handler := Default_Command_Handler'Access;
      User_Data           : System.Address;
      Font                : Pango.Font.Pango_Font_Description;
      History_List        : Histories.History;
      Key                 : Histories.History_Key;
      Highlight           : Gdk_Color := Null_Color;
      Wrap_Mode           : Gtk.Enums.Gtk_Wrap_Mode;
      Empty_Equals_Repeat : Boolean := False);
   --  Internal initialization function.

   procedure Insert
     (Console        : access Interactive_Console_Record;
      Text           : String;
      Add_LF         : Boolean := True;
      Highlight      : Boolean := False;
      Add_To_History : Boolean := False;
      Show_Prompt    : Boolean := True);
   procedure Insert_UTF8
     (Console        : access Interactive_Console_Record;
      UTF8           : Glib.UTF8_String;
      Add_LF         : Boolean := True;
      Highlight      : Boolean := False;
      Add_To_History : Boolean := False;
      Show_Prompt    : Boolean := True);
   --  Suspend the user insertion and insert Text as an information message.
   --  If Add_To_History is True, the inserted text will be inserted in the
   --  user command history.
   --  If Show_Prompt is true, then a prompt is printed after the text.

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
   --  A default completion handler is provided that queries the scripting
   --  language associated with the console, if there is any.

   procedure Set_Interrupt_Handler
     (Console : access Interactive_Console_Record'Class;
      Handler : Interrupt_Handler);
   --  Set Handler to be called when ctrl-c is pressed in the interactive
   --  console. A default interrupt handler is provided that asks the scripting
   --  language to do the necessary work

   procedure Set_Highlight_Color
     (Console : access Interactive_Console_Record'Class;
      Color   : Gdk_Color);
   --  Set the color used for highlighting tags.

   function Get_Chars
     (Console : access Interactive_Console_Record) return String;
   --  Return the contents of the console window.

   function Get_History
     (Console : access Interactive_Console_Record)
      return GNAT.Strings.String_List_Access;
   --  Return the command history.

   function Get_View
     (Console : access Interactive_Console_Record)
      return Gtk.Text_View.Gtk_Text_View;
   --  Return the text view.

   -----------------
   -- Hyper links --
   -----------------
   --  The console supports hyper links, ie clickable text areas that perform
   --  an action as a result of a user click (in general open an editor for
   --  instance)

   type Hyper_Link_Callback_Record is abstract tagged null record;
   type Hyper_Link_Callback is access all Hyper_Link_Callback_Record'Class;
   procedure On_Click
     (Link : access Hyper_Link_Callback_Record;
      Text : String) is abstract;
   --  Called when a link is clicked on

   procedure On_Destroy (Link : in out Hyper_Link_Callback_Record) is null;
   --  Called when Link is destroyed.

   procedure Create_Hyper_Link
     (Console  : access Interactive_Console_Record;
      Regexp   : GNAT.Regpat.Pattern_Matcher;
      Callback : not null access Hyper_Link_Callback_Record'Class);
   --  Register a regular expression that will highlight links when some text
   --  is inserted through Insert_With_Links. Callback will be destroyed when
   --  the console itself is destroyed.
   --  Such callbacks have to be registered separately from the actual
   --  insertion of text for efficiency and memory reasons. This way, a single
   --  instance of the callback need to exist in memory no matter how many
   --  links are actually set for that callback.
   --  If Regexp contains parenthesis, then the part that is highlighted
   --  corresponds to the first group of parenthesis, otherwise the whole
   --  regexp is highlighted.

   procedure Insert_With_Links
     (Console        : access Interactive_Console_Record;
      Text           : String;
      Add_LF         : Boolean := True;
      Highlight      : Boolean := False);
   --  Insert text in the console, highlighting any text that matches one of
   --  hyper links registered with Create_Hyper_Link.
   --  Clicking on these links will call On_Click on the matching Callback.

   ------------------------------
   -- Adapter for GNAT.Scripts --
   ------------------------------
   --  The following functions can be used to integrate an interactive
   --  console with a GNAT.Scripts.Virtual_Console.

   function Get_Or_Create_Virtual_Console
     (Console : Interactive_Console)
      return GNAT.Scripts.Virtual_Console;
   --  Return the virtual console attached to Console.
   --  Create one if necessary

   function Get_Console
     (Virtual : GNAT.Scripts.Virtual_Console) return Interactive_Console;
   --  Return the interactive console associated with Virtual. This only works
   --  for virtual consoles created by Get_Or_Create_Virtual_Console above

private

   type Hyper_Link_Record;
   type Hyper_Links is access Hyper_Link_Record;
   type Hyper_Link_Record is record
      Pattern  : GNAT.Expect.Pattern_Matcher_Access;
      Callback : Hyper_Link_Callback;
      Tag      : Gtk.Text_Tag.Gtk_Text_Tag;
      Next     : Hyper_Links;
   end record;

   type Interactive_Console_Record is new
     Gtk.Scrolled_Window.Gtk_Scrolled_Window_Record
   with record
      Handler    : Command_Handler;
      Virtual    : GNAT.Scripts.Virtual_Console;
      Completion : GUI_Utils.Completion_Handler;
      Interrupt  : Interrupt_Handler;
      User_Data  : System.Address;

      Buffer : Gtk.Text_Buffer.Gtk_Text_Buffer;
      View   : Gtk.Text_View.Gtk_Text_View;

      Prompt : GNAT.Strings.String_Access;
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

      User_Input : GNAT.Strings.String_Access;
      --  The text that was entered at the prompt but was never validated.
      --  Used when commands are issued by menus, etc, replacing the user
      --  input.

      Input_Blocked : Boolean := False;
      --  Set to True means that the console is in uneditable mode.

      Message_Was_Displayed : Boolean := False;
      --  Indicate that a message was displayed, ie the last input point is
      --  not a prompt.

      History : Histories.History;
      Key     : GNAT.Strings.String_Access;
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

      Command_Received : Boolean := False;
      --  Whether the console has received a first command yet

      Links       : Hyper_Links;
      Links_Count : Natural := 0;
      --  List of hyper links that have been registered for this console
      --  Links_Count indicates the number of links that have been registered,
      --  for efficiency
   end record;

end Interactive_Consoles;
