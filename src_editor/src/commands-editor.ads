------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2002-2012, AdaCore                     --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

--  This package implements commands related to the editor

with Glib;              use Glib;
with Basic_Types;       use Basic_Types;
with Gtk.Text_Mark;     use Gtk.Text_Mark;
with Src_Editor_Buffer; use Src_Editor_Buffer;
with GNAT.Strings;

package Commands.Editor is

   type Base_Editor_Command_Type is abstract new Root_Command with record
      Base_Line : Buffer_Line_Type;
      --  This field is guaranteed to be set to the line number corresponding
      --  to the line number right before the command is executed.
   end record;
   type Base_Editor_Command is access all Base_Editor_Command_Type;

   function Debug_String
     (C : Base_Editor_Command_Type) return String is abstract;
   --  Print a string representation of the command. For debugging purposes.

   type Editor_Command_Type is new Base_Editor_Command_Type with private;
   type Editor_Command is access all Editor_Command_Type;

   type Editor_Replace_Slice_Type is new Base_Editor_Command_Type with private;
   type Editor_Replace_Slice is access all Editor_Replace_Slice_Type;

   type Check_Modified_State_Type is new Base_Editor_Command_Type with private;
   type Check_Modified_State is access all Check_Modified_State_Type;

   type Remove_Blank_Lines_Command_Type is new Root_Command with record
      Buffer : Source_Buffer;
      Mark   : Gtk_Text_Mark;
      Number : Natural;
   end record;
   type Remove_Blank_Lines_Command is access
     all Remove_Blank_Lines_Command_Type;

   type Hide_Editable_Lines_Type is new Base_Editor_Command_Type with record
      Buffer : Source_Buffer;
      Number : Editable_Line_Type;
   end record;
   type Hide_Editable_Lines_Command is access all Hide_Editable_Lines_Type;

   type Unhide_Editable_Lines_Type is new Base_Editor_Command_Type with record
      Buffer     : Source_Buffer;
   end record;

   type Unhide_Editable_Lines_Command is access all Unhide_Editable_Lines_Type;

   overriding function Execute
     (Command : access Unhide_Editable_Lines_Type)
      return Command_Return_Type;
   --  Unhides lines from the buffer

   overriding function Execute
     (Command : access Hide_Editable_Lines_Type)
      return Command_Return_Type;
   --  Hides lines from the buffer

   overriding function Execute
     (Command : access Remove_Blank_Lines_Command_Type)
      return Command_Return_Type;
   --  This commands removes the Number blank lines associated with Mark

   type Direction_Type is (Backward, Forward, Extended);
   --  Forward direction indicates a normal command (ie the text is
   --  inserted/deleted before the cursor), and Backward direction indicates
   --  that the text is inserted/deleted after the cursor.
   --  Extended means that the action does not have the cursor as a boundary.

   procedure Create
     (Item   : out Check_Modified_State;
      Buffer : Source_Buffer;
      Queue  : Command_Queue);
   --  Create a new Check_Modified_State command

   overriding function Execute
     (Command : access Check_Modified_State_Type) return Command_Return_Type;
   --  Compare the states of the associated box and queues,
   --  and change the label in the source editor if needed.

   procedure Create
     (Item         : out Editor_Replace_Slice;
      Buffer       : Source_Buffer;
      Start_Line   : Editable_Line_Type;
      Start_Column : Character_Offset_Type;
      End_Line     : Editable_Line_Type;
      End_Column   : Character_Offset_Type;
      Text         : UTF8_String;
      Force_End    : Boolean := False);
   --  Create a new Editor_Replace_Slice command.
   --  If Force_End then the cursor will always be placed at the end of the
   --  newly inserted text, otherwise it is placed at the end when executing
   --  the command, and at the beginning when undoing it.
   --  Text is must be a UTF-8 encoded string.

   overriding function Execute
     (Command : access Editor_Replace_Slice_Type) return Command_Return_Type;
   overriding function Undo
     (Command : access Editor_Replace_Slice_Type) return Boolean;

   type Editor_Command_Mode is (Insertion, Deletion);

   function Is_Null_Command (Command : Editor_Command) return Boolean;
   --  Return True if the command does nothing at all, False otherwise

   function Get_Mode (Command : Editor_Command) return Editor_Command_Mode;
   --  Return the mode associated with Command

   function Get_Direction (Command : Editor_Command) return Direction_Type;
   --  Return the direction associated with Command

   procedure Create
     (Item          : out Editor_Command;
      Mode          : Editor_Command_Mode;
      Buffer        : Source_Buffer;
      User_Executed : Boolean;
      Line          : Editable_Line_Type;
      Column        : Character_Offset_Type;
      Direction     : Direction_Type := Forward;
      Cursor_Line   : Editable_Line_Type := 0;
      Cursor_Column : Character_Offset_Type := 0);
   --  Create a new Editor_Command.
   --  Set User_Executed to True if the command is being interactively entered
   --  by the user.
   --  Cursor_Line and Cursor_Column need to be specified only if the
   --  Direction_Type is Extended.

   procedure Add_Text
     (Item         : Editor_Command;
      UTF8         : UTF8_String;
      Start_Line   : Editable_Line_Type := 0;
      Start_Column : Character_Offset_Type := 0);
   --  Add some text (in UTF-8 format) to the current action.
   --  If values other than -1 are specified, they override the
   --  current values in Item.

   function Get_Text (Item : Editor_Command) return UTF8_String;
   --  Return the text (in UTF-8 format) associated with Item

   procedure Set_Text (Item : Editor_Command; Text : UTF8_String);
   --  Set the text (in UTF-8 format) associated with Item

   overriding function Execute
     (Command : access Editor_Command_Type) return Command_Return_Type;

   overriding function Undo
     (Command : access Editor_Command_Type) return Boolean;

   overriding procedure Free (X : in out Editor_Command_Type);
   overriding procedure Free (X : in out Editor_Replace_Slice_Type);
   overriding procedure Free (X : in out Check_Modified_State_Type);
   overriding procedure Free (X : in out Remove_Blank_Lines_Command_Type);
   overriding procedure Free (X : in out Unhide_Editable_Lines_Type);
   overriding procedure Free (X : in out Hide_Editable_Lines_Type);
   --  Free memory associated to X

   overriding function Debug_String
     (C : Hide_Editable_Lines_Type) return String;
   overriding function Debug_String
     (C : Unhide_Editable_Lines_Type) return String;

private

   type Check_Modified_State_Type is new Base_Editor_Command_Type with record
      Buffer      : Source_Buffer;
      Check_Queue : Command_Queue;
   end record;

   type Editor_Command_Type is new Base_Editor_Command_Type with record
      Buffer                    : Source_Buffer;
      Current_Text              : GNAT.Strings.String_Access;
      Current_Text_Total_Length : Natural := 512;
      Current_Text_Size         : Natural := 0;
      Edition_Mode              : Editor_Command_Mode;
      User_Executed             : Boolean;
      Line                      : Editable_Line_Type;
      Column                    : Character_Offset_Type;
      Direction                 : Direction_Type;

      Cursor_Line               : Editable_Line_Type;
      Cursor_Column             : Character_Offset_Type;
   end record;

   type Editor_Replace_Slice_Type is new Base_Editor_Command_Type with record
      Buffer            : Source_Buffer;

      Start_Line        : Editable_Line_Type;
      Start_Column      : Character_Offset_Type;

      End_Line_Before   : Editable_Line_Type;
      End_Column_Before : Character_Offset_Type;

      End_Line_After    : Editable_Line_Type := 0;
      End_Column_After  : Character_Offset_Type := 0;

      Text_Before       : GNAT.Strings.String_Access;
      Text_After        : GNAT.Strings.String_Access;

      Force_End         : Boolean;
   end record;

   overriding function Debug_String
     (C : Check_Modified_State_Type) return String;

   overriding function Debug_String
     (C : Editor_Command_Type) return String;

   overriding function Debug_String
     (C : Editor_Replace_Slice_Type) return String;

end Commands.Editor;
