-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                    Copyright (C) 2002 - 2003                      --
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

--  This package implements commands related to the editor.

with GNAT.OS_Lib;       use GNAT.OS_Lib;
with Src_Editor_Buffer; use Src_Editor_Buffer;

package Commands.Editor is

   type Editor_Command_Type is new Root_Command with private;
   type Editor_Command is access all Editor_Command_Type;

   type Editor_Replace_Slice_Type is new Root_Command with private;
   type Editor_Replace_Slice is access all Editor_Replace_Slice_Type;

   type Check_Modified_State_Type is new Root_Command with private;
   type Check_Modified_State is access all Check_Modified_State_Type;

   type Direction_Type is (Backward, Forward, Extended);
   --  Forward direction indicates a normal command (ie the text is
   --  inserted/deleted before the cursor), and Backward direction indicates
   --  that the text is inserted/deleted after the cursor.
   --  Extended means that the action does not have the cursor as a boundary.

   procedure Create
     (Item   : out Check_Modified_State;
      Buffer : Source_Buffer;
      Queue  : Command_Queue);
   --  Create a new Check_Modified_State command.

   function Execute
     (Command : access Check_Modified_State_Type) return Command_Return_Type;
   --  Compare the states of the associated box and queues,
   --  and change the label in the source editor if needed.

   procedure Create
     (Item         : out Editor_Replace_Slice;
      Buffer       : Source_Buffer;
      Start_Line   : Integer;
      Start_Column : Integer;
      End_Line     : Integer;
      End_Column   : Integer;
      Text         : String;
      Force_End    : Boolean := False);
   --  Create a new Editor_Replace_Slice command.
   --  If Force_End then the cursor will always be placed at the end of the
   --  newly inserted text, otherwise it is placed at the end when executing
   --  the command, and at the beginning when undoing it.
   --  Text is a string in the current charset, and will be converted in
   --  UTF-8 format.

   function Execute
     (Command : access Editor_Replace_Slice_Type) return Command_Return_Type;
   function Undo (Command : access Editor_Replace_Slice_Type) return Boolean;

   type Editor_Command_Mode is (Insertion, Deletion);

   function Is_Null_Command (Command : Editor_Command) return Boolean;
   --  Return True if the command does nothing at all, False otherwise.

   function Get_Mode (Command : Editor_Command) return Editor_Command_Mode;
   --  Return the mode associated with Command.

   function Get_Direction (Command : Editor_Command) return Direction_Type;
   --  Return the direction associated with Command.

   procedure Create
     (Item          : out Editor_Command;
      Mode          : Editor_Command_Mode;
      Buffer        : Source_Buffer;
      User_Executed : Boolean;
      Line          : Integer;
      Column        : Integer;
      Direction     : Direction_Type := Forward;
      Cursor_Line   : Integer := 0;
      Cursor_Column : Integer := 0);
   --  Create a new Editor_Command.
   --  Set User_Executed to True if the command is being interactively entered
   --  by the user.
   --  Cursor_Line and Cursor_Column need to be specified only if the
   --  Direction_Type is Extended.

   procedure Add_Text
     (Item         : Editor_Command;
      UTF8         : String;
      Start_Line   : Integer := -1;
      Start_Column : Integer := -1);
   --  Add some text (in UTF-8 format) to the current action.
   --  If values other than -1 are specified, they override the
   --  current values in Item.

   function Get_Text (Item : Editor_Command) return String;
   --  Return the text (in UTF-8 format) associated with Item.

   procedure Set_Text (Item : Editor_Command; Text : String);
   --  Set the text (in UTF-8 format) associated with Item.

   function Execute
     (Command : access Editor_Command_Type) return Command_Return_Type;

   function Undo (Command : access Editor_Command_Type) return Boolean;

   procedure Free (X : in out Editor_Command_Type);
   procedure Free (X : in out Editor_Replace_Slice_Type);
   procedure Free (X : in out Check_Modified_State_Type);
   --  Free memory associated to X.

private

   type Check_Modified_State_Type is new Root_Command with record
      Buffer      : Source_Buffer;
      Check_Queue : Command_Queue;
   end record;

   type Editor_Command_Type is new Root_Command with record
      Buffer                    : Source_Buffer;
      Current_Text              : String_Access;
      Current_Text_Total_Length : Natural := 512;
      Current_Text_Size         : Natural := 0;
      Edition_Mode              : Editor_Command_Mode;
      User_Executed             : Boolean;
      Line                      : Integer;
      Column                    : Integer;
      Direction                 : Direction_Type;

      Cursor_Line, Cursor_Column : Integer;
   end record;

   type Editor_Replace_Slice_Type is new Root_Command with record
      Buffer            : Source_Buffer;

      Start_Line        : Integer;
      Start_Column      : Integer;

      End_Line_Before   : Integer;
      End_Column_Before : Integer;

      End_Line_After    : Integer := -1;
      End_Column_After  : Integer := -1;

      Text_Before       : String_Access;
      Text_After        : String_Access;

      Force_End         : Boolean;
   end record;

end Commands.Editor;
