-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2002                         --
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

   type Direction_Type is (Backward, Forward);
   --  Forward direction indicates a normal command (ie the text is
   --  inserted/deleted before the cursor), and Backward direction indicates
   --  that the text is inserted/deleted after the cursor.

   procedure Create
     (Item         : out Editor_Replace_Slice;
      Buffer       : Source_Buffer;
      Start_Line   : Integer;
      Start_Column : Integer;
      End_Line     : Integer;
      End_Column   : Integer;
      Text         : String);
   --  Create a new Editor_Replace_Slice command.

   function Execute
     (Command : access Editor_Replace_Slice_Type) return Boolean;
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
      Direction     : Direction_Type := Forward);
   --  Create a new Editor_Command.
   --  Set User_Executed to True if the command is being interactively entered
   --  by the user.

   procedure Add_Text
     (Item         : Editor_Command;
      Text         : String;
      Start_Line   : Integer := -1;
      Start_Column : Integer := -1);
   --  Add some text to the current action.
   --  If values other than -1 are specified, they override the
   --  current values in Item.

   function Execute (Command : access Editor_Command_Type) return Boolean;

   function Undo (Command : access Editor_Command_Type) return Boolean;

   procedure Free (X : in out Editor_Command_Type);
   procedure Free (X : in out Editor_Replace_Slice_Type);
   --  Free memory associated to X.

private

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
   end record;

end Commands.Editor;
