-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2002                         --
--                            ACT-Europe                             --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Generic_List;

package Codefix.Text_Manager.Ada_Extracts is

   type Ada_Instruction is new Extract with private;

   procedure Get_Unit
     (Current_Text : Text_Navigator_Abstr'Class;
      Position     : File_Cursor'Class;
      Destination  : in out Ada_Instruction);
   --  Initialise Destination considerate that position is on a random position
   --  in an instruction.

   function Clone (This : Ada_Instruction) return Ada_Instruction;
   --  Duplicate all informations associated to an extract, specially
   --  information referenced in pools.

   procedure Remove_Instruction (This : in out Ada_Instruction);
   --  Delete the instruction recored in This.

   function Get_Stop (This : Ada_Instruction) return File_Cursor;
   --  Return the cursor stands at the beginning of the instruction/

   function Get_Stop (This : Ada_Instruction) return File_Cursor;
   --  Return the cursors stands at the end of the instruction.

   type Ada_List is new Ada_Instruction with private;

   procedure Get_Unit
     (Current_Text : Text_Navigator_Abstr'Class;
      Position     : File_Cursor'Class;
      Destination  : in out Ada_List);
   --  Initialise Destination considerate that position is on a random position
   --  in an list (typically a with / use list or a multiple vars declaration).

   function Clone (This : Ada_List) return Ada_List;
   --  Duplicate all informations associated to an extract, specially
   --  information referenced in pools.

   procedure Cut_Off_Elements
     (This        : in out Ada_List;
      New_Instr   : out Dynamic_String;
      First       : Natural;
      Last        : Natural := 0);
   --  Remove elements from the list and write in the string a new list with
   --  these elements, and right declarative part (if needed). If last = 0 then
   --  only First element will be deleted.

   procedure Cut_Off_Elements
     (This        : in out Ada_List;
      New_Instr   : out Dynamic_String;
      First       : String;
      Last        : String := "");
   --  Remove elements from the list and write in the string a new list with
   --  these elements, and right declarative part (if needed). If last = 0 then
   --  only First element will be deleted.

   function Get_Number_Of_Elements (This : Ada_List) return Natural;
   --  Return the number of token contained in the list, including ','.

   procedure Remove_Elements
     (This  : in out Ada_List; First : Natural; Last : Natural := 0);
   --  Remove elements form form First to Last. If Last = 0 then only First
   --  will be removed.

   procedure Remove_Elements
     (This  : in out Ada_List; First : String; Last : String := "");
   --  Remove elements form form First to Last. If Last = 0 then only First
   --  will be removed.

   function Get_Element (This : Ada_List; Num : Natural) return String;
   --  Return one element from the list.

   function Get_Nth_Element (This : Ada_List; Name : String) return Natural;
   --  Return the number of the element Name in the list.

private

   function Is_Closest (Obj, Test1, Test2, Test3 : File_Cursor)
     return Boolean;

   function Is_Comment (Line : String) return Boolean;

   function Is_Blank (Str : String) return Boolean;

   type Ada_Instruction is new Extract with record
      Start, Stop : File_Cursor;
   end record;

   type Token_Record is record
      Line                : Ptr_Extract_Line;
      First_Col, Last_Col : Natural := 0;
      Content             : Dynamic_String;
      Is_Separator        : Boolean;
   end record;

   procedure Free (This : in out Token_Record);

   package Tokens_List is new Generic_List (Token_Record);
   use Tokens_List;

   procedure Get_Token
     (Line      : Ptr_Extract_Line;
      Col       : in out Integer;
      Token     : out Token_Record);

   type Ada_List is new Ada_Instruction with record
      Elements_List : Tokens_List.List;
      Back          : Dynamic_String;
   end record;

   function Is_Alone (This : Token_Record; Offset_Col : Integer)
     return Boolean;

   function Get_Element (This : Ada_List; Num : Natural)
     return Tokens_List.List_Node;

end Codefix.Text_Manager.Ada_Extracts;
