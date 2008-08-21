-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2002-2008, AdaCore              --
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
with GNAT.Strings;

package Codefix.Text_Manager.Ada_Extracts is

   type Remove_Code_Mode is (Erase, Comment);

   ----------------------------------------------------------------------------
   --  type Ada_Instruction
   ----------------------------------------------------------------------------

   subtype Delimiters_Array is Token_List;

   Default_Delimiters : Delimiters_Array :=
     ((Kind => Keyword_Text, Name => new String'("declare")),
      (Kind => Keyword_Text, Name => new String'("begin")),
      (Kind => Keyword_Text, Name => new String'("is")),
      (Kind => Operator_Text, Name => new String'(";")),
      (Kind => Keyword_Text, Name => new String'("then")),
      (Kind => Keyword_Text, Name => new String'("loop")));

   type Ada_Instruction is tagged private;
   --  This type represents an Ada instruction, or a part of an
   --  ada instructions.

   procedure Free (This : in out Ada_Instruction);
   --  Free the memory associated to an Ada_Instruction.

   procedure Get_Unit
     (Current_Text : Text_Navigator_Abstr'Class;
      Position     : File_Cursor'Class;
      Destination  : in out Ada_Instruction;
      Delimiters   : Delimiters_Array := Default_Delimiters);
   --  Initialise Destination considerate that position is on a random position
   --  in an instruction. If the unit is on one of the delimiters, then it will
   --  begins at the given position, otherwise it will go backward until it
   --  finds one. It will then go forward until it finds another delimiter.
   --  The first delimiter is not store, the last is, and the begining or the
   --  end of the file stops the analysis and puts all the founded delimiters
   --  into the instruction.

   function Clone (This : Ada_Instruction) return Ada_Instruction;
   --  Duplicate all informations associated to an extract, specially
   --  information referenced in pools.

   procedure Remove_Instruction
     (This : in out Ada_Instruction;
      Text_Nav : in out Text_Navigator_Abstr'Class);
   --  Delete the instruction recored in This.

   procedure Comment_Instruction
     (This : in out Ada_Instruction;
      Text_Nav : in out Text_Navigator_Abstr'Class);
   --  Comment the instruction recorded in This.

   function Get_Start
     (This     : Ada_Instruction;
      Text_Nav : Text_Navigator_Abstr'Class) return File_Cursor;
   --  Return the cursor stands at the beginning of the instruction/

   function Get_Stop
     (This     : Ada_Instruction;
      Text_Nav : Text_Navigator_Abstr'Class) return File_Cursor;
   --  Return the cursors stands at the end of the instruction.

   ----------------------------------------------------------------------------
   --  type Ada_List
   ----------------------------------------------------------------------------

   type Ada_List is new Ada_Instruction with private;
   --  This type is a representation of an Ada_Instruction as a list of Tokens.

   overriding procedure Free (This : in out Ada_List);
   --  Free the memory associated to an Ada_List.

   overriding procedure Get_Unit
     (Current_Text : Text_Navigator_Abstr'Class;
      Position     : File_Cursor'Class;
      Destination  : in out Ada_List;
      Delimiters   : Delimiters_Array := Default_Delimiters);
   --  Initialise Destination considerate that position is on a random position
   --  in an list (typically a with / use list or a multiple vars declaration).

   overriding function Clone (This : Ada_List) return Ada_List;
   --  Duplicate all informations associated to an extract, specially
   --  information referenced in pools.

   procedure Cut_Off_Elements
     (This         : in out Ada_List;
      Text_Nav     : in out Text_Navigator_Abstr'Class;
      New_Instr    : out GNAT.Strings.String_Access;
      Current_Text : Text_Navigator_Abstr'Class;
      First        : Natural;
      Last         : Natural := 0);
   --  Remove elements from the list and write in the string a new list with
   --  these elements, and right declarative part (if needed). If last = 0 then
   --  only First element will be deleted.

   procedure Cut_Off_Elements
     (This         : in out Ada_List;
      Text_Nav     : in out Text_Navigator_Abstr'Class;
      New_Instr    : out GNAT.Strings.String_Access;
      Current_Text : Text_Navigator_Abstr'Class;
      First        : String;
      Last         : String := "");
   --  Remove elements from the list and write in the string a new list with
   --  these elements, and right declarative part (if needed). If last = 0 then
   --  only First element will be deleted.

   function Get_Number_Of_Elements (This : Ada_List) return Natural;
   --  Return the number of token contained in the list, including ','.

   function Get_Number_Of_Declarations (This : Ada_List) return Natural;
   --  Return the number of entities actually declared in this list.

   procedure Remove_Elements
     (This     : in out Ada_List;
      Text_Nav : in out Text_Navigator_Abstr'Class;
      Mode     : Remove_Code_Mode;
      First    : Natural; Last : Natural := 0);
   --  Remove elements form form First to Last. If Last = 0 then only First
   --  will be removed.

   procedure Remove_Elements
     (This     : in out Ada_List;
      Text_Nav : in out Text_Navigator_Abstr'Class;
      Mode     : Remove_Code_Mode;
      First    : String;
      Last     : String := "");
   --  Remove elements form form First to Last. If Last = 0 then only First
   --  will be removed.

   function Get_Nth_Element (This : Ada_List; Name : String) return Natural;
   --  Return the number of the element Name in the list.

private

   function Is_Comment (Line : String) return Boolean;

   type Ada_Instruction is tagged record
      Start, Stop : Ptr_Mark;
   end record;

   type Token_Record is record
      Line                  : File_Cursor;
      First_Char, Last_Char : Char_Index := 0;
      Content               : GNAT.Strings.String_Access;
   end record;

   procedure Free (This : in out Token_Record);

   function Clone (This : Token_Record) return Token_Record;

   package Tokens_List is new Generic_List (Token_Record);
   use Tokens_List;

   procedure Get_Text_Slice
     (This                     : Ada_List;
      Current_Text             : Text_Navigator_Abstr'Class;
      Start_Index, End_Index   : Integer;
      Start_Cursor, End_Cursor : out File_Cursor);
   --  Return a slice of text according to two token indexes.

   type Ada_List is new Ada_Instruction with record
      Elements_List : Tokens_List.List;
   end record;

   function Get_Element (This : Ada_List; Num : Natural)
     return Tokens_List.List_Node;

end Codefix.Text_Manager.Ada_Extracts;
