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

   function Clone (This : Ada_Instruction) return Ada_Instruction;
   --  Duplicate all informations associated to an extract, specially
   --  information referenced in pools.

   procedure Remove_Instruction (This : in out Ada_Instruction);

   type Ada_List is new Ada_Instruction with private;

   procedure Get_Unit
     (Current_Text : Text_Navigator_Abstr'Class;
      Position     : File_Cursor'Class;
      Destination  : in out Ada_List);

   function Clone (This : Ada_List) return Ada_List;
   --  Duplicate all informations associated to an extract, specially
   --  information referenced in pools.

   procedure Cut_Off_Elements
     (This        : in out Ada_List;
      Destination : out Ada_List;
      First       : Natural;
      Last        : Natural := 0);

   procedure Cut_Off_Elements
     (This        : in out Ada_List;
      Destination : out Ada_List;
      First       : String;
      Last        : String := "");

   function Get_Number_Elements (This : Ada_List) return Natural;

   procedure Remove_Elements
     (This  : in out Ada_List; First : Natural; Last : Natural := 0);

   procedure Remove_Elements
     (This  : in out Ada_List; First : String; Last : String := "");

   function Get_Element (This : Ada_List; Num : Natural) return String;

   function Get_Nth_Element (This : Ada_List; Name : String) return Natural;

private

   function Is_Closest (Obj, Test1, Test2, Test3 : File_Cursor)
     return Boolean;

   function Is_Comment (Line : String) return Boolean;

   function Is_Blank (Str : String) return Boolean;

   type Ada_Instruction is new Extract with record
      Start, Stop : File_Cursor;
   end record;

   type Lines_Array is array (Integer range <>) of Ptr_Extract_Line;
   type Ptr_Lines_Array is access all Lines_Array;

   type Element is record
      Lines               : Ptr_Lines_Array;
      First_Col, Last_Col : Integer;
      Name                : Dynamic_String;
   end record;

   procedure Free (This : in out Element);
   --  Do not destroy the lines !!!

   type Token_Record is record
      First_Col, Last_Col : Natural := 0;
      Line                : Natural := 0;
   end record;

   package Elements_Lists is new Generic_List (Element);
   use Elements_Lists;

   function Is_Separator (Str : String) return Boolean;

   procedure Get_Token
     (Line      : Extract_Line;
      Col       : in out Integer;
      Token     : out Token_Record;
      Str_Token : in out Dynamic_String);

   type Ada_List is new Ada_Instruction with record
      Elements_List : Elements_Lists.List;
      Head, Back    : Element; -- not yet usable
   end record;

   procedure Add
     (File_Name    : String;
      Destination  : in out Ada_List;
      First_Token  : Token_Record;
      Last_Token   : Token_Record;
      Str_Last_Token : String);

   function Is_Alone (This : Element) return Boolean;

   procedure Delete_All_Lines (This : Element);

   procedure Erase (This : Element);

   function Get_Element (This : Ada_List; Num : Natural)
     return Elements_Lists.List_Node;

end Codefix.Text_Manager.Ada_Extracts;
