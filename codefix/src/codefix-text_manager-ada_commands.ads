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

with Codefix.Text_Manager.Ada_Extracts; use Codefix.Text_Manager.Ada_Extracts;

package Codefix.Text_Manager.Ada_Commands is

   ---------------------
   -- Recase_Word_Cmd --
   ---------------------

   type Recase_Word_Cmd is new Text_Command with private;

   procedure Initialize
     (This         : in out Recase_Word_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      Cursor       : File_Cursor'Class;
      Correct_Word : String := "";
      Word_Case    : Case_Type := Mixed);
   --  Set all the marks that will be needed to re-case the word later.

   procedure Execute
     (This         : Recase_Word_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      New_Extract  : in out Extract'Class);
   --  Update an extract with the word recased.

   procedure Free (This : in out Recase_Word_Cmd);
   --  Free the memory associated to a Recase_Word_Cmd.

   procedure Unchecked_Free (This : in out Recase_Word_Cmd);
   --  Initialize all fields of This to default values, but do not free any
   --  memory.

   ----------------------------
   -- Remove_Instruction_Cmd --
   ----------------------------

   type Remove_Instruction_Cmd is new Text_Command with private;

   procedure Initialize
     (This              : in out Remove_Instruction_Cmd;
      Current_Text      : Text_Navigator_Abstr'Class;
      Start_Instruction : File_Cursor'Class);
   --  Set all the marks that will be needed to remove the instruction later.

   procedure Execute
     (This         : Remove_Instruction_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      New_Extract  : in out Extract'Class);
   --  Update an extract with the remove of the instruction.

   procedure Free (This : in out Remove_Instruction_Cmd);
   --  Free the memory associated to a Remove_Instruction_Cmd.

   procedure Unchecked_Free (This : in out Remove_Instruction_Cmd);
   --  Initialize all fields of This to default values, but do not free any
   --  memory.

   -------------------------
   -- Remove_Elements_Cmd --
   -------------------------

   type Remove_Elements_Cmd is new Text_Command with private;

   procedure Add_To_Remove
     (This         : in out Remove_Elements_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      Word         : Word_Cursor);
   --  Add an element to be removed later.

   procedure Execute
     (This         : Remove_Elements_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      New_Extract  : in out Extract'Class);
   --  Update an extract with all the elements removed.

   procedure Free (This : in out Remove_Elements_Cmd);
   --  Free the memory associated to a Remove_Elements_Cmd.

   procedure Unchecked_Free (This : in out Remove_Elements_Cmd);
   --  Initialize all fields of This to default values, but do not free any
   --  memory.

   ----------------------------
   -- Remove_Pkg_Clauses_Cmd --
   ----------------------------

   type Remove_Pkg_Clauses_Cmd is new Text_Command with private;

   procedure Initialize
     (This         : in out Remove_Pkg_Clauses_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      Word         : Word_Cursor;
      Destination  : String := "");
   --  Set all the marks that will be needed to remove package clauses.
   --  If Destination is different from "", then the procedure Execute will add
   --  the removed Pkg clauses at the beginning of the destination file.

   procedure Execute
     (This         : Remove_Pkg_Clauses_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      New_Extract  : in out Extract'Class);
   --  Update an extract with the remove of the package clauses.

   procedure Free (This : in out Remove_Pkg_Clauses_Cmd);
   --  Free the memory associated to a Remove_Pkg_Cmd.

   procedure Unchecked_Free (This : in out Remove_Pkg_Clauses_Cmd);
   --  Initialize all fields of This to default values, but do not free any
   --  memory.

   -----------------------
   -- Remove_Entity_Cmd --
   -----------------------

   type Remove_Entity_Cmd is new Text_Command with private;

   procedure Initialize
     (This         : in out Remove_Entity_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      Start_Entity : File_Cursor'Class);
   --  Set all the marks that will be needed to remove the entity later.

   procedure Execute
     (This         : Remove_Entity_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      New_Extract  : in out Extract'Class);
   --  Update an extract with the remove of the entity.

   procedure Free (This : in out Remove_Entity_Cmd);
   --  Free the memory associated to a Remove_Entity_Cmd.

   procedure Unchecked_Free (This : in out Remove_Entity_Cmd);
   --  Initialize all fields of This to default values, but do not free any
   --  memory.

   --------------------
   -- Add_Pragma_Cmd --
   --------------------

   type Add_Pragma_Cmd is new Text_Command with private;

   procedure Initialize
     (This           : in out Add_Pragma_Cmd;
      Current_Text   : Text_Navigator_Abstr'Class;
      Position       : File_Cursor'Class;
      Name, Argument : String);
   --  Set all the marks that will be neede to add the pragma later.

   procedure Execute
     (This         : Add_Pragma_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      New_Extract  : in out Extract'Class);
   --  Update an extract with the addition of the pragma.

   procedure Free (This : in out Add_Pragma_Cmd);
   --  Free the memory associated to an Add_Pragma_Cmd.

   procedure Unchecked_Free (This : in out Add_Pragma_Cmd);
   --  Initialize all fields of This to default values, but do not free any
   --  memory.

   -----------------------
   -- Make_Constant_Cmd --
   -----------------------

   type Make_Constant_Cmd is new Text_Command with private;

   procedure Initialize
     (This         : in out Make_Constant_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      Position     : File_Cursor'Class;
      Name         : String);
   --  Set all the marks that will be needed to make the constant later.

   procedure Execute
     (This         : Make_Constant_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      New_Extract  : in out Extract'Class);
   --  Update an extract with the making of the constant.

   procedure Free (This : in out Make_Constant_Cmd);
   --  Free the memory associated to a Make_Constant_Cmd.

   procedure Unchecked_Free (This : in out Make_Constant_Cmd);
   --  Initialize all fields of This to default values, but do not free any
   --  memory.

   ----------------------------
   -- Remove_Parenthesis_Cmd --
   ----------------------------

   type Remove_Parenthesis_Cmd is new Text_Command with private;

   procedure Initialize
     (This         : in out Remove_Parenthesis_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      Cursor       : File_Cursor'Class);
   --  Set all the marks that will be needed to remove the conversion later.

   procedure Execute
     (This         : Remove_Parenthesis_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      New_Extract  : in out Extract'Class);
   --  Updade an Extract with the remove of the conversion.

   procedure Free (This : in out Remove_Parenthesis_Cmd);
   --  Free the memory associated to a Remove_Parenthesis_Cmd.

   procedure Unchecked_Free (This : in out Remove_Parenthesis_Cmd);
   --  Initialize all fields of This to default values, but do not free any
   --  memory.

private

   package Mark_List is new Generic_List (Word_Mark);
   use Mark_List;

   package Ada_Lists is new Generic_List (Ada_List);
   use Ada_Lists;

   type Recase_Word_Cmd is new Text_Command with record
      Cursor       : Ptr_Mark;
      Correct_Word : Dynamic_String;
      Word_Case    : Case_Type;
   end record;

   type Remove_Instruction_Cmd is new Text_Command with record
      Begin_Mark : Ptr_Mark;
   end record;

   type Remove_Elements_Cmd is new Text_Command with record
      Remove_List : Mark_List.List;
   end record;

   type Remove_Pkg_Clauses_Cmd is new Text_Command with record
      Instantiation_Pkg : Remove_Instruction_Cmd;
      Clauses_Pkg       : Remove_Elements_Cmd;
      Is_Instantiation  : Boolean;
   end record;

   type Remove_Entity_Cmd is new Text_Command with record
      Spec_Begin, Spec_End : Ptr_Mark;
      Body_Begin, Body_End : Ptr_Mark;
   end record;

   type Add_Pragma_Cmd is new Text_Command with record
      Position       : Ptr_Mark;
      Name, Argument : Dynamic_String;
   end record;

   type Make_Constant_Cmd is new Text_Command with record
      Position : Ptr_Mark;
      Name     : Dynamic_String;
   end record;

   type Remove_Parenthesis_Cmd is new Text_Command with record
      Cursor : Ptr_Mark;
   end record;

end Codefix.Text_Manager.Ada_Commands;
