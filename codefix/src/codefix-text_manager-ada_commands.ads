-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2002-2003                    --
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
with GNAT.OS_Lib;                       use GNAT.OS_Lib;
with VFS;

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
      New_Extract  : out Extract'Class);
   --  Set an extract with the word recased.

   procedure Free (This : in out Recase_Word_Cmd);
   --  Free the memory associated to a Recase_Word_Cmd.

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
      New_Extract  : out Extract'Class);
   --  Set an extract with the remove of the instruction.

   procedure Free (This : in out Remove_Instruction_Cmd);
   --  Free the memory associated to a Remove_Instruction_Cmd.

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
      New_Extract  : out Extract'Class);
   --  Set an extract with all the elements removed.

   procedure Free (This : in out Remove_Elements_Cmd);
   --  Free the memory associated to a Remove_Elements_Cmd.

   ----------------------------
   -- Remove_Pkg_Clauses_Cmd --
   ----------------------------

   type Remove_Pkg_Clauses_Cmd is new Text_Command with private;

   procedure Initialize
     (This         : in out Remove_Pkg_Clauses_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      Word         : Word_Cursor;
      Destination  : VFS.Virtual_File := VFS.No_File;
      Category     : Language_Category := Cat_With);
   --  Set all the marks that will be needed to remove package clauses.
   --  If Destination is different from "", then the procedure Execute will add
   --  the removed Pkg clauses at the beginning of the destination file. If
   --  Word.String_Match is null, then the first with after the position
   --  specified by the cursor will be taken.


   procedure Execute
     (This         : Remove_Pkg_Clauses_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      New_Extract  : out Extract'Class);
   --  Set an extract with the remove of the package clauses.

   procedure Free (This : in out Remove_Pkg_Clauses_Cmd);
   --  Free the memory associated to a Remove_Pkg_Cmd.

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
      New_Extract  : out Extract'Class);
   --  Set an extract with the remove of the entity.

   procedure Free (This : in out Remove_Entity_Cmd);
   --  Free the memory associated to a Remove_Entity_Cmd.

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
      New_Extract  : out Extract'Class);
   --  Set an extract with the addition of the pragma.

   procedure Free (This : in out Add_Pragma_Cmd);
   --  Free the memory associated to an Add_Pragma_Cmd.

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
      New_Extract  : out Extract'Class);
   --  Set an extract with the making of the constant.

   procedure Free (This : in out Make_Constant_Cmd);
   --  Free the memory associated to a Make_Constant_Cmd.

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
      New_Extract  : out Extract'Class);
   --  Set an Extract with the remove of the conversion.

   procedure Free (This : in out Remove_Parenthesis_Cmd);
   --  Free the memory associated to a Remove_Parenthesis_Cmd.

   -----------------------
   -- Paste_Profile_Cmd --
   -----------------------

   type Paste_Profile_Cmd is new Text_Command with private;

   procedure Initialize
     (This         : in out Paste_Profile_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      Destination  : File_Cursor'Class;
      Source       : File_Cursor'Class);
   --  Set all the marks that will be needed to paste the profile later.

   procedure Execute
     (This         : Paste_Profile_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      New_Extract  : out Extract'Class);
   --  Set an extract with the profile source pasted in the profile
   --  Destination.

   procedure Free (This : in out Paste_Profile_Cmd);
   --  Free the memory associated to a Paste_Profile_Cmd.

   ---------------------------------
   -- Get_Visible_Declaration_Cmd --
   ---------------------------------

   type Get_Visible_Declaration_Cmd is new Text_Command with private;

   procedure Add_Use
     (This             : out Get_Visible_Declaration_Cmd;
      Current_Text     : Text_Navigator_Abstr'Class;
      Source_Position  : File_Cursor'Class;
      File_Destination : String;
      With_Could_Miss  : Boolean);
   --  Set all the marks that will be needed to add an use later.

   procedure Prefix_Object
     (This            : out Get_Visible_Declaration_Cmd;
      Current_Text    : Text_Navigator_Abstr'Class;
      Source_Position : File_Cursor'Class;
      Object_Position : File_Cursor'Class;
      With_Could_Miss : Boolean);
   --  Set all the marks that will be needed to prefix the object later.

   procedure Execute
     (This         : Get_Visible_Declaration_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      New_Extract  : out Extract'Class);
   --  Set an extract with the declaration made visible.

   procedure Free (This : in out Get_Visible_Declaration_Cmd);
   --  Free the memory associated to a Get_Visible_Declaration.

private

   package Mark_List is new Generic_List (Word_Mark);
   use Mark_List;

   package Ada_Lists is new Generic_List (Ada_List);
   use Ada_Lists;

   type Recase_Word_Cmd is new Text_Command with record
      Cursor       : Ptr_Mark;
      Correct_Word : GNAT.OS_Lib.String_Access;
      Word_Case    : Case_Type;
   end record;

   type Remove_Instruction_Cmd is new Text_Command with record
      Begin_Mark : Ptr_Mark;
   end record;

   type Remove_Elements_Cmd is new Text_Command with record
      Remove_List : Mark_List.List;
   end record;

   package String_List is new Generic_List (GNAT.OS_Lib.String_Access);
   use String_List;
   --  ??? Should use standard string list

   type Remove_Pkg_Clauses_Cmd is new Text_Command with record
      Instantiation_Pkg : Remove_Instruction_Cmd;
      Clauses_Pkg       : Remove_Elements_Cmd;
      Is_Instantiation  : Boolean;
      Obj_List          : String_List.List;
      Last_With         : Ptr_Mark;
   end record;

   type Remove_Entity_Cmd is new Text_Command with record
      Spec_Begin, Spec_End : Ptr_Mark;
      Body_Begin, Body_End : Ptr_Mark;
   end record;

   type Add_Pragma_Cmd is new Text_Command with record
      Position       : Ptr_Mark;
      Name, Argument : GNAT.OS_Lib.String_Access;
   end record;

   type Make_Constant_Cmd is new Text_Command with record
      Position : Ptr_Mark;
      Name     : GNAT.OS_Lib.String_Access;
   end record;

   type Remove_Parenthesis_Cmd is new Text_Command with record
      Cursor : Ptr_Mark;
   end record;

   type Paste_Profile_Cmd is new Text_Command with record
      Destination_Begin, Destination_End : Ptr_Mark;
      Source_Begin, Source_End           : Ptr_Mark;
   end record;

   type Get_Visible_Declaration_Cmd is new Text_Command with record
      Insert_With         : Add_Line_Cmd;
      Prefix_Obj          : Insert_Word_Cmd;
      Insert_With_Enabled : Boolean := False;
      Prefix_Obj_Enabled  : Boolean := False;
   end record;

end Codefix.Text_Manager.Ada_Commands;
