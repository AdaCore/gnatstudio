------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2002-2026, AdaCore                     --
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

with Refactoring.Services; use Refactoring.Services;

private with GPS_Vectors;
private with Ada.Containers.Vectors;

package Codefix.Text_Manager.Ada_Commands is

   ---------------------
   -- Recase_Word_Cmd --
   ---------------------

   type Recase_Word_Cmd is new Text_Command (Simple) with private;

   procedure Initialize
     (This         : in out Recase_Word_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      Cursor       : File_Cursor'Class;
      Correct_Word : Unbounded_String := Null_Unbounded_String;
      Word_Case    : Case_Type := Mixed);
   --  Set all the marks that will be needed to re-case the word later.

   overriding
   procedure Execute
     (This         : Recase_Word_Cmd;
      Current_Text : in out Text_Navigator_Abstr'Class);
   --  Set an extract with the word recased.

   ----------------------------
   -- Remove_Instruction_Cmd --
   ----------------------------

   type Remove_Instruction_Cmd is new Text_Command (Complex) with private;

   procedure Initialize
     (This              : in out Remove_Instruction_Cmd;
      Current_Text      : Text_Navigator_Abstr'Class;
      Start_Instruction : File_Cursor'Class);
   --  Set all the marks that will be needed to remove the instruction later.

   overriding
   procedure Execute
     (This         : Remove_Instruction_Cmd;
      Current_Text : in out Text_Navigator_Abstr'Class);
   --  Set an extract with the remove of the instruction.

   -------------------------
   -- Remove_Elements_Cmd --
   -------------------------

   type Remove_Elements_Cmd is new Text_Command (Complex) with private;
   --  This type is used to store a list of element that have to be removed.
   --  The default behavior of this class is to erase the elements, but it can
   --  be changed to just comment them.

   procedure Set_Remove_Mode
     (This : in out Remove_Elements_Cmd; Mode : Remove_Code_Mode);
   --  Sets the mode for the removal of elements.

   procedure Add_To_Remove
     (This         : in out Remove_Elements_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      Word         : Word_Cursor);
   --  Add an element to be removed later.

   overriding
   procedure Execute
     (This         : Remove_Elements_Cmd;
      Current_Text : in out Text_Navigator_Abstr'Class);
   --  Set an extract with all the elements removed.

   overriding
   procedure Free (This : in out Remove_Elements_Cmd);
   --  Free the memory associated to a Remove_Elements_Cmd.

   overriding
   function Is_Writable (This : Remove_Elements_Cmd) return Boolean;
   --  See inherited documentation

   ----------------------------
   -- Remove_Pkg_Clauses_Cmd --
   ----------------------------

   type Remove_Pkg_Clauses_Cmd is new Text_Command (Simple) with private;

   procedure Initialize
     (This         : in out Remove_Pkg_Clauses_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      Word         : Word_Cursor;
      Position     : Relative_Position := Specified;
      Destination  : GNATCOLL.VFS.Virtual_File := GNATCOLL.VFS.No_File;
      Category     : Dependency_Category := Cat_With;
      Look_For_Use : Boolean := True);
   --  Set all the marks that will be needed to remove package clauses.
   --  If Destination is different from "", then the procedure Execute will add
   --  the removed Pkg clauses at the beginning of the destination file. If
   --  Word.String_Match is null, then the first with after the position
   --  specified by the cursor will be taken. If Look_For_Use is true, then
   --  use clauses will be removed if the category if on a with.

   overriding
   procedure Execute
     (This         : Remove_Pkg_Clauses_Cmd;
      Current_Text : in out Text_Navigator_Abstr'Class);
   --  Set an extract with the remove of the package clauses.

   overriding
   function Is_Writable (This : Remove_Pkg_Clauses_Cmd) return Boolean;
   --  See inherited documentation

   -----------------------
   -- Remove_Entity_Cmd --
   -----------------------

   type Remove_Entity_Cmd is new Text_Command (Complex) with private;

   procedure Initialize
     (This         : in out Remove_Entity_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      Start_Entity : File_Cursor'Class;
      Mode         : Remove_Code_Mode := Erase);
   --  Set all the marks that will be needed to remove the entity later.

   overriding
   procedure Execute
     (This         : Remove_Entity_Cmd;
      Current_Text : in out Text_Navigator_Abstr'Class);
   --  Set an extract with the remove of the entity.

   --------------------
   -- Add_Pragma_Cmd --
   --------------------

   type Add_Pragma_Cmd is new Text_Command (Simple) with private;

   procedure Initialize
     (This         : in out Add_Pragma_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      Position     : File_Cursor'Class;
      Category     : Language_Category;
      Name         : Unbounded_String;
      Argument     : Unbounded_String);
   --  Set all the marks that will be neede to add the pragma later.

   overriding
   procedure Execute
     (This         : Add_Pragma_Cmd;
      Current_Text : in out Text_Navigator_Abstr'Class);
   --  Set an extract with the addition of the pragma.

   -----------------------
   -- Make_Constant_Cmd --
   -----------------------

   type Make_Constant_Cmd is new Text_Command (Simple) with private;

   procedure Initialize
     (This         : in out Make_Constant_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      Position     : File_Cursor'Class;
      Name         : Unbounded_String);
   --  Set all the marks that will be needed to make the constant later.

   overriding
   procedure Execute
     (This         : Make_Constant_Cmd;
      Current_Text : in out Text_Navigator_Abstr'Class);
   --  Set an extract with the making of the constant.

   ----------------------------
   -- Remove_Parenthesis_Cmd --
   ----------------------------

   type Remove_Conversion_Cmd is new Text_Command (Simple) with private;

   procedure Initialize
     (This         : in out Remove_Conversion_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      Cursor       : File_Cursor'Class);
   --  Set all the marks that will be needed to remove the conversion later.

   overriding
   procedure Execute
     (This         : Remove_Conversion_Cmd;
      Current_Text : in out Text_Navigator_Abstr'Class);
   --  Set an Extract with the remove of the conversion.

   -----------------------
   -- Paste_Profile_Cmd --
   -----------------------

   type Paste_Profile_Cmd is new Text_Command (Complex) with private;

   procedure Initialize
     (This                              : in out Paste_Profile_Cmd;
      Current_Text                      : Text_Navigator_Abstr'Class;
      Source_Cursor, Destination_Cursor : File_Cursor'Class;
      Source_Loc, Destination_Loc       : Relative_Position);
   --  Set all the marks that will be needed to paste the profile later. The
   --  actual entity is looked relatively to the mark according to Source_Loc
   --  and Destination_Loc.

   overriding
   procedure Execute
     (This         : Paste_Profile_Cmd;
      Current_Text : in out Text_Navigator_Abstr'Class);
   --  Set an extract with the profile source pasted in the profile
   --  Destination.

   overriding
   procedure Free (This : in out Paste_Profile_Cmd);
   --  Free the memory associated to a Paste_Profile_Cmd.

   overriding
   function Is_Writable (This : Paste_Profile_Cmd) return Boolean;
   --  See inherited documentation

   ---------------------------------
   -- Get_Visible_Declaration_Cmd --
   ---------------------------------

   type Get_Visible_Declaration_Cmd is new Text_Command (Complex) with private;

   function Get_Package_To_Be_Withed
     (Current_Text    : Text_Navigator_Abstr'Class;
      Source_Position : File_Cursor'Class) return String;
   --  Return the package name that has to be withed in order to be able to
   --  use the entity pointed at the given position

   procedure Add_Use
     (This             : out Get_Visible_Declaration_Cmd;
      Current_Text     : Text_Navigator_Abstr'Class;
      Source_Position  : File_Cursor'Class;
      File_Destination : GNATCOLL.VFS.Virtual_File;
      With_Could_Miss  : Boolean);
   --  Set all the marks that will be needed to add an use later.

   procedure Prefix_Object
     (This            : out Get_Visible_Declaration_Cmd;
      Current_Text    : Text_Navigator_Abstr'Class;
      Source_Position : File_Cursor'Class;
      Object_Position : File_Cursor'Class;
      With_Could_Miss : Boolean);
   --  Set all the marks that will be needed to prefix the object later.

   overriding
   procedure Execute
     (This         : Get_Visible_Declaration_Cmd;
      Current_Text : in out Text_Navigator_Abstr'Class);
   --  Set an extract with the declaration made visible.

   overriding
   procedure Free (This : in out Get_Visible_Declaration_Cmd);
   --  Free the memory associated to a Get_Visible_Declaration.

   overriding
   function Is_Writable (This : Get_Visible_Declaration_Cmd) return Boolean;
   --  See inherited documentation

   ---------------------
   -- Indent_Code_Cmd --
   ---------------------

   type Indent_Code_Cmd is new Text_Command (Simple) with private;

   procedure Initialize
     (This         : in out Indent_Code_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      Line_Cursor  : File_Cursor'Class;
      Force_Column : Visible_Column_Type);
   --  Creates a indentation query - if Force_Column is 0, then the
   --  GNAT Studio indentation engine will get used.

   overriding
   procedure Execute
     (This         : Indent_Code_Cmd;
      Current_Text : in out Text_Navigator_Abstr'Class);

   ---------------------
   -- Add_Clauses_Cmd --
   ---------------------

   type Add_Clauses_Cmd is new Text_Command (Simple) with private;

   procedure Initialize
     (This           : in out Add_Clauses_Cmd;
      Current_Text   : Text_Navigator_Abstr'Class;
      Cursor         : File_Cursor'Class;
      Missing_Clause : Unbounded_String;
      Add_With       : Boolean;
      Add_Use        : Boolean);
   --  Add the missing clause in the text

   overriding
   procedure Execute
     (This         : Add_Clauses_Cmd;
      Current_Text : in out Text_Navigator_Abstr'Class);

   overriding
   function Is_Writable (This : Add_Clauses_Cmd) return Boolean;
   --  See inherited documentation

   ------------------------------
   -- Change_To_Tick_Valid_Cmd --
   ------------------------------

   type Change_To_Tick_Valid_Cmd is new Text_Command (Simple) with private;

   procedure Initialize
     (This           : in out Change_To_Tick_Valid_Cmd;
      Current_Text   : Text_Navigator_Abstr'Class;
      Cursor         : File_Cursor'Class);
   --  Add the missing clause in the text

   overriding
   procedure Execute
     (This         : Change_To_Tick_Valid_Cmd;
      Current_Text : in out Text_Navigator_Abstr'Class);

   -------------------------------
   -- Add_Record_Rep_Clause_Cmd --
   -------------------------------

   type Add_Record_Rep_Clause_Cmd is new Text_Command (Simple) with private;

   procedure Initialize
     (This          : in out Add_Record_Rep_Clause_Cmd;
      Current_Text  : Text_Navigator_Abstr'Class;
      Cursor        : File_Cursor'Class;
      First_Clause  : Unbounded_String;
      Second_Clause : Unbounded_String := Null_Unbounded_String;
      With_Clause   : Unbounded_String := Null_Unbounded_String);
   --  Add the record representation clauses First_Clause and Second_Clause
   --  after the end of the full record type declaration. If With_Clause is
   --  specified then append it to the list of the with clauses of the file.

   overriding
   procedure Execute
     (This         : Add_Record_Rep_Clause_Cmd;
      Current_Text : in out Text_Navigator_Abstr'Class);

   -----------------------------
   -- Remove_Extra_Underlines --
   -----------------------------

   type Remove_Extra_Underlines_Cmd is new Text_Command (Simple) with private;

   procedure Initialize
     (This           : in out Remove_Extra_Underlines_Cmd;
      Current_Text   : Text_Navigator_Abstr'Class;
      Cursor         : File_Cursor'Class);
   --  Add the missing clause in the text

   overriding
   procedure Execute
     (This         : Remove_Extra_Underlines_Cmd;
      Current_Text : in out Text_Navigator_Abstr'Class);

   ---------------------------
   -- Remove_Pragma_Element --
   ---------------------------

   type Remove_Pragma_Element_Cmd is new Text_Command (Simple) with private;

   procedure Initialize
     (This         : in out Remove_Pragma_Element_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      Cursor       : File_Cursor'Class;
      Element_Name : String;
      Pragma_Name  : String);

   overriding
   procedure Execute
     (This         : Remove_Pragma_Element_Cmd;
      Current_Text : in out Text_Navigator_Abstr'Class);

   ----------------------------
   -- Remove_Parenthesis_Cmd --
   ----------------------------

   type Remove_Parenthesis_Cmd is new Text_Command (Simple) with private;
   --  Removes a block of parenthesis, e.g. ((a)) -> (a)

   procedure Initialize
     (This            : in out Remove_Parenthesis_Cmd;
      Current_Text    : Text_Navigator_Abstr'Class;
      Cursor          : File_Cursor'Class);

   overriding
   procedure Execute
     (This         : Remove_Parenthesis_Cmd;
      Current_Text : in out Text_Navigator_Abstr'Class);

   --------------------------
   -- Fix_Index_Number_Cmd --
   --------------------------

   type Fix_Index_Number_Cmd is new Text_Command (Simple) with private;
   --  Removes or adds the index number to array attributes

   type Fix_Index_Number_Cmd_Mode is (Remove, Add);

   procedure Initialize
     (This         : in out Fix_Index_Number_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      Cursor       : File_Cursor'Class;
      Mode         : Fix_Index_Number_Cmd_Mode);
   --  Either removes the index or add the index for the array attribute

   overriding procedure Execute
     (This         : Fix_Index_Number_Cmd;
      Current_Text : in out Text_Navigator_Abstr'Class);

   ----------------------------
   -- Reorder_Subprogram_Cmd --
   ----------------------------

   type Reorder_Subprogram_Cmd is new Text_Command (Simple) with private;
   --  Alphabetically Reorders a subprogram in the subprogram list

   procedure Initialize
     (This         : in out Reorder_Subprogram_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      Cursor       : File_Cursor'Class);
   --  Either removes the index or add the index for the array attribute

   overriding
   procedure Execute
     (This         : Reorder_Subprogram_Cmd;
      Current_Text : in out Text_Navigator_Abstr'Class);

   ---------------------------
   -- Replace_Attribute_Cmd --
   ---------------------------

   type Replace_Attribute_Cmd is new Text_Command (Simple) with private;

   procedure Initialize
     (This         : in out Replace_Attribute_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      Cursor       : File_Cursor'Class;
      Replace_By   : String);
   --  When placed on a tick character, remove the attribute reference, e.g.
   --  useless 'Base and insert Replace_By there.

   overriding procedure Execute
     (This         : Replace_Attribute_Cmd;
      Current_Text : in out Text_Navigator_Abstr'Class);

   --------------------------
   -- Remove_Attribute_Cmd --
   --------------------------

   type Renames_To_Constant_Cmd is new Text_Command (Simple) with private;

   procedure Initialize
     (This         : in out Renames_To_Constant_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      Cursor       : File_Cursor'Class);
   --  Modifies a V : T renames X; to V : constant T := X;

   overriding procedure Execute
     (This         : Renames_To_Constant_Cmd;
      Current_Text : in out Text_Navigator_Abstr'Class);

   ---------------------------
   -- Remove_Comparison_Cmd --
   ---------------------------

   type Remove_Comparison_Cmd is new Text_Command (Simple) with private;

   procedure Initialize
     (This         : in out Remove_Comparison_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      Cursor       : File_Cursor'Class);
   --  Removes the redundant comparison, namely = True or /= True

   overriding procedure Execute
     (This         : Remove_Comparison_Cmd;
      Current_Text : in out Text_Navigator_Abstr'Class);

   ---------------------------
   -- Named_Association_Cmd --
   ---------------------------

   type Named_Association_Cmd is new Text_Command (Simple) with private;

   procedure Initialize
     (This         : in out Named_Association_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      Cursor       : File_Cursor'Class;
      Name         : String);
   --  Force named association (Name => ...) at given position

   overriding procedure Execute
     (This         : Named_Association_Cmd;
      Current_Text : in out Text_Navigator_Abstr'Class);

private

   package Mark_List is new GPS_Vectors (Word_Mark);
   use Mark_List;

   type Recase_Word_Cmd is new Text_Command (Simple) with record
      Correct_Word : Unbounded_String;
      Word_Case    : Case_Type;
   end record;

   type Remove_Instruction_Cmd is new Text_Command (Complex) with null record;

   type Remove_Elements_Cmd is new Text_Command (Complex) with record
      Remove_List : Mark_List.Vector;
      Mode        : Remove_Code_Mode := Erase;
   end record;

   procedure Free (Item : in out Unbounded_String) is null;
   package String_List is
     new Ada.Containers.Vectors (Positive, Unbounded_String);
   use String_List;
   --  ??? Should use standard string list

   type Remove_Pkg_Clauses_Cmd is new Text_Command (Simple) with record
      Word_Str     : Unbounded_String;
      Position     : Relative_Position;
      Destination  : GNATCOLL.VFS.Virtual_File;
      Category     : Dependency_Category;
      Look_For_Use : Boolean;
   end record;

   type Remove_Entity_Cmd is new Text_Command (Complex) with record
      Mode : Remove_Code_Mode := Erase;
   end record;

   type Add_Pragma_Cmd is new Text_Command (Simple) with record
      Name     : Unbounded_String;
      Argument : Unbounded_String;
      Category : Language_Category;
   end record;

   type Make_Constant_Cmd is new Text_Command (Simple) with record
      Name : Unbounded_String;
   end record;

   type Remove_Conversion_Cmd is new Text_Command (Simple) with null record;

   type Paste_Profile_Cmd is new Text_Command (Complex) with record
      Look_For_Source, Look_For_Destination : Relative_Position;
      Source_Mark, Destination_Mark         : Ptr_Mark;
   end record;

   type Get_Visible_Declaration_Cmd_Mode is (Prefix, Add_Use);

   type Get_Visible_Declaration_Cmd is new Text_Command (Complex) with record
      Mode             : Get_Visible_Declaration_Cmd_Mode;
      Source_Position  : Ptr_Mark;
      File_Destination : GNATCOLL.VFS.Virtual_File;
      Object_Position  : Ptr_Mark;
      With_Could_Miss  : Boolean := False;
   end record;

   type Indent_Code_Cmd is new Text_Command (Simple) with record
      Force_Column : Visible_Column_Type;
   end record;

   type Add_Clauses_Cmd is new Text_Command (Simple) with record
      File           : Virtual_File;
      Missing_Clause : Unbounded_String;
      Add_With       : Boolean;
      Add_Use        : Boolean;
   end record;

   type Change_To_Tick_Valid_Cmd is new Text_Command (Simple) with null record;

   type Add_Record_Rep_Clause_Cmd is new Text_Command (Simple) with record
      First_Clause  : Unbounded_String;
      Second_Clause : Unbounded_String;
      With_Clause   : Unbounded_String;
      File          : Virtual_File;
   end record;

   type Remove_Extra_Underlines_Cmd is
     new Text_Command (Simple) with null record;

   type Remove_Pragma_Element_Cmd is new Text_Command (Simple) with record
      Element_Name : Unbounded_String;
      Pragma_Name  : Unbounded_String;
   end record;

   type Remove_Parenthesis_Cmd is new Text_Command (Simple) with null record;

   type Fix_Index_Number_Cmd is new Text_Command (Simple) with record
      Mode : Fix_Index_Number_Cmd_Mode;
   end record;

   type Reorder_Subprogram_Cmd is new Text_Command (Simple) with null record;

   type Replace_Attribute_Cmd is new Text_Command (Simple) with record
      Replace_By : Unbounded_String;
   end record;

   type Renames_To_Constant_Cmd is new Text_Command (Simple) with null record;

   type Remove_Comparison_Cmd is new Text_Command (Simple) with null record;

   type Named_Association_Cmd is new Text_Command (Simple) with record
      Name : Unbounded_String;
   end record;

end Codefix.Text_Manager.Ada_Commands;
