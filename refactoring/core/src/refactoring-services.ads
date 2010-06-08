-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                 Copyright (C) 2003-2010, AdaCore                  --
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

--  This package provides a number of services to query and modify source
--  code. These operations occur at the syntactic and semantic levels (as
--  opposed to the GPS.Editors API, for instance, which operates at the
--  character level).
--  The subprograms in this package never create Undo_Groups, it is the
--  responsibility of the caller.

with Ada.Strings.Unbounded;
with Basic_Types;
with Entities.Queries;
with GNATCOLL.VFS;

package Refactoring.Services is

   ------------------------------------
   -- Constructs <-> Entities bridge --
   ------------------------------------
   --  The following queries are implemented in terms of both the construct
   --  database and the entities database.

   function Get_Entity_Access
     (Context : not null access Factory_Context_Record'Class;
      Entity  : Entities.Entity_Information)
      return Language.Tree.Database.Entity_Access;
   --  Return a pointer to the declaration of Entity. This pointer can be used
   --  to retrieve additional data about the entity (read directly from the
   --  source file).
   --  This returns a pointer to the first declaration (aka "public view") of
   --  the entity. You might need to call Get_Last_Visible_Declaration if you
   --  want the declaration as visible from a specific part of the code (this
   --  could for instance be the declaration in the private part).
   --  Returns Null_Entity_Access if this could not be retrieved.

   function Accepts_Primitive_Ops
     (Context        : not null access Factory_Context_Record'Class;
      Entity         : Entities.Entity_Information;
      Current_Offset : Basic_Types.String_Index_Type) return Boolean;
   --  Whether the entity is an instance of a class or interface.
   --  This returns null for a "'Class".

   procedure Is_Parameter_Of
     (Entity       : Entities.Entity_Information;
      Is_Parameter : out Boolean;
      PType        : out Entities.Queries.Parameter_Type);
   --  Whether Entity is a parameter for a subprogram, and if it is which type

   -------------------------
   -- Entity declarations --
   -------------------------

   type Entity_Declaration is tagged private;
   No_Entity_Declaration : constant Entity_Declaration;

   function Get_Declaration
     (Context : not null access Factory_Context_Record'Class;
      Entity  : Entities.Entity_Information) return Entity_Declaration;
   --  Return the declaration of the entity. From this, one can extract the
   --  initial value,  the type (as set by the user, whether it is a constant,
   --  and other attributes).

   procedure Free (Self : in out Entity_Declaration);
   --  Free the memory used by Self

   function Initial_Value (Self : Entity_Declaration) return String;
   --  Return the initial value of the entity, as set in its declaration. For
   --  instance, if the entity is declared as
   --     A : Integer := 2 + 3;
   --  then the initial_value is "2 + 3".
   --  The empty string is returned if no initial value was specified

   function Display_As_Parameter
     (Self    : Entity_Declaration;
      Context : not null access Factory_Context_Record'Class;
      PType   : Entities.Queries.Parameter_Type) return String;
   --  Return the declaration of the entity as it should be displayed in a
   --  parameter list. This includes the name of the variable.

   function Display_As_Variable
     (Self  : Entity_Declaration) return String;
   --  Return the declaration of the entity as it should be displayed in a
   --  variable declaration. This includes the name of the variable

   procedure Create_Marks
     (Self   : in out Entity_Declaration;
      Buffer : GPS.Editors.Editor_Buffer'Class);
   --  Creates marks in the editor corresponding to the declaration of the
   --  entity.
   --  The result must be freed by the user.

   procedure Remove (Self : Entity_Declaration);
   --  Remove the declaration of the entity from the source file.
   --  You must have called Create_Marks first.

   -------------------
   -- Range of code --
   -------------------

   type Range_Of_Code is new With_Factory with private;
   Empty_Range_Of_Code : constant Range_Of_Code;

   function Create_Range
     (Context     : not null access Factory_Context_Record'Class;
      File        : GNATCOLL.VFS.Virtual_File;
      From_Line   : Integer;
      To_Line     : Integer) return Range_Of_Code;
   --  Create a range of code (ie the part of the code delimited by two lines).

   function File      (Self : Range_Of_Code) return GNATCOLL.VFS.Virtual_File;
   function From_Line (Self : Range_Of_Code) return Integer;
   function To_Line   (Self : Range_Of_Code) return Integer;
   --  Return the various components of the range

   procedure Get_Parent
     (Self   : in out Range_Of_Code;
      Parent : out Entities.Entity_Information);
   --  Return the entity that contains the whole range of the code (in general
   --  the subprogram that includes from From_Line..To_Line).
   --  If both ends of the code are not contained within the same entity, an
   --  error is reported to the context, and No_Entity_Information is returned.

   type Entity_References_Flag is
     (Flag_Modified,
      Flag_Read,
      --  Whether the entity is modified or read in the range of code

      Flag_Ref_Outside_Parent,
      --  Whether the entity is referenced outside of the function containing
      --  the range of code

      Flag_Read_Before, Flag_Modified_Before,
      --  Whether the entity is modified or read before the range of code, but
      --  within the same function.

      Flag_Read_After, Flag_Modified_After
      --  Whether the entity is modified or read after the range of code, but
      --  within the same function
     );

   type Entity_References_Flags is array (Entity_References_Flag) of Boolean;

   procedure For_All_Variable_In_Range
     (Self               : in out Range_Of_Code;
      Callback           : not null access procedure
        (Entity : Entities.Entity_Information;
         Flags  : Entity_References_Flags);
      Success            : out Boolean;
      Omit_Library_Level : Boolean := False);
   --  For each entity references within the given range of code, calls
   --  Callback. The callback is never called for library level entities if
   --  Omit_Library_Level is True.
   --  The callback receives information on where else the entity is referenced
   --  and on how it is used within the selected chunk of code.
   --  Success is set to False if not all entities could be examined because an
   --  error occurred. In such a case, the error has already been reported to
   --  the context.

   -----------------
   -- Subprograms --
   -----------------

   procedure Insert_Subprogram_Declaration
     (Context  : not null access Factory_Context_Record'Class;
      In_File  : GNATCOLL.VFS.Virtual_File;
      Decl     : String;
      Category : String := "");
   --  Insert the subprogram declaration in In_File at an appropriate place.
   --  If Category is specified, Report_Location is called for the context to
   --  report the change to the user.

   procedure Insert_Subprogram_Body
     (Context     : not null access Factory_Context_Record'Class;
      In_File     : GNATCOLL.VFS.Virtual_File;
      Name        : String;
      Code        : String;
      Before_Line : Integer := Integer'Last;
      Category    : String := "");
   --  Insert the body for a subprogram at an appropriate location in In_File.
   --  If Before_Line is specified, the insertion must occur before that line.
   --  Name is the name of the subprogram (so that we can possibly insert a
   --  subprogram box before it if the user wants one.

   -------------
   -- Editors --
   -------------
   --  The following subprograms are rather lower level than the ones above,
   --  and are used to provide a slightly more convenient interface to editors.
   --  When possible, the above subprograms should be preferred.

   function Insert_Text
     (Context                   : not null access Factory_Context_Record'Class;
      In_File                   : GNATCOLL.VFS.Virtual_File;
      Line                      : Integer;
      Column                    : Basic_Types.Visible_Column_Type := 1;
      Text                      : String;
      Indent                    : Boolean;
      Skip_Comments_Backward    : Boolean := False;
      Surround_With_Blank_Lines : Boolean := False;
      Replaced_Length           : Integer := 0;
      Only_If_Replacing         : String := "") return Boolean;
   --  Insert some text in a source file.
   --  If Indent is True, the text is indented automatically.
   --  Replaced_Length is the number of characters that should first be removed
   --  to be replaced by Text.
   --  If Only_If_Replacing is specified, then the replacement of text will be
   --  done only if the text being replaced is Only_If_Replacing (case
   --  insensitive). If it isn't, False is returned.
   --  If Skip_Comments_Backward is True, then the actual insertion will occur
   --  on the first line before any comment lines preceding Line.
   --  If Surround_With_Blank_Lines is True, then the inserted text must end up
   --  with a blank line before and after it (so lines are inserted as needed).
   --  This function returns True if the new text could be inserted.

private

   type Editor_Mark_Access is access all GPS.Editors.Editor_Mark'Class;

   type Entity_Declaration is tagged record
      Entity : Entities.Entity_Information;
      Decl   : Ada.Strings.Unbounded.Unbounded_String;

      SFirst, SLast : Language.Source_Location;
      First, Last   : Editor_Mark_Access;
      --  From the start of the entity name to the ";"

      Shared : Boolean;
      --  Whether multiple entities share the same declaration

      Equal_Loc : Integer := -1;
      --  Location of ":=" in Decl
   end record;

   No_Entity_Declaration : constant Entity_Declaration :=
     (Entity    => null,
      Equal_Loc => -1,
      SFirst    => <>,
      SLast     => <>,
      First     => null,
      Last      => null,
      Shared    => False,
      Decl      => Ada.Strings.Unbounded.Null_Unbounded_String);

   type Range_Of_Code is new With_Factory with record
      File        : GNATCOLL.VFS.Virtual_File;
      Source      : Entities.Source_File;
      From_Line   : Integer;
      To_Line     : Integer;
      Parent      : Entities.Entity_Information;
   end record;

   Empty_Range_Of_Code : constant Range_Of_Code :=
     (null, GNATCOLL.VFS.No_File, null, -1, -1, null);
end Refactoring.Services;
