-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2003-2004                       --
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

with Basic_Types;
with Entities;          use Entities;
with Entities.Queries;  use Entities.Queries;
with VFS;               use VFS;
with Traces;            use Traces;
with Projects;          use Projects;
with Projects.Editor;   use Projects.Editor;
with Projects.Registry; use Projects.Registry;
with Glib.Convert;      use Glib.Convert;
with File_Utils;        use File_Utils;

with Ada.Calendar;   use Ada.Calendar;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Unchecked_Conversion;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with GNAT.Case_Util;        use GNAT.Case_Util;
with GNAT.OS_Lib;    use GNAT.OS_Lib;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;

with ALI;            use ALI;
with Types;          use Types;
with Namet;          use Namet;

package body ALI_Parser is
   Me        : constant Debug_Handle := Create ("ALI", Off);
   Assert_Me : constant Debug_Handle := Create ("ALI.Assert", Off);


   type ALI_Handler_Record is new LI_Handler_Record with record
      Db       : Entities_Database;
      Registry : Project_Registry;
   end record;
   type ALI_Handler is access all ALI_Handler_Record'Class;

   function Get_Source_Info
     (Handler               : access ALI_Handler_Record;
      Source_Filename       : VFS.Virtual_File;
      File_Has_No_LI_Report : File_Error_Reporter := null) return Source_File;
   function Case_Insensitive_Identifiers
     (Handler : access ALI_Handler_Record) return Boolean;
   function Parse_All_LI_Information
     (Handler   : access ALI_Handler_Record;
      Project   : Projects.Project_Type;
      Recursive : Boolean := False) return Integer;
   function Generate_LI_For_Project
     (Handler   : access ALI_Handler_Record;
      Project   : Projects.Project_Type;
      Recursive : Boolean := False) return LI_Handler_Iterator'Class;
   --  See doc for inherited subprograms

   type ALI_Handler_Iterator is new LI_Handler_Iterator with null record;
   procedure Continue
     (Iterator : in out ALI_Handler_Iterator; Finished : out Boolean);
   procedure Destroy (Iterator : in out ALI_Handler_Iterator);
   --  See doc for inherited subprograms


   type E_Kind_To_Char_Map is array (Character range '+' .. 'z') of E_Kind;
   E_Kind_To_Char : constant E_Kind_To_Char_Map :=
     ('a'    => (Array_Kind,             False, False, False),
      'A'    => (Array_Kind,             False, True,  False),
      'b'    => (Boolean_Kind,           False, False, False),
      'B'    => (Boolean_Kind,           False, True,  False),
      'c'    => (Class_Wide,             False, False, False),
      'C'    => (Class_Wide,             False, True,  False),
      'd'    => (Decimal_Fixed_Point,    False, False, False),
      'D'    => (Decimal_Fixed_Point,    False, True,  False),
      'e'    => (Enumeration_Kind,       False, False, False),
      'E'    => (Enumeration_Kind,       False, True,  False),
      'f'    => (Floating_Point,         False, False, False),
      'F'    => (Floating_Point,         False, True,  False),
      'g'    => Unresolved_Entity_Kind,
      'G'    => Unresolved_Entity_Kind,
      'h'    => Unresolved_Entity_Kind,
      'H'    => (Record_Kind,            False, True,  True),
      'i'    => (Signed_Integer,         False, False, False),
      'I'    => (Signed_Integer,         False, True,  False),
      'j'    => Unresolved_Entity_Kind,
      'J'    => Unresolved_Entity_Kind,
      'k'    => (Package_Kind,           True,  True, False),
      'K'    => (Package_Kind,           False, True, False),
      'l'    => (Label_On_Loop,          False, False, False),
      'L'    => (Label_On_Statement,     False, False, False),
      'm'    => (Modular_Integer,        False, False, False),
      'M'    => (Modular_Integer,        False, True,  False),
      'n'    => (Enumeration_Literal,    False, False, False),
      'N'    => (Named_Number,           False, False, False),
      'o'    => (Ordinary_Fixed_Point,   False, False, False),
      'O'    => (Ordinary_Fixed_Point,   False, True,  False),
      'p'    => (Access_Kind,            False, False, False),
      'P'    => (Access_Kind,            False, True,  False),
      'q'    => (Label_On_Block,         False, False, False),
      'Q'    => Unresolved_Entity_Kind,
      'r'    => (Record_Kind,            False, False, False),
      'R'    => (Record_Kind,            False, True,  False),
      's'    => (String_Kind,            False, False, False),
      'S'    => (String_Kind,            False, True,  False),
      't'    => (Task_Kind,              False, False, False),
      'T'    => (Task_Kind,              False, True,  False),
      'u'    => (Procedure_Kind,         True,  True,  False),
      'U'    => (Procedure_Kind,         False, True,  False),
      'v'    => (Function_Or_Operator,   True,  True,  False),
      'V'    => (Function_Or_Operator,   False, True,  False),
      'w'    => (Protected_Kind,         False, False, False),
      'W'    => (Protected_Kind,         False, True,  False),
      'x'    => (Procedure_Kind,         False, True,  True),
      'X'    => (Exception_Entity,       False, False, False),
      'y'    => (Function_Or_Operator,   False, True,  True),
      'Y'    => (Entry_Or_Entry_Family,  False, True,  True),
      'z'    => Unresolved_Entity_Kind, --  ??? Formal of current subprogram
      'Z'    => Unresolved_Entity_Kind,
      '+'    => (Private_Type,           False, True,  False),
      others => Unresolved_Entity_Kind);

   Char_To_Reference_Kind : constant array (Character range ' ' .. 'z')
     of Reference_Kind :=
     (' '    => Instantiation_Reference,
      '<'    => Subprogram_Out_Parameter,
      '='    => Subprogram_In_Out_Parameter,
      '>'    => Subprogram_In_Parameter,
      '^'    => Subprogram_Access_Parameter,
      'b'    => Body_Entity,
      'c'    => Completion_Of_Private_Or_Incomplete_Type,
      'd'    => Discriminant,
      'e'    => End_Of_Spec,
      'i'    => Implicit,
      'k'    => Parent_Package,
      'l'    => Label,
      'm'    => Modification,
      'p'    => Primitive_Operation,
      'P'    => Overriding_Primitive_Operation,
      'r'    => Reference,
      't'    => End_Of_Body,
      'w'    => With_Line,
      'x'    => Type_Extension,
      'z'    => Formal_Generic_Parameter,
      others => Reference);
   --  Conversion from characters read in ALI files to the reference kind. See
   --  the function Char_To_R_Kind

   type Source_Dependency is record
      File        : Source_File;
      Is_Separate : Boolean;
      Is_Unit     : Boolean;
   end record;

   type Sdep_To_Sfile_Table is array (Sdep_Id range <>) of Source_Dependency;
   type Unit_To_Sfile_Table is array (Unit_Id range <>) of Source_File;

   procedure Create_New_ALI
     (Handler               : access ALI_Handler_Record'Class;
      LI                    : LI_File;
      New_ALI               : ALIs_Record;
      First_Sect, Last_Sect : Nat);
   --  Parse an ALI file and adds its information into the structure

   procedure Process_Units
     (LI : LI_File; New_ALI : ALIs_Record; Sunits : out Unit_To_Sfile_Table);
   --  Get a handle for all the units in New_ALI

   function Process_Unit (LI : LI_File; Id : Unit_Id) return Source_File;
   --  Return a handle to the file matching Id.

   procedure Process_Sdep
     (LI      : LI_File;
      Dep_Id  : Sdep_Id;
      Sunits  : Unit_To_Sfile_Table;
      Sfile   : in out Source_Dependency);
   --  Return a handle to a specific file dependency (Dep).

   procedure Process_Sdeps
     (LI               : LI_File;
      New_ALI          : ALIs_Record;
      Sunits           : Unit_To_Sfile_Table;
      Sfiles           : out Sdep_To_Sfile_Table);
   --  Get a handle for all the units dependencies

   procedure Process_Withs_For_Unit
     (Unit              : Unit_Id;
      File              : Source_File;
      Deps              : Sdep_To_Sfile_Table;
      Imported_Projects : Project_Type_Array);
   --  Register the dependencies for all files in Deps into File.

   procedure Process_Withs
     (Sunits            : Unit_To_Sfile_Table;
      Deps              : Sdep_To_Sfile_Table;
      Imported_Projects : Project_Type_Array);
   --  Register the dependencies between all the files referenced in LI

   procedure Load_And_Scan_ALI
     (ALI_Filename          : Virtual_File;
      Reset_First           : Boolean;
      Result                : out ALI_Id;
      First_Sect, Last_Sect : out Nat);
   --  Parse the given file. No_ALI_Id is returned if the file couldn't be
   --  parsed.
   --  The internal GNAT's ALI tables are cleared first if Reset_First is True
   --  First_Sect .. Last_Sect are the specific parts of the Xref_Section
   --  table that contain cross-reference information.

   function Filename_From_Unit
     (Unit              : Unit_Name_Type;
      Imported_Projects : Project_Type_Array) return Name_Id;
   --  Convert from a Unit specification (unit%s or unit%b) to a filename

   function Imported_Projects_Count (Project : Project_Type) return Natural;
   --  Return the number of projects imported by Project

   procedure Get_Imported_Projects
     (Project  : Project_Type;
      Imported : out Project_Type_Array);
   --  Get the projects imported by Project. Imported must have the size
   --  returned by Imported_Projects_Count.

   procedure Process_Xref_Entity
     (Handler               : access ALI_Handler_Record'Class;
      LI                    : LI_File;
      Xref_Sect             : Nat;
      Xref_Ent              : Nat;
      Sfiles                : Sdep_To_Sfile_Table;
      First_Sect, Last_Sect : Nat);
   --  Save the Xref Entity information in the New_LI_File structure.

   procedure Process_Xref_Section
     (Handler               : access ALI_Handler_Record'Class;
      LI                    : LI_File;
      Xref_Sect             : Nat;
      Sfiles                : Sdep_To_Sfile_Table;
      First_Sect, Last_Sect : Nat);
   --  Save the Xref information associated to the given With_Record.

   procedure Process_Xrefs
     (Handler               : access ALI_Handler_Record'Class;
      LI                    : LI_File;
      Sfiles                : Sdep_To_Sfile_Table;
      First_Sect, Last_Sect : Nat);
   --  Save the Xref information in the New_LI_File structure.

   function Update_ALI
     (Handler   : access ALI_Handler_Record'Class; LI : LI_File;
      Reset_ALI : Boolean) return Boolean;
   --  Re-parse the contents of the ALI file, and return True in case of
   --  success.

   function Char_To_E_Kind (C : Character) return E_Kind;
   pragma Inline (Char_To_E_Kind);
   --  Translate the given character into the associated E_Kind value.
   --  Raise ALI_Internal_Error if C does not represent any E_Kind value.

   function Char_To_R_Kind (C : Character) return Reference_Kind;
   pragma Inline (Char_To_R_Kind);
   --  Translate the given character into the associated Reference_Kind value.
   --  Raise ALI_Internal_Error if C does not represent any Reference_Kind.

   procedure Process_Type_Ref
     (Handler               : access ALI_Handler_Record'Class;
      LI                    : LI_File;
      Entity                : Entity_Information;
      Xref_Ent              : Nat;
      Sfiles                :  Sdep_To_Sfile_Table;
      First_Sect, Last_Sect : Nat);
   --  Process the parent type of an entity declared in Xref_Ent.

   function Find_Entity_In_ALI
     (Handler               : access ALI_Handler_Record'Class;
      LI                    : LI_File;
      Sfiles                : Sdep_To_Sfile_Table;
      File_Num              : Sdep_Id;
      Line                  : Nat;
      Column                : Nat;
      First_Sect, Last_Sect : Nat) return Entity_Information;
   --  Find or create an entity information based on the information contained
   --  in the current LI. This returns a placeholder for the declaration, but
   --  no specific information has been set

   procedure Process_Renaming_Ref
     (Handler               : access ALI_Handler_Record'Class;
      LI                    : LI_File;
      Entity                : Entity_Information;
      Xref_Sect             : Nat;
      Xref_Ent              : Nat;
      Sfiles                : Sdep_To_Sfile_Table;
      First_Sect, Last_Sect : Nat);
   --  Process the renaming information in the file

   procedure Process_Entity_Ref
     (Handler               : access ALI_Handler_Record'Class;
      LI                    : LI_File;
      Entity                : Entity_Information;
      Sfiles                : Sdep_To_Sfile_Table;
      Current_Ref           : Nat;
      Current_Sfile         : in out Sdep_Id;
      First_Sect, Last_Sect : Nat);
   --  Process a reference to the entity

   function LI_Filename_From_Source
     (Source_Filename : Virtual_File;
      Project         : Project_Type) return Virtual_File;
   --  Return the ALI file associated with Source_Filename

   function Locate_ALI
     (Short_ALI_Filename : String;
      Project            : Project_Type) return VFS.Virtual_File;
   --  Search for the full name of the ALI file. We also search the parent
   --  unit's ALi file, in case the file is a separate.

   function Get_Source_Info_Internal
     (Handler               : access ALI_Handler_Record'Class;
      Source_Filename       : VFS.Virtual_File;
      File_Has_No_LI_Report : File_Error_Reporter := null;
      Reset_ALI             : Boolean) return Source_File;
   --  Same as Get_Source_Info, but it is possible not to reset the internal
   --  GNAT tables first. This must be used when calling this recursively

   function Get_ALI_Filename (Base_Name : String) return String;
   --  Return the most likely candidate for an ALI file, given a source name

   --------------------
   -- Char_To_E_Kind --
   --------------------

   function Char_To_E_Kind (C : Character) return E_Kind is
      pragma Suppress (All_Checks);
   begin
      if C in E_Kind_To_Char'Range then
         return E_Kind_To_Char (C);
      else
         --  If we reach this point, the character is illegal.
         Trace (Me, "Char_To_E_Kind: Invalid character '" & C & ''');
         return Unresolved_Entity_Kind;
      end if;
   end Char_To_E_Kind;

   --------------------
   -- Char_To_R_Kind --
   --------------------

   function Char_To_R_Kind (C : Character) return Reference_Kind is
      pragma Suppress (All_Checks);
   begin
      if C in Char_To_Reference_Kind'Range then
         return Char_To_Reference_Kind (C);
      else
         --  If we reach this point, the character is illegal.
         Trace (Me, "Char_To_R_Kind: Invalid character '" & C & ''');
         return Reference;
      end if;
   end Char_To_R_Kind;

   ------------------
   -- Process_Unit --
   ------------------

   function Process_Unit (LI : LI_File; Id : Unit_Id) return Source_File is
      Base_Name     : constant String :=
        Locale_To_UTF8 (Get_String (Units.Table (Id).Sfile));
      File          : Source_File;
      Unit          : constant String := Get_String (Units.Table (Id).Uname);
   begin
      Assert (Assert_Me, LI /= null, "Null LI file parsed");

      File := Get_Or_Create
        (Db            => Get_Database (LI),
         Full_Filename => Base_Name,
         LI            => LI);
      Set_Time_Stamp (File, VFS.No_Time);

      --  Strip the %s or %b terminator
      Set_Unit_Name (File, Unit (Unit'First .. Unit'Last - 2));

      return File;
   end Process_Unit;

   -------------------
   -- Process_Units --
   -------------------

   procedure Process_Units
     (LI      : LI_File;
      New_ALI : ALIs_Record;
      Sunits  : out Unit_To_Sfile_Table) is
   begin
      for Current_Unit_Id in New_ALI.First_Unit .. New_ALI.Last_Unit loop
         --  It might happen that a unit is given twice, for library-level
         --  generic packages. In this case, we only want the first one
         --  which is more complete anyway. This avoids duplication
         if Current_Unit_Id = New_ALI.First_Unit
           or else Units.Table (Current_Unit_Id).Sfile /=
              Units.Table (Current_Unit_Id - 1).Sfile
         then
            Sunits (Current_Unit_Id) := Process_Unit (LI, Current_Unit_Id);
         end if;
      end loop;
   end Process_Units;

   ------------------
   -- Process_Sdep --
   ------------------

   procedure Process_Sdep
     (LI      : LI_File;
      Dep_Id  : Sdep_Id;
      Sunits  : Unit_To_Sfile_Table;
      Sfile   : in out Source_Dependency)
   is
      Dep         : Sdep_Record renames Sdep.Table (Dep_Id);
      Is_Separate : constant Boolean := Dep.Subunit_Name /= No_Name;
      Base_Name   : constant String := Locale_To_UTF8 (Get_String (Dep.Sfile));
      L           : LI_File := LI;
   begin
      --  Do we have one of the files belonging to LI itself ?
      for Current_Unit in Sunits'Range loop
         if Units.Table (Current_Unit).Sfile = Dep.Sfile then
            --  ??? Check the original file name for gnatchoped files:
            --  Dep.Rfile and Dep.Start_Line
            Sfile := (Sunits (Current_Unit),
                      Is_Separate => False,
                      Is_Unit     => True);
            return;
         end if;
      end loop;

      if not Is_Separate then
         --  We do not know its ALI file
         L := null;
      end if;

      Sfile := (Get_Or_Create
                  (Db            => Get_Database (LI),
                   Full_Filename => Base_Name,
                   LI            => L),
                Is_Separate => Is_Separate,
                Is_Unit     => False);

      if Is_Separate then
         Set_Time_Stamp (Sfile.File, VFS.No_Time);
      end if;
   end Process_Sdep;

   -------------------
   -- Process_Sdeps --
   -------------------

   procedure Process_Sdeps
     (LI      : LI_File;
      New_ALI : ALIs_Record;
      Sunits  : Unit_To_Sfile_Table;
      Sfiles  : out Sdep_To_Sfile_Table) is
   begin
      for Dep_Id in New_ALI.First_Sdep .. New_ALI.Last_Sdep loop
         Process_Sdep (LI, Dep_Id, Sunits, Sfiles (Dep_Id));
      end loop;
   end Process_Sdeps;

   ------------------------
   -- Filename_From_Unit --
   ------------------------

   function Filename_From_Unit
     (Unit              : Unit_Name_Type;
      Imported_Projects : Project_Type_Array) return Name_Id
   is
      Unit_Name : constant String := Get_String (Unit);
      Is_Spec   : constant Boolean := Unit_Name (Unit_Name'Last) = 's';
      Part      : Unit_Part;
   begin
      --  Parse the imported projects in the reverse order, since we must find
      --  the unit in the top-most project, for instance in case it was
      --  overriden in extending projects

      if Is_Spec then
         Part := Unit_Spec;
      else
         Part := Unit_Body;
      end if;

      for P in reverse Imported_Projects'Range loop
         declare
            N : constant String := Get_Filename_From_Unit
              (Imported_Projects (P),
               Unit_Name (1 .. Unit_Name'Last - 2),
               Part);
         begin
            if N /= "" then
               return Get_String (N);
            end if;
         end;
      end loop;

      --  We end up here for the runtime files, so we just try to append the
      --  standard GNAT extensions.

      return Get_String
        (Get_Filename_From_Unit
           (Imported_Projects (Imported_Projects'First),
            Unit_Name (1 .. Unit_Name'Last - 2),
            Part,
            Check_Predefined_Library => True));
   end Filename_From_Unit;

   ----------------------------
   -- Process_Withs_For_Unit --
   ----------------------------

   procedure Process_Withs_For_Unit
     (Unit              : Unit_Id;
      File              : Source_File;
      Deps              : Sdep_To_Sfile_Table;
      Imported_Projects : Project_Type_Array)
   is
      Has_With   : array (Deps'Range) of Boolean := (others => False);
      With_Files : array (Units.Table (Unit).First_With ..
                            Units.Table (Unit).Last_With) of Name_Id;
   begin
      for W in With_Files'Range loop
         With_Files (W) := Filename_From_Unit
           (Withs.Table (W).Uname, Imported_Projects);
      end loop;

      if Units.Table (Unit).First_With /= No_With_Id then
         for D in Deps'Range loop
            for W in With_Files'Range loop
               if Sdep.Table (D).Sfile = With_Files (W) then
                  Has_With (D) := True;
                  exit;
               end if;
            end loop;
         end loop;
      end if;

      for D in Deps'Range loop
         if Deps (D).File /= File then
            Add_Depends_On (File, Deps (D).File,
                            Explicit_Dependency => Has_With (D));
         end if;
      end loop;
   end Process_Withs_For_Unit;

   -------------------
   -- Process_Withs --
   -------------------

   procedure Process_Withs
     (Sunits            : Unit_To_Sfile_Table;
      Deps              : Sdep_To_Sfile_Table;
      Imported_Projects : Project_Type_Array) is
   begin
      for Unit in Sunits'Range loop
         if Sunits (Unit) /= null then
            Process_Withs_For_Unit
              (Unit, Sunits (Unit), Deps, Imported_Projects);
         end if;
      end loop;

      for Dep in Deps'Range loop
         if Deps (Dep).Is_Separate then
            for D in Deps'Range loop
               if not Deps (D).Is_Separate then
                  Add_Depends_On
                    (Deps (Dep).File, Deps (D).File,
                     Explicit_Dependency => False);
               end if;
            end loop;
         end if;
      end loop;
   end Process_Withs;

   -----------------------------
   -- Imported_Projects_Count --
   -----------------------------

   function Imported_Projects_Count (Project : Project_Type) return Natural is
      Count : Natural := 0;
      Iter  : Imported_Project_Iterator := Start (Project, Recursive => True);
   begin
      while Current (Iter) /= No_Project loop
         Count := Count + 1;
         Next (Iter);
      end loop;
      return Count;
   end Imported_Projects_Count;

   ---------------------------
   -- Get_Imported_Projects --
   ---------------------------

   procedure Get_Imported_Projects
     (Project  : Project_Type;
      Imported : out Project_Type_Array)
   is
      Iter : Imported_Project_Iterator := Start (Project, Recursive => True);
   begin
      for J in Imported'Range loop
         Imported (J) := Current (Iter);
         Next (Iter);
      end loop;
   end Get_Imported_Projects;

   -------------------------
   -- Process_Xref_Entity --
   -------------------------

   procedure Process_Xref_Entity
     (Handler               : access ALI_Handler_Record'Class;
      LI                    : LI_File;
      Xref_Sect             : Nat;
      Xref_Ent              : Nat;
      Sfiles                : Sdep_To_Sfile_Table;
      First_Sect, Last_Sect : Nat)
   is
      Entity         : Entity_Information;
      Current_Sfile  : Sdep_Id;
      File_Num       : constant Sdep_Id :=
                         Xref_Section.Table (Xref_Sect).File_Num;
      Has_Completion : Boolean := False;

      procedure Unchecked_Free (S : Basic_Types.Unchecked_String_Access);
      pragma Import (C, Unchecked_Free, "free");

   begin
      Get_Name_String (Xref_Entity.Table (Xref_Ent).Entity);

      declare
         Buffer : Basic_Types.Unchecked_String_Access;
         Read, Written : aliased Natural;
      begin
         if Name_Buffer (1) = '"' then
            To_Lower (Name_Buffer (2 .. Name_Len - 1));
            Buffer := Basic_Types.To_Unchecked_String (Locale_To_UTF8
              (Name_Buffer (2 .. Name_Len - 1),
               Read'Unchecked_Access, Written'Unchecked_Access));
         else
            To_Lower (Name_Buffer (1 .. Name_Len));
            Buffer := Basic_Types.To_Unchecked_String (Locale_To_UTF8
              (Name_Buffer (1 .. Name_Len),
               Read'Unchecked_Access, Written'Unchecked_Access));
         end if;

         Entity := Get_Or_Create
           (Name   => Buffer (1 .. Written),
            File   => Sfiles (File_Num).File,
            Line   => Integer (Xref_Entity.Table (Xref_Ent).Line),
            Column => Integer (Xref_Entity.Table (Xref_Ent).Col));
         Unchecked_Free (Buffer);
      end;

      Set_Kind
        (Entity, Char_To_E_Kind (Xref_Entity.Table (Xref_Ent).Etype));
      Set_Attributes
        (Entity,
         Attributes => (Global      => Xref_Entity.Table (Xref_Ent).Lib,
                        others      => False));

      --  Do not process types for entities in other ALI files,
      --  since most of the time the closure will not be in the current ALI
      --  file, and this will require us to parse too many files immediately

      if Get_LI (Sfiles (File_Num).File) = LI then
         if Xref_Entity.Table (Xref_Ent).Tref /= Tref_None then
            Process_Type_Ref
              (Handler, LI, Entity, Xref_Ent, Sfiles, First_Sect, Last_Sect);
         end if;

         if Xref_Entity.Table (Xref_Ent).Rref_Line /= 0 then
            Process_Renaming_Ref
              (Handler, LI, Entity, Xref_Sect, Xref_Ent, Sfiles,
               First_Sect, Last_Sect);
         end if;
      end if;

      Current_Sfile := File_Num;
      for Xref_Id in Xref_Entity.Table (Xref_Ent).First_Xref
         .. Xref_Entity.Table (Xref_Ent).Last_Xref
      loop
         if Char_To_R_Kind (Xref.Table (Xref_Id).Rtype) =
           Completion_Of_Private_Or_Incomplete_Type
         then
            Has_Completion := True;
         end if;

         Process_Entity_Ref
           (Handler, LI, Entity, Sfiles, Xref_Id, Current_Sfile,
            First_Sect, Last_Sect);
      end loop;

      --  Work around a bug (?) in GNAT, where an incomplete entity has a
      --  completion and an end-of-scope, but the latter is reported as
      --  End_Of_Spec when it should be End_Of_Body

      if Has_Completion then
         declare
            Location : File_Location;
            Kind     : Reference_Kind;
         begin
            Get_End_Of_Scope (Entity, Location, Kind);
            if Kind = End_Of_Spec then
               Set_End_Of_Scope (Entity, Location, End_Of_Body);
            end if;
         end;
      end if;
   end Process_Xref_Entity;

   ------------------------
   -- Process_Entity_Ref --
   ------------------------

   procedure Process_Entity_Ref
     (Handler               : access ALI_Handler_Record'Class;
      LI                    : LI_File;
      Entity                : Entity_Information;
      Sfiles                : Sdep_To_Sfile_Table;
      Current_Ref           : Nat;
      Current_Sfile         : in out Sdep_Id;
      First_Sect, Last_Sect : Nat)
   is
      Kind      : constant Reference_Kind := Char_To_R_Kind
        (Xref.Table (Current_Ref).Rtype);
      Location  : File_Location;
      Primitive : Entity_Information;

   begin
      --  ??? For the moment, ignore references to the instantiations
      if Kind /= Instantiation_Reference then
         Current_Sfile := Xref.Table (Current_Ref).File_Num;

         Location := (File   => Sfiles (Current_Sfile).File,
                      Line   => Integer (Xref.Table (Current_Ref).Line),
                      Column => Column_Type (Xref.Table (Current_Ref).Col));

         if Is_End_Reference (Kind) then
            --  The info for the body is always seen second, and will override
            --  the one for the spec
            declare
               Previous_End  : File_Location;
               Previous_Kind : Reference_Kind;
            begin
               Get_End_Of_Scope (Entity, Previous_End, Previous_Kind);
               Set_End_Of_Scope (Entity, Location, Kind);

               if Previous_End /= No_File_Location then
                  Add_Reference (Entity, Previous_End, Previous_Kind);
               end if;
            end;

         elsif Kind = Primitive_Operation
           or else Kind = Overriding_Primitive_Operation
         then
            Primitive := Find_Entity_In_ALI
              (Handler,
               LI, Sfiles, Current_Sfile, Xref.Table (Current_Ref).Line,
               Xref.Table (Current_Ref).Col, First_Sect, Last_Sect);

            if Primitive = null then
               Trace (Assert_Me, "Couldn't find primitive in ALI file: "
                      & Full_Name (Get_LI_Filename (LI)).all
                      & Current_Sfile'Img
                      & Xref.Table (Current_Ref).Line'Img
                      & Xref.Table (Current_Ref).Col'Img);
            else
               Add_Primitive_Subprogram (Entity, Primitive);
            end if;

         else
            Add_Reference (Entity, Location, Kind);
         end if;
      end if;
   end Process_Entity_Ref;

   ------------------------
   -- Find_Entity_In_ALI --
   ------------------------

   function Find_Entity_In_ALI
     (Handler               : access ALI_Handler_Record'Class;
      LI                    : LI_File;
      Sfiles                : Sdep_To_Sfile_Table;
      File_Num              : Sdep_Id;
      Line                  : Nat;
      Column                : Nat;
      First_Sect, Last_Sect : Nat) return Entity_Information
   is
      S      : Source_File;
      Entity : Entity_Information;
      Status : Find_Decl_Or_Body_Query_Status;
   begin
      for Sect in First_Sect .. Last_Sect loop
         --  Check declarations only in the current file
         if Xref_Section.Table (Sect).File_Num = File_Num then
            for Entity in Xref_Section.Table (Sect).First_Entity ..
              Xref_Section.Table (Sect).Last_Entity
            loop
               if Xref_Entity.Table (Entity).Line = Line
                 and then Xref_Entity.Table (Entity).Col = Column
               then
                  return Get_Or_Create
                    (Name => Locale_To_UTF8 (To_Lower
                        (Get_String (Xref_Entity.Table (Entity).Entity))),
                     File => Sfiles (File_Num).File,
                     Line => Integer (Line),
                     Column => Integer (Column));
               end if;
            end loop;
         end if;
      end loop;

      --  Do a separate loop for references, in case this was a reference to
      --  the declaration. It will be much faster in that case

      for Sect in First_Sect .. Last_Sect loop
         --  Check all references in the ALI file, since we can have:
         --     32i4 X{integer} 33m24 50m4
         --     33i4 Y=33:24{integer} 51r4
         for Entity in Xref_Section.Table (Sect).First_Entity ..
           Xref_Section.Table (Sect).Last_Entity
         loop
            for Ref in Xref_Entity.Table (Entity).First_Xref
              .. Xref_Entity.Table (Entity).Last_Xref
            loop
               if Xref.Table (Ref).File_Num = File_Num
                 and then Xref.Table (Ref).Line = Line
                 and then Xref.Table (Ref).Col = Column
               then
                  return Get_Or_Create
                    (Name => Locale_To_UTF8 (To_Lower
                        (Get_String (Xref_Entity.Table (Entity).Entity))),
                     File => Sfiles (Xref_Section.Table (Sect).File_Num).File,
                     Line => Integer (Xref_Entity.Table (Entity).Line),
                     Column => Integer (Xref_Entity.Table (Entity).Col));
               end if;
            end loop;
         end loop;
      end loop;

      if Active (Assert_Me) then
         Trace (Assert_Me,
                "Need to resolve closure: parsing "
                & Base_Name (Get_Filename (Sfiles (File_Num).File))
                & " at " & Line'Img & Column'Img);
      end if;

      S := Get_Source_Info_Internal
        (Handler               => Handler,
         Source_Filename       => Get_Filename (Sfiles (File_Num).File),
         File_Has_No_LI_Report => null,
         Reset_ALi             => False);
      Find_Declaration
        (Db              => Get_Database (LI),
         File_Name       => Get_Filename (S),
         Entity_Name     => "",  --  Unknown, we are looking for it
         Line            => Integer (Line),
         Column          => Integer (Column),
         Entity          => Entity,
         Status          => Status,
         Check_Decl_Only => False);

      if Status = Success or else Status = Fuzzy_Match then
         return Entity;
      else
         Trace (Assert_Me, "Couldn't resolve closure");
         return null;
      end if;
   end Find_Entity_In_ALI;

   --------------------------
   -- Process_Renaming_Ref --
   --------------------------

   procedure Process_Renaming_Ref
     (Handler               : access ALI_Handler_Record'Class;
      LI                    : LI_File;
      Entity                : Entity_Information;
      Xref_Sect             : Nat;
      Xref_Ent              : Nat;
      Sfiles                : Sdep_To_Sfile_Table;
      First_Sect, Last_Sect : Nat)
   is
      Renaming : constant Entity_Information := Find_Entity_In_ALI
        (Handler,
         LI, Sfiles, Xref_Section.Table (Xref_Sect).File_Num,
         Xref_Entity.Table (Xref_Ent).Rref_Line,
         Xref_Entity.Table (Xref_Ent).Rref_Col, First_Sect, Last_Sect);
   begin
      if Renaming /= null then
         Set_Is_Renaming_Of (Entity, Renaming);
      else
         Trace (Me, "Couldn't resolve renaming at "
                & Xref_Section.Table (Xref_Sect).File_Num'Img
                & Xref_Entity.Table (Xref_Ent).Rref_Line'Img
                & Xref_Entity.Table (Xref_Ent).Rref_Col'Img);
      end if;
   end Process_Renaming_Ref;

   ----------------------
   -- Process_Type_Ref --
   ----------------------

   procedure Process_Type_Ref
     (Handler               : access ALI_Handler_Record'Class;
      LI                    : LI_File;
      Entity                : Entity_Information;
      Xref_Ent              : Nat;
      Sfiles                : Sdep_To_Sfile_Table;
      First_Sect, Last_Sect : Nat)
   is
      Parent : Entity_Information;
   begin
      if Xref_Entity.Table (Xref_Ent).Tref_Standard_Entity /= No_Name then
         Parent := Get_Or_Create
           (Name   => Locale_To_UTF8
              (To_Lower (Get_String
                 (Xref_Entity.Table (Xref_Ent).Tref_Standard_Entity))),
            File   => Get_Predefined_File (Get_Database (LI)),
            Line   => Predefined_Line,
            Column => Predefined_Column);
      else
         Parent := Find_Entity_In_ALI
           (Handler    => Handler,
            LI         => LI,
            Sfiles     => Sfiles,
            File_Num   => Xref_Entity.Table (Xref_Ent).Tref_File_Num,
            Line       => Xref_Entity.Table (Xref_Ent).Tref_Line,
            Column     => Xref_Entity.Table (Xref_Ent).Tref_Col,
            First_Sect => First_Sect,
            Last_Sect  => Last_Sect);
      end if;

      if Parent = null then
         if Active (Assert_Me) then
            Trace (Assert_Me,
                   "Parent type not found in ALI file: "
                   & Full_Name (Get_LI_Filename (LI)).all
                   & Xref_Entity.Table (Xref_Ent).Tref_File_Num'Img
                   & Xref_Entity.Table (Xref_Ent).Tref_Line'Img
                   & Xref_Entity.Table (Xref_Ent).Tref_Col'Img);
         end if;

         return;
      else
         case Xref_Entity.Table (Xref_Ent).Tref is
            when Tref_None =>
               null;
            when Tref_Access =>
               Set_Pointed_Type (Entity, Parent);
            when Tref_Derived =>
               Set_Type_Of (Entity, Parent, Is_Subtype => True);
            when Tref_Type =>
               if Is_Subprogram (Entity) then
                  Set_Returned_Type (Entity, Parent);
               else
                  Set_Type_Of (Entity, Parent);
               end if;
         end case;
      end if;
   end Process_Type_Ref;

   --------------------------
   -- Process_Xref_Section --
   --------------------------

   procedure Process_Xref_Section
     (Handler               : access ALI_Handler_Record'Class;
      LI                    : LI_File;
      Xref_Sect             : Nat;
      Sfiles                : Sdep_To_Sfile_Table;
      First_Sect, Last_Sect : Nat) is
   begin
      for E in Xref_Section.Table (Xref_Sect).First_Entity
         .. Xref_Section.Table (Xref_Sect).Last_Entity
      loop
         Process_Xref_Entity
           (Handler, LI, Xref_Sect, E, Sfiles, First_Sect, Last_Sect);
      end loop;
   end Process_Xref_Section;

   -------------------
   -- Process_Xrefs --
   -------------------

   procedure Process_Xrefs
     (Handler               : access ALI_Handler_Record'Class;
      LI                    : LI_File;
      Sfiles                : Sdep_To_Sfile_Table;
      First_Sect, Last_Sect : Nat) is
   begin
      for Xref_Sect in First_Sect .. Last_Sect loop
         if Xref_Section.Table (Xref_Sect).File_Num in Sfiles'Range then
            Process_Xref_Section
              (Handler, LI, Xref_Sect, Sfiles, First_Sect, Last_Sect);
         end if;
      end loop;
   end Process_Xrefs;

   --------------------
   -- Create_New_ALI --
   --------------------

   procedure Create_New_ALI
     (Handler               : access ALI_Handler_Record'Class;
      LI                    : LI_File;
      New_ALI               : ALIs_Record;
      First_Sect, Last_Sect : Nat)
   is
      Sunits : Unit_To_Sfile_Table (New_ALI.First_Unit .. New_ALI.Last_Unit);
      Sfiles : Sdep_To_Sfile_Table (New_ALI.First_Sdep .. New_ALI.Last_Sdep);
      Imported_Projects : Project_Type_Array
        (1 .. Imported_Projects_Count (Get_Project (LI)));
   begin
      Get_Imported_Projects (Get_Project (LI), Imported_Projects);

      Process_Units (LI, New_ALI, Sunits);
      Process_Sdeps (LI, New_ALI, Sunits, Sfiles);
      Process_Withs (Sunits, Sfiles, Imported_Projects);
      Process_Xrefs (Handler, LI, Sfiles, First_Sect, Last_Sect);
   end Create_New_ALI;

   -----------------------
   -- Load_And_Scan_ALI --
   -----------------------

   procedure Load_And_Scan_ALI
     (ALI_Filename          : Virtual_File;
      Reset_First           : Boolean;
      Result                : out ALI_Id;
      First_Sect, Last_Sect : out Nat)
   is
      function Convert is new Ada.Unchecked_Conversion
        (GNAT.OS_Lib.String_Access, Text_Buffer_Ptr);
      Full   : constant String := Full_Name (ALI_Filename).all;
      Buffer : String_Access   := Read_File (ALI_Filename);
   begin
      if Buffer = null then
         Trace (Me, "Couldn't open " & Full);
         Result := No_ALI_Id;

      else
         if Active (Assert_Me) then
            Trace (Assert_Me, "Parsing " & Full & " Reset=" & Reset_First'Img);
         end if;

         --  Replace the last char by an EOF. Scan_ALI uses this character
         --  to detect the end of the buffer.
         Buffer (Buffer'Last) := EOF;

         --  Free the memory occupied by previous runs
         if Reset_First then
            Initialize_ALI;
         end if;

         First_Sect := Xref_Section.Last + 1;

         --  Get the ID of the ALI_Filename in the Namet table
         Namet.Name_Buffer (1 .. Full'Length) := Full;
         Namet.Name_Len := Full'Length;

         Result := Scan_ALI
           (Namet.Name_Find,
            Convert (Buffer),
            Ignore_ED     => True,
            Err           => True,
            Ignore_Errors => True,
            Read_Xref     => True);
         Free (Buffer);

         Last_Sect := Xref_Section.Last;
      end if;
   end Load_And_Scan_ALI;

   ----------------
   -- Update_ALI --
   ----------------

   function Update_ALI
     (Handler   : access ALI_Handler_Record'Class; LI : LI_File;
      Reset_ALI : Boolean) return Boolean
   is
      New_ALI_Id            : ALI_Id := No_ALI_Id;
      New_Timestamp         : Time;
      First_Sect, Last_Sect : Nat;
   begin
      Assert (Assert_Me, LI /= null, "No LI to update");

      New_Timestamp := File_Time_Stamp (Get_LI_Filename (LI));

      if New_Timestamp /= Get_Timestamp (LI) then
         Set_Time_Stamp (LI, New_Timestamp);
         Reset (LI);

         Trace (Me, "Load_And_Scan_ALI: "
                & Base_Name (Get_LI_Filename (LI)));
         Load_And_Scan_ALI
           (ALI_Filename   => Get_LI_Filename (LI),
            Reset_First    => Reset_ALI,
            Result         => New_ALI_Id,
            First_Sect     => First_Sect,
            Last_Sect      => Last_Sect);

         if New_ALI_Id = No_ALI_Id then
            Trace (Me, "Cannot parse " & Full_Name (Get_LI_Filename (LI)).all);
            return False;
         end if;

         Create_New_ALI (Handler, LI, ALIs.Table (New_ALI_Id),
                         First_Sect, Last_Sect);
      end if;

      return True;

   exception
      when E : others =>
         Trace (Exception_Handle, "Unexpected error while parsing "
                & Full_Name (Get_LI_Filename (LI)).all & ": "
                & Exception_Information (E));
         return False;
   end Update_ALI;

   ----------------
   -- Locate_ALI --
   ----------------

   function Locate_ALI
     (Short_ALI_Filename : String;
      Project            : Project_Type) return Virtual_File
   is
      procedure Next_Candidate (Last : in out Integer; Dot : String);
      --  Move Last so that Short_ALI_Filename (1 .. Last) is the name of the
      --  next file to test. This might be the parent unit

      procedure Next_Candidate (Last : in out Integer; Dot : String) is
      begin
         while Last > Short_ALI_Filename'First loop
            Last := Last - 1;
            exit when (Last + Dot'Length - 1 <= Short_ALI_Filename'Last
                       and then Short_ALI_Filename
                         (Last .. Last + Dot'Length - 1) = Dot)

            --  Special case when there might be a confusion with the
            --  GNAT runtime files.
              or else (Dot = "-" and then Short_ALI_Filename (Last) = '~');
         end loop;
         Last := Last - 1;
      end Next_Candidate;


      Current_Dir_Name   : constant Character := '.';
      Dir                : String_Access;
      P                  : Project_Type := Project;
      Extension : constant String := File_Extension (Short_ALI_Filename);

   begin
      --  Start searching in the extending projects, in case the file was
      --  recompiled in their context

      while Extending_Project (P) /= No_Project loop
         P := Extending_Project (P);
      end loop;

      --  Compute the search path. If the objects path of the project is
      --  not null, then prepend it to the total search path.
      while P /= No_Project loop
         declare
            Last      : Integer := Short_ALI_Filename'Last - Extension'Length;
            Dot_Replacement : constant String := Get_Attribute_Value
              (P, Dot_Replacement_Attribute, Default => "-");
         begin
            while Dir = null and then Last >= Short_ALI_Filename'First loop
               declare
                  Path : constant String := Object_Path (P, False);
                  File : constant String :=
                    Locale_From_UTF8
                      (Short_ALI_Filename (Short_ALI_Filename'First .. Last)
                       & Extension);
               begin
                  if Path /= "" then
                     Dir := Locate_Regular_File (File, Path);
                  end if;
               end;

               Next_Candidate (Last, Dot_Replacement);
            end loop;
         end;

         if P /= Project then
            P := Parent_Project (P);
         else
            P := No_Project;
         end if;
      end loop;

      --  Still not found ? Check in the predefined object path

      if Dir = null then
         declare
            Predefined_Object_Path : constant String :=
              Get_Predefined_Object_Path
                (Project_Registry (Get_Registry (Project)))
              & Path_Separator & Current_Dir_Name;
            Last : Integer := Short_ALI_Filename'Last - Extension'Length;
         begin
            while Dir = null and then Last >= Short_ALI_Filename'First loop
               Dir := Locate_Regular_File
                 (Short_ALI_Filename
                    (Short_ALI_Filename'First .. Last) & Extension,
                  Predefined_Object_Path);
               Next_Candidate (Last, ".");
            end loop;
         end;
      end if;

      if Dir = null then
         return VFS.No_File;
      else
         declare
            F : constant Virtual_File := Create (Dir.all);
         begin
            Free (Dir);
            return F;
         end;
      end if;
   end Locate_ALI;

   ----------------------
   -- Get_ALI_Filename --
   ----------------------

   function Get_ALI_Filename (Base_Name : String) return String is
      Last_Dot : Natural := Base_Name'Last;
      ALI_Ext  : constant String := ".ali";
   begin
      --  Search the last dot in the filename

      while Last_Dot >= Base_Name'First  loop
         exit when Base_Name (Last_Dot) = '.';
         Last_Dot := Last_Dot - 1;
      end loop;

      if Last_Dot < Base_Name'First then
         --  No dot found, just append the ALI extension
         Last_Dot := Base_Name'Last + 1;
      end if;

      return Base_Name (Base_Name'First .. Last_Dot - 1) & ALI_Ext;
   end Get_ALI_Filename;

   -----------------------------
   -- LI_Filename_From_Source --
   -----------------------------

   function LI_Filename_From_Source
     (Source_Filename : Virtual_File;
      Project         : Project_Type) return Virtual_File
   is
      LI : Virtual_File;
   begin
      case Get_Unit_Part_From_Filename (Project, Source_Filename) is
         when Unit_Body | Unit_Separate =>
            --  When using non-standard naming schemes, separate units are
            --  reported as bodies, but they have no direct ALI file. Thus, in
            --  addition to checking directly for an ALI file, we also check
            --  for ALI file from the parent unit.

            LI := Locate_ALI
              (Get_ALI_Filename (Base_Name (Source_Filename)), Project);
            if LI /= VFS.No_File then
               return LI;
            end if;

            declare
               Unit : constant String := Get_Unit_Name_From_Filename
                 (Project, Source_Filename);
               Last : Integer := Unit'Last;
            begin
               while Last >= Unit'First
                 and then Unit (Last) /= '.'
               loop
                  Last := Last - 1;
               end loop;

               if Last >= Unit'First then
                  return Locate_ALI
                    (Get_ALI_Filename
                       (Get_Filename_From_Unit
                          (Project, Unit (Unit'First .. Last - 1), Unit_Body)),
                     Project);
               else
                  return VFS.No_File;
               end if;
            end;

         when Unit_Spec =>
            --  Use the ALI for the body, if there is a body, otherwise the one
            --  for the spec will do.

            LI :=  Locate_ALI
              (Get_ALI_Filename
                 (Other_File_Base_Name (Project, Source_Filename)),
               Project);
            if LI /= VFS.No_File then
               return LI;
            else
               return Locate_ALI
                 (Get_ALI_Filename (Base_Name (Source_Filename)), Project);
            end if;
      end case;
   end LI_Filename_From_Source;

   ---------------------
   -- Get_Source_Info --
   ---------------------

   function Get_Source_Info
     (Handler               : access ALI_Handler_Record;
      Source_Filename       : VFS.Virtual_File;
      File_Has_No_LI_Report : File_Error_Reporter := null) return Source_File
   is
   begin
      return Get_Source_Info_Internal
        (Handler, Source_Filename, File_Has_No_LI_Report,
         Reset_ALI => True);
   end Get_Source_Info;

   ------------------------------
   -- Get_Source_Info_Internal --
   ------------------------------

   function Get_Source_Info_Internal
     (Handler               : access ALI_Handler_Record'Class;
      Source_Filename       : VFS.Virtual_File;
      File_Has_No_LI_Report : File_Error_Reporter := null;
      Reset_ALI             : Boolean) return Source_File
   is
      LI      : LI_File;
      LI_Name : Virtual_File;
      Source  : Source_File;
      Project : Project_Type;
   begin
      --  If we already know about the file, we get the name of the LI file
      --  from it

      Source := Get_Or_Create
        (Db           => Handler.Db,
         File         => Source_Filename,
         LI           => null);
      if Source /= null and then Get_LI (Source) /= null then
         if not Update_ALI (Handler, Get_LI (Source), Reset_ALI => Reset_ALI)
           and then File_Has_No_LI_Report /= null
         then
            Entities.Error
              (File_Has_No_LI_Report.all, Source);
         end if;

         return Source;
      end if;

      --  Otherwise we have to compute the name of the LI file from scratch

      Project := Get_Project_From_File (Handler.Registry, Source_Filename);

      LI_Name := LI_Filename_From_Source (Source_Filename, Project);

      if LI_Name = VFS.No_File then
         Trace (Me, "No LI found for " & Full_Name (Source_Filename).all);

         if File_Has_No_LI_Report /= null then
            Entities.Error (File_Has_No_LI_Report.all, Source);
         end if;

         return Source;
      else
         if Active (Assert_Me) then
            Trace (Assert_Me, "LI for " & Full_Name (Source_Filename).all
                   & " is " & Full_Name (LI_Name).all);
         end if;
      end if;

      LI := Get_Or_Create
        (Db        => Handler.Db,
         File      => LI_Name,
         Project   => Project);

      if LI = null then
         return Source;
      end if;

      if (not Update_ALI (Handler, LI, Reset_ALI => Reset_ALI)
          or else not Check_LI_And_Source (LI, Source_Filename))
        and then File_Has_No_LI_Report /= null
      then
         Entities.Error (File_Has_No_LI_Report.all, Source);
         LI := null;
      end if;

      --  Do another lookup, to update the LI file, since apparently we didn't
      --  know it before
      return Get_Or_Create
        (Db           => Handler.Db,
         File         => Source_Filename,
         LI           => LI);
   end Get_Source_Info_Internal;

   ----------------------------------
   -- Case_Insensitive_Identifiers --
   ----------------------------------

   function Case_Insensitive_Identifiers
     (Handler : access ALI_Handler_Record) return Boolean
   is
      pragma Unreferenced (Handler);
   begin
      return True;
   end Case_Insensitive_Identifiers;

   ------------------------------
   -- Parse_All_LI_Information --
   ------------------------------

   function Parse_All_LI_Information
     (Handler   : access ALI_Handler_Record;
      Project   : Projects.Project_Type;
      Recursive : Boolean := False) return Integer
   is
      Count : Natural := 0;

      procedure Parse_Directory (Project : Project_Type; Directory : String);
      --  Parses a specific directory

      procedure Parse_Directory (Project : Project_Type; Directory : String) is
         Dir     : Dir_Type;
         File    : String (1 .. 1024);
         Last    : Natural;
         LI      : LI_File;
         LI_File : Virtual_File;
      begin
         Open (Dir, Directory);

         loop
            Read (Dir, File, Last);
            exit when Last = 0;

            if File_Extension (File (File'First .. Last)) = ".ali" then
               LI_File := Create
                 (Full_Filename => Directory & File (File'First .. Last));

               LI := Get_Or_Create
                 (Db      => Handler.Db,
                  File    => LI_File,
                  Project => Project);
               if not Update_ALI (Handler, LI, Reset_ALI => True) then
                  Trace
                    (Me, "Couldn't parse " & Full_Name (LI_File).all);
               end if;

               Count := Count + 1;
            end if;
         end loop;

         Close (Dir);

      exception
         when Directory_Error =>
            Trace (Me, "Couldn't open the directory " & Directory);
      end Parse_Directory;

      P       : Project_Type;
      Iter    : Imported_Project_Iterator := Start
        (Root_Project => Project,
         Recursive    => Recursive);
   begin
      loop
         P := Current (Iter);
         exit when P = No_Project;

         declare
            Objects  : constant String := Object_Path (P, Recursive => False);
            Dir_Iter : Path_Iterator := Start (Objects);
         begin
            while not At_End (Objects, Dir_Iter) loop
               Parse_Directory
                 (P, Name_As_Directory (Current (Objects, Dir_Iter)));
               Dir_Iter := Next (Objects, Dir_Iter);
            end loop;
         end;

         Next (Iter);
      end loop;

      return Count;
   end Parse_All_LI_Information;

   -----------------------------
   -- Generate_LI_For_Project --
   -----------------------------

   function Generate_LI_For_Project
     (Handler   : access ALI_Handler_Record;
      Project   : Projects.Project_Type;
      Recursive : Boolean := False) return LI_Handler_Iterator'Class
   is
      pragma Unreferenced (Handler, Project, Recursive);
      Iterator : ALI_Handler_Iterator;
   begin
      return Iterator;
   end Generate_LI_For_Project;

   ------------------------
   -- Create_ALI_Handler --
   ------------------------

   function Create_ALI_Handler
     (Db       : Entities.Entities_Database;
      Registry : Projects.Registry.Project_Registry) return Entities.LI_Handler
   is
      ALI : constant ALI_Handler := new ALI_Handler_Record;
   begin
      ALI.Db       := Db;
      ALI.Registry := Registry;
      return LI_Handler (ALI);
   end Create_ALI_Handler;

   --------------
   -- Continue --
   --------------

   procedure Continue
     (Iterator : in out ALI_Handler_Iterator; Finished : out Boolean)
   is
      pragma Unreferenced (Iterator);
   begin
      Finished := True;
   end Continue;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Iterator : in out ALI_Handler_Iterator) is
      pragma Unreferenced (Iterator);
   begin
      null;
   end Destroy;

end ALI_Parser;
