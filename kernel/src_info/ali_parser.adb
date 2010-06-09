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

with Ada.Characters.Handling;   use Ada.Characters.Handling;
with Ada.Calendar;              use Ada.Calendar;
with Ada.Exceptions;            use Ada.Exceptions;
with Ada.Unchecked_Conversion;
with GNAT.Calendar.Time_IO;     use GNAT.Calendar.Time_IO;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with GNAT.Strings;
with GNATCOLL.Projects;         use GNATCOLL.Projects;
with GNATCOLL.Symbols;          use GNATCOLL.Symbols;
with GNATCOLL.Traces;           use GNATCOLL.Traces;
with GNATCOLL.Utils;            use GNATCOLL.Utils;
with GNATCOLL.VFS;              use GNATCOLL.VFS;
with GNATCOLL.VFS_Utils;        use GNATCOLL.VFS_Utils;

with Basic_Types;               use Basic_Types;
with Entities.Queries;          use Entities.Queries;
with Entities;                  use Entities;
with Glib.Convert;              use Glib.Convert;
with Projects;                  use Projects;
with Remote;                    use Remote;
with Traces;
with Language.Tree.Database;    use Language.Tree.Database;

with ALI;                       use ALI;
with Types;                     use Types;
with Namet;                     use Namet;

package body ALI_Parser is
   Me        : constant Trace_Handle := Create ("ALI", Off);
   Assert_Me : constant Trace_Handle := Create ("ALI.Assert", Off);

   SLI_Support : constant Trace_Handle := Create ("SPARK.SLI", Off);

   type ALI_Handler_Iterator is new LI_Handler_Iterator with null record;
   overriding procedure Continue
     (Iterator : in out ALI_Handler_Iterator;
      Errors   : Error_Report;
      Finished : out Boolean);
   overriding procedure Destroy (Iterator : in out ALI_Handler_Iterator);
   --  See doc for inherited subprograms

   type E_Kind_To_Char_Map is array (Character range '*' .. 'z') of E_Kind;
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
      'g'    => (Macro,                  False, False, False),
      'G'    => (Function_Macro,         True,  False, False),
      'h'    => (Interface_Kind,         False, True,  True),
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
      'Q'    => (Include_File,           False, False, False),
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
      '*'    => (Private_Type,           False, True,  False),
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
      'o'    => Own_Reference,
      'p'    => Primitive_Operation,
      'P'    => Overriding_Primitive_Operation,
      'r'    => Reference,
      'R'    => Dispatching_Call,
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

   function Get_SLI_From_ALI (LI : LI_File) return LI_File;
   --  Return the SLI file corresponding to LI

   function Is_ALI_File (LI : LI_File) return Boolean;
   --  Return True is LI represents an ALI file (as opposed to e.g. a .sli)

   procedure Process_Units
     (Handler : access LI_Handler_Record'Class;
      LI      : LI_File;
      New_ALI : ALIs_Record; Sunits : out Unit_To_Sfile_Table);
   --  Get a handle for all the units in New_ALI

   function Process_Unit
     (Handler : access LI_Handler_Record'Class;
      LI      : LI_File;
      Id      : Unit_Id) return Source_File;
   --  Return a handle to the file matching Id

   procedure Process_Sdep
     (Handler : access LI_Handler_Record'Class;
      LI      : LI_File;
      Dep_Id  : Sdep_Id;
      Sunits  : Unit_To_Sfile_Table;
      Sfile   : in out Source_Dependency);
   --  Return a handle to a specific file dependency (Dep)

   procedure Process_Sdeps
     (Handler : access LI_Handler_Record'Class;
      LI      : LI_File;
      New_ALI : ALIs_Record;
      Sunits  : Unit_To_Sfile_Table;
      Sfiles  : out Sdep_To_Sfile_Table);
   --  Get a handle for all the units dependencies

   procedure Process_Withs_For_Unit
     (Unit              : Unit_Id;
      File              : Source_File;
      Deps              : Sdep_To_Sfile_Table;
      Imported_Projects : Project_Type_Array);
   --  Register the dependencies for all files in Deps into File

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
      Imported_Projects : Project_Type_Array;
      File_To_Compile   : File_Name_Type) return Name_Id;
   --  Convert from a Unit specification (unit%s or unit%b) to a filename.
   --  File_To_Compile is the name of the file to compile to regenerate the
   --  units'.ali file, and it could be either the body (in general) or the
   --  spec. This is used as a base name for proper handling of GNAT's runtime
   --  files

   function Imported_Projects_Count (Project : Project_Type) return Natural;
   --  Return the number of projects imported by Project. This returns 1 if
   --  No_Project is passed (since runtime files can depend on other runtime
   --  files).

   procedure Get_Imported_Projects
     (Project  : Project_Type;
      Imported : out Project_Type_Array);
   --  Get the projects imported by Project. Imported must have the size
   --  returned by Imported_Projects_Count.
   --  This works for No_Project

   procedure Process_Xref_Entity
     (Handler               : access ALI_Handler_Record'Class;
      LI                    : LI_File;
      Xref_Sect             : Nat;
      Xref_Ent              : Nat;
      Sfiles                : Sdep_To_Sfile_Table;
      First_Sect, Last_Sect : Nat);
   --  Save the Xref Entity information in the New_LI_File structure

   procedure Process_Xref_Section
     (Handler               : access ALI_Handler_Record'Class;
      LI                    : LI_File;
      Xref_Sect             : Nat;
      Sfiles                : Sdep_To_Sfile_Table;
      First_Sect, Last_Sect : Nat);
   --  Save the Xref information associated to the given With_Record

   procedure Process_Xrefs
     (Handler               : access ALI_Handler_Record'Class;
      LI                    : LI_File;
      Sfiles                : Sdep_To_Sfile_Table;
      First_Sect, Last_Sect : Nat);
   --  Save the Xref information in the New_LI_File structure

   function Update_ALI
     (Handler   : access ALI_Handler_Record'Class; LI : LI_File;
      Reset_ALI : Boolean;
      Force_Update : Boolean := False) return Boolean;
   --  Re-parse the contents of the ALI file, and return True in case of
   --  success.
   --  By default, nothing is updated if the database is frozen (see Freeze for
   --  the various freeze modes). However, you can force the check on whether
   --  the file is up-to-date by passing Force_Update to True

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
   --  Process the parent type of an entity declared in Xref_Ent

   procedure Process_Overriding_Ref
     (Handler               : access ALI_Handler_Record'Class;
      LI                    : LI_File;
      Entity                : Entity_Information;
      Xref_Ent              : Nat;
      Sfiles                : Sdep_To_Sfile_Table;
      First_Sect, Last_Sect : Nat);
   --  Process the overriding information declared in Xref_Ent

   function Find_Entity_In_ALI
     (Handler               : access ALI_Handler_Record'Class;
      LI                    : LI_File;
      Sfiles                : Sdep_To_Sfile_Table;
      File_Num              : Sdep_Id;
      Line                  : Nat;
      Column                : Nat;
      First_Sect, Last_Sect : Nat;
      Find_In_Xref          : Boolean := True;
      Allow_Fuzzy           : Boolean := True) return Entity_Information;
   --  Find or create an entity information based on the information contained
   --  in the current LI. This returns a placeholder for the declaration, but
   --  no specific information has been set
   --  If Column is 0, the first entity that matches the line is returned.
   --  If Allow_Fuzzy is true, we also recognize fuzzy matches when looking for
   --  the entity.

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
      Current_Entity        : Nat;
      Current_Ref           : Nat;
      Current_Sfile         : in out Sdep_Id;
      First_Sect, Last_Sect : Nat);
   --  Process a reference to the entity

   procedure LI_Filename_From_Source
     (Handler         : access ALI_Handler_Record'Class;
      Source_Filename : Virtual_File;
      Project         : Project_Type;
      LI              : out Virtual_File;
      Predefined      : out Boolean);
   --  Return the ALI file associated with Source_Filename.
   --  Predefined is set to True if the project was found in the predefined
   --  path (ie does not belong to any project). Project might be No_Project on
   --  input if we know in advance the file does not belong to a project.

   procedure Locate_ALI
     (Handler            : access ALI_Handler_Record'Class;
      Short_ALI_Filename : Filesystem_String;
      Source_Filename    : GNATCOLL.VFS.Virtual_File;
      Project            : Project_Type;
      LI_Filename        : out Virtual_File;
      Predefined         : out Boolean);
   --  Search for the full name of the ALI file. We also search the parent
   --  unit's ALi file, in case the file is a separate.
   --  Predefined is set to True if the project was found in the predefined
   --  path (ie does not belong to any project). Project might be No_Project on
   --  input if we know in advance the file does not belong to a project.

   function Find_Multi_Unit_ALI
     (Handler         : access ALI_Handler_Record'Class;
      Source_Filename : Virtual_File;
      Project         : Project_Type) return Virtual_File;
   --  Parse all ALI files in the object directory of Project and its extending
   --  projects, looking for LI files representing multi-unit source files.
   --  If one is found for Source_Filename, return its name.
   --  It is valid to pass No_Project

   function Get_Source_Info_Internal
     (Handler               : access ALI_Handler_Record'Class;
      Source_Filename       : GNATCOLL.VFS.Virtual_File;
      File_Has_No_LI_Report : File_Error_Reporter := null;
      Reset_ALI             : Boolean) return Source_File;
   --  Same as Get_Source_Info, but it is possible not to reset the internal
   --  GNAT tables first. This must be used when calling this recursively

   --------------------
   -- Char_To_E_Kind --
   --------------------

   function Char_To_E_Kind (C : Character) return E_Kind is
      pragma Suppress (All_Checks);
   begin
      if C in E_Kind_To_Char'Range then
         return E_Kind_To_Char (C);
      else
         --  If we reach this point, the character is illegal
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
         --  If we reach this point, the character is illegal
         Trace (Me, "Char_To_R_Kind: Invalid character '" & C'Img & ''');
         return Reference;
      end if;
   end Char_To_R_Kind;

   ----------------------
   -- Get_SLI_From_ALI --
   ----------------------

   function Get_SLI_From_ALI (LI : LI_File) return LI_File is
      File : constant Virtual_File := Get_LI_Filename (LI);
   begin
      return Get_Or_Create
        (Get_Database (LI),
         Create
           (Dir_Name (File) & Base_Name (File, ".ali") & ".sli",
            Get_Host (File)),
         Get_Project (LI));
   end Get_SLI_From_ALI;

   -----------------
   -- Is_ALI_File --
   -----------------

   function Is_ALI_File (LI : LI_File) return Boolean is
   begin
      return File_Extension (Get_LI_Filename (LI)) = ".ali";
   end Is_ALI_File;

   ------------------
   -- Process_Unit --
   ------------------

   function Process_Unit
     (Handler : access LI_Handler_Record'Class;
      LI      : LI_File;
      Id      : Unit_Id) return Source_File
   is
      Base_Name : constant String := Get_Name_String (Units.Table (Id).Sfile);
      File      : Source_File;

   begin
      Assert (Assert_Me, LI /= null, "Null LI file parsed");

      File := Get_Or_Create
        (Db        => Get_Database (LI),
         Base_Name => +Base_Name,
         Handler   => Handler,
         LI        => LI);

      return File;
   end Process_Unit;

   -------------------
   -- Process_Units --
   -------------------

   procedure Process_Units
     (Handler : access LI_Handler_Record'Class;
      LI      : LI_File;
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
            Sunits (Current_Unit_Id) :=
              Process_Unit (Handler, LI, Current_Unit_Id);
         end if;
      end loop;
   end Process_Units;

   ------------------
   -- Process_Sdep --
   ------------------

   procedure Process_Sdep
     (Handler : access LI_Handler_Record'Class;
      LI      : LI_File;
      Dep_Id  : Sdep_Id;
      Sunits  : Unit_To_Sfile_Table;
      Sfile   : in out Source_Dependency)
   is
      Dep         : Sdep_Record renames Sdep.Table (Dep_Id);
      Is_Separate : constant Boolean := Dep.Subunit_Name /= No_Name;
      Base_Name   : constant String := Get_Name_String (Dep.Sfile);
      --  ??? (UTF8) this seems wrong: what if the filename is not valid UTF8?
      L           : LI_File := LI;
   begin
      --  Do we have one of the files belonging to LI itself ?
      for Current_Unit in Sunits'Range loop
         if Units.Table (Current_Unit).Sfile = Dep.Sfile then
            --  ??? Check the original file name for gnatchoped files:
            --  Dep.Rfile and Dep.Start_Line
            Sfile := (Sunits (Current_Unit),
                      Is_Separate => Is_Separate,
                      Is_Unit     => True);
            return;
         end if;
      end loop;

      if not Is_Separate then
         --  We do not know its ALI file
         L := null;
      end if;

      Sfile := (Get_Or_Create
                  (Db        => Get_Database (LI),
                   Base_Name => +Base_Name,
                   Handler   => Handler,
                   LI        => L),
                Is_Separate => Is_Separate,
                Is_Unit     => False);
   end Process_Sdep;

   -------------------
   -- Process_Sdeps --
   -------------------

   procedure Process_Sdeps
     (Handler : access LI_Handler_Record'Class;
      LI      : LI_File;
      New_ALI : ALIs_Record;
      Sunits  : Unit_To_Sfile_Table;
      Sfiles  : out Sdep_To_Sfile_Table) is
   begin
      for Dep_Id in New_ALI.First_Sdep .. New_ALI.Last_Sdep loop
         Process_Sdep (Handler, LI, Dep_Id, Sunits, Sfiles (Dep_Id));
      end loop;
   end Process_Sdeps;

   ------------------------
   -- Filename_From_Unit --
   ------------------------

   function Filename_From_Unit
     (Unit              : Unit_Name_Type;
      Imported_Projects : Project_Type_Array;
      File_To_Compile   : File_Name_Type) return Name_Id
   is
      Unit_Name : constant String := Get_Name_String (Unit);
      Is_Spec   : constant Boolean := Unit_Name (Unit_Name'Last) = 's';
      Part      : Unit_Parts;
   begin
      --  Parse the imported projects in the reverse order, since we must find
      --  the unit in the top-most project, for instance in case it was
      --  overriden in extending projects

      if Is_Spec then
         Part := Unit_Spec;
      else
         Part := Unit_Body;
      end if;

      if Imported_Projects (Imported_Projects'First) /= No_Project then
         for P in reverse Imported_Projects'Range loop
            declare
               N : constant Filesystem_String :=
                 Imported_Projects (P).File_From_Unit
                 (Unit_Name (1 .. Unit_Name'Last - 2),
                  Part,
                  Language => "ada");
            begin
               if N'Length > 0 then
                  Name_Len := N'Length;
                  Name_Buffer (1 .. Name_Len) := +N;
                  return Name_Find;
               end if;
            end;
         end loop;
      end if;

      --  We end up here for the runtime files, so we just try to append the
      --  standard GNAT extensions.

      if File_To_Compile = Namet.No_File then
         Name_Len := 0;
         return Name_Find;
      else
         declare
            Base : constant String := Get_Name_String (File_To_Compile);
            N    : constant String :=
              +Imported_Projects (Imported_Projects'First).File_From_Unit
              (Base (Base'First .. Base'Last - 4),
               Part,
               Check_Predefined_Library => True,
               Language => "ada");
         begin
            Name_Len := N'Length;
            Name_Buffer (1 .. Name_Len) := N;
            return Name_Find;
         end;
      end if;
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
                            Units.Table (Unit).Last_With) of File_Name_Type;
   begin
      for W in With_Files'Range loop
         With_Files (W) := File_Name_Type (Filename_From_Unit
           (Withs.Table (W).Uname, Imported_Projects,
            Withs.Table (W).Sfile));
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
      Iter  : Project_Iterator;
   begin
      if Project = No_Project then
         return 1;
      end if;

      Iter := Start (Project, Recursive => True);

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
      Iter : Project_Iterator;
   begin
      if Project = No_Project then
         Imported (Imported'First) := No_Project;
      else
         Iter := Start (Project, Recursive => True);

         for J in Imported'Range loop
            Imported (J) := Current (Iter);
            Next (Iter);
         end loop;
      end if;
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
      File_Num         : constant Sdep_Id :=
                           Xref_Section.Table (Xref_Sect).File_Num;
      Kind             : constant E_Kind :=
                           Char_To_E_Kind (Xref_Entity.Table (Xref_Ent).Etype);
      Entity           : Entity_Information;
      Instantiation_Of : Entity_Information;
      Current_Sfile    : Sdep_Id;
      Has_Completion   : Boolean := False;
      Attributes       : Entity_Attributes := (others => False);

   begin
      Get_Name_String (Xref_Entity.Table (Xref_Ent).Entity);

      declare
         First : Positive := 1;
         Last  : Positive := Name_Len;
      begin
         if Name_Buffer (1) = '"' then
            First := 2;
            Last := Name_Len - 1;
         end if;

         Entity := Get_Or_Create
           (Name   => Get_Symbols (Handler.Db).Find
              (Name_Buffer (First .. Last)),
            File   => Sfiles (File_Num).File,
            Line   => Integer (Xref_Entity.Table (Xref_Ent).Line),
            Column => Visible_Column_Type (Xref_Entity.Table (Xref_Ent).Col));
      end;

      Set_Kind (Entity, Kind);

      case Xref_Entity.Table (Xref_Ent).Visibility is
         when Global =>
            Attributes (Global) := True;
         when Static =>
            Attributes (Static_Local) := True;
         when Other =>
            null;
      end case;

      Set_Attributes (Entity, Attributes);

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

         if Xref_Entity.Table (Xref_Ent).Oref_File_Num /= No_Sdep_Id then
            Process_Overriding_Ref
              (Handler, LI, Entity, Xref_Ent, Sfiles, First_Sect, Last_Sect);
         end if;
      end if;

      --  Process the generics instantation information

      if Xref_Entity.Table (Xref_Ent).Iref_File_Num /= No_Sdep_Id then
         Instantiation_Of := Find_Entity_In_ALI
           (Handler,
            LI, Sfiles, Xref_Entity.Table (Xref_Ent).Iref_File_Num,
            Xref_Entity.Table (Xref_Ent).Iref_Line, 0,
            First_Sect, Last_Sect);
         if Instantiation_Of = null then
            if Active (Assert_Me) then
               Trace (Assert_Me, "Couldn't find instantiated entity: "
                      & Xref_Entity.Table (Xref_Ent).Iref_File_Num'Img
                      & Xref_Entity.Table (Xref_Ent).Iref_Line'Img);
            end if;
         else
            Set_Is_Instantiation (Entity, Instantiation_Of);
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
           (Handler, LI, Entity, Sfiles, Xref_Ent, Xref_Id, Current_Sfile,
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
      Current_Entity        : Nat;
      Current_Ref           : Nat;
      Current_Sfile         : in out Sdep_Id;
      First_Sect, Last_Sect : Nat)
   is
      Kind      : constant Reference_Kind := Char_To_R_Kind
        (Xref.Table (Current_Ref).Rtype);
      Location  : File_Location;
      Primitive : Entity_Information;
      Instantiation : Entity_Information := null;
      Ref       : Nat;
      Inst      : Entity_Instantiation;

   begin
      if Xref.Table (Current_Ref).Rtype = Array_Index_Reference then
         if Xref.Table (Current_Ref).Name /= No_Name then
            --  In the ALI file, the predefined entities are always lower-cased
            --  when in fact the entity name in GPS is expected to have its
            --  proper casing. For most users, capitalization is more suitable
            --  than all lower-case when the language is case insensitive (the
            --  ALI parser is used also for C/C++ xref info produced by GCC).

            declare
               Name : constant String :=
                 Get_Name_String (Xref.Table (Current_Ref).Name);
            begin
               if Case_Insensitive_Identifiers (Handler) then
                  Primitive := Get_Or_Create
                    (Name   => Get_Symbols (Handler.Db).Find
                       (Locale_To_UTF8 (Capitalize (Name))),
                     File   => Get_Predefined_File
                                 (Get_Database (LI), Handler),
                     Line   => Predefined_Line,
                     Column => Predefined_Column);
               else
                  Primitive := Get_Or_Create
                    (Name   => Get_Symbols (Handler.Db).Find
                       (Locale_To_UTF8 (Name)),
                     File   => Get_Predefined_File
                                 (Get_Database (LI), Handler),
                     Line   => Predefined_Line,
                     Column => Predefined_Column);
               end if;
            end;
         else
            Primitive := Find_Entity_In_ALI
              (Handler,
               LI, Sfiles,
               Xref.Table (Current_Ref).File_Num,
               Xref.Table (Current_Ref).Line,
               Xref.Table (Current_Ref).Col, First_Sect, Last_Sect);
         end if;

         if Primitive /= null then
            Add_Index_Type (Entity, Primitive);
         end if;

      elsif Xref.Table (Current_Ref).Rtype = Interface_Reference then
         Primitive := Find_Entity_In_ALI
           (Handler,
            LI, Sfiles,
            Xref.Table (Current_Ref).File_Num,
            Xref.Table (Current_Ref).Line,
            Xref.Table (Current_Ref).Col, First_Sect, Last_Sect,
            Find_In_Xref => False);  --  Only search declarations

         if Primitive = null then
            Trace (Assert_Me, "Couldn't find interface in ALI file: "
                   & Display_Full_Name (Get_LI_Filename (LI))
                   & Xref.Table (Current_Ref).File_Num'Img & " "
                   & Xref.Table (Current_Ref).Line'Img
                   & Xref.Table (Current_Ref).Col'Img);

         else
            Set_Type_Of (Entity, Primitive);
         end if;

      --  This is processed in the context of the previous reference already
      elsif Kind /= Instantiation_Reference then
         Current_Sfile := Xref.Table (Current_Ref).File_Num;

         --  Check to avoid the constraint error (index check failed)
         --  reported under E829-005 that is triggered when GPS
         --  earlier than 3.1.0w is used with a GNAT wavefront that
         --  includes implentation for E708-001.

         if Current_Sfile not in Sfiles'Range then
            return;
         end if;

         Location := (File   => Sfiles (Current_Sfile).File,
                      Line   => Integer (Xref.Table (Current_Ref).Line),
                      Column => Visible_Column_Type
                        (Xref.Table (Current_Ref).Col));

         if Is_End_Reference (Kind) then
            --  Only insert the end-of-scope is we are parsing the ALI file
            --  for the file that contains this end-of-scope. Otherwise, we
            --  get duplicate references.
            if Get_LI (Location.File) = LI then
               Set_End_Of_Scope (Entity, Location, Kind);
            end if;

         elsif Kind = Primitive_Operation
           or else Kind = Overriding_Primitive_Operation
         then
            Primitive := Find_Entity_In_ALI
              (Handler,
               LI, Sfiles, Current_Sfile, Xref.Table (Current_Ref).Line,
               Xref.Table (Current_Ref).Col, First_Sect, Last_Sect);

            if Primitive = null then
               Trace (Assert_Me, "Couldn't find primitive in ALI file: "
                      & Display_Full_Name (Get_LI_Filename (LI))
                      & Current_Sfile'Img
                      & Xref.Table (Current_Ref).Line'Img
                      & Xref.Table (Current_Ref).Col'Img);
            else
               --  Only add the primitive if it is a new one. That keeps the
               --  database shorter without redundant information. This also
               --  avoids cases where the entity points to its primitive op,
               --  but the latter points to another one as its parent.

               if Sfiles (Current_Sfile).File =
                 Get_Declaration_Of (Entity).File
                 and then
                   (Is_Primitive_Operation_Of (Primitive) = null
                    or else Sfiles (Current_Sfile).File /=
                      Get_Declaration_Of
                        (Is_Primitive_Operation_Of (Primitive)).File)
               then
                  Add_Primitive_Subprogram (Entity, Primitive);
               end if;

               if Is_Primitive_Operation_Of (Primitive) = null then
                  Add_Primitive_Subprogram (Entity, Primitive);
               end if;
            end if;

         else
            --  Look at the next reference. If it is a generic instantiation,
            --  take it into account
            Ref := Current_Ref + 1;
            while Ref <= Xref_Entity.Table (Current_Entity).Last_Xref
              and then Char_To_R_Kind (Xref.Table (Ref).Rtype) =
                Instantiation_Reference
            loop
               Instantiation := Find_Entity_In_ALI
                 (Handler,
                  LI, Sfiles,
                  Xref.Table (Ref).File_Num,
                  Xref.Table (Ref).Line,
                  0, First_Sect, Last_Sect);
               Inst := Get_Or_Create_Instantiation
                 (Location.File, Instantiation, Inst);
               Ref := Ref + 1;
            end loop;

            Add_Reference (Entity, Location, Kind, Inst);
         end if;
      end if;
   exception
      when E : others =>
         Trace (Traces.Exception_Handle, "Unexpected error while parsing "
                & Display_Full_Name (Get_LI_Filename (LI)) & ": "
                & Exception_Information (E));
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
      First_Sect, Last_Sect : Nat;
      Find_In_Xref          : Boolean := True;
      Allow_Fuzzy           : Boolean := True) return Entity_Information
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
                 and then (Column = 0
                           or else Xref_Entity.Table (Entity).Col = Column)
               then
                  return Get_Or_Create
                    (Name => Get_Symbols (Handler.Db).Find
                       (Locale_To_UTF8
                          (Get_Name_String
                             (Xref_Entity.Table (Entity).Entity))),
                     File => Sfiles (File_Num).File,
                     Line => Integer (Line),
                     Column => Visible_Column_Type
                       (Xref_Entity.Table (Entity).Col));
               end if;
            end loop;
         end if;
      end loop;

      --  Do a separate loop for references, in case this was a reference to
      --  the declaration. It will be much faster in that case

      if Find_In_Xref then
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
                  --  Ignore the interface references, since that might be what
                  --  we are already searching for
                  if Xref.Table (Ref).File_Num = File_Num
                    and then Xref.Table (Ref).Line = Line
                    and then Xref.Table (Ref).Rtype /= Interface_Reference
                    and then (Column = 0
                              or else Xref.Table (Ref).Col = Column)
                  then
                     return Get_Or_Create
                       (Name => Get_Symbols (Handler.Db).Find
                          (Locale_To_UTF8
                             (Get_Name_String
                                (Xref_Entity.Table (Entity).Entity))),
                        File =>
                          Sfiles (Xref_Section.Table (Sect).File_Num).File,
                        Line => Integer (Xref_Entity.Table (Entity).Line),
                        Column => Visible_Column_Type
                          (Xref_Entity.Table (Entity).Col));
                  end if;
               end loop;
            end loop;
         end loop;
      end if;

      if Active (Assert_Me) then
         Trace (Assert_Me,
                "Need to resolve closure: parsing "
                & (+Base_Name (Get_Filename (Sfiles (File_Num).File)))
                & " at " & Line'Img & Column'Img);
      end if;

      S := Get_Source_Info_Internal
        (Handler               => Handler,
         Source_Filename       => Get_Filename (Sfiles (File_Num).File),
         File_Has_No_LI_Report => null,
         Reset_ALI             => False);

      Find_Declaration
        (Db              => Get_Database (LI),
         File_Name       => Get_Filename (S),
         Entity_Name     => "",  --  Unknown, we are looking for it
         Line            => Integer (Line),
         Column          => Visible_Column_Type (Column),
         Entity          => Entity,
         Status          => Status,
         Check_Decl_Only => False);

      if Status = Success
        or else (Allow_Fuzzy and Status = Fuzzy_Match)
      then
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
         Xref_Entity.Table (Xref_Ent).Rref_Col, First_Sect, Last_Sect,
         Allow_Fuzzy => False);
   begin
      if Renaming /= null then
         Set_Is_Renaming_Of (Entity, Renaming);
      else
         if Active (Assert_Me) then
            Trace (Assert_Me, "Couldn't resolve renaming at "
                   & Xref_Section.Table (Xref_Sect).File_Num'Img
                   & Xref_Entity.Table (Xref_Ent).Rref_Line'Img
                   & Xref_Entity.Table (Xref_Ent).Rref_Col'Img);
         end if;
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
         --  In the ALI file, the predefined entities are always lower-cased
         --  when in fact the entity name in GPS is expected to have its
         --  proper casing. For most users, capitalization is more suitable
         --  than all lower-case when the language is case insensitive (the
         --  ALI parser is used also for C/C++ xref info produced by GCC).

         declare
            Name : constant String :=
              Get_Name_String
                (Xref_Entity.Table (Xref_Ent).Tref_Standard_Entity);
         begin
            if Case_Insensitive_Identifiers (Handler) then
               Parent := Get_Or_Create
                 (Name   => Get_Symbols (Handler.Db).Find
                    (Locale_To_UTF8 (Capitalize (Name))),
                  File   => Get_Predefined_File (Get_Database (LI), Handler),
                  Line   => Predefined_Line,
                  Column => Predefined_Column);
            else
               Parent := Get_Or_Create
                 (Name   => Get_Symbols (Handler.Db).Find
                    (Locale_To_UTF8 (Name)),
                  File   => Get_Predefined_File (Get_Database (LI), Handler),
                  Line   => Predefined_Line,
                  Column => Predefined_Column);
            end if;
         end;
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
                   & Display_Full_Name (Get_LI_Filename (LI))
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

   ----------------------------
   -- Process_Overriding_Ref --
   ----------------------------

   procedure Process_Overriding_Ref
     (Handler               : access ALI_Handler_Record'Class;
      LI                    : LI_File;
      Entity                : Entity_Information;
      Xref_Ent              : Nat;
      Sfiles                : Sdep_To_Sfile_Table;
      First_Sect, Last_Sect : Nat)
   is
      Parent : Entity_Information;
   begin
      Parent := Find_Entity_In_ALI
        (Handler    => Handler,
         LI         => LI,
         Sfiles     => Sfiles,
         File_Num   => Xref_Entity.Table (Xref_Ent).Oref_File_Num,
         Line       => Xref_Entity.Table (Xref_Ent).Oref_Line,
         Column     => Xref_Entity.Table (Xref_Ent).Oref_Col,
         First_Sect => First_Sect,
         Last_Sect  => Last_Sect);

      if Parent = null then
         if Active (Assert_Me) then
            Trace (Assert_Me,
                   "Overriding type not found in ALI file: "
                   & Display_Full_Name (Get_LI_Filename (LI))
                   & Xref_Entity.Table (Xref_Ent).Oref_File_Num'Img
                   & Xref_Entity.Table (Xref_Ent).Oref_Line'Img
                   & Xref_Entity.Table (Xref_Ent).Oref_Col'Img);
         end if;
      else
         Set_Overriden_Entity (Entity, Parent);
      end if;
   end Process_Overriding_Ref;

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
      Project : constant Project_Type := Get_Project (LI);
      Imported_Projects : Project_Type_Array
        (1 .. Imported_Projects_Count (Project));
      Is_ALI_For_Separate : Boolean := False;

   begin
      Get_Imported_Projects (Project, Imported_Projects);

      Process_Units (Handler, LI, New_ALI, Sunits);
      Process_Sdeps (Handler, LI, New_ALI, Sunits, Sfiles);

      --  We do not want to generate xref information if we are parsing the
      --  LI file for a separate unit, since such ALI files can only exist when
      --  the user has manually compiled the source, and parsing these would
      --  result in duplicate references, in particular for bodies (and thus
      --  we end up with infinite loop in Find_Body, see D804-012)

      if Sunits'Length /= 1 then
         Is_ALI_For_Separate := False;
      else
         for Dep in Sfiles'Range loop
            if Sfiles (Dep).File = Sunits (Sunits'First) then
               Is_ALI_For_Separate := Sfiles (Dep).Is_Separate;
               exit;
            end if;
         end loop;
      end if;

      if not Is_ALI_For_Separate then
         Process_Withs (Sunits, Sfiles, Imported_Projects);
         Process_Xrefs (Handler, LI, Sfiles, First_Sect, Last_Sect);
      end if;
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
      pragma Warnings (Off);
      function Convert is new Ada.Unchecked_Conversion
        (GNAT.Strings.String_Access, Text_Buffer_Ptr);
      pragma Warnings (On);
      Full   : constant Filesystem_String := Full_Name (ALI_Filename);
      Buffer : GNAT.Strings.String_Access := Read_File (ALI_Filename);

   begin
      if Buffer = null then
         Trace (Me, "Couldn't open " & ALI_Filename.Display_Full_Name);
         Result := No_ALI_Id;

      else
         if Active (Assert_Me) then
            Trace (Assert_Me,
                   "Parsing " & ALI_Filename.Display_Full_Name &
                   " Reset=" & Reset_First'Img);
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
         Namet.Name_Buffer (1 .. Full'Length) := +Full;
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
     (Handler   : access ALI_Handler_Record'Class;
      LI        : LI_File;
      Reset_ALI : Boolean;
      Force_Update : Boolean := False) return Boolean
   is
      New_ALI_Id            : ALI_Id := No_ALI_Id;
      New_Timestamp         : Time := No_Time;
      First_Sect, Last_Sect : Nat;
      Do_Update             : Boolean;
      Dummy                 : Boolean;
      pragma Unreferenced (Dummy);

   begin
      Assert (Assert_Me, LI /= null, "No LI to update");

      case Frozen (Handler.Db) is
         when No_Create_Or_Update =>
            Do_Update := Force_Update;

         when Create_Only =>
            Do_Update := Force_Update or Get_Timestamp (LI) = No_Time;

         when Create_And_Update =>
            Do_Update := True;
      end case;

      if Do_Update then
         New_Timestamp := File_Time_Stamp (Get_LI_Filename (LI));
         Do_Update := Get_Timestamp (LI) /= New_Timestamp;
      end if;

      if Do_Update then
         if Active (Assert_Me) then
            Trace (Assert_Me, "Load_And_Scan_ALI: "
                   & Display_Full_Name (Get_LI_Filename (LI))
                   & " since timestamp incorrect: old="
                   & Image (Get_Timestamp (LI), "%D-%T")
                   & " new="
                   & Image (New_Timestamp, "%D-%T"));
         else
            Trace (Me, "Load_And_Scan_ALI: "
                   & Display_Full_Name (Get_LI_Filename (LI)));
         end if;

         Set_Time_Stamp (LI, New_Timestamp);
         Reset (LI);

         Load_And_Scan_ALI
           (ALI_Filename   => Get_LI_Filename (LI),
            Reset_First    => Reset_ALI,
            Result         => New_ALI_Id,
            First_Sect     => First_Sect,
            Last_Sect      => Last_Sect);

         if New_ALI_Id = No_ALI_Id then
            Trace
              (Me, "Cannot parse " & Display_Full_Name (Get_LI_Filename (LI)));
            return False;
         end if;

         Create_New_ALI (Handler, LI, ALIs.Table (New_ALI_Id),
                         First_Sect, Last_Sect);

         if Active (SLI_Support) and then Is_ALI_File (LI) then
            Dummy := Update_ALI
              (Handler, Get_SLI_From_ALI (LI), Reset_ALI => False);
         end if;

      elsif Active (SLI_Support) and then Is_ALI_File (LI) then
         return Update_ALI (Handler, Get_SLI_From_ALI (LI), Reset_ALI);
      end if;

      return True;

   exception
      when E : others =>
         Trace (Traces.Exception_Handle, "Unexpected error while parsing "
                & Display_Full_Name (Get_LI_Filename (LI)) & ": "
                & Exception_Information (E));
         return False;
   end Update_ALI;

   ----------------
   -- Locate_ALI --
   ----------------

   procedure Locate_ALI
     (Handler            : access ALI_Handler_Record'Class;
      Short_ALI_Filename : Filesystem_String;
      Source_Filename    : GNATCOLL.VFS.Virtual_File;
      Project            : Project_Type;
      LI_Filename        : out Virtual_File;
      Predefined         : out Boolean)
   is
      procedure Next_Candidate
        (Last : in out Integer; Dot : Filesystem_String);
      --  Move Last so that Short_ALI_Filename (1 .. Last) is the name of the
      --  next file to test. This might be the parent unit

      --------------------
      -- Next_Candidate --
      --------------------

      procedure Next_Candidate
        (Last : in out Integer; Dot : Filesystem_String) is
      begin
         while Last > Short_ALI_Filename'First loop
            Last := Last - 1;
            exit when (Last + Dot'Length - 1 <= Short_ALI_Filename'Last
                       and then Equal
                         (Short_ALI_Filename
                            (Last .. Last + Dot'Length - 1),
                          Dot))

            --  Special case when there might be a confusion with the
            --  GNAT runtime files.
              or else (+Dot = "-" and then Short_ALI_Filename (Last) = '~');
         end loop;
         Last := Last - 1;
      end Next_Candidate;

      P                  : Project_Type := Project;
      Extension : constant Filesystem_String :=
        File_Extension (Short_ALI_Filename);
      Is_Parent_LI : Boolean := False;

   begin
      Predefined := False;
      LI_Filename := GNATCOLL.VFS.No_File;

      if P /= No_Project then
         --  Start searching in the extending projects, in case the file was
         --  recompiled in their context

         while Extending_Project (P) /= No_Project loop
            P := Extending_Project (P);
         end loop;

         --  Compute the search path. If the objects path of the project is
         --  not null, then prepend it to the total search path.
         while LI_Filename = GNATCOLL.VFS.No_File
           and then P /= No_Project
         loop
            declare
               Last    : Integer := Short_ALI_Filename'Last - Extension'Length;
               Dot_Replacement : constant Filesystem_String :=
                 +P.Attribute_Value
                 (Dot_Replacement_Attribute, Default => "-");
            begin
               while LI_Filename = GNATCOLL.VFS.No_File
                 and then Last >= Short_ALI_Filename'First
               loop
                  declare
                     Path : constant File_Array :=
                              Object_Path (P, False, True, True);
                     File : constant Filesystem_String :=
                       (Short_ALI_Filename (Short_ALI_Filename'First .. Last)
                        & Extension);
                  begin
                     if Path'Length /= 0 then
                        LI_Filename := Locate_Regular_File (File, Path);
                     end if;
                  end;

                  Is_Parent_LI :=
                    Last /= Short_ALI_Filename'Last - Extension'Length;
                  Next_Candidate (Last, Dot_Replacement);
               end loop;
            end;

            if P /= Project then
               P := Extended_Project (P);
            else
               P := No_Project;
            end if;
         end loop;
      end if;

      --  Still not found ? Check in the predefined object path
      --  We used to search in the current directory as well, but for ALI files
      --  that is irrelevant: they must be in one of the projects' object_dir

      if LI_Filename = GNATCOLL.VFS.No_File then
         declare
            Predefined_Object_Path : constant File_Array :=
              Handler.Registry.Environment.Predefined_Object_Path;
            Last : Integer := Short_ALI_Filename'Last - Extension'Length;
         begin
            Predefined := True;
            while LI_Filename = GNATCOLL.VFS.No_File
              and then Last >= Short_ALI_Filename'First
            loop
               LI_Filename := Locate_Regular_File
                 (Short_ALI_Filename
                    (Short_ALI_Filename'First .. Last) & Extension,
                  Predefined_Object_Path);
               Next_Candidate (Last, ".");
            end loop;
         end;
      end if;

      if LI_Filename = GNATCOLL.VFS.No_File then
         if Active (SLI_Support)
           and then Short_ALI_Filename
                      (Short_ALI_Filename'Last - 3 .. Short_ALI_Filename'Last)
             = ".ali"
         then
            Locate_ALI
              (Handler,
               Short_ALI_Filename
                 (Short_ALI_Filename'First .. Short_ALI_Filename'Last - 3)
                 & "sli",
               Source_Filename,
               Project,
               LI_Filename,
               Predefined);
         end if;
      else
         declare
            LI : LI_File;
         begin
            if Is_Parent_LI then
               --  Check whether the ALI file contains the information for
               --  the file itself

               LI := Get_Or_Create
                 (Db        => Handler.Db,
                  File      => LI_Filename,
                  Project   => Project);

               if LI = null then
                  LI_Filename := GNATCOLL.VFS.No_File;

               --  Do not reset ALI below, since we are in the process of
               --  parsing an ALI file, and need to parse a second one. We
               --  still need to keep the data for the first in memory, though
               elsif not Update_ALI (Handler, LI, Reset_ALI => False)
                 or else not Check_LI_And_Source (LI, Source_Filename)
               then
                  LI_Filename := GNATCOLL.VFS.No_File;
               end if;
            end if;
         end;
      end if;
   end Locate_ALI;

   ----------------------
   -- Get_ALI_Filename --
   ----------------------

   function Get_ALI_Filename
     (Handler   : access ALI_Handler_Record;
      Base_Name : Filesystem_String) return Filesystem_String
   is
      Last_Dot : Natural := Base_Name'Last;
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

      return Base_Name (Base_Name'First .. Last_Dot - 1)
              & Get_ALI_Ext (Handler);
   end Get_ALI_Filename;

   -------------------------
   -- Find_Multi_Unit_ALI --
   -------------------------

   function Find_Multi_Unit_ALI
     (Handler         : access ALI_Handler_Record'Class;
      Source_Filename : Virtual_File;
      Project         : Project_Type) return Virtual_File
   is
      --  The separator character depends on the file system ('$' in most
      --  cases, '~' on VMS).
      Char     : constant Character :=
                   Multi_Unit_Index_Char (Build_Server);
      ALI_Ext  : constant Filesystem_String := Get_ALI_Ext (Handler);

      P        : Project_Type := Project;
      Index    : Natural;
      LI       : LI_File;
      Has_Multi_Unit_Src : Boolean := False;

   begin
      --  Start searching in the extending projects, in case the file was
      --  recompiled in their context

      if Project /= No_Project then
         Has_Multi_Unit_Src := Has_Multi_Unit_Sources (P);
         while Extending_Project (P) /= No_Project loop
            P := Extending_Project (P);
            Has_Multi_Unit_Src :=
              Has_Multi_Unit_Src or else Has_Multi_Unit_Sources (P);
         end loop;
      end if;

      if not Has_Multi_Unit_Src then
         Trace (Me, "Project does not have multi-unit sources: "
                & Project.Name);
         --  No need to do any work here, since there are no multi unit sources
         return GNATCOLL.VFS.No_File;
      end if;

      --  Search for all candidate ALI files. We might end up parsing too many,
      --  but the result is not lost, since put in the entities database, and
      --  therefore we'll know the name of the ALI files for the other units
      --  next time, and thus save this expensive loop.

      while P /= No_Project loop
         declare
            Paths : constant File_Array :=
                      Object_Path (P, False, True, True);
            Path  : Virtual_File;
            Files : File_Array_Access;
         begin
            if Paths'Length > 0 then
               Path := Paths (Paths'First);
               Files := Read_Dir (Path);

               for J in Files'Range loop
                  declare
                     Base : constant Filesystem_String :=
                              Files (J).Base_Name (ALI_Ext);
                  begin
                     if Files (J).Has_Suffix (ALI_Ext) then
                        --  If we have a '~' followed by a digit, we likely
                        --  are seeing a multi-unit source file ALI, so
                        --  parse it anyway. We need to test for the digit
                        --  afterward, otherwise we would end up reloading
                        --  all a~ * .ali files from the runtime which is
                        --  too expensive.

                        Index := Base'Last - 1;

                        while Index > Base'First
                          and then Base (Index) /= Char
                        loop
                           Index := Index - 1;
                        end loop;

                        if Index > Base'First
                          and then Is_Digit (Base (Index + 1))
                        then
                           LI := Get_Or_Create
                             (Db        => Handler.Db,
                              File      => Files (J),
                              Project   => P);

                           --  Do not reset ALI below, since we are in the
                           --  process of parsing an ALI file, and need to
                           --  parse a second one. We still need to keep the
                           --  data for the first in memory, though.
                           if LI /= null
                             and then Update_ALI
                               (Handler, LI, Reset_ALI => False)
                             and then Check_LI_And_Source
                               (LI, Source_Filename)
                           then
                              return Files (J);
                           end if;
                        end if;
                     end if;
                  end;
               end loop;

               Unchecked_Free (Files);
            end if;
         end;

         --  Check other projects earlier in the extending tree
         if P /= Project then
            P := Extended_Project (P);
         else
            P := No_Project;
         end if;
      end loop;

      return GNATCOLL.VFS.No_File;
   end Find_Multi_Unit_ALI;

   -----------------------------
   -- LI_Filename_From_Source --
   -----------------------------

   procedure LI_Filename_From_Source
     (Handler         : access ALI_Handler_Record'Class;
      Source_Filename : Virtual_File;
      Project         : Project_Type;
      LI              : out Virtual_File;
      Predefined      : out Boolean)
   is
      Ext  : Project_Type;
      Info : File_Info;
   begin
      Info := Handler.Registry.Tree.Info (Source_Filename);

      if Active (Me) then
         Trace (Me, "LI_Filename_From_Source "
                & Display_Full_Name (Source_Filename)
                & " project=" & Display_Full_Name (Project_Path (Project))
                & " part=" & Info.Unit_Part'Img
                & " unit=" & Info.Unit_Name);
      end if;

      --  Do we have a runtime file ?

      case Info.Unit_Part is
         when Unit_Body | Unit_Separate =>
            --  Check the most likely ALI file (<file>.ali)
            Locate_ALI
              (Handler,
               Get_ALI_Filename (Handler, Base_Name (Source_Filename)),
               Source_Filename, Project, LI, Predefined);

            if LI /= GNATCOLL.VFS.No_File then
               return;
            end if;

            --  If the source comes from an extended project, look for object
            --  files there in addition

            if Project /= No_Project then
               Ext := Extended_Project (Project);
               if Ext /= No_Project then
                  Trace (Me, "Checking again in extended project");
                  LI_Filename_From_Source
                    (Handler, Source_Filename, Ext, LI, Predefined);

                  if LI /= GNATCOLL.VFS.No_File then
                     return;
                  end if;
               end if;
            end if;

            declare
               Unit : constant String := Info.Unit_Name;
               Last : Integer := Unit'Last;
            begin
               while Last >= Unit'First
                 and then Unit (Last) /= '.'
               loop
                  Last := Last - 1;
               end loop;

               if Last >= Unit'First then
                  --  When using non-standard naming schemes, separate units
                  --  are reported as bodies, but they have no direct ALI file.
                  --  Thus, in addition to checking directly for an ALI file,
                  --  we also check for ALI file from the parent unit.

                  Locate_ALI
                    (Handler,
                     Get_ALI_Filename
                       (Handler,
                        Project.File_From_Unit
                          (Unit (Unit'First .. Last - 1), Unit_Body,
                           Language => "ada")),
                     Source_Filename,
                     Project,
                     LI,
                     Predefined);

               else
                  --  We might have a multi-unit source file, in which case we
                  --  need to traverse all directories and look for candidates.
                  --  This is a little slower, therefore the above algorithm is
                  --  preferred.

                  Trace (Me, "Will Find_Multi_Unit_ALI");
                  LI := Find_Multi_Unit_ALI
                    (Handler, Source_Filename, Project);
                  Predefined := False;
               end if;
            end;

         when Unit_Spec =>
            --  Use the ALI for the body, if there is a body, otherwise the one
            --  for the spec will do.

            Locate_ALI
              (Handler,
               Get_ALI_Filename
                 (Handler,
                  Handler.Registry.Tree.Other_File
                    (Source_Filename).Base_Name),
               Source_Filename,
               Project, LI, Predefined);

            if LI /= GNATCOLL.VFS.No_File then
               return;
            end if;

            --  If the source comes from an extended project, look for object
            --  files there in addition

            if Project /= No_Project then
               Ext := Extended_Project (Project);
               if Ext /= No_Project then
                  LI_Filename_From_Source
                    (Handler, Source_Filename, Ext, LI, Predefined);
                  if LI /= GNATCOLL.VFS.No_File then
                     return;
                  end if;
               end if;
            end if;

            --  No ALI for the body ? Use the one for the spec as a fallback

            Locate_ALI
              (Handler,
               Get_ALI_Filename (Handler, Base_Name (Source_Filename)),
               Source_Filename, Project, LI, Predefined);

            if LI /= GNATCOLL.VFS.No_File then
               return;
            end if;

            --  Still not found ? We might have a multi-unit source file, check
            --  this.

            LI := Find_Multi_Unit_ALI
              (Handler, Source_Filename, Project);
            Predefined := False;
      end case;
   end LI_Filename_From_Source;

   ---------------------
   -- Get_Source_Info --
   ---------------------

   overriding function Get_Source_Info
     (Handler               : access ALI_Handler_Record;
      Source_Filename       : GNATCOLL.VFS.Virtual_File;
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
      Source_Filename       : GNATCOLL.VFS.Virtual_File;
      File_Has_No_LI_Report : File_Error_Reporter := null;
      Reset_ALI             : Boolean) return Source_File
   is
      LI      : LI_File;
      LI_Name : Virtual_File;
      Source  : Source_File;
      Project : Project_Type;
      Predefined : Boolean;
      Is_Up_To_Date : Boolean;
   begin
      --  If we already know about the file, we get the name of the LI file
      --  from it.

      Source := Get_Or_Create
        (Db           => Handler.Db,
         File         => Source_Filename,
         LI           => null,
         Handler      => LI_Handler (Handler),
         Allow_Create => False);

      case Frozen (Handler.Db) is
         when No_Create_Or_Update =>
            return Source;

         when Create_Only =>
            if Source /= null then
               return Source;
            end if;

         when Create_And_Update =>
            null;
      end case;

      if Source /= null
        and then Get_LI (Source) /= null
      then
         --  If we have a file outside of the project hierarchy, its location
         --  can't have changed when a scenario has changed, so no need to
         --  check directories

         Is_Up_To_Date := Get_Project (Get_LI (Source)) = No_Project;

         if not Is_Up_To_Date then
            --  Check that the object directory hasn't changed (in case the
            --  user changed the scenario for instance)
            --  The call to Object_Path might return several directories in the
            --  case of a library project for instance, so we can't simply
            --  compare strings.

            declare
               Dir  : constant Virtual_File :=
                        Get_Parent (Get_LI_Filename (Get_LI (Source)));
               Path : constant File_Array :=
                        Object_Path
                          (Get_Project (Get_LI (Source)), False, True, True);
            begin
               for J in Path'Range loop
                  Is_Up_To_Date := Path (J) = Dir;

                  exit when Is_Up_To_Date;
               end loop;
            end;
         end if;

         --  If we are still using the same LI file, update its contents now.
         --  Otherwise, we'll have to find the new LI file

         if Is_Up_To_Date then
            if not Update_ALI
              (Handler, Get_LI (Source), Reset_ALI => Reset_ALI)
              and then File_Has_No_LI_Report /= null
            then
               Entities.Error
                 (File_Has_No_LI_Report.all, Source);
            end if;

            return Source;
         end if;
      end if;

      --  Otherwise we have to compute the name of the LI file from scratch.
      --  The call below might result in No_Project for runtime files

      Project := Handler.Registry.Tree.Info (Source_Filename).Project;

      LI_Filename_From_Source
        (Handler, Source_Filename, Project, LI_Name, Predefined);

      if LI_Name = GNATCOLL.VFS.No_File then
         if Active (Me) then
            Trace (Me, "No LI found for "
                   & Display_Full_Name (Source_Filename)
                   & " in project " & Project.Name);
         end if;

         if File_Has_No_LI_Report /= null then
            Entities.Error (File_Has_No_LI_Report.all, Source);
         end if;

         return Source;
      else
         if Active (Assert_Me) then
            Trace (Assert_Me, "LI for " & Display_Full_Name (Source_Filename)
                   & " is " & Display_Full_Name (LI_Name));
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

      --  Do another lookup, to update the LI file, since apparently we
      --  didn't know it before
      return Get_Or_Create
        (Db           => Handler.Db,
         File         => Source_Filename,
         LI           => LI,
         Handler      => LI_Handler (Handler),
         Allow_Create => False);
   end Get_Source_Info_Internal;

   ----------------------------------
   -- Case_Insensitive_Identifiers --
   ----------------------------------

   overriding function Case_Insensitive_Identifiers
     (Handler : access ALI_Handler_Record) return Boolean
   is
      pragma Unreferenced (Handler);
   begin
      return True;
   end Case_Insensitive_Identifiers;

   ------------------------------
   -- Parse_All_LI_Information --
   ------------------------------

   overriding function Parse_All_LI_Information
     (Handler   : access ALI_Handler_Record;
      Project   : Project_Type) return LI_Information_Iterator'Class
   is
      Iter     : ALI_Information_Iterator;
      Tmp      : File_Array_Access;
      Objects  : constant File_Array :=
        Object_Path (Project, False, True, True);
   begin
      Freeze (Handler.Db, Mode => Create_Only);

      Trace (Me, "Parse_All_LI_Information in project "
             & Project.Name);

      --  Find all the files to parse immediately. This provides an accurate
      --  count of the total number of files to process, which is useful for
      --  display purposes, and does not require more system calls

      for Dir in Objects'Range loop
         begin
            --  Get the whole content, including subdirs (to avoid system calls
            --  to stat()). Since we will be checking extensions anyway, this
            --  is fine
            --  ??? An issue might exist if we have a subdir with extension
            --  ".ali", but that's unlikely and not worth paying an extra cost
            --  systematically

            Tmp := Read_Dir (Objects (Dir));
            Append (Iter.Files, Tmp.all);
            Unchecked_Free (Tmp);
         exception
            when VFS_Directory_Error =>
               Trace (Me, "Couldn't open the directory " &
                      Objects (Dir).Display_Full_Name);
         end;
      end loop;

      if Iter.Files /= null then
         Iter.Current := Iter.Files'First;
      else
         Iter.Current := 0;
      end if;

      Iter.Handler := ALI_Handler (Handler);
      Iter.Project := Project;

      return Iter;
   end Parse_All_LI_Information;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Iter : in out ALI_Information_Iterator) is
   begin
      Unchecked_Free (Iter.Files);
      Thaw (Iter.Handler.Db);
   end Free;

   ----------
   -- Next --
   ----------

   overriding procedure Next
     (Iter  : in out ALI_Information_Iterator;
      Steps : Natural := Natural'Last;
      Count : out Natural;
      Total : out Natural)
   is
      LI      : LI_File;
      Steps_Done : Natural := 0;

   begin
      if Iter.Files = null then
         Count := 0;
         Total := 0;
         return;
      end if;

      while Iter.Current <= Iter.Files'Last
        and then Steps_Done <= Steps
      loop
         if Iter.Files
           (Iter.Current).Has_Suffix (Get_ALI_Ext (Iter.Handler))
         then
            LI := Get_Or_Create
              (Db      => Iter.Handler.Db,
               File    => Iter.Files (Iter.Current),
               Project => Iter.Project);

            --  We force the update of this ALI if the database is in
            --  'Create_Only' mode. In this mode, this will not force the
            --  update of dependent ALIs (which will be parsed later anyway).

            if not Update_ALI
              (Iter.Handler, LI,
               Reset_ALI => True,
               Force_Update => Frozen (Iter.Handler.Db) = Create_Only)
            then
               if Active (Me) then
                  Trace
                    (Me,
                     "Couldn't parse " &
                     Iter.Files (Iter.Current).Display_Full_Name);
               end if;
            end if;
         end if;

         Steps_Done   := Steps_Done + 1;
         Iter.Current := Iter.Current + 1;
      end loop;

      Count := Iter.Current;

      if Iter.Files = null then
         Total := 0;
      else
         Total := Iter.Files'Length;
      end if;
   end Next;

   -----------------------------
   -- Generate_LI_For_Project --
   -----------------------------

   overriding function Generate_LI_For_Project
     (Handler      : access ALI_Handler_Record;
      Lang_Handler : access Abstract_Language_Handler_Record'Class;
      Project      : Project_Type;
      Errors       : Error_Report;
      Recursive    : Boolean := False) return LI_Handler_Iterator'Class
   is
      pragma Unreferenced (Handler, Project, Recursive, Lang_Handler, Errors);
      Iterator : ALI_Handler_Iterator;
   begin
      return Iterator;
   end Generate_LI_For_Project;

   ------------------------
   -- Create_ALI_Handler --
   ------------------------

   function Create_ALI_Handler
     (Db       : Entities.Entities_Database;
      Registry : Project_Registry'Class) return Entities.LI_Handler
   is
      ALI : constant ALI_Handler := new ALI_Handler_Record;
   begin
      ALI.Db       := Db;
      ALI.Registry := Project_Registry (Registry);
      return LI_Handler (ALI);
   end Create_ALI_Handler;

   --------------
   -- Continue --
   --------------

   overriding procedure Continue
     (Iterator : in out ALI_Handler_Iterator;
      Errors   : Error_Report;
      Finished : out Boolean)
   is
      pragma Unreferenced (Iterator, Errors);
   begin
      Finished := True;
   end Continue;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy (Iterator : in out ALI_Handler_Iterator) is
      pragma Unreferenced (Iterator);
   begin
      null;
   end Destroy;

   --------------
   -- Get_Name --
   --------------

   overriding function Get_Name
     (LI : access ALI_Handler_Record) return String
   is
      pragma Unreferenced (LI);
   begin
      return "Ada";
   end Get_Name;

   -----------------
   -- Get_ALI_Ext --
   -----------------

   function Get_ALI_Ext
     (LI : access ALI_Handler_Record) return Filesystem_String is
      pragma Unreferenced (LI);
   begin
      return ".ali";
   end Get_ALI_Ext;

begin
   --  Enable support for ".sli" files if spark toolset is available

   if not Active (SLI_Support) then
      declare
         S : String_Access := Locate_Exec_On_Path ("spark");
      begin
         if S /= null then
            Set_Active (SLI_Support, True);
            Free (S);
         end if;
      end;
   end if;
end ALI_Parser;
