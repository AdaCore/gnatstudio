-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2003                       --
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

with ALI;                       use ALI;
with Ada.Characters.Handling;   use Ada.Characters.Handling;
with Ada.Exceptions;            use Ada.Exceptions;
with Glib.Convert;              use Glib.Convert;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with Krunch;
with Namet;                     use Namet;
with Projects.Registry;         use Projects, Projects.Registry;
with Src_Info.Prj_Utils;        use Src_Info.Prj_Utils;
with Src_Info.LI_Utils;         use Src_Info.LI_Utils;
with Types;                     use Types;
with Traces;                    use Traces;
with VFS;                       use VFS;
with Ada.Calendar;              use Ada.Calendar;

package body Src_Info.ALI is

   Me : constant Debug_Handle := Create ("Src_Info.Ali");

   Maximum_Filename_Length : constant := 8;
   --  ??? The maximum number of characters in a krunched filename (not
   --  including the .ads/adb extension). Should probably be more dynamic,
   --  but this is not terribly important since most of the time, only the
   --  Run-time file are krunched, and we know that their max_len is 8.

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

   type Char_To_Reference_Kind_Map is array
     (Character range ' ' .. 'z') of Reference_Kind;
   Char_To_Reference_Kind : constant Char_To_Reference_Kind_Map :=
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

   type Tref_Kind_Array is array (Tref_Kind) of Parent_Kind;
   Tref_Kind_To_Parent_Kind : constant Tref_Kind_Array :=
     (Tref_None    => Parent_Type,     --   unused
      Tref_Access  => Pointed_Type,    --   () in ALI
      Tref_Derived => Parent_Type,     --   <> in ALI
      Tref_Type    => Container_Type); --   {} in ALI

   type Sdep_To_Sfile_Table is array (Sdep_Id range <>) of Source_File;
   --  An array used to store the Source_File data for each Sdep ID in
   --  the Sdep table.

   ALI_Internal_Error : exception;
   --  An exception raised when an internal error is detected. It should not
   --  be propagated outside of this package.

   procedure Destroy (T : in out Sdep_To_Sfile_Table);
   pragma Warnings (Off, Destroy);
   --  Free the memory allocated for the given table.
   --  ??? Not used for now

   function Char_To_E_Kind (C : Character) return E_Kind;
   pragma Inline (Char_To_E_Kind);
   --  Translate the given character into the associated E_Kind value.
   --  Raise ALI_Internal_Error if C does not represent any E_Kind value.

   function Char_To_R_Kind (C : Character) return Reference_Kind;
   pragma Inline (Char_To_R_Kind);
   --  Translate the given character into the associated Reference_Kind value.
   --  Raise ALI_Internal_Error if C does not represent any Reference_Kind.

   function Get_ALI_Filename (Base_Name : String) return String;
   --  Converts the given source filename File_Name into the corresponding
   --  ALI filename.
   --  If Project is not No_Project, then the full path of the ALI file is
   --  searched

   function Strip_Unit_Part (Unit_Name : String) return String;
   --  Strip a trailing '%s' or '%b' if present.

   function Krunch
     (Filename : String;
      Max_Len  : Natural := Maximum_Filename_Length) return String;
   --  Krunch the given source filename to at most Max_Len characters
   --  (file extension not included).

   procedure Get_Source_File
     (Handler          : ALI_Handler;
      List             : LI_File_List;
      New_ALI          : ALIs_Record;
      Source_Filename  : File_Name_Type;
      Subunit_Name     : Name_Id := No_Name;
      Project          : Project_Type;
      File             : out Source_File);
   --  Search the unit in LI_File_List which source file is equal to
   --  Source_Filename. If Subunit_Name is set, then only separates are
   --  searched, otherwise, spec and body parts only are searched. If no such
   --  LI_File corresponding to the Source_Filename is found, then a stub one
   --  is created. Note that in the case of non-separate source files for which
   --  a stub is created, the Unit_Name field is left to Null since we don't
   --  have this information yet.

   procedure Get_Unit_Source_File
     (Handler          : ALI_Handler;
      List             : LI_File_List;
      Source_Filename  : Virtual_File;
      Project          : Project_Type;
      File             : out Source_File);
   --  Perform the job of Get_Source_File in the case where Subunit_Name is not
   --  set (case when it is not a separate).

   procedure Get_Unit_Source_File
     (Handler          : ALI_Handler;
      List             : LI_File_List;
      Source_Filename  : Virtual_File;
      Sig_Base_Name    : String;
      Project          : Project_Type;
      Part             : Unit_Part;
      File             : out Source_File);
   --  Perform the job of Get_Unit_Source_File knowing that the Source Filename
   --  from which the ALI filename is derived is Sig_Filename.

   procedure Get_Subunit_Source_File
     (Handler          : ALI_Handler;
      List             : LI_File_List;
      Source_Filename  : Virtual_File;
      Sig_Base_Name    : String;
      Project          : Project_Type;
      Subunit_Name     : Name_Id;
      File             : out Source_File);
   --  Perform the job of Get_Subunit_Source_File knowing the name of the
   --  Source Filename from which the ALI filename is derived.

   function Load_And_Scan_ALI (ALI_Filename : Virtual_File) return ALI_Id;
   --  Load the given ALI file into a buffer and then scan it, filling in the
   --  information in the tables maintained in ali.ads. The new ID associated
   --  to the ALI file is returned. No_ALI_Id is returned if we failed to
   --  read or parse the ALI file.

   procedure Process_Unit
     (New_LI_File : LI_File_Ptr; Id : Unit_Id);
   --  Save the information from the given Unit into the LI_File_Ptr.
   --  Note that the with'ed units list is postponed for the moment.
   --  It is more convenient to process it separately after the dependency
   --  list is built, which we will then update by processing the list of
   --  with'ed units.

   procedure Process_Units (New_LI_File : LI_File_Ptr; New_ALI : ALIs_Record);
   --  Call Process_Unit for all Unit Ids associated to the given LI_File_Ptr.

   procedure Process_Sdep
     (Handler     : ALI_Handler;
      New_LI_File : LI_File_Ptr;
      New_ALI     : ALIs_Record;
      Id          : Sdep_Id;
      Project     : Project_Type;
      Sfiles      : in out Sdep_To_Sfile_Table;
      List        : LI_File_List);
   --  Save the information from the given dependency into the LI_File_Ptr.

   procedure Process_Sdep_As_Self
     (New_LI_File : LI_File_Ptr;
      Id          : Sdep_Id;
      Finfo       : File_Info_Ptr;
      Sfiles      : in out Sdep_To_Sfile_Table);
   --  Save the information from the given dependency into the LI_File_Ptr
   --  for cases where it is applicable to the Spec_Info or Body_Info.
   --  Which of Spec_Info or Body_Info is updated is selected by Finfo.

   procedure Process_Sdep_As_External
     (Handler     : ALI_Handler;
      New_LI_File : LI_File_Ptr;
      New_ALI     : ALIs_Record;
      Id          : Sdep_Id;
      Project     : Project_Type;
      Sfiles      : in out Sdep_To_Sfile_Table;
      List        : LI_File_List;
      Is_Separate : Boolean := False);
   --  Save the information from the given dependency into the list of
   --  dependencies of the LI_File_Ptr. This procedure should be used
   --  only for cases where the dependency is an external dependency.
   --  Is_Separate should be set to true if we are in fact processing a
   --  separate unit, which shouldn't be included in the list of separates for
   --  New_LI_File.

   procedure Process_Sdeps
     (Handler     : ALI_Handler;
      New_LI_File : LI_File_Ptr;
      New_ALI     : ALIs_Record;
      Project     : Project_Type;
      Sfiles      : in out Sdep_To_Sfile_Table;
      List        : LI_File_List);
   --  Call Process_Sdep for all Sdep entries in the given LI_File_Ptr.

   procedure Process_With
     (New_LI_File : LI_File_Ptr;
      Project     : Project_Type;
      UId         : Unit_Id;
      WId         : With_Id);
   --  Save the information associated to the given With_Record.

   procedure Process_Withs
     (New_LI_File : LI_File_Ptr;
      New_ALI     : ALIs_Record;
      Project     : Project_Type);
   --  Save the information associated to all the units withed from this Unit.

   procedure Chain_Declaration_For_Separate
     (New_LI_File     : LI_File_Ptr;
      Source_Filename : String;
      Decl_Info       : E_Declaration_Info);
   --  Inserts Decl_Info at the head of the list of declarations of the
   --  separate unit of New_LI_File whose name is Unit_Name.
   --  Raise ALI_Internal_Error if the separate unit is not found.

   procedure Chain_Declaration_For_Dependency
     (New_LI_File : LI_File_Ptr;
      Sfile       : Source_File;
      Decl_Info   : E_Declaration_Info);
   --  Search the Dependency_File_Info in the list of dependencies of
   --  New_LI_File for which the Source File value is equal to Sfile, and
   --  insert Decl_Info at the head of its declarations list. Raise
   --  ALI_Internal_Error if the Dependency_File_Info is not found.

   procedure Process_Xref_Entity
     (New_LI_File : LI_File_Ptr;
      New_ALI     : ALIs_Record;
      Section_Id  : Nat;
      Entity_Id   : Nat;
      Sfiles      : Sdep_To_Sfile_Table);
   --  Save the Xref Entity information in the New_LI_File structure.

   procedure Process_Xref_Section
     (New_LI_File : LI_File_Ptr;
      New_ALI     : ALIs_Record;
      Section_Id  : Nat;
      Sfiles      : Sdep_To_Sfile_Table);
   --  Save the Xref information associated to the given With_Record.

   procedure Process_Xrefs
     (New_LI_File : LI_File_Ptr;
      New_ALI     : ALIs_Record;
      Sfiles      : Sdep_To_Sfile_Table);
   --  Save the Xref information in the New_LI_File structure.

   procedure Create_New_ALI
     (Handler           : ALI_Handler;
      New_LI_File       : out LI_File_Ptr;
      New_ALI           : ALIs_Record;
      Project           : Project_Type;
      Full_ALI_Filename : Virtual_File;
      List              : LI_File_List);
   --  Create a new LI_File_Ptr from the given ALIs_Record. This LI_File_Ptr
   --  is left unconnected to the LI_File_List.

   procedure Parse_ALI_File
     (Handler                : ALI_Handler;
      Full_ALI_Filename      : Virtual_File;
      Project                : Project_Type;
      List                   : LI_File_List;
      Unit                   : out LI_File_Ptr;
      Success                : out Boolean);
   --  Parse the given ALI file and update the Unit Info list. Also returns
   --  a handle to the Unit Info corresponding to this ALI file to avoid
   --  searching right-back for this handle for immediate queries.

   procedure Create_Or_Complete_LI
     (Handler                : access ALI_Handler_Record'Class;
      File                   : in out LI_File_Ptr;
      Full_Ali_File          : Virtual_File;
      List                   : LI_File_List;
      Project                : Project_Type);
   --  Internal version of Create_Or_Complete_LI

   function Locate_ALI
     (Short_ALI_Filename : String;
      Project                : Project_Type) return String;
   --  Locate an ALI file on the object path. If not found, it will possibly
   --  search for the parent unit's ALI file (for proper handling of
   --  separates). There is no garantee that the returned file matches the
   --  intended source file, this is just the closest ALI file found on the
   --  path.
   --  The empty string is returned if nothing was found.

   -------------
   -- Destroy --
   -------------

   procedure Destroy (T : in out Sdep_To_Sfile_Table) is
   begin
      for Sdep_Index in T'Range loop
         if T (Sdep_Index) /= No_Source_File then
            Destroy (T (Sdep_Index));
         end if;
      end loop;
   end Destroy;

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
         --  No dot found in Sig_Filename, just append the ALI extension

         Last_Dot := Base_Name'Last + 1;
      end if;

      return Base_Name (Base_Name'First .. Last_Dot - 1) & ALI_Ext;
   end Get_ALI_Filename;

   ---------------------
   -- Strip_Unit_Part --
   ---------------------

   function Strip_Unit_Part (Unit_Name : String) return String is
      Last : constant Integer := Unit_Name'Last;
   begin
      if Unit_Name'Length > 2
        and then Unit_Name (Last - 1) = '%'
        and then (Unit_Name (Last) = 's' or else Unit_Name (Last) = 'b')
      then
         return Unit_Name (Unit_Name'First .. Last - 2);
      else
         return Unit_Name;
      end if;
   end Strip_Unit_Part;

   ------------
   -- Krunch --
   ------------

   function Krunch
     (Filename : String;
      Max_Len : Natural := Maximum_Filename_Length) return String
   is
      Flen    : constant Natural := Filename'Length;
      Fext    : constant String := File_Extension (Filename);
      Rlen    : Natural;
      Result  : String (1 .. Flen) := Filename;
      --  Result'First must be equal to 1 because of Krunch.Krunch.

      Sext : constant String := ".ads";
      Bext : constant String := ".adb";
      --  These constants must be defined somewhere in a common place???

   begin
      Assert (Me, Flen /= 0, "Null filename in Krunch");

      for R in Result'Range loop
         if Result (R) = '.' then
            Result (R) := '-';
         end if;
      end loop;

      if Fext = Sext then
         Rlen := Flen - Sext'Length;
         Krunch (Result, Rlen, Max_Len, False);
         return Result (1 .. Rlen) & Sext;

      elsif Fext = Bext then
         Rlen := Flen - Bext'Length;
         Krunch (Result, Rlen, Max_Len, False);
         return Result (1 .. Rlen) & Bext;

      else
         Rlen := Flen;
         Krunch (Result, Rlen, Max_Len, False);
         return Result (1 .. Rlen);
      end if;
   end Krunch;

   ---------------------
   -- Get_Source_File --
   ---------------------

   procedure Get_Source_File
     (Handler          : ALI_Handler;
      List             : LI_File_List;
      New_ALI          : ALIs_Record;
      Source_Filename  : File_Name_Type;
      Subunit_Name     : Name_Id := No_Name;
      Project          : Project_Type;
      File             : out Source_File)
   is
      Source : constant Virtual_File := Create_From_Base
        (Locale_To_UTF8 (Get_String (Source_Filename)));
      Prj : Project_Type;
   begin
      --  Search algorithm:
      --  =================
      --    The first step is to find the name of the ali file associated
      --    to the given source file. Once we have found it, it is then easy
      --    to create the Source_Filename (and the stub in the LI_File_List
      --    if necessary).
      --
      --    The intermediate goal is to find the source filename of the body,
      --    or of the spec if there is no body. Once we have this filename,
      --    deriving the ali filename is done by stripping the extension from
      --    the last '.' (if any) and adding ".ali".
      --
      --  Source filename search algorithm:
      --  =================================
      --
      --  First Case: This is not a separate (Subunit_Name = No_Name)
      --  -----------------------------------------------------------
      --
      --    1/ Check the body filename exception list. If found, DONE.
      --       (we have the filename, and we know it is a body).
      --
      --    2/ Check the spec filename exception list.
      --       If found then we have the Unit_Name. Search it in:
      --          i)    the body filename exception list
      --          ii)   the project units list (Prj.Com.Units)
      --       If still not found, then search the body filename (computed
      --       using the naming scheme):
      --          iii)  the Source_Path
      --       If the body was found then DONE (we have the body filename)
      --       If not found then DONE (we use the spec filename)
      --
      --    3/ Use the Naming_Scheme to deduct whether it is a Spec or a Body
      --       If this is a body, then DONE.
      --
      --    4/ If this is a spec, then use Naming_Scheme to get the Unit_Name
      --       and then search the body filename as described above in 2/.
      --       If the body was found, then DONE.
      --       If not found, then DONE too (we use the spec filename).
      --
      --  Second Case: We have a separate
      --  -------------------------------
      --
      --  The ALI file associated with the separate is the one we are currently
      --  parsing, so this case is much simpler.

      if Subunit_Name = No_Name then
         --  ??? Do we really need to find another project
         Prj := Get_Project_From_File
           (Project_Registry'Class (Get_Registry (Project)), Source);

         if Prj = No_Project then
            --  ??? We have a file that doesn't belong the an project.
            Prj := Project;
         end if;

         Get_Unit_Source_File (Handler, List, Source, Prj, File);
      else
         Get_Subunit_Source_File
           (Handler, List, Source,
            Locale_To_UTF8 (Get_String (New_ALI.Sfile)),
            Project,
            Subunit_Name, File);
      end if;
   end Get_Source_File;

   --------------------------
   -- Get_Unit_Source_File --
   --------------------------

   procedure Get_Unit_Source_File
     (Handler          : ALI_Handler;
      List             : LI_File_List;
      Source_Filename  : Virtual_File;
      Project          : Project_Type;
      File             : out Source_File) is
   begin
      case Get_Unit_Part_From_Filename (Project, Source_Filename) is
         when Unit_Body =>
            Get_Unit_Source_File
              (Handler, List, Source_Filename,
               Base_Name (Source_Filename), Project, Unit_Body, File);

         when Unit_Spec =>
            Get_Unit_Source_File
              (Handler, List, Source_Filename,
               Other_File_Base_Name (Project, Source_Filename),
               Project, Unit_Spec, File);

         when Unit_Separate =>
            raise ALI_Internal_Error;
      end case;
   end Get_Unit_Source_File;

   --------------------------
   -- Get_Unit_Source_File --
   --------------------------

   procedure Get_Unit_Source_File
     (Handler          : ALI_Handler;
      List             : LI_File_List;
      Source_Filename  : Virtual_File;
      Sig_Base_Name    : String;
      Project          : Project_Type;
      Part             : Unit_Part;
      File             : out Source_File)
   is
      ALI_Filename : constant String := Get_ALI_Filename (Sig_Base_Name);
      --   ??? Could we use Sname instead

   begin
      File :=
        (LI              => Get (List.Table.all, ALI_Filename),
         Part            => Part,
         Source_Filename => null);

      --  If there is not LI_File_Ptr yet for the given ALI_Filename then
      --  create a stub

      if File.LI = null then
         declare
            LI : Virtual_File :=
              Create (ALI_Filename, Project, Use_Source_Path => False);
         begin
            if LI = VFS.No_File then
               LI := Create_From_Base (ALI_Filename);
            end if;

            Create_LI_File
              (File        => File.LI,
               List        => List,
               Project     => Project,
               LI_Filename => LI,
               Handler     => LI_Handler (Handler));

            if File.LI = null then
               raise ALI_Internal_Error;
            end if;
         end;
      end if;

      --  If the associated File_Info does not exist, then create it.
      case Part is
         when Unit_Spec =>
            if File.LI.LI.Spec_Info = null then
               Create_File_Info (File.LI.LI.Spec_Info, Source_Filename);
            end if;

         when Unit_Body =>
            if File.LI.LI.Body_Info = null then
               Create_File_Info (File.LI.LI.Body_Info, Source_Filename);
            end if;

         when Unit_Separate =>
            --  programing error
            Trace (Me, "Get_Unit_Source_File: can't handle separate");
            raise ALI_Internal_Error;
      end case;
   end Get_Unit_Source_File;

   -----------------------------
   -- Get_Subunit_Source_File --
   -----------------------------

   procedure Get_Subunit_Source_File
     (Handler          : ALI_Handler;
      List             : LI_File_List;
      Source_Filename  : Virtual_File;
      Sig_Base_Name    : String;
      Project          : Project_Type;
      Subunit_Name     : Name_Id;
      File             : out Source_File)
   is
      Base : constant String := Base_Name (Source_Filename);
      ALI_Filename : constant String := Get_ALI_Filename (Sig_Base_Name);
      --   ??? Could we use Sname
      Sep          : File_Info_Ptr_List;

   begin
      File :=
         (LI             => Get (List.Table.all, Base_Name (ALI_Filename)),
          Part           => Unit_Separate,
         Source_Filename => new String'(Base));
      --  ??? No real need to duplicate the string above, since we know with
      --  the current implementation of VFS that Base will never be freed. But
      --  this is more secure, and the implementation of ALI tables will
      --  change anyway

      --  If there is no LI_File_Ptr yet for the given ALI_Filename then
      --  create a stub

      if File.LI = null then
         declare
            LI : constant String := Locate_ALI (ALI_Filename, Project);
         begin
            if LI /= "" then
               Create_LI_File
                 (File        => File.LI,
                  List        => List,
                  Project     => Project,
                  LI_Filename => Create (Full_Filename => LI),
                  Handler     => LI_Handler (Handler));
            else
               Create_LI_File
                 (File        => File.LI,
                  List        => List,
                  Project     => Project,
                  LI_Filename => Create_From_Base (ALI_Filename),
                  Handler     => LI_Handler (Handler));
            end if;
         end;

         if File.LI = null then
            Destroy (File);
            File := No_Source_File;
            raise ALI_Internal_Error;
         end if;
      end if;

      --  If the associated File_Info does not exist, then create it.

      Sep := File.LI.LI.Separate_Info;

      while Sep /= null loop
         exit when Sep.Value.Source_Filename.all = Base;
         Sep := Sep.Next;
      end loop;

      if Sep = null then
         File.LI.LI.Separate_Info := new File_Info_Ptr_Node'
           (Value => null, Next => File.LI.LI.Separate_Info);
         Create_File_Info
           (File.LI.LI.Separate_Info.Value, Source_Filename,
            Unit_Name => Locale_To_UTF8 (Get_String (Subunit_Name)));
      end if;

   exception
      when others =>
         Destroy (File);
         File := No_Source_File;
         raise;
   end Get_Subunit_Source_File;

   -----------------------
   -- Load_And_Scan_ALI --
   -----------------------

   function Load_And_Scan_ALI (ALI_Filename : Virtual_File) return ALI_Id is
      Full        : constant String := Full_Name (ALI_Filename).all;
      File_Length : Text_Ptr;
      FD          : File_Descriptor;
      Chars_Read  : Integer;
      Filename_Id : File_Name_Type;

   begin
      FD := Open_Read (Full, Fmode => Binary);

      if FD = Invalid_FD then
         Trace (Me, "Couldn't open " & Full);
         return No_ALI_Id;
      end if;

      begin
         File_Length := Text_Ptr (GNAT.OS_Lib.File_Length (FD));
      exception
         when Constraint_Error =>
            --  Probably the case where File_Length > Text_Ptr'Last (that
            --  means a huge ALI file). This is a very unlikely situation
            --  so this is not a big deal for now not to support such
            --  monstruous files. Just close the FD and return a failure.
            Close (FD);
            Trace (Me, "CE while reading length of " & Full);
            return No_ALI_Id;
      end;

      declare
         --  Do not declare Buffer aliased, since we would need to initialize
         --  it explicitely, due to the nature of Text_Buffer, which would be
         --  a vaste of time. Use 'Unrestricted_Access instead.

         Buffer     : Text_Buffer (1 .. File_Length);
         Buffer_Ptr : constant Text_Buffer_Ptr := Buffer'Unrestricted_Access;

      begin
         Chars_Read := Read (FD, Buffer'Address, Integer (File_Length));
         Close (FD);

         --  check that we read the entire file, or else report a failure
         if Text_Ptr (Chars_Read) /= File_Length then
            Trace (Me, "Couldn't read the whole file for " & Full);
            return No_ALI_Id;
         end if;

         --  Replace the last char by an EOF. Scan_ALI uses this character
         --  to detect the end of the buffer.
         Buffer (Buffer'Last) := EOF;

         --  Get the ID of the ALI_Filename in the Namet table
         Namet.Name_Buffer (1 .. Full'Length) := Full;
         Namet.Name_Len := Full'Length;
         Filename_Id := Namet.Name_Find;

         --  Free the memory occupied by previous runs
         Initialize_ALI;

         return Scan_ALI
           (Filename_Id, Buffer_Ptr,
            Ignore_ED => True, Err => True,
            Read_Xref => True);
         --  Notes:
         --    - Ignored_ED = True because we don't need the Elaboration
         --      Desirable indications for the moment...
      end;
   end Load_And_Scan_ALI;

   ------------------
   -- Process_Unit --
   ------------------

   procedure Process_Unit
     (New_LI_File : LI_File_Ptr; Id : Unit_Id)
   is
      Current_Unit  : Unit_Record renames Units.Table (Id);
      New_File_Info : File_Info_Ptr;
   begin
      Create_File_Info
        (Fi_Ptr         => New_File_Info,
         Full_Filename  =>
           Create (Full_Filename =>
                     Locale_To_UTF8 (Get_String (Current_Unit.Sfile))),
         Unit_Name  => Strip_Unit_Part
           (Locale_To_UTF8 (Get_String (Current_Unit.Uname))),
         Set_Time_Stamp => False);

      --  Now save it in the proper place in New_LI_File
      case Current_Unit.Utype is
         when Is_Spec | Is_Spec_Only =>
            New_LI_File.LI.Spec_Info := New_File_Info;
         when Is_Body | Is_Body_Only =>
            New_LI_File.LI.Body_Info := New_File_Info;
      end case;

   exception
      when others =>
         --  Destroy the memory allocated locally before letting the exception
         --  propagate itself, to avoid memory leaks.
         Destroy (New_File_Info);
         raise;
   end Process_Unit;

   -------------------
   -- Process_Units --
   -------------------

   procedure Process_Units
     (New_LI_File : LI_File_Ptr; New_ALI : ALIs_Record) is
   begin
      for Current_Unit_Id in New_ALI.First_Unit .. New_ALI.Last_Unit loop
         Process_Unit (New_LI_File, Current_Unit_Id);
      end loop;
   end Process_Units;

   ------------------
   -- Process_Sdep --
   ------------------

   procedure Process_Sdep
     (Handler     : ALI_Handler;
      New_LI_File : LI_File_Ptr;
      New_ALI     : ALIs_Record;
      Id          : Sdep_Id;
      Project     : Project_Type;
      Sfiles      : in out Sdep_To_Sfile_Table;
      List        : LI_File_List)
   is
      Dep : Sdep_Record renames Sdep.Table (Id);
   begin
      if Dep.Subunit_Name /= No_Name then
         Process_Sdep_As_External
           (Handler, New_LI_File, New_ALI, Id, Project,
            Sfiles, List, Is_Separate => True);

      elsif New_LI_File.LI.Spec_Info /= null
        and then New_LI_File.LI.Spec_Info.Source_Filename.all =
                   Locale_To_UTF8 (Get_String (Dep.Sfile))
      then
         Process_Sdep_As_Self
           (New_LI_File, Id, New_LI_File.LI.Spec_Info, Sfiles);

      elsif New_LI_File.LI.Body_Info /= null
        and then New_LI_File.LI.Body_Info.Source_Filename.all =
                   Locale_To_UTF8 (Get_String (Dep.Sfile))
      then
         Process_Sdep_As_Self
           (New_LI_File, Id, New_LI_File.LI.Body_Info, Sfiles);

      else
         Process_Sdep_As_External
           (Handler, New_LI_File, New_ALI, Id, Project,
            Sfiles, List, Is_Separate => False);
      end if;
   end Process_Sdep;

   --------------------------
   -- Process_Sdep_As_Self --
   --------------------------

   procedure Process_Sdep_As_Self
     (New_LI_File : LI_File_Ptr;
      Id          : Sdep_Id;
      Finfo       : File_Info_Ptr;
      Sfiles      : in out Sdep_To_Sfile_Table)
   is
      Dep  : Sdep_Record renames Sdep.Table (Id);
      Part : Unit_Part := Unit_Spec;

   begin
      Finfo.File_Timestamp := To_Timestamp (Dep.Stamp);

      if Dep.Rfile /= Dep.Sfile then
         Finfo.Original_Filename := new String'
           (Locale_To_UTF8 (Get_String (Dep.Rfile)));
         Finfo.Original_Line := Integer (Dep.Start_Line);
      end if;

      if Finfo = New_LI_File.LI.Body_Info then
         --  To find the Unit_Part to associated to the Sdep (either Spec or
         --  Body), we just compare Finfo againts New_LI_File.Spec/Body_Info.
         --  Note that if this test fails, then Part should be Unit_Spec, which
         --  it already is...

         Part := Unit_Body;
      end if;

      --  Save the Source_File associated to this Sdep for later use.

      Sfiles (Id) :=
        (LI => New_LI_File, Source_Filename => null, Part => Part);
   end Process_Sdep_As_Self;

   ------------------------------
   -- Process_Sdep_As_External --
   ------------------------------

   procedure Process_Sdep_As_External
     (Handler     : ALI_Handler;
      New_LI_File : LI_File_Ptr;
      New_ALI     : ALIs_Record;
      Id          : Sdep_Id;
      Project     : Project_Type;
      Sfiles      : in out Sdep_To_Sfile_Table;
      List        : LI_File_List;
      Is_Separate : Boolean := False)
   is
      Dep     : Sdep_Record renames Sdep.Table (Id);
      New_Dep : Dependency_File_Info;
      Sfile   : Source_File;

   begin
      Get_Source_File
        (Handler, List, New_ALI, Dep.Sfile, Dep.Subunit_Name,
         Project, Sfile);
      Assert
        (Me, Get_File_Info (Sfile).Source_Filename.all =
           Locale_To_UTF8 (Get_String (Dep.Sfile)),
         "Process_Sdep_As_External, invalid source file " &
         Get_File_Info (Sfile).Source_Filename.all & ' '
         & Get_String (Dep.Sfile));
      New_Dep :=
        (File              => Copy (Sfile),
         Dep_Info          => (Depends_From_Spec => False,
                               Depends_From_Body => False),
         Declarations      => null);

      if not Is_Separate then
         New_LI_File.LI.Dependencies_Info :=
           new Dependency_File_Info_Node'
           (Value => New_Dep, Next => New_LI_File.LI.Dependencies_Info);
      end if;


      --  Save the Source_File associated to this Sdep for later use.
      Sfiles (Id) := Sfile;

   exception
      when E : ALI_Internal_Error =>
         --  Temporary: Do nothing in this case, this is probably a dependency
         --  on a file other than an ALI file (preprocessing from gnatmake,...)
         Trace (Me, "ALI_Internal_Error raised " & Exception_Information (E));
         null;

      when E : others =>
         Trace (Me, "Unexpected exception " & Exception_Information (E));
   end Process_Sdep_As_External;

   -------------------
   -- Process_Sdeps --
   -------------------

   procedure Process_Sdeps
     (Handler     : ALI_Handler;
      New_LI_File : LI_File_Ptr;
      New_ALI     : ALIs_Record;
      Project     : Project_Type;
      Sfiles      : in out Sdep_To_Sfile_Table;
      List        : LI_File_List) is
   begin
      for Dep_Id in New_ALI.First_Sdep .. New_ALI.Last_Sdep loop
         Process_Sdep
           (Handler, New_LI_File, New_ALI, Dep_Id, Project, Sfiles, List);
      end loop;
   end Process_Sdeps;

   ------------------
   -- Process_With --
   ------------------

   procedure Process_With
     (New_LI_File : LI_File_Ptr;
      Project     : Project_Type;
      UId         : Unit_Id;
      WId         : With_Id)
   is
      U                : Unit_Record renames Units.Table (UId);
      W                : With_Record renames Withs.Table (WId);

      --  ??? We cannot use W.Sfile unfortunately, since it is the file to
      --  recompile, not the file we actually depend on. However, it would be
      --  much faster (no need to parse the naming scheme), and would handle
      --  krunch names correctly.
      Source_Base     : constant String :=
        Get_Source_Filename (W.Uname, Project);
      Krunch_Name      : constant String := Krunch (Source_Base);
      Current_Sep     : File_Info_Ptr_List;
      Current_Dep     : Dependency_File_Info_List;
      Finfo           : File_Info_Ptr;
   begin
      --  Check that we did not with ourselves nor our separates in which
      --  case the with line does not contain any information we need.
      --  The test is done based on unit names, so that we don't have to
      --  compute the source filename, which could be expensive with
      --  different naming schemes.

      if New_LI_File.LI.Spec_Info /= null
        and then
          (New_LI_File.LI.Spec_Info.Source_Filename.all = Source_Base
           or else
             New_LI_File.LI.Spec_Info.Source_Filename.all = Krunch_Name)
      then
         return;
      end if;

      if New_LI_File.LI.Body_Info /= null
        and then
          (New_LI_File.LI.Body_Info.Source_Filename.all = Source_Base
           or else
             New_LI_File.LI.Body_Info.Source_Filename.all = Krunch_Name)
      then
         return;
      end if;

      Current_Sep := New_LI_File.LI.Separate_Info;

      while Current_Sep /= null loop
         if Current_Sep.Value.Source_Filename.all = Source_Base then
            return;
         end if;

         Current_Sep := Current_Sep.Next;
      end loop;

      --  At this point, we know that we have a real dependency...
      --  Try to find the Dependency_File_Info associated with this unit and
      --  update the missing information.

      Current_Dep := New_LI_File.LI.Dependencies_Info;

      while Current_Dep /= null loop
         Finfo := Get_File_Info (Current_Dep.Value.File);

         if Finfo.Source_Filename.all = Source_Base
           or else Finfo.Source_Filename.all = Krunch_Name
         then
            --  Update the unit name if not present

            if Finfo.Unit_Name = null then
               Finfo.Unit_Name := new String'
                 (Locale_To_UTF8 (Strip_Unit_Part (Get_String (W.Uname))));
            end if;

            --  Update the Depends_From_Spec/Body flags

            case U.Utype is
               when Is_Spec | Is_Spec_Only =>
                  Current_Dep.Value.Dep_Info.Depends_From_Spec := True;
               when Is_Body | Is_Body_Only =>
                  Current_Dep.Value.Dep_Info.Depends_From_Body := True;
            end case;

            return;
         end if;

         Current_Dep := Current_Dep.Next;
      end loop;

      --  We should never reach this point unless we have a bug in the code.
      --  raise the ALI_Internal_Error exception to signal the error.

      Trace (Me, "Process_With: file " & Source_Base
                & ' ' & Locale_To_UTF8 (Get_String (W.Uname))
                & " " & Krunch_Name
                & " from project "
                & Project_Name (Project)
                & " not found in "
                & Full_Name (Get_LI_Filename (New_LI_File)).all);
      raise ALI_Internal_Error;
   end Process_With;

   -------------------
   -- Process_Withs --
   -------------------

   procedure Process_Withs
     (New_LI_File : LI_File_Ptr;
      New_ALI     : ALIs_Record;
      Project     : Project_Type) is
   begin
      for Unit in New_ALI.First_Unit .. New_ALI.Last_Unit loop
         --  Make sure that there is at least one with'ed unit

         if Units.Table (Unit).First_With /= No_With_Id then
            for W in Units.Table (Unit).First_With ..
              Units.Table (Unit).Last_With
            loop
               Process_With (New_LI_File, Project, Unit, W);
            end loop;
         end if;
      end loop;
   end Process_Withs;

   ------------------------------------
   -- Chain_Declaration_For_Separate --
   ------------------------------------

   procedure Chain_Declaration_For_Separate
     (New_LI_File     : LI_File_Ptr;
      Source_Filename : String;
      Decl_Info       : E_Declaration_Info)
   is
      Sep : File_Info_Ptr_List := New_LI_File.LI.Separate_Info;
   begin
      while Sep /= null loop
         exit when Sep.Value.Source_Filename.all = Source_Filename;
         Sep := Sep.Next;
      end loop;

      if Sep = null then
         --  Failed to find the separate, this is a bug
         Trace (Me, "Chain_Declaration_For_Separate: "
                  & Source_Filename & " not found");
         raise ALI_Internal_Error;
      end if;

      Sep.Value.Declarations :=
        new E_Declaration_Info_Node'
          (Value => Decl_Info, Next => Sep.Value.Declarations);
   end Chain_Declaration_For_Separate;

   --------------------------------------
   -- Chain_Declaration_For_Dependency --
   --------------------------------------

   procedure Chain_Declaration_For_Dependency
     (New_LI_File : LI_File_Ptr;
      Sfile       : Source_File;
      Decl_Info   : E_Declaration_Info)
   is
      Dep : Dependency_File_Info_List := New_LI_File.LI.Dependencies_Info;
   begin
      while Dep /= null loop
         exit when Dep.Value.File = Sfile;
         Dep := Dep.Next;
      end loop;

      if Dep = null then
         --  Failed to find the associated dependency. This is a bug.
         Trace (Me, "Chain_Declaration_For_Dependency: SFile not found");
         raise ALI_Internal_Error;
      end if;

      Dep.Value.Declarations :=
        new E_Declaration_Info_Node'
          (Value => Decl_Info, Next => Dep.Value.Declarations);
   end Chain_Declaration_For_Dependency;

   -------------------------
   -- Process_Xref_Entity --
   -------------------------

   procedure Process_Xref_Entity
     (New_LI_File : LI_File_Ptr;
      New_ALI     : ALIs_Record;
      Section_Id  : Nat;
      Entity_Id   : Nat;
      Sfiles      : Sdep_To_Sfile_Table)
   is
      Xref_Sect : Xref_Section_Record renames Xref_Section.Table (Section_Id);
      Xref_Ent  : Xref_Entity_Record renames Xref_Entity.Table (Entity_Id);
      Sfile     : Source_File renames Sfiles (Xref_Sect.File_Num);
      Decl_Info : E_Declaration_Info;
      Decl      : E_Declaration renames Decl_Info.Declaration;

      Current_Sfile : Source_File;
      Ent           : constant String :=
        Locale_To_UTF8 (To_Lower (Get_String (Xref_Ent.Entity)));
      Col           : Natural := Natural (Xref_Ent.Col);
      Is_Operator   : constant Boolean := Ent (Ent'First) = '"';

      List_Subprograms, List_Ref : E_Reference_List;

   begin
      if Is_Operator then
         Decl.Name := new String'(Ent (Ent'First + 1 .. Ent'Last - 1));
         Col := Col + 1;
      else
         Decl.Name := new String'(Ent);
      end if;

      Decl.Location :=
        (File   => Copy (Sfile),
         Line   => Positive (Xref_Ent.Line),
         Column => Col);
      Decl.Kind := Char_To_E_Kind (Xref_Ent.Etype);

      if Xref_Ent.Lib then
         Decl.Scope := Global_Scope;
      else
         Decl.Scope := Local_Scope;
      end if;

      if Xref_Ent.Tref_File_Num /= No_Sdep_Id then
         begin
            Decl.Parent_Location := new File_Location_Node'
              (Value => (File   => Copy (Sfiles (Xref_Ent.Tref_File_Num)),
                         Line   => Positive (Xref_Ent.Tref_Line),
                         Column => Positive (Xref_Ent.Tref_Col)),
               Kind  => Tref_Kind_To_Parent_Kind (Xref_Ent.Tref),
               Predefined_Entity_Name => No_Name,
               Next  => Decl.Parent_Location);

            if Decl.Kind.Kind = Function_Or_Operator
              or else Decl.Kind.Kind = Procedure_Kind
              or else Decl.Kind.Kind = Entry_Or_Entry_Family
            then
               Decl.Parent_Location.Kind := Returned_Type;
            end if;

         exception
            when Constraint_Error =>
               Assert (Me, False,
                       "Invalid Tref file_num in "
                       & Base_Name (Get_File (Decl.Location))
                       & Get_Line (Decl.Location)'Img
                       & Get_Column (Decl.Location)'Img);
         end;


         --  This field no longer exists, but we might want it some day.
         --  Decl.Parent_Kind := Char_To_E_Kind (Xref_Ent.Tref_Type);

      elsif Xref_Ent.Tref_Standard_Entity /= No_Name then
         Decl.Parent_Location := new File_Location_Node'
           (Value => Predefined_Entity_Location,
            Kind  => Tref_Kind_To_Parent_Kind (Xref_Ent.Tref),
            Predefined_Entity_Name => Xref_Ent.Tref_Standard_Entity,
            Next  => Decl.Parent_Location);

         if Decl.Kind.Kind = Function_Or_Operator
           or else Decl.Kind.Kind = Procedure_Kind
           or else Decl.Kind.Kind = Entry_Or_Entry_Family
         then
            Decl.Parent_Location.Kind := Returned_Type;
         end if;

      else
         Decl.Parent_Location := null;
         --  Decl.Parent_Kind := E_Kind'First;
      end if;

      if Xref_Ent.Rref_Line /= 0 then
         --  Search the declaration of the renamed entity, in the entities
         --  defined in the current LI file.
         Sect_Loop :
         for Sect in Xref_Section.First .. Xref_Section.Last loop
            if Xref_Section.Table (Sect).File_Num in
              New_ALI.First_Sdep .. New_ALI.Last_Sdep
            then
               for Entity in Xref_Section.Table (Sect).First_Entity ..
                 Xref_Section.Table (Sect).Last_Entity
               loop
                  for Ref in Xref_Entity.Table (Entity).First_Xref ..
                    Xref_Entity.Table (Entity).Last_Xref
                  loop
                     if Xref.Table (Ref).Line = Xref_Ent.Rref_Line
                       and then Xref.Table (Ref).Col = Xref_Ent.Rref_Col
                       and then Xref.Table (Ref).File_Num = Xref_Sect.File_Num
                     then
                        Decl.Rename :=
                          (Line  => Positive (Xref_Entity.Table (Entity).Line),
                           Column => Natural (Xref_Entity.Table (Entity).Col),
                           File => Copy
                             (Sfiles (Xref_Section.Table (Sect).File_Num)));
                        exit Sect_Loop;
                     end if;
                  end loop;
               end loop;
            end if;
         end loop Sect_Loop;
      end if;

      Current_Sfile := Sfile;

      List_Subprograms := Decl_Info.Declaration.Primitive_Subprograms;
      List_Ref := Decl_Info.References;

      --  We don't make a deep copy for Current_Sfile since it is
      --  a temporary variable used for context information but not
      --  stored in any permanent area.

      for Xref_Id in Xref_Ent.First_Xref .. Xref_Ent.Last_Xref loop
         Xref_Block :
         declare
            Current_Xref : Xref_Record renames Xref.Table (Xref_Id);
            E_Ref        : E_Reference;
         begin
            E_Ref.Kind := Char_To_R_Kind (Current_Xref.Rtype);

            --  ??? For the moment, we shall ignore the reference to the
            --  instantiations. This generates invalid references in the
            --  various such algorithms, and we do not have a good structure to
            --  store them. The simple scheme of adding a File_Location field
            --  to E_Reference goes from 50M to 60M on a "find all references"
            --  to Object_Connect:gtk-handlers.ads:827

            if E_Ref.Kind /= Instantiation_Reference then
               --  Set Kind before any other field, especially the source
               --  file. If an exception is raised, we don't have to deallocate
               --  the memory we just allocated.

               if Current_Xref.File_Num /= No_Sdep_Id then
                  Current_Sfile := Sfiles (Current_Xref.File_Num);
               end if;

               E_Ref.Location :=
                 (File   => Copy (Current_Sfile),
                  Line   => Positive (Current_Xref.Line),
                  Column => Natural (Current_Xref.Col));

               --  ??? Should call Insert_Reference instead, but we need to
               --  add support for operators.


               --  Insert the new Xref in the list of references (except if it
               --  is a body end reference, in which case it is stored in a
               --  special location)
               --  The handling of end_of_scope is the following: if the entity
               --  has only one of these, it is stored in its declaration. If
               --  the entity has two of these (spec+body of a package for
               --  instance, only the one for the body is stored). However, in
               --  the latter case we need to save the end-of-scope for the
               --  spec in the standard list of references so that scope_trees
               --  can be generated.

               if Is_End_Reference (E_Ref.Kind) then
                  if Decl.End_Of_Scope = No_Reference
                    or else E_Ref.Location.File.Part = Unit_Body
                  then
                     Decl.End_Of_Scope := E_Ref;

                     --  For an operator, ignore the quotes
                     if Is_Operator then
                        E_Ref.Location.Column := E_Ref.Location.Column + 1;
                     end if;
                  end if;
               end if;

               if Is_End_Reference (E_Ref.Kind)
                 and then E_Ref.Location.File.Part = Unit_Body
               then
                  --  No need to save the reference again, to save space.
                  null;

               elsif E_Ref.Kind = Primitive_Operation
                 or else E_Ref.Kind = Overriding_Primitive_Operation
               then
                  --  Insert at the end, so that the result of
                  --  "Find_All_References" is properly sorted.
                  if Decl_Info.Declaration.Primitive_Subprograms = null then
                     Decl_Info.Declaration.Primitive_Subprograms :=
                       new E_Reference_Node'(Value => E_Ref, Next => null);
                     List_Subprograms :=
                       Decl_Info.Declaration.Primitive_Subprograms;
                  else
                     List_Subprograms.Next := new E_Reference_Node'
                       (Value => E_Ref, Next => null);
                     List_Subprograms := List_Subprograms.Next;
                  end if;

               else
                  --  Insert at the end, so that the result of
                  --  "Find_All_References" is properly sorted.

                  if List_Ref = null then
                     Decl_Info.References := new E_Reference_Node'
                       (Value => E_Ref, Next => null);
                     List_Ref := Decl_Info.References;
                  else
                     List_Ref.Next := new E_Reference_Node'
                       (Value => E_Ref, Next => null);
                     List_Ref := List_Ref.Next;
                  end if;
               end if;
            end if;
         end Xref_Block;
      end loop;

      --  Attach Decl_Info at the head of the right Declarations list.

      if Sfile.LI = New_LI_File then
         case Sfile.Part is
            when Unit_Spec =>
               New_LI_File.LI.Spec_Info.Declarations :=
                  new E_Declaration_Info_Node'
                    (Value => Decl_Info,
                     Next => New_LI_File.LI.Spec_Info.Declarations);

            when Unit_Body =>
               New_LI_File.LI.Body_Info.Declarations :=
                  new E_Declaration_Info_Node'
                    (Value => Decl_Info,
                     Next => New_LI_File.LI.Body_Info.Declarations);

            when Unit_Separate =>
               Chain_Declaration_For_Separate
                 (New_LI_File, Sfile.Source_Filename.all, Decl_Info);
         end case;

      else
         Chain_Declaration_For_Dependency (New_LI_File, Sfile, Decl_Info);
      end if;

   exception
      when others =>
         --  Local handling of all exceptions to avoid memory leaks. The
         --  exception is then propagated.

         Destroy (Decl_Info);
         raise;
   end Process_Xref_Entity;

   --------------------------
   -- Process_Xref_Section --
   --------------------------

   procedure Process_Xref_Section
     (New_LI_File : LI_File_Ptr;
      New_ALI     : ALIs_Record;
      Section_Id  : Nat;
      Sfiles      : Sdep_To_Sfile_Table)
   is
      Xref_Sect : Xref_Section_Record renames Xref_Section.Table (Section_Id);
   begin
      for E in Xref_Sect.First_Entity .. Xref_Sect.Last_Entity loop
         Process_Xref_Entity (New_LI_File, New_ALI, Section_Id, E, Sfiles);
      end loop;
   end Process_Xref_Section;

   -------------------
   -- Process_Xrefs --
   -------------------

   procedure Process_Xrefs
     (New_LI_File : LI_File_Ptr;
      New_ALI     : ALIs_Record;
      Sfiles      : Sdep_To_Sfile_Table) is
   begin
      for Xref_Sect in Xref_Section.First .. Xref_Section.Last loop
         if Xref_Section.Table (Xref_Sect).File_Num in
           New_ALI.First_Sdep .. New_ALI.Last_Sdep
         then
            Process_Xref_Section (New_LI_File, New_ALI, Xref_Sect, Sfiles);
         end if;
      end loop;
   end Process_Xrefs;

   --------------------
   -- Create_New_ALI --
   --------------------

   procedure Create_New_ALI
     (Handler           : ALI_Handler;
      New_LI_File       : out LI_File_Ptr;
      New_ALI           : ALIs_Record;
      Project           : Project_Type;
      Full_ALI_Filename : Virtual_File;
      List              : LI_File_List)
   is
      Sfiles         : Sdep_To_Sfile_Table
        (New_ALI.First_Sdep ..  New_ALI.Last_Sdep) :=
          (others => No_Source_File);

      LI_File_Is_New : constant Boolean := New_LI_File = null;
      LI_File_Copy   : LI_File_Constrained;

   begin
      if LI_File_Is_New then
         Create_LI_File
           (File        => New_LI_File,
            List        => List,
            Project     => Project,
            LI_Filename => Full_ALI_Filename,
            Handler     => LI_Handler (Handler));
         if New_LI_File = null then
            raise ALI_Internal_Error;
         end if;

         Convert_To_Parsed
           (New_LI_File,
            Full_LI_Name       => Full_ALI_Filename,
            Update_Timestamp   => True,
            Compilation_Errors => New_ALI.Compile_Errors);

      else
         --  Make a copy of the old LI_File to be able to restore it later
         --  if we encounter some problems while creating the new one.

         LI_File_Copy := New_LI_File.all;

         --  Blank the LI_File to avoid reading some relics of the old LI_File
         New_LI_File.LI :=
           (Handler                  => LI_Handler (Handler),
            Parsed                   => True,
            LI_Filename_Key          => new String'
              (Base_Name (Full_ALI_Filename)),
            LI_Filename              => Full_ALI_Filename,
            Project                  => Project,
            Spec_Info                => null,
            Body_Info                => null,
            Separate_Info            => null,
            LI_Timestamp             => File_Time_Stamp (Full_ALI_Filename),
            Compilation_Errors_Found => New_ALI.Compile_Errors,
            Dependencies_Info        => null);
      end if;

      --  Build the rest of the structure

      Process_Units (New_LI_File, New_ALI);
      Process_Sdeps
        (Handler, New_LI_File, New_ALI, Project, Sfiles, List);

      Process_Withs (New_LI_File, New_ALI, Project);
      Process_Xrefs (New_LI_File, New_ALI, Sfiles);

      Destroy (Sfiles);

      if not LI_File_Is_New then
         Destroy (LI_File_Copy.LI);
      end if;

   exception
      when E : others =>
         --  Catch all exceptions temporarily to free all memory allocated
         --  before letting the exception propagate itself up.
         --  We also set a minimal entry in the table
         Trace (Me, "Create_New_ALI: Unexpected exception: "
                & Exception_Information (E));

         if LI_File_Is_New then
            Destroy (New_LI_File.LI.Spec_Info);
            Destroy (New_LI_File.LI.Body_Info);
            Destroy (New_LI_File.LI.Separate_Info);
            Destroy (New_LI_File.LI.Dependencies_Info);
            New_LI_File.LI :=
              (Handler       => LI_Handler (Handler),
               Parsed        => False,
               Project       => Project,
               LI_Filename   => New_LI_File.LI.LI_Filename,
               LI_Filename_Key => New_LI_File.LI.LI_Filename_Key,
               LI_Timestamp  => New_LI_File.LI.LI_Timestamp,
               Spec_Info     => null,
               Body_Info     => null,
               Separate_Info => null);
         else
            Destroy (New_LI_File.LI);
            New_LI_File.all := LI_File_Copy;
         end if;

         raise;
   end Create_New_ALI;

   ----------------
   -- Locate_ALI --
   ----------------

   function Locate_ALI
     (Short_ALI_Filename : String;
      Project                : Project_Type) return String
   is
      Current_Dir_Name   : constant Character := '.';
      Dir                : String_Access;
      P : Project_Type := Project;
      Predefined_Object_Path : constant String :=
        Get_Predefined_Object_Path (Project_Registry (Get_Registry (P)));

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
            Extension : constant String := File_Extension (Short_ALI_Filename);
            Last      : Integer := Short_ALI_Filename'Last - Extension'Length;
            Dot_Replacement : constant String := Get_Attribute_Value
              (P, Dot_Replacement_Attribute, Default => "-");
         begin
            while Last >= Short_ALI_Filename'First loop
               declare
                  Path : constant String := Object_Path (P, False);
                  File : constant String :=
                    Locale_From_UTF8
                      (Short_ALI_Filename (Short_ALI_Filename'First .. Last)
                       & Extension);
               begin
                  --  ??? Not really efficient to always search
                  --  Predefined_Object_Path and Current_Dir_Name.
                  if Path /= "" then
                     Dir := Locate_Regular_File
                       (File, Path & Path_Separator &
                        Predefined_Object_Path
                        & Path_Separator & Current_Dir_Name);
                  else
                     Dir := Locate_Regular_File
                       (File,
                        Predefined_Object_Path
                        & Path_Separator & Current_Dir_Name);
                  end if;
               end;

               exit when Dir /= null;

               --  Search the next candidate: it might be the parent if we have
               --  a separate unit.

               while Last > Short_ALI_Filename'First loop
                  Last := Last - 1;
                  exit when (Last + Dot_Replacement'Length - 1 <=
                               Short_ALI_Filename'Last
                             and then Short_ALI_Filename
                               (Last .. Last + Dot_Replacement'Length - 1) =
                               Dot_Replacement)

                  --  Special case when there might be a confusion with the
                  --  GNAT runtime files.
                    or else (Dot_Replacement = "-"
                             and then Short_ALI_Filename (Last) = '~');
               end loop;
               Last := Last - 1;
            end loop;
         end;

         if Dir = null and then P /= Project then
            P := Parent_Project (P);
         else
            P := No_Project;
         end if;
      end loop;

      if Dir = null then
         return "";

      else
         declare
            D : constant String := Dir.all;
         begin
            Free (Dir);
            return D;
         end;
      end if;
   end Locate_ALI;

   --------------------
   -- Parse_ALI_File --
   --------------------

   procedure Parse_ALI_File
     (Handler                : ALI_Handler;
      Full_ALI_Filename      : Virtual_File;
      Project                : Project_Type;
      List                   : LI_File_List;
      Unit                   : out LI_File_Ptr;
      Success                : out Boolean)
   is
      New_ALI_Id    : ALI_Id := No_ALI_Id;

   begin
      if Full_ALI_Filename /= VFS.No_File then
         New_ALI_Id := Load_And_Scan_ALI (Full_ALI_Filename);
      end if;

      if New_ALI_Id = No_ALI_Id then
         Unit    := null;
         Success := False;
         return;
      end if;

      Create_New_ALI
        (Handler, Unit, ALIs.Table (New_ALI_Id), Project,
         Full_ALI_Filename, List);
      Success := True;

   exception
      when ALI_Internal_Error =>
         Trace (Me, "Parse_ALI_File: got ALI_Internal_Error");
         Unit    := null;
         Success := False;

      when E : others =>
         Trace (Me, "Parse_ALI_File: got unexpected exception "
                & Exception_Information (E));
         Unit    := null;
         Success := False;

         --  To trap *all* exceptions instead of just trapping
         --  ALI_Internal_Error, to avoid killing the process just because we
         --  failed to read an ALI file.
   end Parse_ALI_File;

   -----------------------------
   -- LI_Filename_From_Source --
   -----------------------------

   function LI_Filename_From_Source
     (Handler                : access ALI_Handler_Record;
      Source_Filename        : Virtual_File;
      Project                : Project_Type) return Virtual_File
   is
      pragma Unreferenced (Handler);
   begin
      case Get_Unit_Part_From_Filename (Project, Source_Filename) is
         when Unit_Body | Unit_Separate =>
            --  When using non-standard naming schemes, separate units are
            --  reported as bodies, but they have no direct ALI file. Thus, in
            --  addition to checking directly for an ALI file, we also check
            --  for ALI file from the parent unit.

            declare
               Body_LI : constant String := Locate_ALI
                 (Get_ALI_Filename (Base_Name (Source_Filename)), Project);
            begin
               if Body_LI /= "" then
                  return Create (Body_LI);
               end if;
            end;

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
                  return Create
                    (Locate_ALI
                       (Get_ALI_Filename
                          (Get_Filename_From_Unit
                             (Project,
                              Unit (Unit'First .. Last - 1),
                              Unit_Body)),
                        Project));
               else
                  return VFS.No_File;
               end if;
            end;

         when Unit_Spec =>
            --  Use the ALI for the body, if there is a body, otherwise the one
            --  for the spec will do.
            declare
               Body_LI : constant String := Locate_ALI
                 (Get_ALI_Filename
                    (Other_File_Base_Name (Project, Source_Filename)),
                  Project);
            begin
               if Body_LI /= "" then
                  return Create (Body_LI);
               else
                  return Create
                    (Locate_ALI
                       (Get_ALI_Filename (Base_Name (Source_Filename)),
                        Project));
               end if;
            end;
      end case;
   end LI_Filename_From_Source;

   ---------------------------
   -- Create_Or_Complete_LI --
   ---------------------------

   procedure Create_Or_Complete_LI
     (Handler                : access ALI_Handler_Record;
      File                   : in out LI_File_Ptr;
      Source_Filename        : Virtual_File;
      List                   : LI_File_List;
      Project                : Project_Type;
      Check_Timestamp        : Boolean := True)
   is

      F : File_Info_Ptr_List;
   begin
      --  If the file has already been parsed, we just check that it is still
      --  up-to-date
      File := Locate_From_Source (List, Source_Filename);

      if File /= No_LI_File then
         if Check_Timestamp or else Is_Incomplete (File) then
            Create_Or_Complete_LI
              (Handler                => Handler,
               File                   => File,
               Full_Ali_File          => File.LI.LI_Filename,
               List                   => List,
               Project                => Project);
         end if;

      else
         declare
            LI_Name : constant Virtual_File := LI_Filename_From_Source
              (Handler, Source_Filename, Project);
            Base : constant String := Base_Name (Source_Filename);
         begin
            --  In some cases (sources with no associated LI file), we will
            --  not get a LI_Name. Nothing to do in this case

            if Full_Name (LI_Name).all = "" then
               Trace (Me, "No LI found for "
                      & Full_Name (Source_Filename).all);
               File := No_LI_File;

            else
               --  Else we might already have an incomplete version of the LI
               --  file.
               File := Locate (List, LI_Name);

               Create_Or_Complete_LI
                 (Handler                => Handler,
                  File                   => File,
                  Full_Ali_File          => LI_Name,
                  List                   => List,
                  Project                => Project);

               --  Make sure that the LI file we just parsed does contain the
               --  information for the initial source file name. Since for
               --  separate units we might have been looking for the parent ALI
               --  file (see Locate_ALI), we now have to make sure we found the
               --  correct LI file.

               if File /= No_LI_File then
                  if (File.LI.Spec_Info /= null
                      and then File.LI.Spec_Info.Source_Filename.all = Base)
                    or else
                      (File.LI.Body_Info /= null
                       and then File.LI.Body_Info.Source_Filename.all = Base)
                  then
                     return;
                  end if;

                  F := File.LI.Separate_Info;
                  while F /= null loop
                     if F.Value /= null
                       and then F.Value.Source_Filename.all = Base
                     then
                        return;
                     end if;
                     F := F.Next;
                  end loop;

                  Trace
                    (Me, "Parsed LI file didn't contain the info for "
                     & Base & " " & Full_Name (LI_Name).all);
                  File := No_LI_File;
               else
                  File := No_LI_File;
               end if;
            end if;
         end;
      end if;
   end Create_Or_Complete_LI;

   ---------------------------
   -- Create_Or_Complete_LI --
   ---------------------------

   procedure Create_Or_Complete_LI
     (Handler                : access ALI_Handler_Record'Class;
      File                   : in out LI_File_Ptr;
      Full_Ali_File          : Virtual_File;
      List                   : LI_File_List;
      Project                : Project_Type)
   is
      Success : Boolean := True;
   begin
      if Full_Ali_File /= VFS.No_File then
         if File = No_LI_File
           or else Is_Incomplete (File)
           or else File_Time_Stamp (Full_Ali_File) > File.LI.LI_Timestamp
         then
            Trace (Me, "Creating/Updating LI file: "
                   & Full_Name (Full_Ali_File).all);

            Parse_ALI_File
              (ALI_Handler (Handler), Full_Ali_File, Project,
               List, File, Success);

            if not Success then
               File := No_LI_File;
            end if;
         end if;
      end if;

      if File /= No_LI_File
        and then not File.LI.Parsed
      then
         File := No_LI_File;
      end if;
   end Create_Or_Complete_LI;

   ----------------------------------
   -- Case_Insensitive_Identifiers --
   ----------------------------------

   function Case_Insensitive_Identifiers (Handler : access ALI_Handler_Record)
      return Boolean
   is
      pragma Unreferenced (Handler);
   begin
      return True;
   end Case_Insensitive_Identifiers;

   ------------------------------
   -- Parse_All_LI_Information --
   ------------------------------

   procedure Parse_All_LI_Information
     (Handler                : access ALI_Handler_Record;
      List                   : LI_File_List;
      In_Directory           : String;
      Project                : Project_Type)
   is
      Dir : Dir_Type;
      File : String (1 .. 1024);
      Last : Natural;
      LI : LI_File_Ptr;
      LI_File : Virtual_File;
   begin
      Open (Dir, In_Directory);

      loop
         Read (Dir, File, Last);
         exit when Last = 0;

         if File_Extension (File (File'First .. Last)) = ".ali" then
            LI_File := Create
              (Full_Filename => In_Directory & File (File'First .. Last));
            LI := Locate (List, LI_File);
            Create_Or_Complete_LI
              (Handler                => Handler,
               File                   => LI,
               Full_Ali_File          => LI_File,
               List                   => List,
               Project                => Project);
         end if;
      end loop;

      Close (Dir);

   exception
      when Directory_Error =>
         Trace (Me, "Couldn't open the directory " & In_Directory);
   end Parse_All_LI_Information;

   ----------------------------
   -- Generate_LI_For_Source --
   ----------------------------

   function Generate_LI_For_Source
     (Handler       : access ALI_Handler_Record;
      Root_Project  : Project_Type;
      File_Project  : Project_Type;
      Full_Filename : VFS.Virtual_File) return LI_Handler_Iterator'Class
   is
      pragma Unreferenced (Handler, Root_Project, File_Project, Full_Filename);
      Iterator : ALI_Handler_Iterator;
   begin
      return Iterator;
   end Generate_LI_For_Source;

   -----------------------------
   -- Generate_LI_For_Project --
   -----------------------------

   function Generate_LI_For_Project
     (Handler      : access ALI_Handler_Record;
      Root_Project : Project_Type;
      Project      : Project_Type;
      Recursive    : Boolean := False) return LI_Handler_Iterator'Class
   is
      pragma Unreferenced
        (Handler, Root_Project, Project, Recursive);
      Iterator : ALI_Handler_Iterator;
   begin
      return Iterator;
   end Generate_LI_For_Project;

   --------------
   -- Continue --
   --------------

   procedure Continue
     (Iterator : in out ALI_Handler_Iterator;
      Finished : out Boolean)
   is
      pragma Unreferenced (Iterator);
   begin
      Finished := True;
   end Continue;

end Src_Info.ALI;
