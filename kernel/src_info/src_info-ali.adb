-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
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
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with Krunch;
with Namet;                     use Namet;
with Prj;                       use Prj;
with Prj_API;                   use Prj_API;
with Prj.Com;
with Prj.Env;                   use Prj.Env;
with Src_Info.ALI_Maps;         use Src_Info.ALI_Maps;
with Src_Info.Prj_Utils;        use Src_Info.Prj_Utils;
with Src_Info.LI_Utils;         use Src_Info.LI_Utils;
with Types;                     use Types;
with Traces;                    use Traces;

package body Src_Info.ALI is

   Me : constant Debug_Handle := Create ("Src_Info.Ali");

   Maximum_Filename_Length : constant := 8;
   --  ??? The maximum number of characters in a krunched filename (not
   --  including the .ads/adb extension). Should probably be more dynamic,
   --  but this is not terribly important since most of the time, only the
   --  Run-time file are krunched, and we know that their max_len is 8.

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
   --  Translate the given character into the associated E_Kind value.
   --  Raise ALI_Internal_Error if C does not represent any E_Kind value.

   function Char_To_R_Kind (C : Character) return Reference_Kind;
   --  Translate the given character into the associated Reference_Kind value.
   --  Raise ALI_Internal_Error if C does not represent any Reference_Kind.

   function Get_ALI_Filename (Sig_Filename : File_Name_Type) return String;
   --  Converts the given source filename Sig_Filename into the corresponding
   --  ALI filename.

   function Strip_Unit_Part (Unit_Name : String) return String;
   --  Strip a trailing '%s' or '%b' if present.

   function Krunch
     (Filename : String;
      Max_Len  : Natural := Maximum_Filename_Length) return String;
   --  Krunch the given source filename to at most Max_Len characters
   --  (file extension not included).

   procedure Get_Source_File
     (Handler          : ALI_Handler;
      List             : in out LI_File_List;
      New_ALI          : ALIs_Record;
      Source_Filename  : File_Name_Type;
      Subunit_Name     : Name_Id := No_Name;
      Project          : Prj.Project_Id;
      Source_Path      : String;
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
      List             : in out LI_File_List;
      Source_Filename  : File_Name_Type;
      Project          : Prj.Project_Id;
      Source_Path      : String;
      File             : out Source_File);
   --  Perform the job of Get_Source_File in the case where Subunit_Name is not
   --  set (case when it is not a separate).

   procedure Get_Unit_Source_File
     (Handler          : ALI_Handler;
      List             : in out LI_File_List;
      Source_Filename  : File_Name_Type;
      Sig_Filename     : File_Name_Type;
      Part             : Unit_Part;
      File             : out Source_File);
   --  Perform the job of Get_Unit_Source_File knowing that the Source Filename
   --  from which the ALI filename is derived is Sig_Filename.

   procedure Get_Subunit_Source_File
     (Handler          : ALI_Handler;
      List             : in out LI_File_List;
      Source_Filename  : File_Name_Type;
      Sig_Filename     : File_Name_Type;
      Subunit_Name     : Name_Id;
      File             : out Source_File);
   --  Perform the job of Get_Subunit_Source_File knowing the name of the
   --  Source Filename from which the ALI filename is derived.

   function Load_And_Scan_ALI (ALI_Filename : String) return ALI_Id;
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
      Project     : Prj.Project_Id;
      Source_Path : String;
      Sfiles      : in out Sdep_To_Sfile_Table;
      List        : in out LI_File_List);
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
      Project     : Prj.Project_Id;
      Source_Path : String;
      Sfiles      : in out Sdep_To_Sfile_Table;
      List        : in out LI_File_List);
   --  Save the information from the given dependency into the list of
   --  dependencies of the LI_File_Ptr. This procedure should be used
   --  only for cases where the dependency is an external dependency.

   procedure Process_Sdeps
     (Handler     : ALI_Handler;
      New_LI_File : LI_File_Ptr;
      New_ALI     : ALIs_Record;
      Project     : Prj.Project_Id;
      Source_Path : String;
      Sfiles      : in out Sdep_To_Sfile_Table;
      List        : in out LI_File_List);
   --  Call Process_Sdep for all Sdep entries in the given LI_File_Ptr.

   procedure Process_With
     (New_LI_File : LI_File_Ptr;
      Project     : Prj.Project_Id;
      UId         : Unit_Id;
      WId         : With_Id);
   --  Save the information associated to the given With_Record.

   procedure Process_Withs
     (New_LI_File : LI_File_Ptr;
      New_ALI     : ALIs_Record;
      Project     : Prj.Project_Id);
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
     (Handler      : ALI_Handler;
      New_LI_File  : out LI_File_Ptr;
      New_ALI      : ALIs_Record;
      Project      : Prj.Project_Id;
      Source_Path  : String;
      List         : in out LI_File_List);
   --  Create a new LI_File_Ptr from the given ALIs_Record. This LI_File_Ptr
   --  is left unconnected to the LI_File_List.

   function Locate_Load_And_Scan_ALI
     (Short_ALI_Filename     : String;
      Project                : Prj.Project_Id;
      Predefined_Object_Path : String) return ALI_Id;
   --  Try to locate ALI_Filename inside the Project object path, then
   --  in Predefined_Object_Path, and finally in the current directory. Then
   --  scan the ALI file if found.

   procedure Parse_ALI_File
     (Handler                : ALI_Handler;
      Short_ALI_Filename     : String;
      Project                : Prj.Project_Id;
      Predefined_Source_Path : String;
      Predefined_Object_Path : String;
      List                   : in out LI_File_List;
      Unit                   : out LI_File_Ptr;
      Success                : out Boolean);
   --  Parse the given ALI file and update the Unit Info list. Also returns
   --  a handle to the Unit Info corresponding to this ALI file to avoid
   --  searching right-back for this handle for immediate queries.

   procedure Create_Or_Complete_LI
     (Handler                : access ALI_Handler_Record'Class;
      File                   : in out LI_File_Ptr;
      Ali_File               : String;
      List                   : in out LI_File_List;
      Project                : Prj.Project_Id;
      Predefined_Source_Path : String;
      Predefined_Object_Path : String);
   --  Internal version of Create_Or_Complete_LI

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
   begin
      for E in E_Kind loop
         if E_Kind_To_Char (E) = C then
            return E;
         end if;
      end loop;

      --  If we reach this point, the character is illegal.
      Trace (Me, "Char_To_E_KindL Invalid character " & C);
      raise ALI_Internal_Error;
   end Char_To_E_Kind;

   --------------------
   -- Char_To_R_Kind --
   --------------------

   function Char_To_R_Kind (C : Character) return Reference_Kind is
   begin
      for R in Reference_Kind loop
         if Reference_Kind_To_Char (R) = C then
            return R;
         end if;
      end loop;

      --  If we reach this point, the character is illegal.
      Trace (Me, "Char_To_R_Kind: Invalid character " & C);
      raise ALI_Internal_Error;
   end Char_To_R_Kind;

   ----------------------
   -- Get_ALI_Filename --
   ----------------------

   function Get_ALI_Filename
     (Sig_Filename : File_Name_Type) return String
   is
      Last_Dot : Natural;
      ALI_Ext  : constant String := ".ali";

   begin
      --  Retrieve the Sig_Filename string and store in the Namet Name Buffer

      Get_Name_String (Sig_Filename);

      --  Search the last dot in the filename

      Last_Dot := Namet.Name_Len;

      while Last_Dot >= Namet.Name_Buffer'First  loop
         exit when Namet.Name_Buffer (Last_Dot) = '.';
         Last_Dot := Last_Dot - 1;
      end loop;

      if Last_Dot < Namet.Name_Buffer'First then
         --  No dot found in Sig_Filename, just append the ALI extension

         Last_Dot := Namet.Name_Len + 1;
      end if;

      --  Add the extension and return the result

      Namet.Name_Buffer (Last_Dot .. Last_Dot + ALI_Ext'Length - 1) := ALI_Ext;
      return Namet.Name_Buffer (1 .. Last_Dot + ALI_Ext'Length - 1);
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
      List             : in out LI_File_List;
      New_ALI          : ALIs_Record;
      Source_Filename  : File_Name_Type;
      Subunit_Name     : Name_Id := No_Name;
      Project          : Prj.Project_Id;
      Source_Path      : String;
      File             : out Source_File)
   is
      Prj : Project_Id;
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
         Prj := Get_Project_From_File
           (Project, Get_Name_String (Source_Filename));

         if Prj = No_Project then
            --  ??? We have a file that doesn't belong the an project.
            Prj := Project;
         end if;

         Get_Unit_Source_File
           (Handler, List, Source_Filename, Prj, Source_Path, File);
      else
         Get_Subunit_Source_File
           (Handler, List, Source_Filename, New_ALI.Sfile,
            Subunit_Name, File);
      end if;
   end Get_Source_File;

   --------------------------
   -- Get_Unit_Source_File --
   --------------------------

   procedure Get_Unit_Source_File
     (Handler          : ALI_Handler;
      List             : in out LI_File_List;
      Source_Filename  : File_Name_Type;
      Project          : Prj.Project_Id;
      Source_Path      : String;
      File             : out Source_File)
   is
      Prj_Data  : Project_Data renames Prj.Projects.Table (Project);
      Naming    : Naming_Data renames Prj_Data.Naming;

      Body_Id   : Array_Element_Id;
      Spec_Id   : Array_Element_Id;
      Prj_Unit  : Prj.Com.Unit_Id;
      Filename  : File_Name_Type;
      Dir       : String_Access;
      Part      : Unit_Part;

   begin
      --  Note that the search algorigthm in this procedure is documented
      --  inside Get_Source_File. Any change to this algorithm should be
      --  documented there.

      --  First, check the body filename exception list.

      Body_Id := Search_Filename (Naming.Bodies, Source_Filename);

      if Body_Id /= No_Array_Element then
         Get_Unit_Source_File
           (Handler, List, Source_Filename,
            Get_Filename (Body_Id), Unit_Body, File);
         return;
      end if;

      --  Not found. Check the spec filename exception list

      Spec_Id := Search_Filename (Naming.Specifications, Source_Filename);

      --  If found then search if we can find the associated body. We should
      --  always use the ALI file from the body if available, since it contains
      --  much more information.

      if Spec_Id /= No_Array_Element then
         --  Search the body exception list for any filename with the same
         --  unit name id

         Body_Id := Search_Unit_Name (Naming.Bodies, Get_Unit_Name (Spec_Id));

         if Body_Id /= No_Array_Element then
            Get_Unit_Source_File
              (Handler, List, Source_Filename,
               Get_Filename (Body_Id), Unit_Body, File);
            return;
         end if;

         --  Not found. Search the project units list

         Prj_Unit := Prj.Com.Units_Htable.Get (Get_Unit_Name (Spec_Id));
         Filename := Get_Body_Filename (Prj_Unit);

         if Filename /= No_Name then
            if Source_Filename = Filename then
               Part := Unit_Body;
            else
               Part := Unit_Spec;
            end if;

            Get_Unit_Source_File
              (Handler, List, Source_Filename, Filename, Part, File);
            return;
         end if;

         --  Search in the Source path

         Filename := Get_Body_Filename (Get_Unit_Name (Spec_Id), Naming);
         Dir := Locate_Regular_File (Get_Name_String (Filename), Source_Path);

         if Dir /= null then
            Free (Dir);

            if Source_Filename = Filename then
               Part := Unit_Body;
            else
               Part := Unit_Spec;
            end if;

            Get_Unit_Source_File
              (Handler, List, Source_Filename, Filename, Part, File);
            return;
         end if;

         --  Not found anywhere, so this is a spec only file
         Get_Unit_Source_File
           (Handler, List, Source_Filename,
            Get_Filename (Spec_Id), Unit_Spec, File);
         return;
      end if;  --  filename found in spec exception list

      --  Else if both Spec_Id and Body_Id were null

      case Get_Unit_Part_From_Filename
        (Get_Name_String (Source_Filename), Project)
      is

         --  Is this a body, according to the naming scheme?

         when Unit_Body =>
            Get_Unit_Source_File
              (Handler, List, Source_Filename,
               Source_Filename, Unit_Body, File);

         --  Is this a spec, according to the naming scheme?
         --  (it should be, or that would be a bug)

         when Unit_Spec =>
            declare
               Unit_Name : constant Name_Id :=
                 Get_Unit_Name (Source_Filename, Project);
            begin
               if Unit_Name = No_Name then
                  --  This is a bug...
                  Trace (Me, "Get_Unit_Source_File: no unit name");
                  raise ALI_Internal_Error;
               end if;

               --  Search in the body exception list
               Body_Id := Search_Unit_Name (Naming.Bodies, Unit_Name);

               if Body_Id /= No_Array_Element then
                  Get_Unit_Source_File
                    (Handler, List, Source_Filename, Get_Filename (Body_Id),
                     Unit_Body, File);
                  return;
               end if;

               --  Search the project units list
               Prj_Unit := Prj.Com.Units_Htable.Get (Unit_Name);
               Filename := Get_Body_Filename (Prj_Unit);

               if Get_Body_Filename (Prj_Unit) /= No_Name then
                  if Filename = Source_Filename then
                     Part := Unit_Body;
                  else
                     Part := Unit_Spec;
                  end if;

                  Get_Unit_Source_File
                    (Handler, List, Source_Filename, Filename, Part, File);
                  return;
               end if;

               --  Search the source path
               Filename := Get_Body_Filename (Unit_Name, Naming);
               Dir :=
                 Locate_Regular_File (Get_Name_String (Filename), Source_Path);

               if Dir /= null then
                  if Filename = Source_Filename then
                     Part := Unit_Body;
                  else
                     Part := Unit_Spec;
                  end if;

                  Free (Dir);
                  Get_Unit_Source_File
                    (Handler, List, Source_Filename, Filename, Part, File);
                  return;
               end if;

               --  The body was not found anywhere, so this is a spec only file
               Get_Unit_Source_File
                 (Handler, List, Source_Filename,
                  Source_Filename, Unit_Spec, File);
               return;
            end;

            --  If we reach this point, then there is a bug somewhere

         when Unit_Separate =>
            Trace (Me, "Get_Unit_Source_File: reached the end unexpectedly "
                   & Get_Name_String (Source_Filename)
                   & " in project "
                   & Get_Name_String (Projects.Table (Project).Name));
            raise ALI_Internal_Error;
      end case;
   end Get_Unit_Source_File;

   procedure Get_Unit_Source_File
     (Handler          : ALI_Handler;
      List             : in out LI_File_List;
      Source_Filename  : File_Name_Type;
      Sig_Filename     : File_Name_Type;
      Part             : Unit_Part;
      File             : out Source_File)
   is
      Sname        : constant String := Get_Name_String (Source_Filename);
      ALI_Filename : constant String := Get_ALI_Filename (Sig_Filename);

   begin
      File :=
        (LI              => Get (List.Table, ALI_Filename),
         Part            => Part,
         Source_Filename => null);

      --  If there is not LI_File_Ptr yet for the given ALI_Filename then
      --  create a stub

      if File.LI = null then
         Create_LI_File
           (File        => File.LI,
            List        => List,
            LI_Filename => ALI_Filename,
            Handler     => LI_Handler (Handler));
         if File.LI = null then
            raise ALI_Internal_Error;
         end if;
      end if;

      --  If the associated File_Info does not exist, then create it.
      case Part is
         when Unit_Spec =>
            if File.LI.LI.Spec_Info = null then
               Create_File_Info (File.LI.LI.Spec_Info, Sname);
            end if;

         when Unit_Body =>
            if File.LI.LI.Body_Info = null then
               Create_File_Info (File.LI.LI.Body_Info, Sname);
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
      List             : in out LI_File_List;
      Source_Filename  : File_Name_Type;
      Sig_Filename     : File_Name_Type;
      Subunit_Name     : Name_Id;
      File             : out Source_File)
   is
      Sname        : constant String := Get_Name_String (Source_Filename);
      ALI_Filename : constant String := Get_ALI_Filename (Sig_Filename);
      Sep          : File_Info_Ptr_List;

   begin
      File :=
         (LI              => Get (List.Table, ALI_Filename),
          Part            => Unit_Separate,
          Source_Filename => new String' (Sname));

      --  If there is no LI_File_Ptr yet for the given ALI_Filename then
      --  create a stub

      if File.LI = null then
         Create_LI_File
           (File        => File.LI,
            List        => List,
            LI_Filename => ALI_Filename,
            Handler     => LI_Handler (Handler));

         if File.LI = null then
            raise ALI_Internal_Error;
         end if;
      end if;

      --  If the associated File_Info does not exist, then create it.

      Sep := File.LI.LI.Separate_Info;

      while Sep /= null loop
         exit when Sep.Value.Source_Filename.all = Sname;
         Sep := Sep.Next;
      end loop;

      if Sep = null then
         File.LI.LI.Separate_Info := new File_Info_Ptr_Node'
           (Value => null, Next => File.LI.LI.Separate_Info);
         Create_File_Info
           (File.LI.LI.Separate_Info.Value, Sname,
            Unit_Name => Get_Name_String (Subunit_Name));
      end if;

   exception
      when others =>
         Destroy (File);
         raise;
   end Get_Subunit_Source_File;

   -----------------------
   -- Load_And_Scan_ALI --
   -----------------------

   function Load_And_Scan_ALI (ALI_Filename : String) return ALI_Id is
      File_Length : Text_Ptr;
      FD          : File_Descriptor;
      Chars_Read  : Integer;
      Filename_Id : File_Name_Type;

   begin
      FD := Open_Read (ALI_Filename & ASCII.NUL, Fmode => Binary);

      if FD = Invalid_FD then
         Trace (Me, "Couldn't open " & ALI_Filename);
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
            Trace (Me, "CE while reading length of " & ALI_Filename);
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
            Trace (Me, "Couldn't read the whole file for " & ALI_Filename);
            return No_ALI_Id;
         end if;

         --  Replace the last char by an EOF. Scan_ALI uses this character
         --  to detect the end of the buffer.
         Buffer (Buffer'Last) := EOF;

         --  Get the ID of the ALI_Filename in the Namet table
         Namet.Name_Buffer (1 .. ALI_Filename'Length) := ALI_Filename;
         Namet.Name_Len := ALI_Filename'Length;
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
         Full_Filename  => Get_Name_String (Current_Unit.Sfile),
         Unit_Name  => Strip_Unit_Part (Get_Name_String (Current_Unit.Uname)),
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
      Project     : Prj.Project_Id;
      Source_Path : String;
      Sfiles      : in out Sdep_To_Sfile_Table;
      List        : in out LI_File_List)
   is
      Dep : Sdep_Record renames Sdep.Table (Id);
   begin
      if Dep.Subunit_Name /= No_Name then
         Process_Sdep_As_External
           (Handler, New_LI_File, New_ALI, Id, Project,
            Source_Path, Sfiles, List);

      elsif New_LI_File.LI.Spec_Info /= null
        and then New_LI_File.LI.Spec_Info.Source_Filename.all =
                   Get_Name_String (Dep.Sfile)
      then
         Process_Sdep_As_Self
           (New_LI_File, Id, New_LI_File.LI.Spec_Info, Sfiles);

      elsif New_LI_File.LI.Body_Info /= null
        and then New_LI_File.LI.Body_Info.Source_Filename.all =
                   Get_Name_String (Dep.Sfile)
      then
         Process_Sdep_As_Self
           (New_LI_File, Id, New_LI_File.LI.Body_Info, Sfiles);

      else
         Process_Sdep_As_External
           (Handler, New_LI_File, New_ALI, Id, Project,
            Source_Path, Sfiles, List);
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
         Finfo.Original_Filename := new String'(Get_Name_String (Dep.Rfile));
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
      Project     : Prj.Project_Id;
      Source_Path : String;
      Sfiles      : in out Sdep_To_Sfile_Table;
      List        : in out LI_File_List)
   is
      Dep     : Sdep_Record renames Sdep.Table (Id);
      New_Dep : Dependency_File_Info;
      Sfile   : Source_File;

   begin
      Get_Source_File
        (Handler, List, New_ALI, Dep.Sfile, Dep.Subunit_Name,
         Project, Source_Path, Sfile);
      New_Dep :=
        (File              => Sfile,
         Dep_Info          => (Depends_From_Spec => False,
                               Depends_From_Body => False),
         Declarations      => null);
      pragma Assert
        (Get_Source_Filename (Sfile) = Get_Name_String (Dep.Sfile));

      New_LI_File.LI.Dependencies_Info :=
        new Dependency_File_Info_Node'
          (Value => New_Dep, Next => New_LI_File.LI.Dependencies_Info);
      --  Save the Source_File associated to this Sdep for later use.
      Sfiles (Id) := Sfile;
   end Process_Sdep_As_External;

   -------------------
   -- Process_Sdeps --
   -------------------

   procedure Process_Sdeps
     (Handler     : ALI_Handler;
      New_LI_File : LI_File_Ptr;
      New_ALI     : ALIs_Record;
      Project     : Prj.Project_Id;
      Source_Path : String;
      Sfiles      : in out Sdep_To_Sfile_Table;
      List        : in out LI_File_List) is
   begin
      for Dep_Id in New_ALI.First_Sdep .. New_ALI.Last_Sdep loop
         Process_Sdep
           (Handler, New_LI_File, New_ALI, Dep_Id, Project,
            Source_Path, Sfiles, List);
      end loop;
   end Process_Sdeps;

   ------------------
   -- Process_With --
   ------------------

   procedure Process_With
     (New_LI_File : LI_File_Ptr;
      Project     : Prj.Project_Id;
      UId         : Unit_Id;
      WId         : With_Id)
   is
      U                : Unit_Record renames Units.Table (UId);
      W                : With_Record renames Withs.Table (WId);

      Withed_File_Name : constant String :=
        Get_Source_Filename (W.Uname, Project);
      Krunched_Name    : constant String := Krunch (Withed_File_Name);
      Current_Sep      : File_Info_Ptr_List;
      Current_Dep      : Dependency_File_Info_List;
      Finfo            : File_Info_Ptr;

   begin
      --  Check that we did not with ourselves nor our separates in which
      --  case the with line does not contain any information we need.
      --  The test is done based on unit names, so that we don't have to
      --  compute the source filename, which could be expensive with different
      --  naming schems

      if New_LI_File.LI.Spec_Info /= null
        and then New_LI_File.LI.Spec_Info.Source_Filename.all =
           Withed_File_Name
      then
         return;
      end if;

      if New_LI_File.LI.Body_Info /= null
        and then New_LI_File.LI.Body_Info.Source_Filename.all =
           Withed_File_Name
      then
         return;
      end if;

      Current_Sep := New_LI_File.LI.Separate_Info;

      while Current_Sep /= null loop
         if Current_Sep.Value.Source_Filename.all = Withed_File_Name then
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

         if Finfo.Source_Filename.all = Withed_File_Name
           or else Finfo.Source_Filename.all = Krunched_Name
         then
            --  Update the unit name if not present

            if Finfo.Unit_Name = null then
               Finfo.Unit_Name :=
                 new String' (Strip_Unit_Part (Get_Name_String (W.Uname)));
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

      Trace (Me, "Process_With: unexpected end " & Withed_File_Name
             & " in project "
             & Get_Name_String (Projects.Table (Project).Name));
      raise ALI_Internal_Error;
   end Process_With;

   -------------------
   -- Process_Withs --
   -------------------

   procedure Process_Withs
     (New_LI_File : LI_File_Ptr;
      New_ALI     : ALIs_Record;
      Project     : Prj.Project_Id) is
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

   begin
      Decl.Name := new String'
        (To_Lower (Get_Name_String (Xref_Ent.Entity)));
      Decl.Location :=
        (File   => Sfile,
         Line   => Positive (Xref_Ent.Line),
         Column => Positive (Xref_Ent.Col));
      Decl.Kind := Char_To_E_Kind (Xref_Ent.Etype);

      if Xref_Ent.Lib then
         Decl.Scope := Global_Scope;
      else
         Decl.Scope := Local_Scope;
      end if;

      if Xref_Ent.Tref_File_Num /= No_Sdep_Id then
         Decl.Parent_Location := new File_Location_Node'
           (Value => (File   => Copy (Sfiles (Xref_Ent.Tref_File_Num)),
                      Line   => Positive (Xref_Ent.Tref_Line),
                      Column => Positive (Xref_Ent.Tref_Col)),
            Next  => Decl.Parent_Location);

         --  This field no longer exists, but we might want it some day.
         --  Decl.Parent_Kind := Char_To_E_Kind (Xref_Ent.Tref_Type);

      else
         Decl.Parent_Location := null;
         --  Decl.Parent_Kind := E_Kind'First;
      end if;

      if Xref_Ent.Rref_Line /= 0 then
         --  Search the declaration of the renamed entity, in the entities
         --  defined in the current LI file.
         Sect_Loop :
         for Sect in  Xref_Section.First .. Xref_Section.Last loop
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
                           File =>
                             Sfiles (Xref_Section.Table (Sect).File_Num));
                        exit Sect_Loop;
                     end if;
                  end loop;
               end loop;
            end if;
         end loop Sect_Loop;
      end if;

      --  ??? Note that in the part of this procedure that follows, we assume
      --  that there is always at least one xref. To be verified.

      Current_Sfile := Sfile;

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

            --  Insert the new Xref at the head of the References table
            --  (except if it is an end reference, in which case it is stored
            --  in a special location)

            if Is_End_Reference (E_Ref.Kind) then
               Decl.End_Of_Scope := E_Ref;
            else
               Decl_Info.References := new E_Reference_Node'
                 (Value => E_Ref, Next => Decl_Info.References);
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
     (Handler      : ALI_Handler;
      New_LI_File  : out LI_File_Ptr;
      New_ALI      : ALIs_Record;
      Project      : Prj.Project_Id;
      Source_Path  : String;
      List         : in out LI_File_List)
   is
      Tmp            : LI_File_Ptr;
      Sfiles         : Sdep_To_Sfile_Table
        (New_ALI.First_Sdep ..  New_ALI.Last_Sdep) :=
          (others => No_Source_File);
      ALI_Filename   : constant String := Get_Name_String (New_ALI.Afile);

      LI_File_Is_New : Boolean;
      LI_File_Copy   : LI_File_Constrained;

   begin
      Tmp := Get (List.Table, ALI_Filename);
      LI_File_Is_New := Tmp = null;

      if LI_File_Is_New then
         Create_LI_File
           (File               => Tmp,
            List               => List,
            LI_Filename        => Get_Name_String (New_ALI.Afile),
            Handler            => LI_Handler (Handler));
         if Tmp = null then
            raise ALI_Internal_Error;
         end if;

         Convert_To_Parsed
           (Tmp,
            Update_Timestamp   => True,
            Compilation_Errors => New_ALI.Compile_Errors);

      else
         --  Make a copy of the old LI_File to be able to restore it later
         --  if we encounter some problems while creating the new one.

         LI_File_Copy := Tmp.all;

         --  Blank the LI_File to avoid reading some relics of the old LI_File
         Tmp.LI :=
           (Handler                  => LI_Handler (Handler),
            Parsed                   => True,
            LI_Filename              => new String'
              (Base_Name (Get_Name_String (New_ALI.Afile))),
            Spec_Info                => null,
            Body_Info                => null,
            Separate_Info            => null,
            LI_Timestamp             => To_Timestamp
              (File_Time_Stamp (Get_Name_String (New_ALI.Afile))),
            Compilation_Errors_Found => New_ALI.Compile_Errors,
            Dependencies_Info        => null);
      end if;

      --  Build the rest of the structure

      Process_Units (Tmp, New_ALI);
      Process_Sdeps
        (Handler, Tmp, New_ALI, Project, Source_Path, Sfiles, List);

      Process_Withs (Tmp, New_ALI, Project);
      Process_Xrefs (Tmp, New_ALI, Sfiles);

      if not LI_File_Is_New then
         Destroy (LI_File_Copy.LI);
      end if;

      New_LI_File := Tmp;

   exception
      when others =>
         --  Catch all exceptions temporarily to free all memory allocated
         --  before letting the exception propagate itself up.
         --  We also set a minimal entry in the table

         if LI_File_Is_New then
            Destroy (Tmp.LI.Spec_Info);
            Destroy (Tmp.LI.Body_Info);
            Destroy (Tmp.LI.Separate_Info);
            Destroy (Tmp.LI.Dependencies_Info);
            Tmp.LI := (Handler                  => LI_Handler (Handler),
                       Parsed                   => False,
                       LI_Filename              => Tmp.LI.LI_Filename,
                       LI_Timestamp             => Tmp.LI.LI_Timestamp,
                       Spec_Info                => null,
                       Body_Info                => null,
                       Separate_Info            => null);
         else
            Destroy (Tmp.LI);
            Tmp.all := LI_File_Copy;
         end if;

         New_LI_File := Tmp;
         raise;
   end Create_New_ALI;

   ------------------------------
   -- Locate_Load_And_Scan_ALI --
   ------------------------------

   function Locate_Load_And_Scan_ALI
     (Short_ALI_Filename     : String;
      Project                : Prj.Project_Id;
      Predefined_Object_Path : String) return ALI_Id
   is
      Current_Dir_Name   : constant Character := '.';
      Dir                : String_Access;
      Result             : ALI_Id;
      Extension : constant String := File_Extension (Short_ALI_Filename);
      Last      : Integer := Short_ALI_Filename'Last - Extension'Length;
      Dot_Replacement : constant String :=
        Get_Name_String (Projects.Table (Project).Naming.Dot_Replacement);

   begin
      --  Compute the search path. If the objects path of the project is
      --  not null, then prepend it to the total search path.
      while Last >= Short_ALI_Filename'First loop
         if Prj.Env.Ada_Objects_Path (Project) /= null then
            Dir := Locate_Regular_File
              (Short_ALI_Filename (Short_ALI_Filename'First .. Last)
               & Extension,
               Prj.Env.Ada_Objects_Path (Project).all & Path_Separator &
                 Predefined_Object_Path & Path_Separator & Current_Dir_Name);

         else
            Dir := Locate_Regular_File
              (Short_ALI_Filename (Short_ALI_Filename'First .. Last)
               & Extension,
               Predefined_Object_Path & Path_Separator & Current_Dir_Name);
         end if;

         exit when Dir /= null;

         --  Search the next candidate: it might be the parent if we have a
         --  separate unit.
         while Last > Short_ALI_Filename'First loop
            Last := Last - 1;
            exit when
              (Last + Dot_Replacement'Length - 1 <= Short_ALI_Filename'Last
               and then Short_ALI_Filename
                 (Last .. Last + Dot_Replacement'Length - 1) = Dot_Replacement)

               --  Special case when there might be a confusion with the GNAT
               --  runtime files.
              or else (Dot_Replacement = "-"
                       and then Short_ALI_Filename (Last) = '~');
         end loop;
         Last := Last - 1;
      end loop;

      --  Abort if we did not find the ALI file.

      if Dir = null then
         Trace (Me, "Could not locate ALI file: " & Short_ALI_Filename);
         return No_ALI_Id;
      end if;

      --  ??? Should check that the ALI file we have found indeed matches the
      --  unit name we were looking for (since we were looking for the parent
      --  ALI file possibly)

      --  Scan the ALI file and return the result. Add a trace if we failed
      --  to scan it.

      Result := Load_And_Scan_ALI (Dir.all);

      if Result = No_ALI_Id then
         Trace (Me, "Failed to scan " & Dir.all);
      end if;

      Free (Dir);

      return Result;
   end Locate_Load_And_Scan_ALI;

   --------------------
   -- Parse_ALI_File --
   --------------------

   procedure Parse_ALI_File
     (Handler                : ALI_Handler;
      Short_ALI_Filename     : String;
      Project                : Prj.Project_Id;
      Predefined_Source_Path : String;
      Predefined_Object_Path : String;
      List                   : in out LI_File_List;
      Unit                   : out LI_File_Ptr;
      Success                : out Boolean)
   is
      New_ALI_Id    : constant ALI_Id := Locate_Load_And_Scan_ALI
        (Short_ALI_Filename, Project, Predefined_Object_Path);

   begin
      if New_ALI_Id = No_ALI_Id then
         Unit    := null;
         Success := False;
         Trace (Me, "Parse_ALI_File: couldn't locate and load file "
                & Short_ALI_Filename);
         return;
      end if;

      Trace (Me, "Parse_ALI_File: parsing " & Short_ALI_Filename);
      Create_New_ALI
        (Handler, Unit, ALIs.Table (New_ALI_Id), Project,
         Predefined_Source_Path, List);
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
      Source_Filename        : String;
      Project                : Prj.Project_Id;
      Predefined_Source_Path : String) return String
   is
      pragma Unreferenced (Handler);
      Prj_Data       : Project_Data renames Prj.Projects.Table (Project);
      Naming         : Naming_Data renames Prj_Data.Naming;

      Source_Name_Id : File_Name_Type;
      Body_Id        : Array_Element_Id;
      Spec_Id        : Array_Element_Id;
      Prj_Unit       : Prj.Com.Unit_Id;
      Dir            : String_Access;
      Filename       : Name_Id;
      Part           : Unit_Part;

   begin
      --  Store the source filename in the Namet buffer and get its Name ID

      Namet.Name_Buffer (1 .. Source_Filename'Length) := Source_Filename;
      Namet.Name_Len := Source_Filename'Length;
      Source_Name_Id := Namet.Name_Find;

      --  First, check the body filename exception list

      Body_Id := Search_Filename (Naming.Bodies, Source_Name_Id);

      if Body_Id /= No_Array_Element then
         declare
            Ali : constant String := Get_ALI_Filename (Source_Name_Id);
         begin
            Trace (Me, "ALI-1 file for " & Source_Filename
                   & " is " & Ali);
            return Ali;
         end;
      end if;

      --  Not found, check in the spec filename exception list

      Spec_Id := Search_Filename (Naming.Specifications, Source_Name_Id);

      --  If found, then we have the Unit_Name, check if we can find a
      --  body for this unit.

      if Spec_Id /= No_Array_Element then
         --  Search the body exception list first

         Body_Id := Search_Unit_Name (Naming.Bodies, Get_Unit_Name (Spec_Id));

         if Body_Id /= No_Array_Element then
            declare
               Ali : constant String :=
                 Get_ALI_Filename (Get_Filename (Body_Id));
            begin
               Trace (Me, "ALI-2 file for " & Source_Filename
                      & " is " & Ali);
               return Ali;
            end;
         end if;

         --  Not found, so search in the project units lists

         Prj_Unit := Prj.Com.Units_Htable.Get (Get_Unit_Name (Spec_Id));

         if Get_Body_Filename (Prj_Unit) /= No_Name then
            declare
               Ali : constant String :=
                 Get_ALI_Filename (Get_Body_Filename (Prj_Unit));
            begin
               Trace (Me, "ALI-3 file for " & Source_Filename
                      & " is " & Ali);
               return Ali;
            end;
         end if;

         --  Still not found, search in the source path

         Filename := Get_Body_Filename (Get_Unit_Name (Spec_Id), Naming);
         Dir := Locate_Regular_File
           (Get_Name_String (Filename), Predefined_Source_Path);

         if Dir /= null then
            declare
               Ali : constant String := Get_ALI_Filename (Filename);
            begin
               Trace (Me, "ALI-4 file for " & Source_Filename
                      & " is " & Ali);
               Free (Dir);
               return Ali;
            end;
         end if;

         --  Not found anywhere, so this is a spec only file

         declare
            Ali : constant String :=
              Get_ALI_Filename (Get_Filename (Spec_Id));
         begin
            Trace (Me, "ALI-5 file for " & Source_Filename
                   & " is " & Ali);
            return Ali;
         end;

      end if; -- Spec_Id /= No_Array_Element

      --  Is this a body, according to the naming scheme?

      Part := Get_Unit_Part_From_Filename
        (Get_Name_String (Source_Name_Id), Project);

      if Part = Unit_Body then
         declare
            Ali : constant String := Get_ALI_Filename (Source_Name_Id);
         begin
            Trace (Me, "ALI-6 file for " & Source_Filename
                   & " is " & Ali);
            return Ali;
         end;
      end if;

      --  At this point, we know that we have a spec, so check the extension
      --  to make sure that it matches the naming scheme. Otherwise, return
      --  the empty string.

      if Part /= Unit_Spec then
         Trace (Me, "No ALI-7 file for " & Source_Filename);
         return "";
      end if;

      --  So at this point, we know that that we have a spec with the correct
      --  extension. Let's see if we can find a body for this spec...

      declare
         Unit_Name : constant Name_Id :=
           Get_Unit_Name (Source_Name_Id, Project);

      begin
         if Unit_Name = No_Name then
            --  ??? This is a bug, return the empty string for now, but we
            --  need a better error reporting mechanism later.
            Trace (Me, "No ALI-8 file for " & Source_Filename);
            return "";
         end if;

         --  Search the body exception list

         Body_Id := Search_Filename (Naming.Bodies, Unit_Name);

         if Body_Id /= No_Array_Element then
            declare
               Ali : constant String :=
                 Get_ALI_Filename (Get_Filename (Body_Id));
            begin
               Trace (Me, "ALI-9 file for " & Source_Filename
                      & " is " & Ali);
               return Ali;
            end;
         end if;

         --  Not found, so check in the project units list

         Prj_Unit := Prj.Com.Units_Htable.Get (Unit_Name);

         if Get_Body_Filename (Prj_Unit) /= No_Name then
            declare
               Ali : constant String :=
                 Get_ALI_Filename (Get_Body_Filename (Prj_Unit));
            begin
               Trace (Me, "ALI-10 file for " & Source_Filename
                      & " is " & Ali);
               return Ali;
            end;
         end if;

         --  Search the body filename in the source path

         Filename := Get_Body_Filename (Unit_Name, Naming);
         Dir :=
           Locate_Regular_File
             (Get_Name_String (Filename), Predefined_Source_Path);

         if Dir /= null then
            declare
               Ali : constant String := Get_ALI_Filename (Filename);
            begin
               Trace (Me, "ALI-11 file for " & Source_Filename
                      & " is " & Ali);
               Free (Dir);
               return Ali;
            end;
         end if;

         --  The body was not found anywhere, so this is a spec only file
         declare
            Ali : constant String := Get_ALI_Filename (Source_Name_Id);
         begin
            Trace (Me, "ALI-12 file for " & Source_Filename
                   & " is " & Ali);
            Free (Dir);
            return Ali;
         end;
      end;
   end LI_Filename_From_Source;

   ---------------------------
   -- Create_Or_Complete_LI --
   ---------------------------

   procedure Create_Or_Complete_LI
     (Handler                : access ALI_Handler_Record;
      File                   : in out LI_File_Ptr;
      Source_Filename        : String;
      List                   : in out LI_File_List;
      Project                : Prj.Project_Id;
      Predefined_Source_Path : String;
      Predefined_Object_Path : String)
   is
      F : File_Info_Ptr_List;
      Base : constant String := Base_Name (Source_Filename);
   begin
      File := Locate_From_Source (List, Base);
      Create_Or_Complete_LI
        (Handler                => Handler,
         File                   => File,
         Ali_File               => LI_Filename_From_Source
           (Handler, Base, Project, Predefined_Source_Path),
         List                   => List,
         Project                => Project,
         Predefined_Source_Path => Predefined_Source_Path,
         Predefined_Object_Path => Predefined_Object_Path);

      --  Make sure that the LI file we just parsed does contain the
      --  information for the initial source file name. Since for separate
      --  units we might have been looking for the parent ALI file (see
      --  Locate_Load_And_Scan_ALI), we now have to make sure we found the
      --  correct LI file.

      if File /= No_LI_File then
         if (File.LI.Spec_Info /= null
             and then File.LI.Spec_Info.Source_Filename.all = Base)
           or else (File.LI.Body_Info /= null
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

         Trace (Me, "Parsed LI file didn't contain the info for " & Base);
         File := No_LI_File;
      end if;
   end Create_Or_Complete_LI;

   ---------------------------
   -- Create_Or_Complete_LI --
   ---------------------------

   procedure Create_Or_Complete_LI
     (Handler                : access ALI_Handler_Record'Class;
      File                   : in out LI_File_Ptr;
      Ali_File               : String;
      List                   : in out LI_File_List;
      Project                : Prj.Project_Id;
      Predefined_Source_Path : String;
      Predefined_Object_Path : String)
   is
      Success : Boolean := True;
   begin
      if File = No_LI_File then
         Parse_ALI_File
           (ALI_Handler (Handler), Ali_File, Project,
            Predefined_Source_Path, Predefined_Object_Path,
            List, File, Success);

         if not Success then
            Trace (Me, "Couldn't parse LI file " & Ali_File);
            File := No_LI_File;
         end if;

      else
         declare
            Full_File : constant String := Find_File
              (File.LI.LI_Filename.all, Object_Path (Project, True),
               Predefined_Object_Path);
         begin
            if Is_Incomplete (File)
              or else To_Timestamp (File_Time_Stamp (Full_File)) >
              File.LI.LI_Timestamp
            then
               Parse_ALI_File
                 (ALI_Handler (Handler), File.LI.LI_Filename.all, Project,
                  Predefined_Source_Path, Predefined_Object_Path,
                  List, File, Success);

               if not Success then
                  Trace (Me, "Couldn't parse LI file " & Full_File);
                  File := No_LI_File;
               end if;
            end if;
         end;
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
      List                   : in out LI_File_List;
      In_Directory           : String;
      Project                : Prj.Project_Id;
      Predefined_Source_Path : String;
      Predefined_Object_Path : String)
   is
      Dir : Dir_Type;
      File : String (1 .. 1024);
      Last : Natural;
      LI : LI_File_Ptr;
   begin
      Open (Dir, In_Directory);

      loop
         Read (Dir, File, Last);
         exit when Last = 0;

         if File_Extension (File (File'First .. Last)) = ".ali" then
            LI := Locate (List, File (File'First .. Last));
            Create_Or_Complete_LI
              (Handler                => Handler,
               File                   => LI,
               Ali_File               => File (File'First .. Last),
               List                   => List,
               Project                => Project,
               Predefined_Source_Path => Predefined_Source_Path,
               Predefined_Object_Path => Predefined_Object_Path);
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
      Root_Project  : Prj.Project_Id;
      File_Project  : Prj.Project_Id;
      Full_Filename : String) return LI_Handler_Iterator'Class
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
      Root_Project : Prj.Project_Id;
      Project      : Prj.Project_Id;
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
