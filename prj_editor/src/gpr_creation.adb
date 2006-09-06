-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2004-2006                       --
--                              AdaCore                              --
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
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with GNAT.OS_Lib;               use GNAT.OS_Lib;
with Traces;                    use Traces;
with HTables;
with Ada.Unchecked_Deallocation;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Projects.Editor;           use Projects, Projects.Editor;
with Projects.Registry;         use Projects.Registry;
with VFS;                       use VFS;

package body GPR_Creation is

   Me : constant Debug_Handle := Create ("Creator");

   type File_Info_Record is record
      Src_Dir_Index : Integer;
      --  Index of the source directory to which the file belongs

      Obj_Dir_Index : Integer;
      --  Index of the object directory that contains the object files for the
      --  file
   end record;
   type File_Info is access File_Info_Record;
   No_File_Info : constant File_Info := null;

   procedure Free is new Ada.Unchecked_Deallocation
     (File_Info_Record, File_Info);

   type Header_Num is range 1 .. 5000;

   function Hash is new HTables.Hash (Header_Num);
   --  Subprograms used for the hash table

   package File_Htables is new HTables.Simple_HTable
     (Header_Num   => Header_Num,
      Element      => File_Info,
      Free_Element => Free,
      No_Element   => No_File_Info,
      Key          => String,
      Hash         => Hash,
      Equal        => "=");
   use File_Htables;

   type Is_Related_To is array (Natural range <>, Natural range <>) of Boolean;
   --  Whether two directories are related (source dir has object in object
   --  dir). The first index is for source dirs, the second for obj dir.

   type Object_Directory_Info is record
      Project_Num  : Integer := -1;
      --  The number of the project file associated with that directory

      Files        : String_List_Access;
      --  List of object files in that directory
   end record;

   type Object_Directory_Info_Array
     is array (Natural range <>) of Object_Directory_Info;

   procedure Free (X : in out Object_Directory_Info_Array);
   procedure Free (X : in out Object_Directory_Info);
   --  Free the information in X

   procedure Process_List
     (Project         : Project_Type;
      Attribute       : Attribute_Pkg;
      List            : String_List;
      Attribute_Index : String := "");
   --  Create a new GPR attribute in File, with List as its value

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (String_List, String_List_Access);

   procedure Process_Switches
     (Project         : Project_Type;
      Attribute       : Attribute_Pkg;
      Value           : String;
      Attribute_Index : String := "");
   --  Process a switches attribute, which needs to be splitted in the .gpr
   --  file

   procedure Parse_Source_Dirs
     (Source_Dirs : GNAT.OS_Lib.String_List;
      Files       : in out File_Htables.HTable);
   --  Process all source directories, and find the list of source files.
   --  Result is stored in the Directories/Files parameters

   procedure Parse_Object_Dirs
     (Object_Dirs     : GNAT.OS_Lib.String_List;
      Related_To      : in out Is_Related_To;
      Directories     : in out Object_Directory_Info_Array;
      Src_Files       : in out File_Htables.HTable;
      Obj_Files_Count : out Natural;
      Spec_Extension  : String;
      Body_Extension  : String);
   --  Process all the object directories, and compute the dependencies between
   --  source dirs and object dirs.

   procedure Process_Obj_File
     (Obj_Dir_Index  : Integer;
      Obj_Filename   : String;
      Related_To     : in out Is_Related_To;
      Src_Files      : in out File_Htables.HTable;
      Spec_Extension : String;
      Body_Extension : String);
   --  Process the object file, linking the object directory and source
   --  directories together

   procedure Generate_Project_Attributes
     (Project           : Project_Type;
      Is_Root_Project   : Boolean;
      Main_Units        : String_List_Access := null;
      Builder_Switches  : String;
      Compiler_Switches : String;
      Binder_Switches   : String;
      Linker_Switches   : String;
      Cross_Prefix      : String := "";
      Spec_Extension    : String := ".ads";
      Body_Extension    : String := ".adb");
   --  Generate all attributes and packages for a project.
   --  If the project is not a root project, some attributes are ignored.

   function Src_Dirs_Have_Unique_Obj_Dir
     (Related_To : Is_Related_To) return Boolean;
   --  Return true if each source directory has object files in a single
   --  directory.

   procedure Generate_Withs
     (Projects        : Project_Type_Array;
      Root_Project    : Project_Type;
      Related_To      : Is_Related_To;
      Source_Dirs     : String_List;
      Obj_Dirs        : String_List;
      Src_Files       : File_Htables.HTable;
      Object_Dirs     : in out Object_Directory_Info_Array;
      Current_Project : Integer;
      All_Source_Dirs : Boolean := False);
   --  Generate all the withs statements.
   --  All projects import all other projects, since we do not know how
   --  to test the source dependencies.
   --  No statement is added for the project Omit_Project (or Root_Project
   --  if Omit_Project is 0).
   --  If All_Source_Dirs is True, then all the source dirs are the same for
   --  all the projects, and a Source_Files attribute is generated

   procedure Generate_Source_Files_List
     (Project       : Project_Type;
      Src_Files     : File_Htables.HTable;
      Obj_Dir_Index : Integer);
   --  Generate the list of source files for the given Obj_Dir

   function Image (Num : Integer) return String;
   --  Return the image of an integer, with no leading whitespace

   ----------
   -- Free --
   ----------

   procedure Free (X : in out Object_Directory_Info_Array) is
   begin
      for D in X'Range loop
         Free (X (D));
      end loop;
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (X : in out Object_Directory_Info) is
   begin
      Free (X.Files);
   end Free;

   -----------
   -- Image --
   -----------

   function Image (Num : Integer) return String is
      N : constant String := Integer'Image (Num);
   begin
      if N (N'First) = ' ' then
         return N (N'First + 1 .. N'Last);
      else
         return N;
      end if;
   end Image;

   -----------------------
   -- Parse_Source_Dirs --
   -----------------------

   procedure Parse_Source_Dirs
     (Source_Dirs : GNAT.OS_Lib.String_List;
      Files       : in out File_Htables.HTable)
   is
      Dir  : Dir_Type;
      File : String (1 .. 1024);
      Last : Natural;
   begin
      for Src in Source_Dirs'Range loop
         begin
            Open (Dir, Source_Dirs (Src).all);

            loop
               Read (Dir, File, Last);
               exit when Last = 0;

               Trace (Me, "   Push source file: " & File (1 .. Last));
               File_Htables.Set
                 (Files,
                  File (1 .. Last),
                  new File_Info_Record'(Src_Dir_Index => Src,
                                        Obj_Dir_Index => -1));
            end loop;

            Close (Dir);

         exception
            when Directory_Error =>
               --  Ignored
               null;
         end;
      end loop;
   end Parse_Source_Dirs;

   ----------------------
   -- Process_Obj_File --
   ----------------------

   procedure Process_Obj_File
     (Obj_Dir_Index  : Integer;
      Obj_Filename   : String;
      Related_To     : in out Is_Related_To;
      Src_Files      : in out File_Htables.HTable;
      Spec_Extension : String;
      Body_Extension : String)
   is
      Extension : constant String := File_Extension (Obj_Filename);
      Base      : constant String := Obj_Filename
        (Obj_Filename'First  .. Obj_Filename'Last - Extension'Length);
      Spec_Name : constant String := Base & Spec_Extension;
      Body_Name : constant String := Base & Body_Extension;

      Base_Extension : constant String := File_Extension (Base);
      Base_Base : constant String := Base
        (Base'First .. Base'Last - Base_Extension'Length);
      Base_Spec : constant String := Base_Base & Spec_Extension;
      Base_Body : constant String := Base_Base & Body_Extension;

      File      : File_Info;
   begin
      Trace (Me, "  Process obj file: " & Obj_Filename);

      File := File_Htables.Get (Src_Files, Spec_Name);
      if File /= No_File_Info then
         File.Obj_Dir_Index := Obj_Dir_Index;
         Related_To (File.Src_Dir_Index, Obj_Dir_Index) := True;
      else
         --  In some naming schemes, the extension is ".1.ada", and the object
         --  file will end with ".1.o", so we should remove an extra level of
         --  extension.
         --  ??? Cleaner solution would be to revert the logic of the tool, and
         --  start from sources to find object files.

         File := File_Htables.Get (Src_Files, Base_Spec);
         if File /= No_File_Info then
            File.Obj_Dir_Index := Obj_Dir_Index;
            Related_To (File.Src_Dir_Index, Obj_Dir_Index) := True;
         end if;
      end if;

      File := File_Htables.Get (Src_Files, Body_Name);
      if File /= No_File_Info then
         File.Obj_Dir_Index := Obj_Dir_Index;
         Related_To (File.Src_Dir_Index, Obj_Dir_Index) := True;
      else
         File := File_Htables.Get (Src_Files, Base_Body);
         if File /= No_File_Info then
            File.Obj_Dir_Index := Obj_Dir_Index;
            Related_To (File.Src_Dir_Index, Obj_Dir_Index) := True;
         end if;
      end if;
   end Process_Obj_File;

   -----------------------
   -- Parse_Object_Dirs --
   -----------------------

   procedure Parse_Object_Dirs
     (Object_Dirs     : GNAT.OS_Lib.String_List;
      Related_To      : in out Is_Related_To;
      Directories     : in out Object_Directory_Info_Array;
      Src_Files       : in out File_Htables.HTable;
      Obj_Files_Count : out Natural;
      Spec_Extension  : String;
      Body_Extension  : String)
   is
      Tmp  : String_List_Access;
      Info : Object_Directory_Info;
      Dir  : Dir_Type;
      File : String (1 .. 1024);
      Last : Natural;
   begin
      Obj_Files_Count := 0;

      for Obj in Object_Dirs'Range loop
         begin
            Trace (Me, "Push object directory: " & Object_Dirs (Obj).all);
            Open (Dir, Object_Dirs (Obj).all);

            --  Put this after the open, so that we only count directories
            --  which actually exist.

            Info := (Obj - Object_Dirs'First, null);

            loop
               Read (Dir, File, Last);
               exit when Last = 0;

               if (Last > 1 and then File (Last - 1 .. Last) = ".o")
                 or else (Last > 4 and then File (Last - 3 .. Last) = ".ali")
               then
                  if Info.Files = null then
                     Info.Files := new String_List (1 .. 1);
                  else
                     Tmp := Info.Files;
                     Info.Files := new String_List (1 .. Info.Files'Last + 1);
                     Info.Files (Tmp'Range) := Tmp.all;
                     Unchecked_Free (Tmp);
                  end if;

                  Info.Files (Info.Files'Last) :=
                    new String'(File (1 .. Last));

                  Obj_Files_Count := Obj_Files_Count + 1;
                  Process_Obj_File
                    (Obj_Dir_Index  => Obj,
                     Obj_Filename   => File (1 .. Last),
                     Related_To     => Related_To,
                     Src_Files      => Src_Files,
                     Spec_Extension => Spec_Extension,
                     Body_Extension => Body_Extension);
               end if;
            end loop;

            Directories (Obj) := Info;

            Close (Dir);

         exception
            when Directory_Error =>
               --  Ignored, this was legal with .adp files
               null;
         end;
      end loop;
   end Parse_Object_Dirs;

   --------------------------------
   -- Generate_Source_Files_List --
   --------------------------------

   procedure Generate_Source_Files_List
     (Project       : Project_Type;
      Src_Files     : File_Htables.HTable;
      Obj_Dir_Index : Integer)
   is
      Iter  : File_Htables.Iterator;
      Info  : File_Info;
      Count : Natural := 0;
   begin
      Get_First (Src_Files, Iter);
      loop
         Info := Get_Element (Iter);
         exit when Info = No_File_Info;

         if Info.Obj_Dir_Index = Obj_Dir_Index then
            Count := Count + 1;
         end if;

         Get_Next (Src_Files, Iter);
      end loop;

      declare
         List : String_List (1 .. Count);
      begin
         Count := List'First;

         Get_First (Src_Files, Iter);
         loop
            Info := Get_Element (Iter);
            exit when Info = No_File_Info;

            if Info.Obj_Dir_Index = Obj_Dir_Index then
               List (Count) := new String'(Get_Key (Iter));
               Count := Count + 1;
            end if;

            Get_Next (Src_Files, Iter);
         end loop;

         Update_Attribute_Value_In_Scenario
           (Project            => Project,
            Scenario_Variables => No_Scenario,
            Attribute          => Source_Files_Attribute,
            Values             => List);

         for F in List'Range loop
            Free (List (F));
         end loop;
      end;
   end Generate_Source_Files_List;

   ------------------
   -- Process_List --
   ------------------

   procedure Process_List
     (Project         : Project_Type;
      Attribute       : Attribute_Pkg;
      List            : String_List;
      Attribute_Index : String := "") is
   begin
      Update_Attribute_Value_In_Scenario
        (Project            => Project,
         Scenario_Variables => No_Scenario,
         Attribute          => Attribute,
         Values             => List,
         Attribute_Index    => Attribute_Index);
   end Process_List;

   ----------------------
   -- Process_Switches --
   ----------------------

   procedure Process_Switches
     (Project         : Project_Type;
      Attribute       : Attribute_Pkg;
      Value           : String;
      Attribute_Index : String := "")
   is
      List : Argument_List_Access;
   begin
      if Value /= "" then
         List := Argument_String_To_List (Value);
         Update_Attribute_Value_In_Scenario
           (Project            => Project,
            Scenario_Variables => No_Scenario,
            Attribute          => Attribute,
            Values             => List.all,
            Attribute_Index    => Attribute_Index);
         Free (List);
      end if;
   end Process_Switches;

   ---------------------------------
   -- Generate_Project_Attributes --
   ---------------------------------

   procedure Generate_Project_Attributes
     (Project           : Project_Type;
      Is_Root_Project   : Boolean;
      Main_Units        : String_List_Access := null;
      Builder_Switches  : String;
      Compiler_Switches : String;
      Binder_Switches   : String;
      Linker_Switches   : String;
      Cross_Prefix      : String := "";
      Spec_Extension    : String := ".ads";
      Body_Extension    : String := ".adb") is
   begin
      --  ??? Exact test should be whether the file belongs to the project or
      --  not.
      if Main_Units /= null
        and then Main_Units'Length /= 0
      then
         Process_List (Project, Main_Attribute, Main_Units.all);
      end if;

      if Is_Root_Project then
         Process_Switches
           (Project, Builder_Default_Switches_Attribute,
            Builder_Switches, "Ada");
      end if;

      Process_Switches
        (Project, Compiler_Default_Switches_Attribute,
         Compiler_Switches, "Ada");

      if Is_Root_Project then
         Process_Switches
           (Project, Binder_Default_Switches_Attribute,
            Binder_Switches, "Ada");
         Process_Switches
           (Project, Linker_Default_Switches_Attribute,
            Linker_Switches, "Ada");

         if Cross_Prefix /= "" then
            Update_Attribute_Value_In_Scenario
              (Project            => Project,
               Scenario_Variables => No_Scenario,
               Attribute          => Compiler_Command_Attribute,
               Value              => Cross_Prefix & "gnatmake",
               Attribute_Index    => "Ada");
            Update_Attribute_Value_In_Scenario
              (Project            => Project,
               Scenario_Variables => No_Scenario,
               Attribute          => Debugger_Command_Attribute,
               Value              => Cross_Prefix & "gdb");
         end if;
      end if;

      if Spec_Extension /= ".ads"
        or else Body_Extension /= ".adb"
      then
         Update_Attribute_Value_In_Scenario
           (Project            => Project,
            Scenario_Variables => No_Scenario,
            Attribute          => Specification_Suffix_Attribute,
            Value              => Spec_Extension,
            Attribute_Index    => "Ada");
         Update_Attribute_Value_In_Scenario
           (Project            => Project,
            Scenario_Variables => No_Scenario,
            Attribute          => Implementation_Suffix_Attribute,
            Value              => Body_Extension,
            Attribute_Index    => "Ada");
      end if;
   end Generate_Project_Attributes;

   ----------------------------------
   -- Src_Dirs_Have_Unique_Obj_Dir --
   ----------------------------------

   function Src_Dirs_Have_Unique_Obj_Dir
     (Related_To  : Is_Related_To) return Boolean
   is
      Count : Natural;
   begin
      for Src in Related_To'Range (1) loop
         Count := 0;
         for Obj in Related_To'Range (2) loop
            if Related_To (Src, Obj) then
               Count := Count + 1;
               if Count > 1 then
                  return False;
               end if;
            end if;
         end loop;
      end loop;
      return True;
   end Src_Dirs_Have_Unique_Obj_Dir;

   --------------------
   -- Generate_Withs --
   --------------------

   procedure Generate_Withs
     (Projects        : Project_Type_Array;
      Root_Project    : Project_Type;
      Related_To      : Is_Related_To;
      Source_Dirs     : String_List;
      Obj_Dirs        : String_List;
      Src_Files       : File_Htables.HTable;
      Object_Dirs     : in out Object_Directory_Info_Array;
      Current_Project : Integer;
      All_Source_Dirs : Boolean := False)
   is
      Current_Dir : Natural;
      Tmp         : Import_Project_Error;
   begin
      for D in Object_Dirs'Range loop
         --  Have we found the object directory matching our current project ?
         if Object_Dirs (D).Project_Num = Current_Project then
            Current_Dir := D;

         else
            Tmp := Add_Imported_Project
              (Root_Project      => Root_Project,
               Project           => Projects (Current_Project),
               Imported_Project  => Projects (Object_Dirs (D).Project_Num),
               Use_Relative_Path => True,
               Limited_With      => True);
            if Tmp /= Success then
               Trace (Me, "Error when adding with " & Tmp'Img
                      & " current=" & Current_Project'Img
                      & " num=" & Object_Dirs (D).Project_Num'Img);
            end if;
         end if;
      end loop;

      Update_Attribute_Value_In_Scenario
        (Project            => Projects (Current_Project),
         Scenario_Variables => No_Scenario,
         Attribute          => Obj_Dir_Attribute,
         Value              => Obj_Dirs (Current_Dir).all);

      if not All_Source_Dirs then
         declare
            List  : String_List (Related_To'Range (1));
            Index : Integer := List'First;
         begin
            for Src in Related_To'Range (1) loop
               if Related_To (Src, Current_Dir) then
                  List (Index) := Source_Dirs (Src);
                  Index := Index + 1;
               end if;
            end loop;

            Update_Attribute_Value_In_Scenario
              (Project            => Projects (Current_Project),
               Scenario_Variables => No_Scenario,
               Attribute          => Source_Dirs_Attribute,
               Values             => List (List'First .. Index - 1));
         end;

      else
         Process_List
           (Projects (Current_Project), Source_Dirs_Attribute, Source_Dirs);
         Generate_Source_Files_List
           (Project       => Projects (Current_Project),
            Src_Files     => Src_Files,
            Obj_Dir_Index => Current_Dir);
      end if;
   end Generate_Withs;

   ----------------------
   -- Create_Gpr_Files --
   ----------------------

   procedure Create_Gpr_Files
     (Registry          : Projects.Registry.Project_Registry'Class;
      Root_Project      : Project_Type;
      Source_Dirs       : GNAT.OS_Lib.String_List;
      Object_Dirs       : GNAT.OS_Lib.String_List;
      Spec_Extension    : String;
      Body_Extension    : String;
      Main_Units        : GNAT.OS_Lib.String_List_Access := null;
      Builder_Switches  : String;
      Compiler_Switches : String;
      Binder_Switches   : String;
      Linker_Switches   : String;
      Cross_Prefix      : String := "")
   is
      Project         : Project_Type;
      Obj_Dirs        : Object_Directory_Info_Array (Object_Dirs'Range);
      Src_Files       : File_Htables.HTable;
      Obj_Files_Count : Natural;
      Single_Obj_Dir  : Boolean;
      Tmp             : Boolean;
      Related_To      : Is_Related_To (Source_Dirs'Range, Object_Dirs'Range) :=
                               (others => (others => False));
      pragma Unreferenced (Tmp);
   begin
      Parse_Source_Dirs (Source_Dirs, Src_Files);
      Parse_Object_Dirs
        (Object_Dirs,
         Related_To      => Related_To,
         Directories     => Obj_Dirs,
         Src_Files       => Src_Files,
         Obj_Files_Count => Obj_Files_Count,
         Spec_Extension  => Spec_Extension,
         Body_Extension  => Body_Extension);

      if Obj_Files_Count = 0
        or else Object_Dirs'Length = 1
      then
         Trace (Me, "Simple case: obj_files_count=" & Obj_Files_Count'Img);

         --  Ignore all other object directories but the first one.
         --  The other directories do not contain any object file anyway

         Trace (Me, "Creating " & Full_Name (Project_Path (Root_Project)).all);
         Project := Root_Project;
         Process_List (Project, Source_Dirs_Attribute, Source_Dirs);

         if Object_Dirs'Length = 0 then
            Update_Attribute_Value_In_Scenario
              (Project            => Project,
               Scenario_Variables => No_Scenario,
               Attribute          => Obj_Dir_Attribute,
               Value              => ".");
         else
            Update_Attribute_Value_In_Scenario
              (Project            => Project,
               Scenario_Variables => No_Scenario,
               Attribute          => Obj_Dir_Attribute,
               Value              => Object_Dirs (Object_Dirs'First).all);
         end if;

         Generate_Project_Attributes
           (Project           => Project,
            Is_Root_Project   => True,
            Main_Units        => Main_Units,
            Builder_Switches  => Builder_Switches,
            Compiler_Switches => Compiler_Switches,
            Binder_Switches   => Binder_Switches,
            Linker_Switches   => Linker_Switches,
            Cross_Prefix      => Cross_Prefix,
            Spec_Extension    => Spec_Extension,
            Body_Extension    => Body_Extension);
         Tmp := Save_Project (Project);

      else
         Single_Obj_Dir := Src_Dirs_Have_Unique_Obj_Dir (Related_To);
         Trace (Me, "Case: complex case. Source dirs associated with single"
                & " obj dir: " & Boolean'Image (Single_Obj_Dir));

         declare
            Projects : Project_Type_Array (0 .. Object_Dirs'Length - 1);
         begin

            --  Create all projects immedatiately, so that we can create
            --  dependencies between them later on

            Projects (0) := Root_Project;

            for P in 1 .. Object_Dirs'Length - 1 loop
               Projects (P) := Create_Project
                 (Registry,
                  Name => "project" & Image (P),
                  Path => Project_Directory (Root_Project));
            end loop;

            --  Then complete the contents of each project

            for P in 0 .. Object_Dirs'Length - 1 loop
               Trace
                 (Me, "Manipulating "
                  & Full_Name (Project_Path (Projects (P))).all);

               Generate_Withs
                 (Projects          => Projects,
                  Related_To        => Related_To,
                  Source_Dirs       => Source_Dirs,
                  Root_Project      => Root_Project,
                  Obj_Dirs          => Object_Dirs,
                  Src_Files         => Src_Files,
                  Object_Dirs       => Obj_Dirs,
                  Current_Project   => P,
                  All_Source_Dirs   => not Single_Obj_Dir);

               Generate_Project_Attributes
                 (Project           => Projects (P),
                  Is_Root_Project   => P = 0,
                  Main_Units        => Main_Units,
                  Builder_Switches  => Builder_Switches,
                  Compiler_Switches => Compiler_Switches,
                  Binder_Switches   => Binder_Switches,
                  Linker_Switches   => Linker_Switches,
                  Cross_Prefix      => Cross_Prefix,
                  Spec_Extension    => Spec_Extension,
                  Body_Extension    => Body_Extension);

               Tmp := Save_Project (Projects (P));
            end loop;
         end;
      end if;

      Free (Obj_Dirs);
      File_Htables.Reset (Src_Files);
   end Create_Gpr_Files;

   procedure Create_Gpr_Files
     (Registry          : Projects.Registry.Project_Registry'Class;
      Root_Project      : Projects.Project_Type;
      Source_Dirs       : VFS.File_Array;
      Object_Dirs       : VFS.File_Array;
      Spec_Extension    : String;
      Body_Extension    : String;
      Main_Units        : GNAT.OS_Lib.String_List_Access := null;
      Builder_Switches  : String;
      Compiler_Switches : String;
      Binder_Switches   : String;
      Linker_Switches   : String;
      Cross_Prefix      : String := "")
   is
      S_Source_Dirs : GNAT.OS_Lib.String_List (Source_Dirs'Range);
      S_Object_Dirs : GNAT.OS_Lib.String_List (Object_Dirs'Range);
   begin

      for J in Source_Dirs'Range loop
         S_Source_Dirs (J) := new String'(VFS.Full_Name (Source_Dirs (J)).all);
      end loop;

      for J in Object_Dirs'Range loop
         S_Object_Dirs (J) := new String'(VFS.Full_Name (Object_Dirs (J)).all);
      end loop;

      Create_Gpr_Files
        (Registry,
         Root_Project,
         S_Source_Dirs,
         S_Object_Dirs,
         Spec_Extension,
         Body_Extension,
         Main_Units,
         Builder_Switches,
         Compiler_Switches,
         Binder_Switches,
         Linker_Switches,
         Cross_Prefix);
   end Create_Gpr_Files;

end GPR_Creation;
