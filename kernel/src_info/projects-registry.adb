-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2002-2004                       --
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

with Ada.Exceptions;            use Ada.Exceptions;
with Ada.Unchecked_Deallocation;
with Ada.Characters.Handling;   use Ada.Characters.Handling;
with Ada.Strings.Fixed;         use Ada.Strings.Fixed;
with ALI;
with Atree;
with Basic_Types;               use Basic_Types;
with Csets;
with Errout;
with File_Utils;                use File_Utils;
with String_Utils;              use String_Utils;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with GNAT.Case_Util;            use GNAT.Case_Util;
pragma Warnings (Off);
with GNAT.Expect.TTY;           use GNAT.Expect, GNAT.Expect.TTY;
pragma Warnings (On);
with Glide_Intl;                use Glide_Intl;
with Glib.Convert;              use Glib.Convert;
with Namet;                     use Namet;
with Opt;                       use Opt;
with Output;                    use Output;
with Osint;                     use Osint;
with OS_Utils;                  use OS_Utils;
with Prj.Com;                   use Prj.Com;
with Prj.Ext;                   use Prj.Ext;
with Prj.PP;                    use Prj.PP;
with Prj.Util;                  use Prj.Util;
with Prj.Part;                  use Prj.Part;
with Prj.Proc;                  use Prj.Proc;
with Prj.Tree;                  use Prj.Tree;
with Prj;                       use Prj;
with Projects.Editor;           use Projects.Editor;
with Snames;                    use Snames;
with Stringt;
with String_Hash;
with Traces;                    use Traces;
with Types;                     use Types;
with VFS;                       use VFS;

package body Projects.Registry is

   Me : constant Debug_Handle := Create ("Projects.Registry");

   Project_Backward_Compatibility : constant Boolean := True;
   --  Should be set to true if saved project should be compatible with GNAT
   --  3.15a1, False if they only need to be compatible with GNAT 3.16 >=
   --  20021024

   Unknown_Language : Name_Id;
   --  Constant used to mark source files set in the project, but for which no
   --  matching language has been found.

   procedure Do_Nothing (Project : in out Project_Type);
   --  Do not free the project (in the hash tables), since it shared by several
   --  entries and several htables

   package Project_Htable is new String_Hash
     (Data_Type      => Project_Type,
      Free_Data      => Do_Nothing,
      Null_Ptr       => No_Project,
      Case_Sensitive => False);
   use Project_Htable.String_Hash_Table;

   type Directory_Dependency is (Direct, As_Parent, None);
   --  The way a directory belongs to the project: either as a direct
   --  dependency, or because one of its subdirs belong to the project, or
   --  doesn't belong at all
`
   procedure Do_Nothing (Dep : in out Directory_Dependency);

   package Directory_Htable is new String_Hash
     (Data_Type      => Directory_Dependency,
      Free_Data      => Do_Nothing,
      Null_Ptr       => None,
      Case_Sensitive => Filenames_Are_Case_Sensitive);
   use Directory_Htable.String_Hash_Table;

   type Source_File_Data is record
      Project   : Project_Type;
      Lang      : Name_Id;
      Directory : Name_Id;
   end record;
   No_Source_File_Data : constant Source_File_Data :=
     (No_Project, No_Name, No_Name);
   --   In some case, Lang might be set to Unknown_Language, if the file was
   --   set in the project (for instance through the Source_Files attribute),
   --   but no matching language was found.

   procedure Do_Nothing (Data : in out Source_File_Data);

   package Source_Htable is new String_Hash
     (Data_Type      => Source_File_Data,
      Free_Data      => Do_Nothing,
      Null_Ptr       => No_Source_File_Data,
      Case_Sensitive => Filenames_Are_Case_Sensitive);
   use Source_Htable.String_Hash_Table;

   procedure Do_Nothing (Name : in out Name_Id);
   package Languages_Htable is new String_Hash
     (Data_Type => Name_Id,
      Free_Data => Do_Nothing,
      Null_Ptr  => No_Name);
   use Languages_Htable.String_Hash_Table;

   type Project_Registry_Data is record
      Root    : Project_Type := No_Project;
      --  The root of the project hierarchy

      Sources  : Source_Htable.String_Hash_Table.HTable;
      --  Index on base source file names, return the managing project

      Directories : Directory_Htable.String_Hash_Table.HTable;
      --  Index on directory name

      Projects : Project_Htable.String_Hash_Table.HTable;
      --  Index on project names. Some project of the hierarchy might not
      --  exist, since the Project_Type are created lazily the first time they
      --  are needed.

      Scenario_Variables : Scenario_Variable_Array_Access;
      --  Cached value of the scenario variables. This should be accessed only
      --  through the function Scenario_Variables, since it needs to be
      --  initialized first.

      Predefined_Object_Path : GNAT.OS_Lib.String_Access;
      --  Predefined object path for the runtime library

      Predefined_Source_Path : GNAT.OS_Lib.String_Access;
      --  Predefined source paths for the runtime library

      Predefined_Source_Files : VFS.File_Array_Access;
      --  The list of source files in Predefined_Source_Path.

      Extensions : Languages_Htable.String_Hash_Table.HTable;
      --  The extensions registered for each language.

      Trusted_Mode : Boolean := True;
      --  Whether we are in trusted mode when recomputing the project view:

      --  Implicit dependency on the global htables in the Prj.* packages.
   end record;

   procedure Add_Foreign_Source_Files
     (Registry : Project_Registry;
      Project  : Project_Type;
      Errors   : Put_Line_Access);
   --  Add to Project the list of source files for languages other than
   --  Ada. These sources are also cached in the registry.

   procedure Reset
     (Registry  : in out Project_Registry;
      View_Only : Boolean);
   --  Reset the contents of the project registry. This should be called only
   --  if a new project is loaded, otherwise no project is accessible to the
   --  application any more.
   --  If View_Only is true, then the projects are not destroyed, but all the
   --  fields related to the current view are reset.

   procedure Create_Environment_Variables (Registry : in out Project_Registry);
   --  Make sure that all the environment variables actually exist (possibly
   --  with their default value). Otherwise, GNAT will not be able to compute
   --  the project view.

   procedure Reset_Environment_Variables (Registry : Project_Registry);
   --  Find all the environment variables for the project, and cache the list
   --  in the registry.
   --  Does nothing if the cache is not empty.

   procedure Parse_Source_Files
     (Registry : in out Project_Registry;
      Errors   : Put_Line_Access);
   --  Find all the source files for the project, and cache them in the
   --  registry.
   --  At the same time, check that the gnatls attribute is coherent between
   --  all projects and subprojects, and memorize the sources in the
   --  hash-table.

   procedure Add_Runtime_Files
     (Registry : in out Project_Registry; Path : String);
   --  Add all runtime files to the caches

   function Normalize_Project_Path (Path : String) return String;
   --  Normalize the full path to a project (and make sure the project file
   --  extension is set)

   procedure Canonicalize_File_Names_In_Project (Registry : Project_Registry);
   --  Canonicalize the file and directory names in the project tree, as
   --  needed.

   procedure Canonicalize_File_Names_In_Project (P : Project_Type);
   --  Canonicalize the file and directory names for a single project

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Scenario_Variable_Array, Scenario_Variable_Array_Access);

   ----------------
   -- Do_Nothing --
   ----------------

   procedure Do_Nothing (Project : in out Project_Type) is
      pragma Unreferenced (Project);
   begin
      null;
   end Do_Nothing;

   procedure Do_Nothing (Data : in out Source_File_Data) is
      pragma Unreferenced (Data);
   begin
      null;
   end Do_Nothing;

   procedure Do_Nothing (Dep : in out Directory_Dependency) is
      pragma Unreferenced (Dep);
   begin
      null;
   end Do_Nothing;

   procedure Do_Nothing (Name : in out Name_Id) is
      pragma Unreferenced (Name);
   begin
      null;
   end Do_Nothing;

   ---------------------------
   -- Is_Valid_Project_Name --
   ---------------------------

   function Is_Valid_Project_Name (Name : String) return Boolean is
   begin
      if Name'Length = 0
        or else (Name (Name'First) not in 'a' .. 'z'
                 and then Name (Name'First) not in 'A' .. 'Z')
      then
         return False;
      end if;

      for N in Name'First + 1 .. Name'Last loop
         if Name (N) not in 'a' .. 'z'
           and then Name (N) not in 'A' .. 'Z'
           and then Name (N) not in '0' .. '9'
           and then Name (N) /= '_'
           and then Name (N) /= '.'
         then
            return False;
         end if;
      end loop;

      return True;
   end Is_Valid_Project_Name;

   ----------------------
   -- Set_Trusted_Mode --
   ----------------------

   procedure Set_Trusted_Mode
     (Registry        : Project_Registry'Class;
      Trusted_Mode    : Boolean) is
   begin
      Registry.Data.Trusted_Mode := Trusted_Mode;
   end Set_Trusted_Mode;

   ------------------------------------
   -- Reset_Scenario_Variables_Cache --
   ------------------------------------

   procedure Reset_Scenario_Variables_Cache (Registry : Project_Registry) is
   begin
      Unchecked_Free (Registry.Data.Scenario_Variables);
   end Reset_Scenario_Variables_Cache;

   ----------------------
   -- Reset_Name_Table --
   ----------------------

   procedure Reset_Name_Table
     (Registry           : Project_Registry;
      Project            : Project_Type;
      Old_Name, New_Name : String) is
   begin
      if Registry.Data /= null then
         Set (Registry.Data.Projects, Old_Name, No_Project);
         Set (Registry.Data.Projects, New_Name, Project);
      end if;
   end Reset_Name_Table;

   --------------------
   -- Unload_Project --
   --------------------

   procedure Unload_Project
     (Registry  : Project_Registry; View_Only : Boolean := False)
   is
      Project : Project_Type;
      Iter    : Project_Htable.String_Hash_Table.Iterator;
   begin
      if Registry.Data /= null then
         --  Free all projects

         Get_First (Registry.Data.Projects, Iter);
         loop
            Project := Get_Element (Iter);
            exit when Project = No_Project;

            if View_Only then
               Reset (Project);
            else
               Destroy (Project);
            end if;
            Get_Next (Registry.Data.Projects, Iter);
         end loop;

         if not View_Only then
            Reset (Registry.Data.Projects);
            Prj.Ext.Reset;
            Prj.Tree.Tree_Private_Part.Projects_Htable.Reset;
            Registry.Data.Root := No_Project;
         end if;

         Unchecked_Free (Registry.Data.Predefined_Source_Files);
         Reset (Registry.Data.Sources);
         Reset (Registry.Data.Directories);
         Reset_Scenario_Variables_Cache (Registry);
      end if;
   end Unload_Project;

   -----------
   -- Reset --
   -----------

   procedure Reset
     (Registry  : in out Project_Registry;
      View_Only : Boolean) is
   begin
      if Registry.Data /= null then
         Unload_Project (Registry, View_Only);
      else
         Registry.Data := new Project_Registry_Data;
         Reset (Registry.Data.Sources);
         Reset (Registry.Data.Directories);
      end if;
   end Reset;

   ------------------
   -- Load_Or_Find --
   ------------------

   function Load_Or_Find
     (Registry     : Project_Registry;
      Project_Path : String) return Project_Type
   is
      P : Project_Type;
      Node : Project_Node_Id;
      Path : constant String :=
        Base_Name (Project_Path, Project_File_Extension);
   begin
      Name_Len := Path'Length;
      Name_Buffer (1 .. Name_Len) := Path;
      P := Get_Project_From_Name (Registry, Name_Find);
      if P = No_Project then
         Prj.Part.Parse (Node, Normalize_Project_Path (Project_Path), True);
         P := Get_Project_From_Name (Registry, Prj.Tree.Name_Of (Node));
      end if;

      return P;
   end Load_Or_Find;

   ----------------------------
   -- Normalize_Project_Path --
   ----------------------------

   function Normalize_Project_Path (Path : String) return String is
      function Extension return String;
      --  Return the extension to add to the file name (.gpr if not already
      --  there)

      function Extension return String is
      begin
         if File_Extension (Path) /= Project_File_Extension then
            return Project_File_Extension;
         else
            return "";
         end if;
      end Extension;

   begin
      return Normalize_Pathname (Path, Resolve_Links => False) & Extension;
   end Normalize_Project_Path;

   ----------
   -- Load --
   ----------

   procedure Load
     (Registry           : in out Project_Registry;
      Root_Project_Path  : String;
      Errors             : Projects.Error_Report;
      New_Project_Loaded : out Boolean)
   is
      Project : Project_Node_Id;

      procedure Fail (S1, S2, S3 : String);
      --  Replaces Osint.Fail

      ----------
      -- Fail --
      ----------

      procedure Fail (S1, S2, S3 : String) is
      begin
         if Errors /= null then
            Errors (S1 & S2 & S3);
         end if;
      end Fail;

   begin
      if not Is_Regular_File (Root_Project_Path) then
         Trace (Me, "Load: " & Root_Project_Path & " is not a regular file");

         if Errors /= null then
            Errors (Root_Project_Path & (-" is not a regular file"));
         end if;

         New_Project_Loaded := False;
         return;
      end if;

      New_Project_Loaded := True;

      --  Use the full path name so that the messages are sent to the result
      --  view.
      Opt.Full_Path_Name_For_Brief_Errors := True;
      Output.Set_Special_Output (Output.Output_Proc (Errors));
      Reset (Registry, View_Only => False);

      Prj.Com.Fail := Fail'Unrestricted_Access;
      Prj.Part.Parse (Project, Root_Project_Path, True);
      Prj.Com.Fail := null;

      Opt.Full_Path_Name_For_Brief_Errors := False;

      if Project = Empty_Node then
         if Errors /= null then
            Errors (-"Couldn't parse the project " & Root_Project_Path
                    & ASCII.LF & (-"Using default project instead"));
         end if;
         Load_Default_Project (Registry, Get_Current_Dir);
         return;
      end if;

      Registry.Data.Root := Get_Project_From_Name
        (Registry, Prj.Tree.Name_Of (Project));
      Unchecked_Free (Registry.Data.Scenario_Variables);

      Set_Status (Registry.Data.Root, From_File);
      Output.Set_Special_Output (null);

      Trace (Me, "End of Load project");

   exception
      when E : others =>
         Trace (Exception_Handle, "Load: unexpected exception: "
                & Exception_Information (E));
         Output.Set_Special_Output (null);
         raise;
   end Load;

   -------------------------
   -- Load_Custom_Project --
   -------------------------

   procedure Load_Custom_Project
     (Registry  : Project_Registry;
      Project   : Project_Type) is
   begin
      Registry.Data.Root := Project;
      Set_Status (Registry.Data.Root, Default);
      Set_Project_Modified (Registry.Data.Root, False);
   end Load_Custom_Project;

   --------------------------
   -- Load_Default_Project --
   --------------------------

   procedure Load_Default_Project
     (Registry  : in out Project_Registry;
      Directory : String) is
   begin
      Reset (Registry, View_Only => False);
      Load_Custom_Project
        (Registry, Create_Default_Project (Registry, "default", Directory));
   end Load_Default_Project;

   --------------------
   -- Recompute_View --
   --------------------

   procedure Recompute_View
     (Registry : in out Project_Registry;
      Errors   : Projects.Error_Report)
   is
      Set_As_Incomplete_When_Errors : Boolean := True;
      Language : constant Name_Id :=
        Get_String (String (Languages_Attribute));

      procedure Report_Error (S : String; Project : Project_Id);
      --  Handler called when the project parser finds an error

      procedure Normalize_View (Project : Project_Type);
      --  Normalize the view of the project. In particular, make sure the
      --  languages attribute is lower case.
      --  ??? Should this be done directly by the GNAT parser ?

      procedure Report_Error (S : String; Project : Project_Id) is
         P    : Project_Type;
      begin
         if Project = Prj.No_Project then
            if Set_As_Incomplete_When_Errors then
               declare
                  Iter : Imported_Project_Iterator :=
                    Start (Registry.Data.Root);
               begin
                  while Current (Iter) /= No_Project loop
                     Set_View_Is_Complete (Current (Iter), False);
                     Next (Iter);
                  end loop;
               end;
            end if;

         else
            P := Get_Project_From_Name
              (Registry, Prj.Projects.Table (Project).Name);

            if Set_As_Incomplete_When_Errors then
               Set_View_Is_Complete (P, False);
            end if;
         end if;

         Trace (Me, "Recompute_View: error " & S);

         if Errors /= null then
            if Project = Prj.No_Project then
               Errors (S);

            elsif Status (Registry.Data.Root) = From_File
              or else Project_Modified (Registry.Data.Root)
            then
               Errors (Project_Name (P) & ": " & S);
            end if;
         end if;
      end Report_Error;

      --------------------
      -- Normalize_View --
      --------------------

      procedure Normalize_View (Project : Project_Type) is
         Var : constant Variable_Id :=
           Prj.Projects.Table (Get_View (Project)).Decl.Attributes;
         Value : constant Variable_Value := Value_Of (Language, Var);
         Lang : String_List_Id;
      begin
         if Value /= Nil_Variable_Value then
            Lang := Value.Values;
            while Lang /= Nil_String loop
               Get_Name_String (String_Elements.Table (Lang).Value);
               To_Lower (Name_Buffer (1 .. Name_Len));
               String_Elements.Table (Lang).Value := Name_Find;

               Lang := String_Elements.Table (Lang).Next;
            end loop;
         end if;
      end Normalize_View;

      View    : Project_Id;
      Success : Boolean;
      Iter    : Imported_Project_Iterator := Start (Registry.Data.Root);
   begin
      while Current (Iter) /= No_Project loop
         Set_View_Is_Complete (Current (Iter), True);
         Next (Iter);
      end loop;

      Reset (Registry, View_Only => True);

      Unchecked_Free (Registry.Data.Scenario_Variables);

      Create_Environment_Variables (Registry);

      Prj.Reset;
      Errout.Initialize;
      Prj.Proc.Process
        (View, Success, Registry.Data.Root.Node,
         Report_Error'Unrestricted_Access,
         Follow_Links => not Registry.Data.Trusted_Mode);

      --  Lower case the languages attribute

      Iter := Start (Registry.Data.Root);
      while Current (Iter) /= No_Project loop
         Normalize_View (Current (Iter));
         Next (Iter);
      end loop;

      --  Do not set the project as incomplete if no sources existed for a
      --  given language. Otherwise, a dialog is displayed when editing the
      --  project properties, and this might be confusing for the user.
      Set_As_Incomplete_When_Errors := False;

      Parse_Source_Files (Registry, Report_Error'Unrestricted_Access);
   end Recompute_View;

   ------------------------
   -- Parse_Source_Files --
   ------------------------

   procedure Parse_Source_Files
     (Registry : in out Project_Registry;
      Errors   : Put_Line_Access)
   is
      procedure Register_Directory (Directory : String);
      --  Register Directory as belonging to Project.
      --  The parent directories are also registered

      ------------------------
      -- Register_Directory --
      ------------------------

      procedure Register_Directory (Directory : String) is
         Last : Integer := Directory'Last - 1;
         Dir  : String := Directory;
      begin
         if not Filenames_Are_Case_Sensitive then
            To_Lower (Dir);
         end if;

         Set (Registry.Data.Directories, K => Dir, E => Direct);

         loop
            while Last >= Dir'First
              and then not OS_Utils.Is_Directory_Separator (Dir (Last))
            loop
               Last := Last - 1;
            end loop;

            Last := Last - 1;

            exit when Last <= Dir'First;

            if Get (Registry.Data.Directories,
                    Dir (Dir'First .. Last)) /= Direct
            then
               Set (Registry.Data.Directories,
                    K => Dir (Dir'First .. Last), E => As_Parent);
            end if;
         end loop;
      end Register_Directory;

      Iter : Imported_Project_Iterator := Start (Registry.Data.Root, True);
      Gnatls : constant String := Get_Attribute_Value
        (Registry.Data.Root, Gnatlist_Attribute);
      Sources : String_List_Id;
      P       : Project_Type;

   begin
      loop
         P := Current (Iter);
         exit when P = No_Project;

         declare
            Ls : constant String := Get_Attribute_Value
              (P, Gnatlist_Attribute);
         begin
            if Ls /= "" and then Ls /= Gnatls and then Errors /= null then
               Errors
                 (-("gnatls attribute is not the same in this project as in"
                    & " the root project. It will be ignored in the"
                    & " subproject."), Get_View (P));
            end if;
         end;

         --  Add the directories

         Sources := Prj.Projects.Table (Get_View (P)).Source_Dirs;
         while Sources /= Nil_String loop
            Register_Directory
              (Get_String (String_Elements.Table (Sources).Value));
            Sources := String_Elements.Table (Sources).Next;
         end loop;

         Register_Directory
           (Get_String (Prj.Projects.Table (Get_View (P)).Object_Directory));
         Register_Directory
           (Get_String (Prj.Projects.Table (Get_View (P)).Exec_Directory));

         --  Add the Ada sources that are already in the project.
         --  Convert the names to UTF8 for proper handling in GPS

         Sources := Prj.Projects.Table (Get_View (P)).Sources;
         while Sources /= Nil_String loop
            Get_Name_String (String_Elements.Table (Sources).Value);

            declare
               Current_Source : constant Name_Id :=
                                  String_Elements.Table (Sources).Value;
               --  ??? Should avoid function returning unconstrained array
               UTF8 : constant String := Locale_To_UTF8
                 (Name_Buffer (1 .. Name_Len));
               Directory : Name_Id := No_Name;
               Unit      : Unit_Project;
            begin
               Name_Len := UTF8'Length;
               Name_Buffer (1 .. Name_Len) := UTF8;
               String_Elements.Table (Sources).Value := Name_Find;

               Unit := Prj.Com.Files_Htable.Get (Current_Source);

               --  If we are in the fast-project loading mode, then no symbolic
               --  link is resolved, and files are seend through links. We get
               --  in the project explorer the directories as mentioned in the
               --  project file, and any link in these is properly displayed.
               --
               --  However, if not in this mode, symbolic links are resolved.
               --  This means that if we have sub6 in the project, which
               --  contains a link to sub4/d.adb, then the latter's directory,
               --  which is not part of the project, will prevent the file from
               --  being seen in the explorer.

               if Registry.Data.Trusted_Mode then
                  if Unit /= Prj.Com.No_Unit_Project then
                     for S in Prj.Com.Spec_Or_Body'Range loop
                        if Units.Table (Unit.Unit).File_Names (S).Name =
                          Current_Source
                        then
                           Directory := Units.Table
                             (Unit.Unit).File_Names (S).Display_Path;
                           exit;
                        end if;
                     end loop;
                  end if;
               end if;

               Set (Registry.Data.Sources,
                    K => UTF8,
                    E => (P, Name_Ada, Directory));
               Sources := String_Elements.Table (Sources).Next;
            end;
         end loop;

         --  Canonicalize the file names in the naming exception lists
         Canonicalize_File_Names_In_Project (Registry);

         --  Add the other languages' files

         Add_Foreign_Source_Files (Registry, P, Errors);

         Next (Iter);
      end loop;
   end Parse_Source_Files;

   -----------------------
   -- Add_Runtime_Files --
   -----------------------

   procedure Add_Runtime_Files
     (Registry : in out Project_Registry; Path : String)
   is
      Iter   : Path_Iterator := Start (Path);
      Dir    : Dir_Type;
      File   : String (1 .. 1024);
      Last   : Natural;
      Directory : Name_Id;
      Info   : Source_File_Data;
   begin
      while not At_End (Path, Iter) loop
         declare
            Curr : constant String :=
                     Name_As_Directory (Current (Path, Iter));
         begin
            if Curr /= "" then
               Open (Dir, Curr);

               loop
                  Read (Dir, File, Last);
                  exit when Last = 0;

                  if Last > 4
                    and then (File (Last - 3 .. Last) = ".ads"
                              or else File (Last - 3 .. Last) = ".adb")
                  then
                     --  Do not override runtime files that are in the
                     --  current project
                     Info := Get (Registry.Data.Sources, File (1 .. Last));
                     if Info.Directory = No_Name then
                        Name_Len  := Curr'Length;
                        Name_Buffer (1 .. Name_Len) := Curr;
                        Name_Buffer (Name_Len + 1 .. Name_Len + Last) :=
                          File (1 .. Last);
                        Name_Len := Name_Len + Last;

                        Directory := Name_Find;

                        Set (Registry.Data.Sources,
                             K => File (1 .. Last),
                             E => (No_Project, Name_Ada, Directory));
                     end if;
                  end if;
               end loop;

               Close (Dir);
            end if;

         exception
            when Directory_Error =>
               Trace (Me, "Unexpected exception while opening " & Curr);
         end;

         Iter := Next (Path, Iter);
      end loop;
   end Add_Runtime_Files;

   ----------------------------------------
   -- Canonicalize_File_Names_In_Project --
   ----------------------------------------

   procedure Canonicalize_File_Names_In_Project (P : Project_Type) is
      procedure Process_List (List : Array_Element_Id);
      --  Canonicalize all the files in the given list

      procedure Process_List (List : Array_Element_Id) is
         Arr : Array_Element_Id := List;
         Str : String_List_Id;

      begin
         while Arr /= No_Array_Element loop
            case Array_Elements.Table (Arr).Value.Kind is
               when Undefined | Single =>
                  null;  --  Unexpected case, but probably not an error
                  Trace (Me, "Canonicalize_File_Names, error in type");

               when Prj.List =>
                  Str := Array_Elements.Table (Arr).Value.Values;
                  while Str /= Nil_String loop
                     Get_Name_String (String_Elements.Table (Str).Value);
                     Canonical_Case_File_Name (Name_Buffer (1 .. Name_Len));
                     String_Elements.Table (Str).Value := Name_Find;
                     Str := String_Elements.Table (Str).Next;
                  end loop;
            end case;

            Arr := Array_Elements.Table (Arr).Next;
         end loop;
      end Process_List;

      Naming : constant Naming_Data :=
        Prj.Projects.Table (Get_View (P)).Naming;
   begin
      Process_List (Naming.Implementation_Exceptions);
      Process_List (Naming.Specification_Exceptions);
   end Canonicalize_File_Names_In_Project;

   ----------------------------------------
   -- Canonicalize_File_Names_In_Project --
   ----------------------------------------

   procedure Canonicalize_File_Names_In_Project
     (Registry : Project_Registry)
   is
      Iter : Imported_Project_Iterator;
      P    : Project_Type;
   begin
      if True or else not Filenames_Are_Case_Sensitive then
         Iter := Start (Registry.Data.Root);

         loop
            P := Current (Iter);
            exit when P = No_Project;

            Canonicalize_File_Names_In_Project (P);

            Next (Iter);
         end loop;
      end if;
   end Canonicalize_File_Names_In_Project;

   ----------------------------------
   -- Create_Environment_Variables --
   ----------------------------------

   procedure Create_Environment_Variables
     (Registry : in out Project_Registry) is
   begin
      Reset_Environment_Variables (Registry);

      for J in Registry.Data.Scenario_Variables'Range loop
         Ensure_External_Value (Registry.Data.Scenario_Variables (J));
      end loop;
   end Create_Environment_Variables;

   ---------------------------------
   -- Reset_Environment_Variables --
   ---------------------------------

   procedure Reset_Environment_Variables (Registry : Project_Registry) is
   begin
      if Registry.Data.Scenario_Variables = null then
         Trace (Me, "Reset_Environment_Variables");
         Registry.Data.Scenario_Variables := new Scenario_Variable_Array'
           (Find_Scenario_Variables
            (Registry.Data.Root, Parse_Imported => True));
      end if;
   end Reset_Environment_Variables;

   ------------------------------
   -- Add_Foreign_Source_Files --
   ------------------------------

   procedure Add_Foreign_Source_Files
     (Registry : Project_Registry;
      Project  : Project_Type;
      Errors   : Put_Line_Access)
   is
      Sources_Specified : Boolean := False;
      Specified_Sources_Count : Natural := 0;

      procedure Record_Source (Dir, File : String; Lang : Name_Id);
      --  Add file to the list of source files for Project.

      function File_In_Sources (File : String) return Boolean;
      --  Whether File belongs to the list of source files for this project

      procedure Process_Explicit_Source (File : String);
      --  Do the required processing for a source file that has been specified
      --  explicitly by the user, either through Source_Files or
      --  Source_List_File attributes

      -----------------------------
      -- Process_Explicit_Source --
      -----------------------------

      procedure Process_Explicit_Source (File : String) is
         F   : String := File;
         Src : Source_File_Data;
      begin
         Canonical_Case_File_Name (F);
         Specified_Sources_Count := Specified_Sources_Count + 1;

         Src := Get (Registry.Data.Sources, F);
         if Src.Lang = Name_Ada then
            --  No warning, this was already processed
            null;

         elsif Src.Lang = No_Name then
            Set (Registry.Data.Sources,
                 K => F,
                 E => (No_Project, Unknown_Language, No_Name));

         elsif Src.Project /= No_Project then
            Errors (-("Warning, duplicate source file ") & F
                    & " (already in "
                    & Project_Name (Src.Project) & ')',
                    Get_View (Project));
         else
            Errors (-("Warning, duplicate source file ")
                    & F, Get_View (Project));
         end if;
      end Process_Explicit_Source;

      -------------------
      -- Record_Source --
      -------------------

      procedure Record_Source (Dir, File : String; Lang : Name_Id) is
         Full_Path : constant Name_Id := Get_String (Dir & File);
      begin
         String_Elements.Increment_Last;
         String_Elements.Table (String_Elements.Last) :=
           (Value         => Full_Path,
            Display_Value => Get_String (File),
            Flag          => False,  --  Irrelevant for files
            Location      => No_Location,
            Index         => 0,
            Next          => Prj.Projects.Table (Get_View (Project)).Sources);
         Prj.Projects.Table (Get_View (Project)).Sources :=
           String_Elements.Last;

         Set (Registry.Data.Sources, K => File,
              E => (Project, Lang, Full_Path));
      end Record_Source;

      function File_In_Sources (File : String) return Boolean is
      begin
         return not Sources_Specified
           or else Get (Registry.Data.Sources, File).Lang /= No_Name;
      end File_In_Sources;

      Languages  : Argument_List := Get_Languages (Project);
      Languages2 : Name_Id_Array (Languages'Range);
      Languages3 : Name_Id_Array (Languages'Range);
      --  The list of languages. Languages2 is a precomputed version of
      --  Languages to speed up string comparison. Languages3's contents is
      --  reset to No_Name if at least one file exists for the given language.
      --  Thus we can easily issue warnings when a language has no file.

      Dirs      : String_Array_Access;
      Dir       : Dir_Type;
      Length    : Natural;
      Buffer    : String (1 .. 2048);
      Lang      : Name_Id;
      Has_File  : Boolean;
      Dirs_List : String_List_Id;

   begin
      --  We already know if the directories contain Ada files. Update the
      --  status accordingly, in case they don't contain any other file

      Dirs_List := Prj.Projects.Table (Get_View (Project)).Source_Dirs;
      while Dirs_List /= Nil_String loop
         Update_Directory_Cache
           (Project, Get_String (String_Elements.Table (Dirs_List).Value),
            String_Elements.Table (Dirs_List).Flag);
         Dirs_List := String_Elements.Table (Dirs_List).Next;
      end loop;

      --  Nothing to do if the only language is Ada, since this has already
      --  been taken care of

      if Languages'Length = 0
        or else (Languages'Length = 1
                 and then Languages (Languages'First).all = Ada_String)
      then
         Free (Languages);
         return;
      end if;

      --  Preprocess the Source_Files attribute
      --  ??? Should be shared with the project parser (Prj.Nmsc)

      declare
         Data : constant Project_Data :=
           Prj.Projects.Table (Get_View (Project));
         Sources : constant Variable_Value :=
           Prj.Util.Value_Of (Name_Source_Files, Data.Decl.Attributes);
         File : String_List_Id := Sources.Values;

      begin
         if not Sources.Default then
            Sources_Specified := True;
            while File /= Nil_String loop
               Get_Name_String (String_Elements.Table (File).Value);
               Process_Explicit_Source (Name_Buffer (1 .. Name_Len));
               File := String_Elements.Table (File).Next;
            end loop;
         end if;
      end;

      --  Preprocess the Source_List_File attribute
      --  ??? Should also be shared with the project parser (Prj.Nmsc)

      declare
         Data : constant Project_Data :=
           Prj.Projects.Table (Get_View (Project));
         Source_List_File : constant Variable_Value :=
           Prj.Util.Value_Of (Name_Source_List_File, Data.Decl.Attributes);
         File : Prj.Util.Text_File;
         Line : String (1 .. 2000);
         Last : Natural;

      begin
         if not Source_List_File.Default then
            Sources_Specified := True;
            declare
               F : constant String :=
                 Name_As_Directory (Get_String (Data.Directory))
                 & Get_String (Source_List_File.Value);
            begin
               if Is_Regular_File (F) then
                  Open (File, F);

                  while not Prj.Util.End_Of_File (File) loop
                     Prj.Util.Get_Line (File, Line, Last);

                     if Last /= 0
                       and then (Last = 1 or else Line (1 .. 2) /= "--")
                     then
                        Process_Explicit_Source (Line (1 .. Last));
                     end if;
                  end loop;

                  Close (File);
               end if;
            end;
         end if;
      end;

      --  ??? Should check locally removed file, but this should really be done
      --  by sharing code with Prj-Nmsc.
      --  Given the implementation in prj-nmsc, does this attribute apply for
      --  languages other than Ada ?

      --  Parse all directories to find the files that match the naming
      --  scheme.

      for L in Languages'Range loop
         Languages2 (L) := Get_String (Languages (L).all);
      end loop;
      Languages3 := Languages2;

      Dirs := Source_Dirs (Project, False);

      for D in Dirs'Range loop
         Open (Dir, Dirs (D).all);
         Has_File := False;

         loop
            Read (Dir, Buffer, Length);
            exit when Length = 0;

            --  Need to normalize the casing, before we call
            --  File_In_Sources. Unfortunately,
            --  Get_Unit_Part_And_Name_From_Filename will do the same again,
            --  which is slightly inefficient

            Canonical_Case_File_Name (Buffer (1 .. Length));

            --  Convert the file to UTF8

            declare
               UTF8 : constant String := Locale_To_UTF8 (Buffer (1 .. Length));
               Part : Unit_Part;
               Unit_Name : Name_Id;
            begin
               --  Check if the file is in the list of sources for this,
               --  project, as specified in the project file.

               if File_In_Sources (UTF8) then
                  --  First check naming scheme in the project, in case the
                  --  naming scheme overrides GPS's default
                  Get_Unit_Part_And_Name_From_Filename
                    (UTF8, Project, Part, Unit_Name, Lang);

                  if Lang = No_Name then
                     declare
                        Ext : constant String := File_Extension (UTF8);
                     begin
                        if Ext = "" then
                           --  No extension, use Base_Name to find the
                           --  proper language for this file. This is needed
                           --  for makefile and ChangeLog files for example.
                           Lang := Languages_Htable.String_Hash_Table.Get
                             (Registry.Data.Extensions, To_Lower (UTF8));
                        else
                           Lang := Languages_Htable.String_Hash_Table.Get
                             (Registry.Data.Extensions, Ext);
                        end if;
                     end;
                  end if;

                  --  Check if the returned language belongs to the supported
                  --  languages for the project

                  if Lang /= No_Name
                    and then Lang /= Name_Ada
                  then
                     for Index in Languages2'Range loop
                        if Languages2 (Index) = Lang then
                           Record_Source (Dirs (D).all, UTF8, Lang);
                           Has_File := True;
                           Languages3 (Index) := No_Name;
                           exit;
                        end if;
                     end loop;
                  end if;
               end if;
            end;
         end loop;

         if Has_File then
            Update_Directory_Cache (Project, Dirs (D).all, Has_File);
         end if;

         Close (Dir);
      end loop;

      --  Print error messages for remaining messages

      if not Sources_Specified
        or else Specified_Sources_Count /= 0
      then
         Length := 0;
         for L in Languages2'Range loop
            if Languages3 (L) /= No_Name
              and then Languages3 (L) /= Name_Ada
            then
               Length := Length + Languages (L)'Length + 2;
            end if;
         end loop;

         if Length /= 0 then
            declare
               Error : String (1 .. Length);
               Index : Natural := Error'First;
            begin
               for L in Languages2'Range loop
                  if Languages3 (L) /= No_Name
                    and then Languages3 (L) /= Name_Ada
                  then
                     Error (Index .. Index + Languages (L)'Length + 1) :=
                       Languages (L).all & ", ";
                     Index := Index + Languages (L).all'Length + 2;
                  end if;
               end loop;

               if Errors /= null then
                  Errors (-("Warning, no source files for ")
                          & Error (Error'First .. Error'Last - 2),
                          Get_View (Project));
               end if;
            end;
         end if;
      end if;

      Free (Languages);
      Free (Dirs);
   end Add_Foreign_Source_Files;

   ------------------------
   -- Scenario_Variables --
   ------------------------

   function Scenario_Variables (Registry : Project_Registry)
      return Projects.Scenario_Variable_Array is
   begin
      Reset_Environment_Variables (Registry);
      return Registry.Data.Scenario_Variables.all;
   end Scenario_Variables;

   ----------------------
   -- Get_Root_Project --
   ----------------------

   function Get_Root_Project (Registry : Project_Registry)
      return Projects.Project_Type is
   begin
      return Registry.Data.Root;
   end Get_Root_Project;

   ---------------------------
   -- Get_Project_From_Name --
   ---------------------------

   function Get_Project_From_Name
     (Registry : Project_Registry; Name : Types.Name_Id) return Project_Type
   is
      P : Project_Type;
      Node : Project_Node_Id;
   begin
      if Registry.Data = null then
         Trace (Me, "Get_Project_From_Name: Registry not initialized");
         return No_Project;

      else
         Get_Name_String (Name);
         P := Get (Registry.Data.Projects, Name_Buffer (1 .. Name_Len));

         if P = No_Project then
            Node := Prj.Tree.Tree_Private_Part.Projects_Htable.Get (Name).Node;

            if Node = Empty_Node then
               P := No_Project;
               Trace (Me, "Get_Project_From_Name: "
                      & Get_String (Name) & " wasn't found");

            else
               Create_From_Node (P, Registry, Node);
               Set (Registry.Data.Projects, Name_Buffer (1 .. Name_Len), P);
            end if;
         end if;

         return P;
      end if;
   end Get_Project_From_Name;

   ---------------------------
   -- Get_Project_From_File --
   ---------------------------

   function Get_Project_From_File
     (Registry          : Project_Registry;
      Source_Filename   : Virtual_File;
      Root_If_Not_Found : Boolean := True)
      return Project_Type is
   begin
      return Get_Project_From_File
        (Registry,
         Base_Name         => Base_Name (Source_Filename),
         Root_If_Not_Found => Root_If_Not_Found);
   end Get_Project_From_File;

   ---------------------------
   -- Get_Project_From_File --
   ---------------------------

   function Get_Project_From_File
     (Registry          : Project_Registry;
      Base_Name         : String;
      Root_If_Not_Found : Boolean := True)
      return Project_Type
   is
      P : constant Project_Type :=
        Get (Registry.Data.Sources, Base_Name).Project;
   begin
      if P = No_Project and then Root_If_Not_Found then
         return Registry.Data.Root;
      end if;
      return P;
   end Get_Project_From_File;

   ----------------------------
   -- Get_Language_From_File --
   ----------------------------

   function Get_Language_From_File
     (Registry : Project_Registry; Source_Filename : Virtual_File)
      return Types.Name_Id
   is
      Base_Name : constant String := VFS.Base_Name (Source_Filename);
      S         : constant Source_File_Data :=
        Get (Registry.Data.Sources, Base_Name);

   begin
      Trace (Me,  "MANU Get_Language_From_File " & Base_Name
               & " Lang=" & Get_String (S.Lang));
      if S = No_Source_File_Data then
         --  This is most probably one of the runtime files.
         --  For now, we simply consider the standard GNAT extensions, although
         --  we should search in the list of registered languages
         --  (language_handlers-glide)

         declare
            Ext : constant String := File_Extension (Base_Name);
         begin
            if Ext = ".ads" or else Ext = ".adb" then
               return Name_Ada;

            elsif Ext = "" then
               --  This is a file without extension like Makefile or
               --  ChangeLog for example. Use the filename to get the proper
               --  language for this file.

               return Languages_Htable.String_Hash_Table.Get
                 (Registry.Data.Extensions, To_Lower (Base_Name));

            else
               --  Try with the top-level project. This contains the default
               --  registered extensions when the languages were registered.
               --  At least, we might be able to display files that don't
               --  directly belong to a project with the appropriate
               --  highlighting.
               --  If there is no root project, this will use the default
               --  naming scheme

               return Languages_Htable.String_Hash_Table.Get
                 (Registry.Data.Extensions, File_Extension (Source_Filename));
            end if;
         end;
      else
         return S.Lang;
      end if;
   end Get_Language_From_File;

   -----------------------------------------
   -- Register_Default_Language_Extension --
   -----------------------------------------

   procedure Register_Default_Language_Extension
     (Registry            : Project_Registry;
      Language_Name       : String;
      Default_Spec_Suffix : String;
      Default_Body_Suffix : String) is
   begin
      Prj.Register_Default_Naming_Scheme
        (Language            => Get_String (To_Lower (Language_Name)),
         Default_Spec_Suffix => Get_String (Default_Spec_Suffix),
         Default_Body_Suffix => Get_String (Default_Body_Suffix));

      Add_Language_Extension (Registry, Language_Name, Default_Spec_Suffix);
      Add_Language_Extension (Registry, Language_Name, Default_Body_Suffix);
   end Register_Default_Language_Extension;

   ----------------------------
   -- Add_Language_Extension --
   ----------------------------

   procedure Add_Language_Extension
     (Registry      : Project_Registry;
      Language_Name : String;
      Extension     : String)
   is
      Lang : constant Name_Id := Get_String (To_Lower (Language_Name));
   begin
      Languages_Htable.String_Hash_Table.Set
        (Registry.Data.Extensions, Extension, Lang);
   end Add_Language_Extension;

   -------------------------------
   -- Get_Registered_Extensions --
   -------------------------------

   function Get_Registered_Extensions
     (Registry      : Project_Registry;
      Language_Name : String) return GNAT.OS_Lib.Argument_List
   is
      Lang : constant Name_Id := Get_String (To_Lower (Language_Name));
      Iter : Languages_Htable.String_Hash_Table.Iterator;
      Name : Name_Id;
      Count : Natural := 0;
   begin
      Get_First (Registry.Data.Extensions, Iter);
      loop
         Name := Get_Element (Iter);
         exit when Name = No_Name;

         if Name = Lang then
            Count := Count + 1;
         end if;

         Get_Next (Registry.Data.Extensions, Iter);
      end loop;

      declare
         Args : Argument_List (1 .. Count);
      begin
         Count := Args'First;
         Get_First (Registry.Data.Extensions, Iter);
         loop
            Name := Get_Element (Iter);
            exit when Name = No_Name;

            if Name = Lang then
               Args (Count) := new String'(Get_Key (Iter));
               Count := Count + 1;
            end if;

            Get_Next (Registry.Data.Extensions, Iter);
         end loop;

         return Args;
      end;
   end Get_Registered_Extensions;

   ----------------------
   -- Language_Matches --
   ----------------------

   function Language_Matches
     (Registry        : Project_Registry;
      Source_Filename : Virtual_File;
      Filter          : Projects.Name_Id_Array) return Boolean
   is
      Lang : Name_Id;
   begin
      if Filter'Length = 0 then
         return True;
      end if;

      Lang := Get_Language_From_File (Registry, Source_Filename);
      for L in Filter'Range loop
         if Filter (L) = Lang then
            return True;
         end if;
      end loop;

      return False;
   end Language_Matches;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Registry : in out Project_Registry) is
   begin
      Unload_Project (Registry);
   end Destroy;

   ------------------
   -- Pretty_Print --
   ------------------

   procedure Pretty_Print
     (Project                            : Project_Type;
      Increment                          : Positive      := 3;
      Eliminate_Empty_Case_Constructions : Boolean       := False;
      Minimize_Empty_Lines               : Boolean       := False;
      W_Char                             : Prj.PP.Write_Char_Ap := null;
      W_Eol                              : Prj.PP.Write_Eol_Ap  := null;
      W_Str                              : Prj.PP.Write_Str_Ap  := null) is
   begin
      Pretty_Print
        (Project.Node,
         Increment,
         Eliminate_Empty_Case_Constructions,
         Minimize_Empty_Lines,
         W_Char, W_Eol, W_Str,
         Backward_Compatibility => Project_Backward_Compatibility);
   end Pretty_Print;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Namet.Initialize;
      Csets.Initialize;
      Snames.Initialize;
      Prj.Initialize;
      Prj.Tree.Initialize;

      Name_C_Plus_Plus := Get_String (Cpp_String);
      Any_Attribute_Name := Get_String (Any_Attribute);
      Unknown_Language := Get_String ("unknown");
   end Initialize;

   --------------
   -- Finalize --
   --------------

   procedure Finalize is
   begin
      Prj.Reset;
      Prj.Ext.Reset;
      Prj.Tree.Tree_Private_Part.Projects_Htable.Reset;

      --  The following call appears as double-deallocation in gnatmem, but
      --  this is just because that package uses realloc() internally, which
      --  is not handled by gnatmem
      Prj.Tree.Tree_Private_Part.Project_Nodes.Free;

      Namet.Finalize;
      Stringt.Initialize;

      --  ??? Should this be done every time we parse an ali file ?
      ALI.ALIs.Free;
      ALI.Units.Free;
      ALI.Withs.Free;
      ALI.Args.Free;
      ALI.Linker_Options.Free;
      ALI.Sdep.Free;
      ALI.Xref.Free;
      Atree.Atree_Private_Part.Nodes.Free;
   end Finalize;

   --------------------------------
   -- Get_Predefined_Source_Path --
   --------------------------------

   function Get_Predefined_Source_Path
     (Registry : Project_Registry) return String is
   begin
      if Registry.Data.Predefined_Source_Path /= null then
         return Registry.Data.Predefined_Source_Path.all;
      else
         return "";
      end if;
   end Get_Predefined_Source_Path;

   --------------------------------
   -- Get_Predefined_Object_Path --
   --------------------------------

   function Get_Predefined_Object_Path
     (Registry : Project_Registry) return String is
   begin
      if Registry.Data.Predefined_Object_Path /= null then
         return Registry.Data.Predefined_Object_Path.all;
      else
         return "";
      end if;
   end Get_Predefined_Object_Path;

   ---------------------------------
   -- Get_Predefined_Source_Files --
   ---------------------------------

   function Get_Predefined_Source_Files
     (Registry : Project_Registry)
      return VFS.File_Array_Access is
   begin
      --  ??? A nicer way would be to implement this with a predefined project,
      --  and rely on the project parser to return the source
      --  files. Unfortunately, this doesn't work with the current
      --  implementation of this parser, since one cannot have two separate
      --  project hierarchies at the same time.

      if Registry.Data.Predefined_Source_Files = null then
         Registry.Data.Predefined_Source_Files := Read_Files_From_Dirs
           (Get_Predefined_Source_Path (Registry));
      end if;

      --  Make a copy of the result, so that we can keep a cache in the kernel

      return new File_Array'(Registry.Data.Predefined_Source_Files.all);
   end Get_Predefined_Source_Files;

   --------------------------------
   -- Set_Predefined_Source_Path --
   --------------------------------

   procedure Set_Predefined_Source_Path
     (Registry : in out Project_Registry; Path : String) is
   begin
      Free (Registry.Data.Predefined_Source_Path);
      Registry.Data.Predefined_Source_Path := new String'(Path);

      Unchecked_Free (Registry.Data.Predefined_Source_Files);

      Add_Runtime_Files (Registry, Path);
   end Set_Predefined_Source_Path;

   --------------------------------
   -- Set_Predefined_Object_Path --
   --------------------------------

   procedure Set_Predefined_Object_Path
     (Registry : in out Project_Registry; Path : String) is
   begin
      Free (Registry.Data.Predefined_Object_Path);
      Registry.Data.Predefined_Object_Path := new String'(Path);
   end Set_Predefined_Object_Path;

   -----------------------------
   -- Get_Full_Path_From_File --
   -----------------------------

   procedure Get_Full_Path_From_File
     (Registry        : Project_Registry;
      Filename        : Glib.UTF8_String;
      Use_Source_Path : Boolean;
      Use_Object_Path : Boolean;
      Project         : Project_Type := No_Project)
   is
      Locale   : constant String := Locale_From_UTF8 (Filename);
      Project2, Real_Project : Project_Type;
      Path     : GNAT.OS_Lib.String_Access;
      Iterator : Imported_Project_Iterator;
      Info     : Source_File_Data := No_Source_File_Data;

   begin
      if Is_Absolute_Path (Filename) then
         declare
            S : constant String := Locale_To_UTF8
              (Normalize_Pathname (Locale, Resolve_Links => False));
         begin
            Name_Len := S'Length;
            Name_Buffer (1 .. Name_Len) := S;
            return;
         end;

      else
         --  First check the cache

         if Use_Source_Path then
            Info := Get (Registry.Data.Sources, Filename);

            if Info.Directory = Name_A then
               --  Not found previously, we do not try again
               Name_Len := 0;
               return;

            elsif Info.Directory /= No_Name then
               Get_Name_String (Info.Directory);
               return;
            end if;
         end if;

         --  If we are editing a project file, check in the loaded tree first
         --  (in case an old copy is kept somewhere in the source or object
         --  path)

         if GNAT.Directory_Operations.File_Extension (Locale) =
           Project_File_Extension
           and then Get_Root_Project (Registry) /= No_Project
         then
            Iterator := Start (Get_Root_Project (Registry));

            loop
               Project2 := Current (Iterator);
               exit when Project2 = No_Project;

               if Project_Name (Project2) & Project_File_Extension =
                 Filename
               then
                  declare
                     S : constant String := Project_Path (Project2);
                  begin
                     Name_Len := S'Length;
                     Name_Buffer (1 .. Name_Len) := S;
                     return;
                  end;
               end if;

               Next (Iterator);
            end loop;
         end if;

         --  Otherwise we have a source file

         Real_Project :=
           Get_Project_From_File (Registry, Base_Name => Filename);

         if Project /= No_Project then
            Project2 := Project;
         else
            Project2 := Real_Project;
         end if;

         if Project2 /= No_Project then
            if Use_Source_Path then
               Path := Locate_Regular_File
                 (Locale, Include_Path (Project2, False));
            end if;

            if Path = null
              and then Use_Object_Path
            then
               Path := Locate_Regular_File
                 (Locale, Object_Path (Project2, False));
            end if;
         end if;

         if Get_Root_Project (Registry) /= No_Project then
            if Path = null and then Use_Source_Path then
               Path := Locate_Regular_File
                 (Locale,
                  Include_Path (Get_Root_Project (Registry), True)
                  & Path_Separator & Get_Predefined_Source_Path (Registry)
                  & Path_Separator & ".");
            end if;

            if Path = null and then Use_Object_Path then
               Path := Locate_Regular_File
                 (Locale,
                  Object_Path (Get_Root_Project (Registry), True)
                  & Path_Separator & Get_Predefined_Object_Path (Registry));
            end if;
         end if;

         if Path /= null then
            declare
               Full : constant String := Locale_To_UTF8
                 (Normalize_Pathname
                    (Path.all, Resolve_Links => False));
            begin
               Free (Path);
               Name_Len := Full'Length;
               Name_Buffer (1 .. Name_Len) := Full;

               --  If this is one of the source files, we cache the
               --  directory. This significantly speeds up the explorer. If
               --  this is not a source file, not need to cache it, it is a
               --  one-time request most probably.
               --  We do not cache anything if the project was forced, however
               --  since this wouldn't work with extended projects were sources
               --  can be duplicated.

               if Info /= No_Source_File_Data
                 and then Project2 = Real_Project
               then
                  Info.Directory := Name_Find;
                  Set (Registry.Data.Sources, Filename, Info);
               end if;
            end;

         elsif Project2 = Real_Project then
            --  Still update the cache, to avoid further system calls to
            --  Locate_Regular_File. However, we shouldn't update the cache
            --  if the project was forced, since it might store incorrect
            --  information when we are using extended projects (with duplicate
            --  source files)
            if Use_Source_Path then
               Info := (Project   => Project2,
                        Lang      => No_Name,
                        Directory => Name_A);
               Set (Registry.Data.Sources, Filename, Info);
            end if;

            Name_Len := 0;
         end if;
      end if;
   end Get_Full_Path_From_File;

   -----------------------------
   -- Get_Full_Path_From_File --
   -----------------------------

   function Get_Full_Path_From_File
     (Registry        : Project_Registry;
      Filename        : String;
      Use_Source_Path : Boolean;
      Use_Object_Path : Boolean;
      Project         : Project_Type := No_Project) return String is
   begin
      Get_Full_Path_From_File
        (Registry, Filename, Use_Source_Path, Use_Object_Path, Project);
      return Name_Buffer (1 .. Name_Len);
   end Get_Full_Path_From_File;

   ----------------------------------
   -- Directory_Belongs_To_Project --
   ----------------------------------

   function Directory_Belongs_To_Project
     (Registry  : Project_Registry;
      Directory : String;
      Direct_Only : Boolean := True) return Boolean
   is
      Belong : Directory_Dependency;
   begin
      if Filenames_Are_Case_Sensitive then
         Belong := Get (Registry.Data.Directories, Directory);
      else
         Belong := Get (Registry.Data.Directories, To_Lower (Directory));
      end if;

      return Belong = Direct
        or else (not Direct_Only and then Belong = As_Parent);
   end Directory_Belongs_To_Project;

   ------------
   -- Create --
   ------------

   function Create
     (Name            : Glib.UTF8_String;
      Registry        : Project_Registry'Class;
      Use_Source_Path : Boolean := True;
      Use_Object_Path : Boolean := True) return VFS.Virtual_File is
   begin
      if Is_Absolute_Path_Or_URL (Name) then
         return Create (Full_Filename => Name);
      else
         declare
            Full : constant String := Get_Full_Path_From_File
              (Registry        => Registry,
               Filename        => Name,
               Use_Source_Path => Use_Source_Path,
               Use_Object_Path => Use_Object_Path);

         begin
            if Full /= "" then
               return Create (Full_Filename => Full);
            end if;
         end;

         --  Else just open the relative paths. This is mostly intended
         --  for files opened from the command line.
         return Create
           (Full_Filename => Locale_To_UTF8
              (Normalize_Pathname (Locale_From_UTF8 (Name),
                                   Resolve_Links => False)));
      end if;
   end Create;

   ------------------------------
   -- Compute_Predefined_Paths --
   ------------------------------

   procedure Compute_Predefined_Paths
     (Registry     : in out Project_Registry;
      GNAT_Version : access GNAT.OS_Lib.String_Access;
      Gnatls_Path  : String;
      Gnatls_Args  : GNAT.OS_Lib.Argument_List_Access)
   is
      Current : GNAT.OS_Lib.String_Access := new String'("");

      procedure Add_Directory (S : String);
      --  Add S to the search path.
      --  If Source_Path is True, the source path is modified.
      --  Otherwise, the object path is modified.

      -------------------
      -- Add_Directory --
      -------------------

      procedure Add_Directory (S : String) is
         Tmp : GNAT.OS_Lib.String_Access;
      begin
         if S = ""
           or else S = "<Current_Directory>"
         then
            --  Do not include "." in the default source paths: when the user
            --  is compiling, it would represent the object directory, when the
            --  user is searching file it would represent whatever the current
            --  directory is at that point, ...
            return;

         else
            Tmp := Current;
            Current := new String'(Current.all & Path_Separator & S);
            Free (Tmp);
         end if;
      end Add_Directory;

      Fd     : TTY_Process_Descriptor;
      Result : Expect_Match;

   begin
      Non_Blocking_Spawn
        (Fd, Gnatls_Path,
         Gnatls_Args (2 .. Gnatls_Args'Last),
         Buffer_Size => 0, Err_To_Out => True);

      Expect (Fd, Result, "GNATLS .+ Copyright", Timeout => -1);

      declare
         S : constant String := Expect_Out_Match (Fd);
      begin
         GNAT_Version.all := new String'(S (S'First + 7 .. S'Last - 10));
      end;

      Expect (Fd, Result, "Source Search Path:", Timeout => -1);

      loop
         Expect (Fd, Result, "\n", Timeout => -1);

         declare
            S : constant String :=
              Trim (Strip_CR (Expect_Out (Fd)), Ada.Strings.Left);
         begin
            if S = "Object Search Path:" & ASCII.LF then
               Set_Predefined_Source_Path (Registry, Current.all);
               Free (Current);
               Current := new String'("");
            else
               Add_Directory (S (S'First .. S'Last - 1));
            end if;
         end;
      end loop;

   exception
      when Process_Died =>
         Set_Predefined_Object_Path (Registry, Current.all);
         Free (Current);
         Close (Fd);
   end Compute_Predefined_Paths;

end Projects.Registry;
