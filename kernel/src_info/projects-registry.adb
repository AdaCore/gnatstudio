-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2002-2003                       --
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
with ALI;
with Atree;
with Basic_Types;               use Basic_Types;
with Csets;
with Errout;
with File_Utils;                use File_Utils;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with Glide_Intl;                use Glide_Intl;
with Namet;                     use Namet;
with Opt;                       use Opt;
with Output;                    use Output;
with OS_Utils;                  use OS_Utils;
with Prj.Ext;                   use Prj.Ext;
with Prj.PP;                    use Prj.PP;
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

package body Projects.Registry is

   Me : constant Debug_Handle := Create ("Projects.Registry");

   Project_Backward_Compatibility : constant Boolean := True;
   --  Should be set to true if saved project should be compatible with GNAT
   --  3.15a1, False if they only need to be compatible with GNAT 3.16 >=
   --  20021024

   procedure Do_Nothing (Project : in out Project_Type);
   --  Do not free the project (in the hash tables), since it shared by several
   --  entries and several htables

   package Project_Htable is new String_Hash
     (Data_Type => Project_Type,
      Free_Data => Do_Nothing,
      Null_Ptr  => No_Project);
   use Project_Htable.String_Hash_Table;

   type Directory_Dependency is (Direct, As_Parent, None);
   --  The way a directory belongs to the project: either as a direct
   --  dependency, or because one of its subdirs belong to the project, or
   --  doesn't belong at all

   procedure Do_Nothing (Dep : in out Directory_Dependency);

   package Directory_Htable is new String_Hash
     (Data_Type => Directory_Dependency,
      Free_Data => Do_Nothing,
      Null_Ptr  => None);
   use Directory_Htable.String_Hash_Table;

   type Source_File_Data is record
      Project   : Project_Type;
      Lang      : Name_Id;
      Directory : Name_Id;
   end record;
   No_Source_File_Data : constant Source_File_Data :=
     (No_Project, No_Name, No_Name);

   procedure Do_Nothing (Data : in out Source_File_Data);

   package Source_Htable is new String_Hash
     (Data_Type => Source_File_Data,
      Free_Data => Do_Nothing,
      Null_Ptr  => No_Source_File_Data);
   use Source_Htable.String_Hash_Table;

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

      Predefined_Source_Files : Basic_Types.String_Array_Access;
      --  The list of source files in Predefined_Source_Path.

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

   function Normalize_Project_Path (Path : String) return String;
   --  Normalize the full path to a project (and make sure the project file
   --  extension is set)

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
         then
            return False;
         end if;
      end loop;

      return True;
   end Is_Valid_Project_Name;

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

   -----------
   -- Reset --
   -----------

   procedure Reset
     (Registry  : in out Project_Registry;
      View_Only : Boolean)
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

         Reset (Registry.Data.Sources);
         Reset (Registry.Data.Directories);
         Reset_Scenario_Variables_Cache (Registry);
      else
         Registry.Data := new Project_Registry_Data;
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
      Path : constant String := Normalize_Project_Path (Root_Project_Path);
      Project : Project_Node_Id;
   begin
      if not Is_Regular_File (Path) then
         Trace (Me, "Load: " & Path & " is not a regular file");
         if Errors /= null then
            Errors (Root_Project_Path & (-" is not a a regular file"));
         end if;
         New_Project_Loaded := False;
         return;
      end if;

      if Registry.Data /= null
        and then Project_Path (Get_Root_Project (Registry)) = Path
      then
         Trace (Me, "Load: " & Path & " already loaded");
         if Errors /= null then
            Errors (Root_Project_Path & (-" already loaded"));
         end if;
         New_Project_Loaded := False;
         return;
      end if;

      New_Project_Loaded := True;

      --  Use the full path name so that the messages are sent to the result
      --  view.
      Opt.Full_Path_Name_For_Brief_Errors := True;
      Output.Set_Special_Output (Output_Proc (Errors));
      Reset (Registry, View_Only => False);

      Prj.Part.Parse (Project, Path, True);

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

      Set_Is_Default (Registry.Data.Root, False);
      Output.Set_Special_Output (null);

      Trace (Me, "End of Load project");

   exception
      when E : others =>
         Trace (Me, "Load: unexpected exception: "
                & Exception_Information (E));
         Output.Set_Special_Output (null);
         raise;
   end Load;

   --------------------------
   -- Load_Default_Project --
   --------------------------

   procedure Load_Default_Project
     (Registry  : in out Project_Registry;
      Directory : String) is
   begin
      Reset (Registry, View_Only => False);
      Registry.Data.Root := Create_Default_Project
        (Registry, "default", Directory);
      Set_Is_Default (Registry.Data.Root, True);
      Set_Project_Modified (Registry.Data.Root, False);
   end Load_Default_Project;

   --------------------
   -- Recompute_View --
   --------------------

   procedure Recompute_View
     (Registry : in out Project_Registry;
      Errors   : Projects.Error_Report)
   is
      Set_As_Incomplete_When_Errors : Boolean := True;

      procedure Report_Error (S : String; Project : Project_Id);
      --  Handler called when the project parser finds an error

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

            elsif not Is_Default (Registry.Data.Root)
              or else Project_Modified (Registry.Data.Root)
            then
               Errors (Project_Name (P) & ": " & S);
            end if;
         end if;
      end Report_Error;

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
         Report_Error'Unrestricted_Access);

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
      begin
         Set (Registry.Data.Directories, K => Directory, E => Direct);

         loop
            while Last >= Directory'First
              and then not Is_Directory_Separator (Directory (Last))
            loop
               Last := Last - 1;
            end loop;

            Last := Last - 1;

            exit when Last <= Directory'First;

            if Get (Registry.Data.Directories,
                    Directory (Directory'First .. Last)) /= Direct
            then
               Set (Registry.Data.Directories,
                    K => Directory (Directory'First .. Last), E => As_Parent);
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

         Sources := Prj.Projects.Table (Get_View (P)).Sources;
         while Sources /= Nil_String loop
            Set (Registry.Data.Sources,
                 K => Get_String (String_Elements.Table (Sources).Value),
                 E => (P, Name_Ada, No_Name));
            Sources := String_Elements.Table (Sources).Next;
         end loop;

         --  Add the other languages' files

         Add_Foreign_Source_Files (Registry, P, Errors);

         Next (Iter);
      end loop;
   end Parse_Source_Files;

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
      procedure Record_Source (File : String; Lang : Name_Id);
      --  Add file to the list of source files for Project

      procedure Record_Source (File : String; Lang : Name_Id) is
      begin
         String_Elements.Increment_Last;
         String_Elements.Table (String_Elements.Last) :=
           (Value    => Get_String (File),
            Flag     => False,  --  Irrelevant for files
            Location => No_Location,
            Next     => Prj.Projects.Table (Get_View (Project)).Sources);
         Prj.Projects.Table (Get_View (Project)).Sources :=
           String_Elements.Last;

         Set (Registry.Data.Sources, K => File, E => (Project, Lang, No_Name));
      end Record_Source;

      Languages : Argument_List := Get_Languages (Project);
      Dirs      : String_Array_Access;
      Dir       : Dir_Type;
      Length    : Natural;
      Buffer    : String (1 .. 2048);
      Part      : Unit_Part;
      Unit, Lang : Name_Id;
      Has_File : Boolean;

   begin
      --  Nothing to do if the only language is Ada, since this has already
      --  been taken care of

      if Languages'Length = 0
        or else (Languages'Length = 1
                 and then Languages (Languages'First).all = Ada_String)
      then
         Free (Languages);

         --  Check which directories contain source files.

         declare
            Dirs : String_List_Id;
         begin
            Dirs := Prj.Projects.Table (Get_View (Project)).Source_Dirs;
            while Dirs /= Nil_String loop
               Update_Directory_Cache
                 (Project, Get_String (String_Elements.Table (Dirs).Value),
                  String_Elements.Table (Dirs).Flag);
               Dirs := String_Elements.Table (Dirs).Next;
            end loop;
         end;

         return;
      end if;

      --  Note: we do not have to check Source_File_List and Source_Files
      --  attributes, since they have already been processed by the Ada parser.

      --  ??? We are parsing Ada files twice for projects that have Ada and at
      --  least another language.

      Dirs := Source_Dirs (Project, False);

      for D in Dirs'Range loop
         Open (Dir, Dirs (D).all);
         Has_File := False;

         loop
            Read (Dir, Buffer, Length);
            exit when Length = 0;

            --  Have to use the naming scheme, since the hash-table hasn't been
            --  filled yet (Get_Language_From_File wouldn't work)

            Get_Unit_Part_And_Name_From_Filename
              (Filename  => Buffer (1 .. Length),
               Project   => Get_View (Project),
               Part      => Part,
               Unit_Name => Unit,
               Lang      => Lang);

            if Lang /= No_Name and then Lang /= Name_Ada then
               Get_Name_String (Lang);

               for L in Languages'Range loop
                  if Languages (L) /= null
                    and then Languages (L).all = Name_Buffer (1 .. Name_Len)
                  then
                     Free (Languages (L));
                     exit;
                  end if;
               end loop;

               Record_Source (Buffer (1 .. Length), Lang);
               Has_File := True;
            end if;
         end loop;

         Update_Directory_Cache (Project, Dirs (D).all, Has_File);

         Close (Dir);
      end loop;

      --  Print error messages for remaining messages

      Length := 0;
      for L in Languages'Range loop
         if Languages (L) /= null
           and then Languages (L).all /= Ada_String
         then
            Length := Length + Languages (L)'Length + 2;
         end if;
      end loop;

      if Length /= 0 then
         declare
            Error : String (1 .. Length);
            Index : Natural := Error'First;
         begin
            for L in Languages'Range loop
               if Languages (L) /= null
                 and then Languages (L).all /= Ada_String
               then
                  Error (Index .. Index + Languages (L)'Length + 1) :=
                    Languages (L).all & ", ";
                  Index := Index + Languages (L)'Length + 2;
               end if;
            end loop;

            if Errors /= null then
               Errors (-("Warning, no source files for ")
                       & Error (Error'First .. Error'Last - 2),
                       Get_View (Project));
            end if;
         end;
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
      Source_Filename   : String;
      Root_If_Not_Found : Boolean := True)
      return Project_Type
   is
      P : constant Project_Type :=
        Get (Registry.Data.Sources, Base_Name (Source_Filename)).Project;
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
     (Registry : Project_Registry; Source_Filename : String)
      return Types.Name_Id
   is
      S : constant Source_File_Data := Get
        (Registry.Data.Sources, Source_Filename);
      Part : Unit_Part;
      Unit, Lang : Name_Id;
   begin
      if S = No_Source_File_Data then
         --  This is most probably one of the runtime files.
         --  For now, we simply consider the standard GNAT extensions, although
         --  we should search in the list of registered languages
         --  (language_handlers-glide)

         declare
            Ext : constant String := File_Extension (Source_Filename);
         begin
            if Ext = ".ads" or else Ext = ".adb" then
               return Name_Ada;

            elsif Get_Root_Project (Registry) /= No_Project then
               --  Try with the top-level project. This contains the default
               --  registered extensions when the languages were registered.
               --  At least, we might be able to display files that don't
               --  directly belong to a project with the appropriate
               --  highlighting.

               Get_Unit_Part_And_Name_From_Filename
                 (Filename  => Source_Filename,
                  Project   => Get_View (Get_Root_Project (Registry)),
                  Part      => Part,
                  Unit_Name => Unit,
                  Lang      => Lang);
               return Lang;

            else
               return No_Name;
            end if;
         end;
      else
         return S.Lang;
      end if;
   end Get_Language_From_File;

   ----------------------
   -- Language_Matches --
   ----------------------

   function Language_Matches
     (Registry        : Project_Registry;
      Source_Filename : String;
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
      Reset (Registry, View_Only => False);
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
   end Initialize;

   --------------
   -- Finalize --
   --------------

   procedure Finalize is
   begin
      Prj.Reset;
      Prj.Ext.Reset;
      Prj.Tree.Tree_Private_Part.Projects_Htable.Reset;
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
      return Basic_Types.String_Array_Access
   is
      Result : String_Array_Access;
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

      Result := new String_Array (Registry.Data.Predefined_Source_Files'Range);

      for S in Registry.Data.Predefined_Source_Files'Range loop
         Result (S) := new String'
           (Registry.Data.Predefined_Source_Files (S).all);
      end loop;

      return Result;
   end Get_Predefined_Source_Files;

   --------------------------------
   -- Set_Predefined_Source_Path --
   --------------------------------

   procedure Set_Predefined_Source_Path
     (Registry : in out Project_Registry; Path : String) is
   begin
      Free (Registry.Data.Predefined_Source_Path);
      Registry.Data.Predefined_Source_Path := new String'(Path);

      Free (Registry.Data.Predefined_Source_Files);
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
      Filename        : String;
      Use_Source_Path : Boolean;
      Use_Object_Path : Boolean)
   is
      Project : Project_Type;
      Path : GNAT.OS_Lib.String_Access;
      Iterator : Imported_Project_Iterator;
      Info : Source_File_Data;

   begin
      if Is_Absolute_Path (Filename) then
         declare
            S : constant String := Normalize_Pathname (Filename);
         begin
            Name_Len := S'Length;
            Name_Buffer (1 .. Name_Len) := S;
            return;
         end;

      else
         --  First check the cache

         Info := Get (Registry.Data.Sources, Filename);

         if Info.Directory /= No_Name then
            Get_Name_String (Info.Directory);
            return;
         end if;

         --  If we are editing a project file, check in the loaded tree first
         --  (in case an old copy is kept somewhere in the source or object
         --  path)

         if GNAT.Directory_Operations.File_Extension (Filename) =
           Project_File_Extension
           and then Get_Root_Project (Registry) /= No_Project
         then
            Iterator := Start (Get_Root_Project (Registry));

            loop
               Project := Current (Iterator);
               exit when Project = No_Project;

               if Project_Name (Project) & Project_File_Extension =
                 Filename
               then
                  declare
                     S : constant String := Project_Path (Project);
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
         --  ??? Seems Prj.Nmsc is already computing and storing the path
         --  somewhere, unfortunately it requires a Unit_Name.

         Project := Get_Project_From_File (Registry, Filename);

         if Project /= No_Project then
            if Use_Source_Path then
               Path := Locate_Regular_File
                 (Filename, Include_Path (Project, False));
            end if;

            if Path = null
              and then Use_Object_Path
            then
               Path := Locate_Regular_File
                 (Filename, Object_Path (Project, False));
            end if;
         end if;

         if Get_Root_Project (Registry) /= No_Project then
            if Path = null and then Use_Source_Path then
               Path := Locate_Regular_File
                 (Filename,
                  Include_Path (Get_Root_Project (Registry), True)
                  & Path_Separator & Get_Predefined_Source_Path (Registry)
                  & Path_Separator & ".");
            end if;

            if Path = null and then Use_Object_Path then
               Path := Locate_Regular_File
                 (Filename,
                  Object_Path (Get_Root_Project (Registry), True)
                  & Path_Separator & Get_Predefined_Object_Path (Registry));
            end if;
         end if;

         if Path /= null then
            declare
               Full : constant String := Normalize_Pathname
                 (Path.all, Resolve_Links => False);
            begin
               Free (Path);
               Name_Len := Full'Length;
               Name_Buffer (1 .. Name_Len) := Full;

               --  If this is one of the source files, we cache the
               --  directory. This significantly speeds up the explorer. If
               --  this is not a source file, not need to cache it, it is a
               --  one-time request most probably.

               if Info /= No_Source_File_Data then
                  Info.Directory := Name_Find;

                  Set (Registry.Data.Sources, Filename, Info);
               end if;
            end;
         else
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
      Use_Object_Path : Boolean) return String is
   begin
      Get_Full_Path_From_File
        (Registry, Filename, Use_Source_Path, Use_Object_Path);
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
      Belong : constant Directory_Dependency :=
        Get (Registry.Data.Directories, Directory);
   begin
      return Belong = Direct
        or else (not Direct_Only and then Belong = As_Parent);
   end Directory_Belongs_To_Project;

end Projects.Registry;
