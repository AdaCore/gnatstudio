-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                  Copyright (C) 2002-2010, AdaCore                 --
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

with Ada.Characters.Handling;    use Ada.Characters.Handling;
with Ada.Strings;                use Ada.Strings;
with Ada.Strings.Hash;
with Ada.Text_IO;                use Ada, Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with GNAT.Case_Util;             use GNAT.Case_Util;
with GNAT.Directory_Operations;  use GNAT.Directory_Operations;
with GNAT.OS_Lib;                use GNAT.OS_Lib;
with GNATCOLL.Traces;            use GNATCOLL.Traces;
with GNATCOLL.Utils;             use GNATCOLL.Utils;
with GNATCOLL.VFS;               use GNATCOLL.VFS;
with GNATCOLL.VFS_Utils;         use GNATCOLL.VFS_Utils;

with Casing;                     use Casing;
with File_Utils;                 use File_Utils;
with Namet;                      use Namet;
with Osint;                      use Osint;
with OS_Utils;                   use OS_Utils;
with Prj.Env;                    use Prj, Prj.Env;
with Prj.Ext;
with Prj.PP;                     use Prj.PP;
with Prj.Tree;                   use Prj.Tree;
with Prj.Util;                   use Prj.Util;
with Projects.Graphs;            use Projects.Graphs;
with Projects.Editor;            use Projects.Editor;
with Projects.Registry;          use Projects.Registry;
with Remote;                     use Remote;
with Snames;                     use Snames;
with Traces;
with Types;                      use Types;

package body Projects is

   Me    : constant Trace_Handle := Create ("Projects");
   Debug : constant Trace_Handle := Create ("Projects.Debug", Default => Off);

   type Name_Id_Array_Access is access Name_Id_Array;

   type Project_Type_Data is record
      View : Prj.Project_Id;

      Modified : Boolean := False;
      --  True if the project has been modified by the user, and not saved
      --  yet.

      Paths_Type : Paths_Type_Information := From_Pref;
      --  True if the paths in the project file should be stored as relative
      --  paths.

      Normalized : Boolean := False;
      --  True if the project has been normalized

      Imported_Projects  : Name_Id_Array_Access;
      Importing_Projects : Name_Id_Array_Access;
      --  Sorted list of imported projects (Cache for
      --  Imported_Project_Iterator)

      Non_Recursive_Include_Path : GNATCOLL.VFS.File_Array_Access;
      --  The include path for this project

      Registry   : Project_Registry_Access;
      --  Needed so that we can return other projects like imported projects

      View_Is_Complete : Boolean := False;
      --  True if the view for the project was correctly computed

      Files : GNATCOLL.VFS.File_Array_Access;
      --  The list of source files for this project

      Uses_Variables : Boolean := False;
      --  If the project uses variables ("foo := .."), then it cannot be
      --  edited graphically, since GPS would break it.

      Status : Project_Status := From_File;
   end record;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Name_Id_Array, Name_Id_Array_Access);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Project_Registry'Class, Project_Registry_Access);

   function Get_View
     (Tree : Prj.Project_Tree_Ref; Name : Name_Id) return Prj.Project_Id;
   --  Return the project view for the project Name

   type External_Variable_Callback is access function
     (Variable : Project_Node_Id; Prj : Project_Node_Id) return Boolean;
   --  Called for a typed variable declaration that references an external
   --  variable in Prj.
   --  Stops iterating if this subprogram returns False.

   procedure For_Each_External_Variable_Declaration
     (Project   : Project_Type;
      Recursive : Boolean;
      Callback  : External_Variable_Callback);
   --  Iterate other all the typed variable declarations that reference
   --  external variables in Project (or one of its imported projects if
   --  Recursive is true).
   --  Callback is called for each of them.

   function Substitute_Dot
     (Unit_Name : String; Dot_Replacement : String) return String;
   --  Replace the '.' in unit_name with Dot_Replacement

   procedure Compute_Importing_Projects
     (Root_Project : Project_Type; Project : Project_Type);
   --  Compute the list of all projects that import, possibly indirectly,
   --  Project.

   function String_Elements
     (P : Project_Type) return Prj.String_Element_Table.Table_Ptr;
   function Array_Elements
     (P : Project_Type) return Prj.Array_Element_Table.Table_Ptr;
   function Packages
     (P : Project_Type) return Prj.Package_Table.Table_Ptr;
   pragma Inline (String_Elements);
   pragma Inline (Array_Elements);
   pragma Inline (Packages);
   --  Return access to the various tables that contain information about the
   --  project

   function Get_Filename_From_Unit
     (Project                  : Project_Type;
      Unit_Name                : String;
      Part                     : Unit_Part;
      Check_Predefined_Library : Boolean := False;
      File_Must_Exist          : Boolean := True;
      Language                 : Name_Id) return Filesystem_String;
   --  Internal version of Get_Filename_From_Unit

   ---------------------
   -- String_Elements --
   ---------------------

   function String_Elements
     (P : Project_Type) return Prj.String_Element_Table.Table_Ptr is
   begin
      return P.View_Tree.String_Elements.Table;
   end String_Elements;

   --------------------
   -- Array_Elements --
   --------------------

   function Array_Elements
     (P : Project_Type) return Prj.Array_Element_Table.Table_Ptr is
   begin
      return P.View_Tree.Array_Elements.Table;
   end Array_Elements;

   --------------
   -- Packages --
   --------------

   function Packages
     (P : Project_Type) return Prj.Package_Table.Table_Ptr is
   begin
      return P.View_Tree.Packages.Table;
   end Packages;

   ------------------
   -- Save_Project --
   ------------------

   function Save_Project
     (Project       : Project_Type;
      Path          : Virtual_File := GNATCOLL.VFS.No_File;
      Force         : Boolean := False;
      Report_Error  : Error_Report := null) return Boolean
   is
      File     : Text_IO.File_Type;
      Filename : Virtual_File;

      procedure Internal_Write_Char (C : Character);
      procedure Internal_Write_Str (S : String);
      procedure Internal_Write_Eol;

      -------------------------
      -- Internal_Write_Char --
      -------------------------

      procedure Internal_Write_Char (C : Character) is
      begin
         Put (File, C);
      end Internal_Write_Char;

      ------------------------
      -- Internal_Write_Str --
      ------------------------

      procedure Internal_Write_Str (S : String) is
      begin
         Put (File, S);
      end Internal_Write_Str;

      ------------------------
      -- Internal_Write_Eol --
      ------------------------

      procedure Internal_Write_Eol is
      begin
         Put (File, ASCII.LF);
      end Internal_Write_Eol;

   begin
      if not Is_Regular_File (Project_Path (Project))
        or else Project_Modified (Project)
        or else Force
      then
         if Is_Regular_File (Project_Path (Project))
           and then not Is_Writable (Project_Path (Project))
         then
            if Report_Error /= null then
               Report_Error
                 ("The file " & Display_Full_Name (Project_Path (Project))
                  & " is not writable. Project not saved");
            end if;
            Trace (Me, "Project file not writable: "
                   & Project_Path (Project).Display_Full_Name);
            return False;
         end if;

         if Path = GNATCOLL.VFS.No_File then
            Filename :=  Project_Path (Project);
         else
            Filename := Path;
         end if;

         declare
            Dirname  : Virtual_File renames Dir (Filename);
         begin
            Trace (Me, "Save_Project: Creating new file "
                   & Filename.Display_Full_Name);

            begin
               Make_Dir_Recursive (Dirname);
            exception
               when Directory_Error =>
                  Trace (Me, "Couldn't create directory " &
                         Dirname.Display_Full_Name);

                  if Report_Error /= null then
                     Report_Error
                       ("Couldn't create directory " &
                        Dirname.Display_Full_Name);
                  end if;

                  return False;
            end;

            Normalize_Cases (Project);

            Create (File, Mode => Out_File, Name => +Full_Name (Filename));
            Pretty_Print
              (Project => Project,
               W_Char  => Internal_Write_Char'Unrestricted_Access,
               W_Eol   => Internal_Write_Eol'Unrestricted_Access,
               W_Str   => Internal_Write_Str'Unrestricted_Access);
            Close (File);

            Set_Project_Modified (Project, False);
            Set_Status (Project, From_File);
            return True;

         exception
            when Ada.Text_IO.Name_Error =>
               Trace (Me, "Couldn't create " & Filename.Display_Full_Name);

               if Report_Error /= null then
                  Report_Error
                    ("Couldn't create file " & Filename.Display_Full_Name);
               end if;

               return False;
         end;
      end if;
      return False;
   end Save_Project;

   ------------------
   -- Project_Name --
   ------------------

   function Project_Name (Project : Project_Type) return String is
   begin
      if Project = No_Project then
         return "default";

      elsif Get_View (Project) /= Prj.No_Project then
         return Get_String (Get_View (Project).Display_Name);

      else
         return Get_String
           (Prj.Tree.Name_Of (Project.Node, Project.Tree));
      end if;
   end Project_Name;

   ------------------
   -- Project_Name --
   ------------------

   function Project_Name (Project : Project_Type) return Name_Id is
   begin
      if Project = No_Project then
         return Name_Default;
      else
         return Prj.Tree.Name_Of (Project.Node, Project.Tree);
      end if;
   end Project_Name;

   -----------------------
   -- Project_Name_Hash --
   -----------------------

   function Project_Name_Hash
     (Project : Project_Type) return Ada.Containers.Hash_Type is
   begin
      return Strings.Hash (Project_Name (Project));
   end Project_Name_Hash;

   ------------------
   -- Project_Path --
   ------------------

   function Project_Path
     (Project : Project_Type;
      Host    : String := Local_Host) return GNATCOLL.VFS.Virtual_File
   is
      View : constant Prj.Project_Id := Get_View (Project);
   begin
      if Project.Node = Empty_Node then
         return GNATCOLL.VFS.No_File;

      elsif View = Prj.No_Project then
         --  View=Prj.No_Project case needed for the project wizard

         return To_Remote
           (Create (+Get_String (Path_Name_Of (Project.Node, Project.Tree))),
            Host);

      else
         return To_Remote
           (Create (+Get_String (View.Path.Display_Name)), Host);
      end if;
   end Project_Path;

   -----------------------
   -- Project_Directory --
   -----------------------

   function Project_Directory
     (Project : Project_Type;
      Host    : String := Local_Host) return GNATCOLL.VFS.Virtual_File is
   begin
      return Dir (Project_Path (Project, Host));
   end Project_Directory;

   -----------------
   -- Source_Dirs --
   -----------------

   function Source_Dirs (Project : Project_Type) return Name_Id_Array is
      View : constant Project_Id := Get_View (Project);
   begin
      if View = Prj.No_Project then
         return (1 .. 0 => No_Name);

      else
         declare
            Src     : String_List_Id := View.Source_Dirs;
            Count   : constant Natural := Length (Project.View_Tree, Src);
            Sources : Name_Id_Array (1 .. Count);
         begin
            for C in Sources'Range loop
               Sources (C) := String_Elements (Project)(Src).Display_Value;
               Src := String_Elements (Project)(Src).Next;
            end loop;
            return Sources;
         end;
      end if;
   end Source_Dirs;

   -----------------
   -- Source_Dirs --
   -----------------

   function Source_Dirs
     (Project   : Project_Type;
      Recursive : Boolean;
      Has_VCS   : Boolean := False) return GNATCOLL.VFS.File_Array_Access
   is
      Current_Dir : constant Filesystem_String := Get_Current_Dir;
      Iter        : Imported_Project_Iterator := Start (Project, Recursive);
      Count       : Natural := 0;
      P           : Project_Type;
      View        : Project_Id;
      Sources     : File_Array_Access;
      Result      : File_Array_Access;
      Src         : String_List_Id;
      Index       : Natural := 1;
      --  Index points to the first free element in Sources
   begin
      loop
         P := Current (Iter);
         exit when P = No_Project;

         View := Get_View (P);
         exit when View = Prj.No_Project;

         Count := Count + Length (Project.View_Tree, View.Source_Dirs);
         Next (Iter);
      end loop;

      Iter := Start (Project, Recursive);
      Sources := new File_Array (1 .. Count);

      loop
         P := Current (Iter);
         exit when P = No_Project;

         View := Get_View (P);
         exit when View = Prj.No_Project;

         if not Has_VCS
           or else Get_Attribute_Value (P, VCS_Kind_Attribute) /= ""
         then
            Src := View.Source_Dirs;

            while Src /= Nil_String loop
               Sources (Index) := Create
                 (Normalize_Pathname
                    (+Get_String (String_Elements (P) (Src).Display_Value),
                     Current_Dir,
                     Resolve_Links => False));
               Ensure_Directory (Sources (Index));
               Index := Index + 1;
               Src   := String_Elements (P) (Src).Next;
            end loop;
         end if;

         Next (Iter);
      end loop;

      if Index - 1 < Sources'Last then
         Result := new File_Array'(Sources (1 .. Index - 1));
         Unchecked_Free (Sources);
      else
         Result := Sources;
      end if;

      return Result;
   end Source_Dirs;

   ------------------
   -- Include_Path --
   ------------------

   function Include_Path
     (Project : Project_Type; Recursive : Boolean) return File_Array is
   begin
      if Get_View (Project) = Prj.No_Project then
         return (1 .. 0 => <>);
      end if;

      --  ??? The project parser doesn't cache the non-recursive version
      if not Recursive then
         if Project.Data.Non_Recursive_Include_Path = null then
            Project.Data.Non_Recursive_Include_Path := new File_Array'
              (From_Path
                 (+Prj.Env.Ada_Include_Path
                    (Get_View (Project), Project.View_Tree, Recursive)));
         end if;

         return Project.Data.Non_Recursive_Include_Path.all;
      end if;

      return From_Path
        (+Prj.Env.Ada_Include_Path
           (Get_View (Project), Project.View_Tree, Recursive));
   end Include_Path;

   -----------------
   -- Object_Path --
   -----------------

   function Object_Path
     (Project : Project_Type) return GNATCOLL.VFS.Virtual_File
   is
      Ret : constant File_Array := Object_Path (Project, False, False);
   begin
      if Ret'Length > 0 then
         return Ret (Ret'First);
      else
         return GNATCOLL.VFS.No_File;
      end if;
   end Object_Path;

   -----------------
   -- Object_Path --
   -----------------

   function Object_Path
     (Project             : Project_Type;
      Recursive           : Boolean;
      Including_Libraries : Boolean;
      Xrefs_Dirs          : Boolean := False) return File_Array
   is
      View : constant Project_Id := Get_View (Project);

      function Handle_Subdir
        (Id : Namet.Path_Name_Type) return Filesystem_String;
      --  for all directories defined in Path, detect if "From_Subdir" exists,
      --  and return it instead.

      -------------------
      -- Handle_Subdir --
      -------------------

      function Handle_Subdir
        (Id : Namet.Path_Name_Type) return Filesystem_String
      is
         Path : constant Filesystem_String :=
                  Name_As_Directory (+Get_String (Id));
         Reg  : constant Project_Registry :=
                  Project_Registry (Get_Registry (Project).all);

      begin
         if not Xrefs_Dirs
           or else Get_Xrefs_Subdir (Registry => Reg)'Length = 0
         then
            return Path;
         elsif View.Externally_Built then
            return Path;
         elsif Prj.Subdirs /= null then
            return Name_As_Directory
              (Path (Path'First .. Path'Last - Prj.Subdirs.all'Length - 1) &
               Get_Xrefs_Subdir (Registry => Reg));
         else
            return Name_As_Directory
              (Path & Get_Xrefs_Subdir (Registry => Reg));
         end if;
      end Handle_Subdir;

   begin
      if View = Prj.No_Project then
         return (1 .. 0 => <>);

      elsif Recursive then
         return From_Path
           (+Prj.Env.Ada_Objects_Path (View, Including_Libraries).all);

      elsif Including_Libraries
        and then View.Library
        and then View.Library_ALI_Dir /= No_Path_Information
      then
         if View.Object_Directory = No_Path_Information then
            return (1 => Create (Handle_Subdir (View.Library_ALI_Dir.Name)));
         else
            return
              (Create (Handle_Subdir (View.Object_Directory.Display_Name)),
               Create (Handle_Subdir (View.Library_ALI_Dir.Name)));
         end if;

      elsif View.Object_Directory /= No_Path_Information then
         return
           (1 => Create (Handle_Subdir (View.Object_Directory.Display_Name)));
      else
         return (1 .. 0 => <>);
      end if;
   end Object_Path;

   --------------------------
   -- Direct_Sources_Count --
   --------------------------

   function Direct_Sources_Count (Project : Project_Type) return Natural is
   begin
      --  ??? Should directly use the size of Source_Files, since this is now
      --  precomputed when the project is loaded
      if Get_View (Project) = Prj.No_Project then
         return 0;

      else
         return Project.Data.Files'Length;
      end if;
   end Direct_Sources_Count;

   ------------
   -- Create --
   ------------

   function Create
     (Base_Name       : Filesystem_String;
      Project         : Projects.Project_Type;
      Use_Source_Path : Boolean := True;
      Use_Object_Path : Boolean := True) return GNATCOLL.VFS.Virtual_File
   is
      File : Virtual_File;
   begin
      Get_Full_Path_From_File
        (Project_Registry (Get_Registry (Project).all), Base_Name,
         Use_Source_Path, Use_Object_Path, Project,
         File => File);
      return File;
   end Create;

   ---------------------
   -- Get_Source_File --
   ---------------------

   function Get_Source_File
     (Project : Project_Type; Index : Positive) return Virtual_File is
   begin
      if Index <= Project.Data.Files'Last then
         return Project.Data.Files (Index);
      else
         return GNATCOLL.VFS.No_File;
      end if;
   end Get_Source_File;

   ----------------------
   -- Get_Source_Files --
   ----------------------

   function Get_Source_Files
     (Project   : Project_Type;
      Recursive : Boolean) return GNATCOLL.VFS.File_Array_Access
   is
      Count   : Natural;
      Index   : Natural := 1;
      P       : Project_Type;
      Sources : File_Array_Access;
      Ret     : File_Array_Access;

   begin
      if not Recursive then
         if Project.Data.Files = null then
            return new File_Array (1 .. 0);
         else
            return new File_Array'(Project.Data.Files.all);
         end if;
      end if;

      declare
         Iter : Imported_Project_Iterator := Start (Project, Recursive);
      begin
         Count := 0;

         --  Count files

         loop
            P := Current (Iter);
            exit when P = No_Project;

            --  Files may be null in case of a parse error

            if P.Data.Files /= null then
               Count := Count + P.Data.Files'Length;
            end if;

            Next (Iter);
         end loop;

         Sources := new File_Array (1 .. Count);
         Iter    := Start (Project, Recursive);

         --  Now add files to the Sources array

         loop
            P := Current (Iter);
            exit when P = No_Project;

            if P.Data.Files /= null then
               for S in P.Data.Files'Range loop
                  Sources (Index) := P.Data.Files (S);
                  Index := Index + 1;
               end loop;
            end if;

            Next (Iter);
         end loop;

         Ret := new File_Array'(Sources (Sources'First .. Index - 1));
         Unchecked_Free (Sources);
         return Ret;
      end;
   end Get_Source_Files;

   ------------------------------------------
   -- Get_Unit_Part_And_Name_From_Filename --
   ------------------------------------------

   procedure Get_Unit_Part_And_Name_From_Filename
     (Filename  : Filesystem_String;
      Project   : Project_Type;
      Part      : out Unit_Part;
      Unit_Name : out Name_Id;
      Lang      : out Name_Id)
   is
      F      : String := +Filename;
      P      : Project_Type;
      S      : Source_Id;
   begin
      Lang      := No_Name;
      Part      := Unit_Separate;
      Unit_Name := No_Name;

      if Project /= No_Project then
         Get_Source_And_Lang_From_File
           (Project.Data.Registry.all,
            Base_Name => Filename,
            Project   => P,
            Source    => S,
            Lang      => Lang);

         if P = Project and then S /= No_Source then
            if S.Unit /= No_Unit_Index then
               Unit_Name := S.Unit.Name;
            else
               Unit_Name := Get_String (F);
            end if;

            case S.Kind is
               when Spec => Part := Unit_Spec;
               when Impl => Part := Unit_Body;
               when Sep  => Part := Unit_Separate;
            end case;
         else
            Unit_Name := Get_String (F);
         end if;

         return;
      end if;

      --  ??? Below, should really be computed when we are parsing predefined
      --  files

      Canonical_Case_File_Name (F);

      --  Special case for the default GNAT extensions, since whatever the user
      --  naming scheme, the runtime always has the same naming scheme

      if GNAT.Directory_Operations.File_Extension (F) = ".ads" then
         Part      := Unit_Spec;
         Unit_Name := Get_String (+Base_Name (Filename, ".ads"));
         Lang      := Name_Ada;

      elsif GNAT.Directory_Operations.File_Extension (F) = ".adb" then
         Part      := Unit_Spec;
         Unit_Name := Get_String (+Base_Name (Filename, ".ads"));
         Lang      := Name_Ada;

      else
         Unit_Name := Get_String (+Base_Name (Filename));
      end if;
   end Get_Unit_Part_And_Name_From_Filename;

   ---------------------------------
   -- Get_Unit_Part_From_Filename --
   ---------------------------------

   function Get_Unit_Part_From_Filename
     (Project : Project_Type; Filename : Virtual_File) return Unit_Part
   is
      Unit       : Unit_Part;
      Name, Lang : Name_Id;
   begin
      Get_Unit_Part_And_Name_From_Filename
        (Base_Name (Filename), Project, Unit, Name, Lang);
      return Unit;
   end Get_Unit_Part_From_Filename;

   ---------------------------------
   -- Get_Unit_Name_From_Filename --
   ---------------------------------

   function Get_Unit_Name_From_Filename
     (Project : Project_Type; Filename : Virtual_File) return String
   is
      Unit       : Unit_Part;
      Name, Lang : Name_Id;
   begin
      Get_Unit_Part_And_Name_From_Filename
        (Base_Name (Filename), Project, Unit, Name, Lang);
      return Get_String (Name);
   end Get_Unit_Name_From_Filename;

   --------------------
   -- Substitute_Dot --
   --------------------

   function Substitute_Dot
     (Unit_Name       : String;
      Dot_Replacement : String) return String
   is
      Dot_Count : Natural := 0;
   begin
      for U in Unit_Name'Range loop
         if Unit_Name (U) = '.' then
            Dot_Count := Dot_Count + 1;
         end if;
      end loop;

      declare
         Uname : String
           (1 .. Unit_Name'Length + Dot_Count * (Dot_Replacement'Length - 1));
         Index : Natural := Uname'First;
      begin
         for U in Unit_Name'Range loop
            if Unit_Name (U) = '.' then
               Uname (Index .. Index + Dot_Replacement'Length - 1) :=
                 Dot_Replacement;
               Index := Index + Dot_Replacement'Length;
            else
               Uname (Index) := Unit_Name (U);
               Index := Index + 1;
            end if;
         end loop;
         return Uname;
      end;
   end Substitute_Dot;

   ----------------------------
   -- Get_Filename_From_Unit --
   ----------------------------

   function Get_Filename_From_Unit
     (Project                  : Project_Type;
      Unit_Name                : String;
      Part                     : Unit_Part;
      Check_Predefined_Library : Boolean := False;
      File_Must_Exist          : Boolean := True;
      Language                 : String) return Filesystem_String
   is
   begin
      return Get_Filename_From_Unit
        (Project, Unit_Name, Part, Check_Predefined_Library,
         File_Must_Exist, Get_String (To_Lower (Language)));
   end Get_Filename_From_Unit;

   ----------------------------
   -- Get_Filename_From_Unit --
   ----------------------------

   function Get_Filename_From_Unit
     (Project                  : Project_Type;
      Unit_Name                : String;
      Part                     : Unit_Part;
      Check_Predefined_Library : Boolean := False;
      File_Must_Exist          : Boolean := True;
      Language                 : Name_Id) return Filesystem_String
   is
      function Has_Predefined_Prefix (S : String) return Boolean;
      --  Return True is S has a name that starts like a predefined unit
      --  (e.g. a.b, which should be replaced by a~b)

      ---------------------------
      -- Has_Predefined_Prefix --
      ---------------------------

      function Has_Predefined_Prefix (S : String) return Boolean is
         C : constant Character := S (S'First);
      begin
         return S (S'First + 1) = '-'
           and then (C = 'a' or else C = 'g' or else C = 'i' or else C = 's');
      end Has_Predefined_Prefix;

      Unit    : Name_Id;
      UIndex  : Unit_Index;
      Lang    : Language_Ptr;
   begin
      --  Standard GNAT naming scheme
      --  ??? This isn't language independent, what if other languages have
      --  similar requirements. Should use configuration files as gprbuild does

      if Project = No_Project
        or else (Check_Predefined_Library and then Language = Name_Ada)
      then
         case Part is
            when Unit_Body =>
               return +Substitute_Dot (Unit_Name, "-") & ".adb";
            when Unit_Spec =>
               return +Substitute_Dot (Unit_Name, "-") & ".ads";
            when others =>
               Assert (Me, False, "Unexpected Unit_Part");
               return "";
         end case;

      --  The project naming scheme
      else
         Name_Len := Unit_Name'Length;
         Name_Buffer (1 .. Name_Len) := Unit_Name;
         Unit := Name_Find;

         --  Take advantage of computation done by the project manager when we
         --  looked for source files

         UIndex := Units_Htable.Get (Project.View_Tree.Units_HT, Unit);
         if UIndex /= No_Unit_Index then
            case Part is
               when Unit_Body | Unit_Separate =>
                  if UIndex.File_Names (Impl) /= null then
                     return +Get_String (UIndex.File_Names (Impl).File);
                  end if;

               when Unit_Spec =>
                  if UIndex.File_Names (Spec) /= null then
                     return +Get_String (UIndex.File_Names (Spec).File);
                  end if;
            end case;
         end if;

         --  The unit does not exist yet. Perhaps we are creating a new file
         --  and trying to guess the correct file name

         if File_Must_Exist then
            return "";
         end if;

         --  We can only perform guesses if the language is a valid for the
         --  project.

         Lang := Get_Language_From_Name
           (Get_View (Project), Get_Name_String (Language));

         if Lang = null then
            return "";
         end if;

         declare
            Dot_Replacement : constant String := Get_String
              (Name_Id (Lang.Config.Naming_Data.Dot_Replacement));
            Uname           : String := Substitute_Dot
              (Unit_Name, Dot_Replacement);

         begin
            case Lang.Config.Naming_Data.Casing is
               when All_Lower_Case => To_Lower (Uname);
               when All_Upper_Case => To_Upper (Uname);
               when others => null;
            end case;

            --  Handle properly special naming such as a.b -> a~b

            if Language = Name_Ada
              and then Uname'Length > 2
              and then Has_Predefined_Prefix (Uname)
            then
               Uname (Uname'First + 1) := '~';
            end if;

            case Part is
               when Unit_Body =>
                  return +(Uname
                           & Get_Name_String
                             (Name_Id (Lang.Config.Naming_Data.Body_Suffix)));

               when Unit_Spec =>
                  return +(Uname
                           & Get_Name_String
                             (Name_Id (Lang.Config.Naming_Data.Spec_Suffix)));

               when others =>
                  return "";
            end case;
         end;
      end if;
   end Get_Filename_From_Unit;

   ------------------------
   -- Delete_File_Suffix --
   ------------------------

   function Delete_File_Suffix
     (Filename : Filesystem_String; Project : Project_Type) return Natural
   is
      View  : constant Project_Id := Get_View (Project);
      Lang  : Language_Ptr;
      Suffix : Name_Id;
   begin
      --  View will be null when called from the project wizard

      if View /= Prj.No_Project then
         Lang := View.Languages;
         while Lang /= null loop
            Suffix := Name_Id (Lang.Config.Naming_Data.Spec_Suffix);
            if Suffix /= No_Name
              and then Suffix_Matches (Filename, +Get_Name_String (Suffix))
            then
               return Filename'Last - Natural (Length_Of_Name (Suffix));
            end if;

            Suffix := Name_Id (Lang.Config.Naming_Data.Body_Suffix);
            if Suffix /= No_Name
              and then Suffix_Matches (Filename, +Get_Name_String (Suffix))
            then
               return Filename'Last - Natural (Length_Of_Name (Suffix));
            end if;

            Lang := Lang.Next;
         end loop;
      end if;

      --  Check the default naming scheme as well ? Otherwise, it might happen
      --  that a project has its own naming scheme, but still references files
      --  in the runtime with the default naming scheme.

      declare
         Ext : constant String :=
           GNAT.Directory_Operations.File_Extension (+Filename);
      begin
         if  Ext = ".ads" or else Ext = ".adb" then
            return Filename'Last - 4;
         end if;
      end;

      return Filename'Last;
   end Delete_File_Suffix;

   --------------------------
   -- Other_File_Base_Name --
   --------------------------

   function Other_File_Base_Name
     (Project         : Project_Type;
      Source_Filename : Virtual_File) return Filesystem_String
   is
      Unit, Part : Unit_Part;
      Name, Lang : Name_Id;
   begin
      Get_Unit_Part_And_Name_From_Filename
        (Base_Name (Source_Filename), Project, Unit, Name, Lang);

      case Unit is
         when Unit_Spec                 => Part := Unit_Body;
         when Unit_Body | Unit_Separate => Part := Unit_Spec;
      end case;

      Get_Name_String (Name);
      declare
         Unit : constant String := Name_Buffer (1 .. Name_Len);
         N    : constant Filesystem_String := Get_Filename_From_Unit
           (Project, Unit, Part, Language => Lang);
      begin
         if N'Length > 0 then
            return N;

         elsif Lang = Name_Ada then
            --  Default to the GNAT naming scheme (for runtime files)
            declare
               N2 : constant Filesystem_String := Get_Filename_From_Unit
                 (Project, Unit, Part, Check_Predefined_Library => True,
                  Language => Lang);
            begin
               if N2'Length > 0 then
                  return N2;
               end if;
            end;
         end if;

         return Base_Name (Source_Filename);
      end;
   end Other_File_Base_Name;

   -------------------------
   -- Get_Attribute_Value --
   -------------------------

   function Get_Attribute_Value
     (Project      : Project_Type;
      Attribute    : Attribute_Pkg;
      Default      : String := "";
      Index        : String := "";
      Use_Extended : Boolean := False) return String
   is
      View  : constant Project_Id := Get_View (Project);
      Value : Variable_Value;
      Lang  : Language_Ptr;
      Unit  : Unit_Index;
   begin
      if Project = No_Project or else View = Prj.No_Project then
         return Default;
      end if;

      --  Special case for the naming scheme, since we need to get access to
      --  the default registered values for foreign languages

      if Attribute = Spec_Suffix_Attribute
        or else Attribute = Specification_Suffix_Attribute
      then
         Lang := Get_Language_From_Name (View, Index);
         if Lang /= null then
            return Get_String (Lang.Config.Naming_Data.Spec_Suffix);
         else
            return "";
         end if;

      elsif Attribute = Impl_Suffix_Attribute
        or else Attribute = Implementation_Suffix_Attribute
      then
         Lang := Get_Language_From_Name (View, Index);
         if Lang /= null then
            return Get_String (Lang.Config.Naming_Data.Body_Suffix);
         else
            return "";
         end if;

      elsif Attribute = Separate_Suffix_Attribute then
         Lang := Get_Language_From_Name (View, "ada");
         if Lang /= null then
            return Get_String (Lang.Config.Naming_Data.Separate_Suffix);
         else
            return "";
         end if;

      elsif Attribute = Casing_Attribute then
         Lang := Get_Language_From_Name (View, "ada");
         if Lang /= null then
            return Prj.Image (Lang.Config.Naming_Data.Casing);
         else
            return "";
         end if;

      elsif Attribute = Dot_Replacement_Attribute then
         Lang := Get_Language_From_Name (View, "ada");
         if Lang /= null then
            return Get_String (Lang.Config.Naming_Data.Dot_Replacement);
         else
            return "";
         end if;

      elsif Attribute = Old_Implementation_Attribute
        or else Attribute = Implementation_Attribute
      then
         --  Index is a unit name
         Unit := Units_Htable.Get
           (Project.View_Tree.Units_HT, Get_String (Index));
         if Unit /= No_Unit_Index
           and then Unit.File_Names (Impl) /= null
         then
            return Get_String (Unit.File_Names (Impl).Display_File);
         else
            return "";
         end if;

      elsif Attribute = Old_Specification_Attribute
        or else Attribute = Specification_Attribute
      then
         --  Index is a unit name
         Unit := Units_Htable.Get
           (Project.View_Tree.Units_HT, Get_String (Index));
         if Unit /= No_Unit_Index
           and then Unit.File_Names (Spec) /= null
         then
            return Get_String (Unit.File_Names (Spec).Display_File);
         else
            return "";
         end if;

      else
         Value := Get_Attribute_Value
           (Project, Attribute, Index, Use_Extended);
      end if;

      case Value.Kind is
         when Undefined => return Default;
         when Single    => return Value_Of (Value, Default);
         when List      =>
            Trace (Me, "Attribute " & String (Attribute)
                   & " is not a single string");
            return Default;
      end case;
   end Get_Attribute_Value;

   -------------------------
   -- Get_Attribute_Value --
   -------------------------

   function Get_Attribute_Value
     (Project      : Project_Type;
      Attribute    : Attribute_Pkg;
      Use_Extended : Boolean := False) return Associative_Array
   is
      Sep            : constant Natural := Split_Package (Attribute);
      Pkg_Name       : constant String :=
                         String (Attribute (Attribute'First .. Sep - 1));
      Attribute_Name : constant String :=
                         String (Attribute (Sep + 1 .. Attribute'Last));
      Project_View   : constant Project_Id := Get_View (Project);
      Pkg            : Package_Id := No_Package;
      Arr            : Array_Id;
      Elem, Elem2    : Array_Element_Id;
      N              : Name_Id;
      Count          : Natural := 0;
   begin
      if Project_View = Prj.No_Project then
         return (1 .. 0 => (No_Name, Nil_Variable_Value));
      end if;

      if Pkg_Name /= "" then
         Pkg := Value_Of
           (Get_String (Pkg_Name),
            In_Packages => Project_View.Decl.Packages,
            In_Tree     => Project.View_Tree);

         if Pkg = No_Package then
            if Use_Extended
              and then Extended_Project (Project) /= No_Project
            then
               return Get_Attribute_Value
                 (Extended_Project (Project), Attribute, Use_Extended);
            else
               return (1 .. 0 => (No_Name, Nil_Variable_Value));
            end if;
         end if;

         Arr := Packages (Project)(Pkg).Decl.Arrays;

      else
         Arr := Project_View.Decl.Arrays;
      end if;

      N := Get_String (Attribute_Name);
      Elem := Value_Of (N,
                        In_Arrays => Arr,
                        In_Tree   => Project.View_Tree);
      if Elem = No_Array_Element
        and then Use_Extended
        and then Extended_Project (Project) /= No_Project
      then
         return Get_Attribute_Value
           (Extended_Project (Project), Attribute, Use_Extended);
      end if;

      Elem2 := Elem;
      while Elem2 /= No_Array_Element loop
         Count := Count + 1;
         Elem2 := Array_Elements (Project)(Elem2).Next;
      end loop;

      declare
         Result : Associative_Array (1 .. Count);
      begin
         Count := Result'First;

         while Elem /= No_Array_Element loop
            Result (Count) := (Index => Array_Elements (Project)(Elem).Index,
                               Value => Array_Elements (Project)(Elem).Value);
            Count := Count + 1;
            Elem := Array_Elements (Project)(Elem).Next;
         end loop;

         return Result;
      end;
   end Get_Attribute_Value;

   -------------------------
   -- Get_Attribute_Value --
   -------------------------

   function Get_Attribute_Value
     (Project      : Project_Type;
      Attribute    : Attribute_Pkg;
      Index        : String := "";
      Use_Extended : Boolean := False) return GNAT.OS_Lib.Argument_List
   is
      Value : constant Variable_Value := Get_Attribute_Value
        (Project, Attribute, Index, Use_Extended);
   begin
      return To_Argument_List (Project.View_Tree, Value);
   end Get_Attribute_Value;

   -------------------
   -- Get_Languages --
   -------------------

   function Get_Languages
     (Project : Project_Type; Recursive : Boolean := False)
      return GNAT.OS_Lib.Argument_List
   is
      Iter          : Imported_Project_Iterator := Start (Project, Recursive);
      Num_Languages : Natural := 0;
      Val           : Variable_Value;
      Value         : String_List_Id;
      P             : Project_Type;

      procedure Add_Language
        (Lang : in out Argument_List; Index : in out Natural; Str : String);
      --  Add a new language in the list, if not already there

      ------------------
      -- Add_Language --
      ------------------

      procedure Add_Language
        (Lang : in out Argument_List; Index : in out Natural; Str : String) is
      begin
         for L in Lang'First .. Index - 1 loop
            if Lang (L).all = Str then
               return;
            end if;
         end loop;

         Lang (Index) := new String'(Str);
         Index := Index + 1;
      end Add_Language;

   begin
      if Get_View (Project) = Prj.No_Project then
         return GNAT.OS_Lib.Argument_List'(1 .. 1 => new String'("ada"));
      end if;

      loop
         P := Current (Iter);
         exit when P = No_Project;

         Val := Get_Attribute_Value (P, Languages_Attribute);
         if Val.Kind /= Undefined then
            Num_Languages := Num_Languages + Length (P.View_Tree, Val.Values);
         end if;

         Next (Iter);
      end loop;

      Iter := Start (Project, Recursive);

      declare
         --  If no project defines the language attribute, then they have
         --  Ada as an implicit language. Save space for it.
         Lang  : Argument_List (1 .. Num_Languages + 1);
         Index : Natural := Lang'First;
      begin
         loop
            P := Current (Iter);
            exit when P = No_Project;

            if not Attribute_Is_Defined (P, Languages_Attribute) then
               Add_Language (Lang, Index, "ada");

            else
               Val := Get_Attribute_Value (P, Languages_Attribute);
               if Val.Kind /= Undefined then
                  Value := Val.Values;

                  while Value /= Nil_String loop
                     Add_Language
                       (Lang, Index,
                        Get_String (String_Elements (P)(Value).Value));
                     Value := String_Elements (P)(Value).Next;
                  end loop;
               end if;
            end if;

            Next (Iter);
         end loop;

         return Lang (Lang'First .. Index - 1);
      end;
   end Get_Languages;

   ------------------
   -- Has_Language --
   ------------------

   function Has_Language
     (Project : Project_Type; Language : String) return Boolean
   is
      Normalized_Lang : constant Name_Id := Get_String (To_Lower (Language));
      P               : constant Project_Id := Get_View (Project);
      Lang            : Language_Ptr;
   begin
      if P /= Prj.No_Project then
         Lang := P.Languages;
         while Lang /= null loop
            if Lang.Name = Normalized_Lang then
               return True;
            end if;

            Lang := Lang.Next;
         end loop;

      end if;
      return False;
   end Has_Language;

   ------------------
   -- Is_Main_File --
   ------------------

   function Is_Main_File
     (Project : Project_Type; File : String) return Boolean
   is
      Value : Argument_List := Get_Attribute_Value
        (Project, Attribute => Main_Attribute);
   begin
      if Is_Case_Sensitive (Get_Nickname (Build_Server)) then
         for V in Value'Range loop
            if Value (V).all = File then
               Free (Value);
               return True;
            end if;
         end loop;

      else
         declare
            F : constant String := To_Lower (File);
         begin
            for V in Value'Range loop
               if To_Lower (Value (V).all) = F then
                  Free (Value);
                  return True;
               end if;
            end loop;
         end;
      end if;

      Free (Value);
      return False;
   end Is_Main_File;

   ---------------------------
   -- Executables_Directory --
   ---------------------------

   function Executables_Directory
     (Project : Project_Type) return Virtual_File
   is
   begin
      if Project = No_Project or else Get_View (Project) = Prj.No_Project then
         return GNATCOLL.VFS.No_File;

      else
         declare
            Exec : constant Filesystem_String := +Get_String
              (Name_Id (Get_View (Project).Exec_Directory.Display_Name));

         begin
            if Exec'Length > 0 then
               return Create (Name_As_Directory (Exec));
            else
               return Object_Path (Project);
            end if;
         end;
      end if;
   end Executables_Directory;

   -----------
   -- Start --
   -----------

   function Start
     (Root_Project     : Project_Type;
      Recursive        : Boolean := True;
      Direct_Only      : Boolean := False;
      Include_Extended : Boolean := True) return Imported_Project_Iterator
   is
      Iter : Imported_Project_Iterator;
   begin
      Assert (Me, Root_Project.Data /= null,
              "Start: Uninitialized project passed as argument");

      if Root_Project.Data.Imported_Projects = null then
         Root_Project.Data.Imported_Projects := new Name_Id_Array'
           (Topological_Sort (Root_Project.Tree, Root_Project.Node));
         if Active (Debug) then
            Trace (Debug, "Start: compute deps for "
                   & Project_Name (Root_Project));
            Trace (Debug, "Start: " & Project_Name (Root_Project));
            for N in Root_Project.Data.Imported_Projects'Range loop
               Trace (Debug, "    => "
                      & Get_String (Root_Project.Data.Imported_Projects (N)));
            end loop;
         end if;
      end if;

      if Recursive then
         Iter := Imported_Project_Iterator'
           (Root             => Root_Project,
            Direct_Only      => Direct_Only,
            Importing        => False,
            Current_Cache    => No_Project,
            Include_Extended => Include_Extended,
            Current          => Root_Project.Data.Imported_Projects'Last + 1);
         Next (Iter);
         return Iter;
      else
         return Imported_Project_Iterator'
           (Root             => Root_Project,
            Direct_Only      => Direct_Only,
            Importing        => False,
            Current_Cache    => No_Project,
            Include_Extended => Include_Extended,
            Current          => Root_Project.Data.Imported_Projects'First);
      end if;
   end Start;

   ---------------------
   -- Project_Imports --
   ---------------------

   procedure Project_Imports
     (Parent           : Project_Type;
      Child            : Project_Type;
      Include_Extended : Boolean := False;
      Imports          : out Boolean;
      Is_Limited_With  : out Boolean)
   is
      With_Clause : Project_Node_Id;
      Extended    : Project_Node_Id;
   begin
      Assert (Me, Child /= No_Project, "Project_Imports: no child provided");

      if Parent = No_Project then
         Imports := True;
         Is_Limited_With := False;
         return;
      end if;

      With_Clause := First_With_Clause_Of (Parent.Node, Parent.Tree);
      while With_Clause /= Empty_Node loop
         if Project_Node_Of (With_Clause, Parent.Tree) = Child.Node then
            Imports         := True;
            Is_Limited_With :=
              Non_Limited_Project_Node_Of (With_Clause, Parent.Tree)
              = Empty_Node;
            return;
         end if;

         With_Clause := Next_With_Clause_Of (With_Clause, Parent.Tree);
      end loop;

      --  Handling for extending projects ?

      if Include_Extended then
         Extended := Extended_Project_Of
           (Project_Declaration_Of (Parent.Node, Parent.Tree),
            Parent.Tree);
         if Extended = Child.Node then
            Imports := True;
            Is_Limited_With := False;
            return;
         end if;
      end if;

      Imports := False;
      Is_Limited_With := False;
   end Project_Imports;

   --------------------------------
   -- Compute_Importing_Projects --
   --------------------------------

   procedure Compute_Importing_Projects
     (Root_Project : Project_Type;
      Project      : Project_Type)
   is
      type Boolean_Array is array (Positive range <>) of Boolean;
      Imported  : Name_Id_Array_Access renames
        Root_Project.Data.Imported_Projects;
      Current   : Project_Type;
      Start     : Project_Type;
      Include   : Boolean_Array (Imported'Range) := (others => False);
      Name      : Name_Id;
      Index     : Integer;
      Parent    : Project_Type;
      Decl, N   : Project_Node_Id;
      Importing : Name_Id_Array_Access;
      Imports, Is_Limited_With : Boolean;

      procedure Merge_Project (P : Project_Type);
      --  Merge the imported projects of P with the ones for Project

      -------------------
      -- Merge_Project --
      -------------------

      procedure Merge_Project (P : Project_Type) is
         Index2 : Integer := Imported'First;
      begin
         for J in P.Data.Importing_Projects'Range loop
            while Imported (Index2) /= P.Data.Importing_Projects (J) loop
               Index2 := Index2 + 1;
            end loop;

            Include (Index2) := True;
         end loop;
      end Merge_Project;

   begin
      if Project.Data.Importing_Projects /= null then
         return;
      end if;

      --  Process all extending and extended projects as a single one: they
      --  will all have the same list of importing projects.

      N := Project.Node;
      loop
         Decl := Project_Declaration_Of (N, Project.Tree);
         exit when Extending_Project_Of (Decl, Project.Tree) = Empty_Node;
         N := Extending_Project_Of (Decl, Project.Tree);
      end loop;

      Current := Get_Project_From_Name
        (Project.Data.Registry.all, Prj.Tree.Name_Of (N, Project.Tree));
      Start := Current;

      loop
         Index := Imported'Last;

         --  We first start by the lowest possible project, then go up to the
         --  root project. Note that no project that appears before Project can
         --  import it, so we can save some time.

         Name := Prj.Tree.Name_Of (Current.Node, Project.Tree);
         while Index >= Imported'First loop
            exit when Name = Imported (Index);
            Index := Index - 1;
         end loop;

         --  We must check that Index is different from 0 which is the case
         --  when name is not in the list of imported anymore. This can happen
         --  when playing with project dependencies and the dependency
         --  graph.

         if Index >= Imported'First then
            Include (Index) := True;
            Index := Index - 1;
         end if;

         while Index >= Imported'First loop
            Parent := Get_Project_From_Name
              (Current.Data.Registry.all, Imported (Index));

            --  Avoid processing a project twice
            --  ??? We still process twice the projects that do not
            --  import Project
            if not Include (Index) then
               Project_Imports
                 (Parent, Child => Current, Include_Extended => False,
                  Imports         => Imports,
                  Is_Limited_With => Is_Limited_With);

               if Imports then
                  Compute_Importing_Projects (Root_Project, Parent);
                  Merge_Project (Parent);
               end if;
            end if;

            Index := Index - 1;
         end loop;

         Current := Extended_Project (Current);
         exit when Current = No_Project;
      end loop;

      --  Done processing everything

      Index := 0;
      for Inc in Include'Range loop
         if Include (Inc) then
            Index := Index + 1;
         end if;
      end loop;

      Importing := new Name_Id_Array (1 .. Index);

      Index := Include'First;
      for Imp in Importing'Range loop
         while not Include (Index) loop
            Index := Index + 1;
         end loop;

         Importing (Imp) := Imported (Index);
         Index := Index + 1;
      end loop;

      loop
         if Start = Project then
            Start.Data.Importing_Projects := Importing;
         else
            Start.Data.Importing_Projects := new Name_Id_Array'(Importing.all);
         end if;

         Start := Extended_Project (Start);
         exit when Start = No_Project;
      end loop;

      --  The code below is used for debugging sessions

      if Active (Debug) then
         Trace (Debug, "Find_All_Projects_Importing: "
                & Get_String (Name));
         for J in Project.Data.Importing_Projects'Range loop
            Trace (Debug, Get_String (Project.Data.Importing_Projects (J)));
         end loop;
      end if;

   exception
      when E : others => Trace (Traces.Exception_Handle, E);
   end Compute_Importing_Projects;

   ---------------------------------
   -- Find_All_Projects_Importing --
   ---------------------------------

   function Find_All_Projects_Importing
     (Project      : Project_Type;
      Include_Self : Boolean := False;
      Direct_Only  : Boolean := False) return Imported_Project_Iterator
   is
      Root_Project : constant Project_Type := Get_Root_Project
        (Project_Registry (Get_Registry (Project).all));
      Iter         : Imported_Project_Iterator;
   begin
      if Project = No_Project then
         return Start (Root_Project, Recursive => True);
      end if;

      if Project.Data.Importing_Projects = null then
         if Root_Project.Data.Imported_Projects = null then
            Root_Project.Data.Imported_Projects := new Name_Id_Array'
              (Topological_Sort (Root_Project.Tree, Root_Project.Node));
            Trace (Me, "Start: recomputing dependencies for "
                   & Project_Name (Root_Project));
         end if;

         Compute_Importing_Projects (Root_Project, Project);
      end if;

      Iter := Imported_Project_Iterator'
        (Root             => Project,
         Direct_Only      => Direct_Only,
         Importing        => True,
         Current_Cache    => No_Project,
         Include_Extended => True,   --  ??? Should this be configurable
         Current          => Project.Data.Importing_Projects'Last + 1);

      --  The project iself is always at index 'Last
      if not Include_Self then
         Iter.Current := Iter.Current - 1;
      end if;

      Next (Iter);
      return Iter;
   end Find_All_Projects_Importing;

   -------------
   -- Current --
   -------------

   function Current
     (Iterator : Imported_Project_Iterator) return Project_Type
   is
      P : Project_Type;
   begin
      if Iterator.Current_Cache /= No_Project then
         return Iterator.Current_Cache;
      end if;

      if Iterator.Importing then
         if Iterator.Current >=
           Iterator.Root.Data.Importing_Projects'First
         then
            return Get_Project_From_Name
              (Iterator.Root.Data.Registry.all,
               Iterator.Root.Data.Importing_Projects (Iterator.Current));
         end if;

      elsif Iterator.Current >= Iterator.Root.Data.Imported_Projects'First then
         P := Get_Project_From_Name
           (Iterator.Root.Data.Registry.all,
            Iterator.Root.Data.Imported_Projects (Iterator.Current));
         Assert (Me, P /= No_Project,
                 "Current: project not found: "
                 & Get_String (Iterator.Root.Data.Imported_Projects
                               (Iterator.Current)));
         return P;
      end if;

      return No_Project;
   end Current;

   ---------------------
   -- Is_Limited_With --
   ---------------------

   function Is_Limited_With
     (Iterator : Imported_Project_Iterator) return Boolean
   is
      Imports, Is_Limited_With : Boolean;
   begin
      if Iterator.Importing then
         Project_Imports
           (Current (Iterator), Iterator.Root,
            Include_Extended => False,
            Imports          => Imports,
            Is_Limited_With  => Is_Limited_With);

      else
         Project_Imports
           (Iterator.Root, Current (Iterator),
            Include_Extended => False,
            Imports          => Imports,
            Is_Limited_With  => Is_Limited_With);
      end if;

      return Imports and Is_Limited_With;
   end Is_Limited_With;

   ----------
   -- Next --
   ----------

   procedure Next (Iterator : in out Imported_Project_Iterator) is
      Imports, Is_Limited_With : Boolean;
   begin
      Iterator.Current_Cache := No_Project;
      Iterator.Current := Iterator.Current - 1;

      if Iterator.Direct_Only then
         if Iterator.Importing then
            while Iterator.Current >=
              Iterator.Root.Data.Importing_Projects'First
            loop
               Project_Imports
                 (Current (Iterator), Iterator.Root, Iterator.Include_Extended,
                  Imports => Imports, Is_Limited_With => Is_Limited_With);
               exit when Imports;
               Iterator.Current := Iterator.Current - 1;
            end loop;

         else
            while Iterator.Current >=
              Iterator.Root.Data.Imported_Projects'First
            loop
               Project_Imports
                 (Iterator.Root, Current (Iterator), Iterator.Include_Extended,
                  Imports => Imports, Is_Limited_With => Is_Limited_With);
               exit when Imports;
               Iterator.Current := Iterator.Current - 1;
            end loop;
         end if;
      end if;
   end Next;

   -----------------------------
   -- Find_Scenario_Variables --
   -----------------------------

   function Find_Scenario_Variables
     (Project        : Project_Type;
      Parse_Imported : Boolean := True) return Scenario_Variable_Array
   is
      function Count_Vars return Natural;
      --  Return the number of scenario variables in In_Project, its packages

      procedure Register_Vars
        (List    : in out Scenario_Variable_Array;
         Current : in out Positive);
      --  Register all the scenario variables from In_Projects, its packages

      procedure Add_If_Not_In_List
        (Var     : Project_Node_Id;
         List    : in out Scenario_Variable_Array;
         Current : in out Positive);
      --  Add Var in the list of scenario if it is not already there (see the
      --  documentation for Find_Scenario_Variables for the exact rules used to
      --  detect aliases).

      function External_Default (Var : Project_Node_Id) return Name_Id;
      --  Return the default value for the variable. Var must be a variable
      --  declaration or a variable reference. This routine supports only
      --  single expressions (no composite values).

      ----------------
      -- Count_Vars --
      ----------------

      function Count_Vars return Natural is
         Count : Natural := 0;

         function Cb
           (Variable : Project_Node_Id; Prj : Project_Node_Id) return Boolean;
         --  Increment the total number of variables

         --------
         -- Cb --
         --------

         function Cb
           (Variable : Project_Node_Id; Prj : Project_Node_Id) return Boolean
         is
            pragma Unreferenced (Variable, Prj);
         begin
            Count := Count + 1;
            return True;
         end Cb;

      begin
         For_Each_External_Variable_Declaration
           (Project, Parse_Imported, Cb'Unrestricted_Access);
         return Count;
      end Count_Vars;

      ----------------------
      -- External_Default --
      ----------------------

      function External_Default (Var : Project_Node_Id) return Name_Id is
         Proj : Project_Type    := Project;
         Expr : Project_Node_Id := Expression_Of (Var, Project.Tree);
      begin
         Expr := First_Term   (Expr, Proj.Tree);
         Expr := Current_Term (Expr, Proj.Tree);

         if Kind_Of (Expr, Proj.Tree) = N_External_Value then
            Expr := External_Default_Of (Expr, Project.Tree);

            if Expr = Empty_Node then
               return No_Name;
            end if;

            if Kind_Of (Expr, Proj.Tree) /= N_Literal_String then
               Expr := First_Term (Expr, Proj.Tree);
               Assert (Me, Next_Term (Expr, Proj.Tree) = Empty_Node,
                       "Default value cannot be a concatenation");

               Expr := Current_Term (Expr, Proj.Tree);

               if Kind_Of (Expr, Proj.Tree) = N_Variable_Reference then
                  --  A variable reference, look for the corresponding string
                  --  literal.

                  declare
                     Var    : constant Name_Id :=
                                Prj.Tree.Name_Of (Expr, Proj.Tree);
                     In_Prj : constant Project_Node_Id :=
                                Project_Node_Of (Expr, Proj.Tree);
                     Decl   : Project_Node_Id;
                  begin
                     if In_Prj /= Empty_Node then
                        --  This variable is defined in another project, get
                        --  project reference.
                        Proj := Get_Project_From_Name
                          (Project_Registry (Get_Registry (Project).all),
                           Prj.Tree.Name_Of (In_Prj, Proj.Tree));
                     end if;

                     --  Look for Var declaration into the project

                     Decl := First_Declarative_Item_Of
                       (Project_Declaration_Of
                          (Proj.Node, Proj.Tree), Proj.Tree);

                     while Decl /= Empty_Node loop
                        Expr := Current_Item_Node (Decl, Proj.Tree);

                        if Prj.Tree.Name_Of (Expr, Proj.Tree) = Var then
                           Expr := Expression_Of (Expr, Proj.Tree);
                           Expr := First_Term (Expr, Proj.Tree);
                           --  Get expression and corresponding term

                           --  Check now that this is not a composite value

                           Assert
                             (Me,
                              Next_Term (Expr, Proj.Tree) = Empty_Node,
                              "Default value cannot be a concatenation");

                           --  Get the string literal

                           Expr := Current_Term (Expr, Proj.Tree);
                           exit;
                        end if;
                        Decl := Next_Declarative_Item (Decl, Proj.Tree);
                     end loop;
                  end;
               end if;

               if Kind_Of (Expr, Proj.Tree) /= N_Literal_String then
                  Trace (Me, "Default value can only be literal string");
                  Proj.Data.Uses_Variables := True; --  prevent edition
                  return No_Name;
               end if;
            end if;

            return String_Value_Of (Expr, Proj.Tree);

         else
            return No_Name;
         end if;
      end External_Default;

      ------------------------
      -- Add_If_Not_In_List --
      ------------------------

      procedure Add_If_Not_In_List
        (Var     : Project_Node_Id;
         List    : in out Scenario_Variable_Array;
         Current : in out Positive)
      is
         V : constant Name_Id := External_Reference_Of (Var, Project.Tree);
         N : constant String := Get_String (V);
      begin
         for Index in 1 .. Current - 1 loop
            if External_Reference_Of (List (Index)) = N then
               return;
            end if;
         end loop;

         Get_Name_String (V);

         List (Current) := Scenario_Variable'
           (Name        => Name_Find,
            Default     => External_Default (Var),
            String_Type => String_Type_Of (Var, Project.Tree));
         Current := Current + 1;
      end Add_If_Not_In_List;

      -------------------
      -- Register_Vars --
      -------------------

      procedure Register_Vars
        (List    : in out Scenario_Variable_Array;
         Current : in out Positive)
      is
         function Cb (Variable : Project_Node_Id; Prj : Project_Node_Id)
            return Boolean;
         --  Add the new variable in the list if needed

         --------
         -- Cb --
         --------

         function Cb (Variable : Project_Node_Id; Prj : Project_Node_Id)
            return Boolean
         is
            pragma Unreferenced (Prj);
         begin
            Add_If_Not_In_List (Variable, List, Current);
            return True;
         end Cb;

      begin
         For_Each_External_Variable_Declaration
           (Project, Parse_Imported, Cb'Unrestricted_Access);
      end Register_Vars;

      Count : constant Natural := Count_Vars;
      List  : Scenario_Variable_Array (1 .. Count);
      Curr  : Positive := List'First;
   begin
      Register_Vars (List, Curr);
      return List (1 .. Curr - 1);
   end Find_Scenario_Variables;

   ---------------------------
   -- External_Reference_Of --
   ---------------------------

   function External_Reference_Of (Var : Scenario_Variable) return String is
   begin
      return Get_String (Var.Name);
   end External_Reference_Of;

   ----------------------
   -- External_Default --
   ----------------------

   function External_Default (Var : Scenario_Variable) return String is
   begin
      return Get_String (Var.Default);
   end External_Default;

   ---------------------------
   -- Ensure_External_Value --
   ---------------------------

   procedure Ensure_External_Value
     (Var  : Scenario_Variable;
      Tree : Project_Node_Tree_Ref)
   is
      N : constant String := External_Reference_Of (Var);
   begin
      if Prj.Ext.Value_Of (Tree, Var.Name) = No_Name then
         if Var.Default /= No_Name then
            Prj.Ext.Add (Tree, N, External_Default (Var));
         else
            Get_Name_String
              (String_Value_Of
                 (First_Literal_String (Var.String_Type, Tree), Tree));
            Prj.Ext.Add (Tree, N, Name_Buffer (Name_Buffer'First .. Name_Len));
         end if;
      end if;
   end Ensure_External_Value;

   ----------------------
   -- Project_Modified --
   ----------------------

   function Project_Modified
     (Project   : Project_Type;
      Recursive : Boolean := False) return Boolean
   is
      Iter : Imported_Project_Iterator := Start (Project, Recursive);
      P    : Project_Type;
   begin
      loop
         P := Current (Iter);
         exit when P = No_Project;

         if P.Data.Modified then
            return True;
         end if;
         Next (Iter);
      end loop;

      return False;
   end Project_Modified;

   --------------------------
   -- Set_Project_Modified --
   --------------------------

   procedure Set_Project_Modified
     (Project : Project_Type; Modified : Boolean) is
   begin
      Project.Data.Modified := Modified;
   end Set_Project_Modified;

   --------------------
   -- Set_Paths_Type --
   --------------------

   procedure Set_Paths_Type
     (Project : Project_Type; Paths : Paths_Type_Information) is
   begin
      Project.Data.Paths_Type := Paths;
   end Set_Paths_Type;

   --------------------
   -- Get_Paths_Type --
   --------------------

   function Get_Paths_Type
     (Project : Project_Type) return Paths_Type_Information is
   begin
      return Project.Data.Paths_Type;
   end Get_Paths_Type;

   -----------
   -- Reset --
   -----------

   procedure Reset (Project : in out Project_Type) is
   begin
      Project.Data.View := Prj.No_Project;
      --  No need to reset Project.Data.Imported_Projects, since this doesn't
      --  change when the view changes.

      Unchecked_Free (Project.Data.Non_Recursive_Include_Path);
      Unchecked_Free (Project.Data.Files);
   end Reset;

   --------------
   -- Get_View --
   --------------

   function Get_View
     (Tree : Prj.Project_Tree_Ref; Name : Name_Id) return Prj.Project_Id
   is
      Proj : Project_List := Tree.Projects;
   begin
      while Proj /= null loop
         if Proj.Project.Name = Name
           and then Proj.Project.Qualifier /= Configuration
         then
            return Proj.Project;
         end if;
         Proj := Proj.Next;
      end loop;

      return Prj.No_Project;
   end Get_View;

   --------------
   -- Get_View --
   --------------

   function Get_View (Project : Project_Type) return Prj.Project_Id is
   begin
      if Project.Node = Empty_Node then
         return Prj.No_Project;

      elsif Project.Data.View = Prj.No_Project then
         Project.Data.View :=
           Get_View (Project.View_Tree,
                     Prj.Tree.Name_Of (Project.Node, Project.Tree));
      end if;

      return Project.Data.View;
   end Get_View;

   ----------------------
   -- Create_From_Node --
   ----------------------

   procedure Create_From_Node
     (Project   : out Project_Type;
      Registry  : Abstract_Registry'Class;
      Tree      : Prj.Tree.Project_Node_Tree_Ref;
      View_Tree : Prj.Project_Tree_Ref;
      Node      : Prj.Tree.Project_Node_Id) is
   begin
      Assert (Me, Registry in Project_Registry'Class,
              "Invalid type for Registry");
      Project.Node := Node;
      Project.Tree := Tree;

      if Project.Data = null then
         Project.Data := new Project_Type_Data;
      end if;

      Project.Data.Registry := new Project_Registry'Class'
        (Project_Registry'Class (Registry));
      Project.Data.View := Prj.No_Project;
      Project.View_Tree := View_Tree;

      Trace (Debug, "Create_From_Node: "  & Project_Name (Project));
   end Create_From_Node;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Project : in out Project_Type) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Project_Type_Data, Project_Type_Data_Access);
   begin
      Reset (Project);
      Unchecked_Free (Project.Data.Imported_Projects);
      Unchecked_Free (Project.Data.Importing_Projects);
      Unchecked_Free (Project.Data.Registry);
      Unchecked_Free (Project.Data);
      Project := No_Project;
   end Destroy;

   -----------------
   -- Is_Editable --
   -----------------

   function Is_Editable (Project : Project_Type) return Boolean is
   begin
      return not Project.Data.Uses_Variables;
   end Is_Editable;

   --------------------------------------------
   -- For_Each_External_Variable_Declaration --
   --------------------------------------------

   procedure For_Each_External_Variable_Declaration
     (Project   : Project_Type;
      Recursive : Boolean;
      Callback  : External_Variable_Callback)
   is
      Iterator : Imported_Project_Iterator := Start (Project, Recursive);
      P        : Project_Type;

      procedure Process_Prj (Prj : Project_Node_Id);
      --  Process all the declarations in a single project

      -----------------
      -- Process_Prj --
      -----------------

      procedure Process_Prj (Prj : Project_Node_Id) is
         Pkg : Project_Node_Id := Prj;
         Var : Project_Node_Id;
      begin
         --  For all the packages and the common section
         while Pkg /= Empty_Node loop
            Var := First_Variable_Of (Pkg, Project.Tree);

            while Var /= Empty_Node loop
               if Kind_Of (Var, Project.Tree) = N_Typed_Variable_Declaration
                 and then Is_External_Variable (Var, Project.Tree)
                 and then not Callback (Var, Prj)
               then
                  exit;

               elsif Kind_Of (Var, Project.Tree) = N_Variable_Declaration
                 or else
                   (Kind_Of (Var, Project.Tree) = N_Typed_Variable_Declaration
                    and then not Is_External_Variable (Var, Project.Tree))
               then
                  Trace (Me, "Uses variable in " & Project_Name (P));
                  if Active (Debug) then
                     Pretty_Print
                       (Var, Project.Tree, Backward_Compatibility => False);
                  end if;
                  P.Data.Uses_Variables := True;
               end if;

               Var := Next_Variable (Var, Project.Tree);
            end loop;

            if Pkg = Prj then
               Pkg := First_Package_Of (Prj, Project.Tree);
            else
               Pkg := Next_Package_In_Project (Pkg, Project.Tree);
            end if;
         end loop;
      end Process_Prj;

   begin
      loop
         P := Current (Iterator);
         exit when P = No_Project;

         P.Data.Uses_Variables := False;
         Process_Prj (P.Node);
         Next (Iterator);
      end loop;
   end For_Each_External_Variable_Declaration;

   -------------------
   -- Is_Normalized --
   -------------------

   function Is_Normalized (Project : Project_Type) return Boolean is
   begin
      return Project.Data.Normalized;
   end Is_Normalized;

   -----------------------
   -- Set_Is_Normalized --
   -----------------------

   procedure Set_Is_Normalized
     (Project : Project_Type; Normalized : Boolean) is
   begin
      Project.Data.Normalized := Normalized;
   end Set_Is_Normalized;

   ------------------
   -- Get_Registry --
   ------------------

   function Get_Registry (Project : Project_Type)
      return Abstract_Registry_Access is
   begin
      return Abstract_Registry_Access (Project.Data.Registry);
   end Get_Registry;

   ------------------
   -- Get_Switches --
   ------------------

   procedure Get_Switches
     (Project          : Project_Type;
      In_Pkg           : String;
      File             : GNATCOLL.VFS.Virtual_File;
      Language         : String;
      Value            : out Variable_Value;
      Is_Default_Value : out Boolean) is
   begin
      Value := Nil_Variable_Value;

      --  Do we have some file-specific switches ?
      if Project /= No_Project and then File /= GNATCOLL.VFS.No_File then
         Value := Get_Attribute_Value
           (Project        => Project,
            Attribute      =>
              Attribute_Pkg (In_Pkg & '#' & Get_String (Name_Switches)),
            Index          => +Base_Name (File));

         Is_Default_Value := Value = Nil_Variable_Value;
      end if;

      --  Search if the user has defined default switches for that tool
      if Project /= No_Project and then Value = Nil_Variable_Value then
         Value := Get_Attribute_Value
           (Project        => Project,
            Attribute      =>
              Attribute_Pkg
                (In_Pkg & '#' & Get_String (Name_Default_Switches)),
            Index          => Language);
         Is_Default_Value := True;
      end if;
   end Get_Switches;

   --------------------------
   -- Is_External_Variable --
   --------------------------

   function Is_External_Variable
     (Var  : Prj.Tree.Project_Node_Id;
      Tree : Project_Node_Tree_Ref) return Boolean is
   begin
      return Kind_Of
        (Current_Term (First_Term (Expression_Of (Var, Tree), Tree), Tree),
         Tree)
        = N_External_Value;
   end Is_External_Variable;

   ---------------------------
   -- External_Reference_Of --
   ---------------------------

   function External_Reference_Of
     (Var  : Prj.Tree.Project_Node_Id;
      Tree : Project_Node_Tree_Ref) return Namet.Name_Id
   is
      Expr : Project_Node_Id := Expression_Of (Var, Tree);
   begin
      Expr := First_Term   (Expr, Tree);
      Expr := Current_Term (Expr, Tree);

      if Kind_Of (Expr, Tree) = N_External_Value then
         Expr := External_Reference_Of (Expr, Tree);
         return String_Value_Of (Expr, Tree);
      else
         return No_Name;
      end if;
   end External_Reference_Of;

   ------------
   -- Status --
   ------------

   function Status (Project : Project_Type) return Project_Status is
   begin
      return Project.Data.Status;
   end Status;

   ----------------
   -- Set_Status --
   ----------------

   procedure Set_Status (Project : Project_Type; Status : Project_Status) is
   begin
      Project.Data.Status := Status;
   end Set_Status;

   --------------------
   -- Enum_Values_Of --
   --------------------

   function Enum_Values_Of
     (Var : Scenario_Variable; Registry : Abstract_Registry'Class)
      return String_List_Utils.String_List.List
   is
      Values : String_List_Utils.String_List.List;
      Tree   : constant Prj.Tree.Project_Node_Tree_Ref :=
                 Get_Tree (Project_Registry (Registry));
      Iter   : String_List_Iterator := Value_Of (Tree, Var);
   begin
      while not Done (Iter) loop
         --  We know this is a list of static strings
         Get_Name_String (Projects.Editor.Data (Tree, Iter));
         String_List_Utils.String_List.Append
           (Values, Name_Buffer (Name_Buffer'First .. Name_Len));
         Iter := Next (Tree, Iter);
      end loop;

      return Values;
   end Enum_Values_Of;

   --------------------
   -- Enum_Values_Of --
   --------------------

   function Enum_Values_Of
     (Var : Scenario_Variable; Reg : Abstract_Registry'Class)
      return GNAT.Strings.String_List
   is
      use String_List_Utils.String_List;

      List   : String_List_Utils.String_List.List :=
                 Enum_Values_Of (Var, Reg);
      Result : GNAT.Strings.String_List (1 .. Length (List));
      It     : String_List_Utils.String_List.List_Node := First (List);
      Index  : Integer := 1;
   begin
      while It /= String_List_Utils.String_List.Null_Node loop
         Result (Index) := new String'(Data (It));

         Index := Index + 1;
         It := Next (It);
      end loop;

      Free (List);

      return Result;
   end Enum_Values_Of;

   ----------------
   -- Get_String --
   ----------------

   function Get_String (Id : Namet.Name_Id) return String is
   begin
      if Id = No_Name then
         return "";
      end if;

      return Get_Name_String (Id);
   exception
      when E : others =>
         Trace (Traces.Exception_Handle, E);
         return "";
   end Get_String;

   function Get_String (Id : Namet.File_Name_Type) return String is
   begin
      if Id = Namet.No_File then
         return "";
      end if;

      return Get_Name_String (Id);
   exception
      when E : others =>
         Trace (Traces.Exception_Handle, E);
         return "";
   end Get_String;

   function Get_String (Id : Namet.Path_Name_Type) return String is
   begin
      if Id = Namet.No_Path then
         return "";
      end if;

      return Get_Name_String (Id);

   exception
      when E : others =>
         Trace (Traces.Exception_Handle, E);
         return "";
   end Get_String;

   function Get_String (Id : Namet.Unit_Name_Type) return String is
   begin
      if Id = Namet.No_Unit_Name then
         return "";
      end if;

      return Get_Name_String (Id);
   exception
      when E : others =>
         Trace (Traces.Exception_Handle, E);
         return "";
   end Get_String;

   ---------------------------
   -- Has_Imported_Projects --
   ---------------------------

   function Has_Imported_Projects (Project : Project_Type) return Boolean is
      Iter : constant Imported_Project_Iterator := Start
        (Project, Recursive => True, Direct_Only => True);
   begin
      return Current (Iter) /= No_Project;
   end Has_Imported_Projects;

   ----------
   -- Prj1 --
   ----------

   function "=" (Prj1, Prj2 : Project_Type) return Boolean is
   begin
      return Prj1.Node = Prj2.Node;
   end "=";

   -----------------
   -- Reset_Cache --
   -----------------

   procedure Reset_Cache (Project : Project_Type; Imported_By : Boolean) is
   begin
      Trace (Me, "Reseting cache for " & Project_Name (Project)
             & " Imported_by=" & Imported_By'Img);

      if Project = No_Project then
         return;
      end if;

      if Imported_By then
         Unchecked_Free (Project.Data.Importing_Projects);
      else
         Unchecked_Free (Project.Data.Imported_Projects);
      end if;
   end Reset_Cache;

   ----------------------
   -- Extended_Project --
   ----------------------

   function Extended_Project
     (Project : Project_Type) return Project_Type
   is
      Extended : constant Project_Node_Id := Extended_Project_Of
        (Project_Declaration_Of (Project.Node, Project.Tree),
         Project.Tree);
   begin
      if Extended = Empty_Node then
         return No_Project;
      else
         return Get_Project_From_Name
           (Project.Data.Registry.all,
            Prj.Tree.Name_Of (Extended, Project.Tree));
      end if;
   end Extended_Project;

   -----------------------
   -- Extending_Project --
   -----------------------

   function Extending_Project
     (Project : Project_Type; Recurse : Boolean := False) return Project_Type
   is
      Extend : Project_Node_Id;
   begin
      if Recurse then
         Extend := Project.Node;

         while Extending_Project_Of
           (Project_Declaration_Of (Extend, Project.Tree),
            Project.Tree) /= Empty_Node
         loop
            Extend := Extending_Project_Of
              (Project_Declaration_Of (Extend, Project.Tree),
               Project.Tree);
         end loop;

      else
         Extend := Extending_Project_Of
           (Project_Declaration_Of (Project.Node, Project.Tree),
           Project.Tree);
      end if;

      if Extend = Empty_Node then
         return No_Project;
      else
         return Get_Project_From_Name
           (Project.Data.Registry.all,
            Prj.Tree.Name_Of (Extend, Project.Tree));
      end if;
   end Extending_Project;

   --------------------------
   -- Set_View_Is_Complete --
   --------------------------

   procedure Set_View_Is_Complete
     (Project : Project_Type; Complete : Boolean) is
   begin
      Project.Data.View_Is_Complete := Complete;
   end Set_View_Is_Complete;

   ----------------------
   -- View_Is_Complete --
   ----------------------

   function View_Is_Complete (Project : Project_Type) return Boolean is
   begin
      return Status (Project) /= From_File
        or else Project.Data.View_Is_Complete;
   end View_Is_Complete;

   -------------------
   -- Split_Package --
   -------------------

   function Split_Package (Attribute : Attribute_Pkg) return Natural is
   begin
      for N in Attribute'Range loop
         if Attribute (N) = '#' then
            return N;
         end if;
      end loop;

      return Attribute'First - 1;
   end Split_Package;

   -----------
   -- Build --
   -----------

   function Build
     (Package_Name, Attribute_Name : String) return Attribute_Pkg is
   begin
      return Attribute_Pkg
        (To_Lower (Package_Name) & '#' & To_Lower (Attribute_Name));
   end Build;

   -------------------------
   -- Get_Executable_Name --
   -------------------------

   function Get_Executable_Name
     (Project : Project_Type;
      File    : Filesystem_String) return Filesystem_String
   is
      Base         : constant Filesystem_String := Base_Name (File);
      Default_Exec : constant Filesystem_String := Base
        (Base'First .. Delete_File_Suffix (Base, Project));

   begin
      if Project = No_Project then
         --  Simply remove the current extension, since we don't have any
         --  information on the file itself.
         return Default_Exec;

      else
         declare
            From_Project : constant String := Get_Attribute_Value
              (Project, Executable_Attribute,
               Index => String (Base), Default => "");
         begin
            if From_Project = "" then
               --  Check whether the file is a special naming scheme for an
               --  Ada unit. If this is the case, the name of the unit is the
               --  name of the executable

               declare
                  Base_Id : constant Name_Id := Get_String (+Base);
                  Args    : constant Associative_Array := Get_Attribute_Value
                    (Project, Attribute => Implementation_Attribute);
               begin
                  for A in Args'Range loop
                     if Args (A).Value.Value = Base_Id then
                        return +Get_String (Args (A).Index);
                     end if;
                  end loop;
               end;

               --  Else use the default
               return Default_Exec;
            end if;

            return +From_Project;
         end;
      end if;
   end Get_Executable_Name;

   --------------
   -- Get_Tree --
   --------------

   function Get_Tree (Project : Project_Type) return Project_Tree_Ref is
   begin
      return Project.View_Tree;
   end Get_Tree;

   ----------------------
   -- Set_Source_Files --
   ----------------------

   procedure Set_Source_Files
     (Project      : Project_Type;
      Source_Files : GNATCOLL.VFS.File_Array_Access) is
   begin
      if Project.Data.Files /= null then
         Unchecked_Free (Project.Data.Files);
      end if;

      Project.Data.Files := Source_Files;
   end Set_Source_Files;

   ------------------
   -- Create_Flags --
   ------------------

   function Create_Flags
     (On_Error        : Prj.Error_Handler;
      Require_Sources : Boolean := True) return Processing_Flags is
   begin
      if Require_Sources then
         return Create_Flags
           (Report_Error               => On_Error,
            When_No_Sources            => Warning,
            Require_Sources_Other_Lang => True,
            Compiler_Driver_Mandatory  => False,
            Allow_Duplicate_Basenames  => True,
            Allow_Invalid_External     => Warning);
      else
         return Create_Flags
           (Report_Error               => On_Error,
            When_No_Sources            => Silent,
            Require_Sources_Other_Lang => False,
            Compiler_Driver_Mandatory  => False,
            Allow_Duplicate_Basenames  => True,
            Allow_Invalid_External     => Silent);
      end if;
   end Create_Flags;

   ----------------------------
   -- Has_Multi_Unit_Sources --
   ----------------------------

   function Has_Multi_Unit_Sources (Project : Project_Type) return Boolean is
      View : constant Project_Id := Get_View (Project);
   begin
      if View /= Prj.No_Project then
         return View.Has_Multi_Unit_Sources;
      end if;
      return False;
   end Has_Multi_Unit_Sources;

end Projects;
