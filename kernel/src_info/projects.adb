-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                  Copyright (C) 2002-2007, AdaCore                 --
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
with Ada.Strings.Fixed;          use Ada.Strings.Fixed;
with Ada.Strings.Hash;
with Ada.Strings.Maps.Constants; use Ada.Strings.Maps.Constants;
with Ada.Text_IO;                use Ada, Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with GNAT.Directory_Operations;  use GNAT.Directory_Operations;
with GNAT.OS_Lib;                use GNAT.OS_Lib;
with GNAT.Traces;                use GNAT.Traces;

with Basic_Types;                use Basic_Types;
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
with Remote.Path.Translator;     use Remote, Remote.Path.Translator;
with Snames;                     use Snames;
with String_Hash;
with Traces;
with VFS;                        use VFS;

package body Projects is

   Me    : constant Trace_Handle := Create ("Projects");
   Debug : constant Trace_Handle := Create ("Projects.Debug", Default => Off);

   type Name_Id_Array_Access is access Name_Id_Array;

   type Directory_Info is record
      Has_Files : Boolean;
   end record;
   No_Directory_Info : constant Directory_Info := (Has_Files => False);

   procedure Do_Nothing (Dep : in out Directory_Info);

   package Directory_Htable is new String_Hash
     (Data_Type => Directory_Info,
      Free_Data => Do_Nothing,
      Null_Ptr  => No_Directory_Info);
   use Directory_Htable.String_Hash_Table;

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

      Directories : Directory_Htable.String_Hash_Table.HTable;
      --  Information for the various directories of the project

      Non_Recursive_Include_Path : GNAT.Strings.String_Access;
      --  The include path for this project

      Registry   : Project_Registry_Access;
      --  Needed so that we can return other projects like imported projects

      View_Is_Complete : Boolean := False;
      --  True if the view for the project was correctly computed

      Files : VFS.File_Array_Access;
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

   function Check_Full_File
     (Tree : Prj.Project_Tree_Ref;
      File : Name_Id;
      List : Array_Element_Id) return Array_Element_Id;
   --  Check whether File is in the List. Return the index in the list

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

   procedure Check_Suffix_List
     (Tree     : Prj.Project_Tree_Ref;
      Filename : String;
      Langs    : String_List_Id;
      List     : in out Array_Element_Id;
      Len      : out Natural);
   pragma Inline (Check_Suffix_List);
   --  Check in List whether any suffix matches Filename.
   --  Len is set to the length of the suffix, and List to the matching item
   --  (or No_Array_Element if there is no match)

   function Substitute_Dot
     (Unit_Name : String; Dot_Replacement : String) return String;
   --  Replace the '.' in unit_name with Dot_Replacement

   procedure Compute_Importing_Projects
     (Root_Project : Project_Type; Project : Project_Type);
   --  Compute the list of all projects that import, possibly indirectly,
   --  Project.

   function String_Elements
     (P : Project_Type) return Prj.String_Element_Table.Table_Ptr;
   function Projects_Table
     (P : Project_Type) return Prj.Project_Table.Table_Ptr;
   function Array_Elements
     (P : Project_Type) return Prj.Array_Element_Table.Table_Ptr;
   function Packages
     (P : Project_Type) return Prj.Package_Table.Table_Ptr;
   pragma Inline (String_Elements);
   pragma Inline (Projects_Table);
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
      Language                 : Name_Id) return String;
   --  Internal version of Get_Filename_From_Unit.

   ---------------------
   -- String_Elements --
   ---------------------

   function String_Elements
     (P : Project_Type) return Prj.String_Element_Table.Table_Ptr is
   begin
      return P.View_Tree.String_Elements.Table;
   end String_Elements;

   --------------------
   -- Projects_Table --
   --------------------

   function Projects_Table
     (P : Project_Type) return Prj.Project_Table.Table_Ptr is
   begin
      return P.View_Tree.Projects.Table;
   end Projects_Table;

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

   ----------------
   -- Do_Nothing --
   ----------------

   procedure Do_Nothing (Dep : in out Directory_Info) is
      pragma Unreferenced (Dep);
   begin
      null;
   end Do_Nothing;

   ----------------------------
   -- Update_Directory_Cache --
   ----------------------------

   procedure Update_Directory_Cache
     (Project   : Project_Type;
      Dir_Name  : String;
      Has_Files : Boolean) is
   begin
      Set (Project.Data.Directories,
           Name_As_Directory (Dir_Name), (Has_Files => Has_Files));
   end Update_Directory_Cache;

   ------------------
   -- Save_Project --
   ------------------

   function Save_Project
     (Project       : Project_Type;
      Path          : Virtual_File := VFS.No_File;
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
                 ("The file " & Full_Name (Project_Path (Project)).all
                  & " is not writable. Project not saved");
            end if;
            Trace (Me, "Project file not writable: "
                   & Full_Name (Project_Path (Project)).all);
            return False;
         end if;

         if Path = VFS.No_File then
            Filename :=  Project_Path (Project);
         else
            Filename := Path;
         end if;

         declare
            Dirname  : constant String := Dir_Name (Filename).all;
         begin
            Trace (Me, "Save_Project: Creating new file "
                   & Full_Name (Filename).all);

            begin
               Make_Dir_Recursive (Dirname);
            exception
               when Directory_Error =>
                  Trace (Me, "Couldn't create directory " & Dirname);

                  if Report_Error /= null then
                     Report_Error ("Couldn't create directory " & Dirname);
                  end if;

                  return False;
            end;

            Normalize_Cases (Project);

            Create (File, Mode => Out_File, Name => Full_Name (Filename).all);
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
               Trace (Me, "Couldn't create " & Full_Name (Filename).all);

               if Report_Error /= null then
                  Report_Error ("Couldn't create file "
                                & Full_Name (Filename).all);
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
         return Get_String
           (Projects_Table (Project) (Get_View (Project)).Display_Name);

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
      Host    : String := "") return VFS.Virtual_File
   is
      View : constant Prj.Project_Id := Get_View (Project);
   begin
      if Get_View (Project) = Prj.No_Project then
         --  View=Prj.No_Project case needed for the project wizard

         return Create
           (Host,
            To_Remote
              (Get_String (Path_Name_Of (Project.Node, Project.Tree)),
               Host));

      else
         return Create
           (Host,
            To_Remote
              (Get_String (Projects_Table (Project)(View).Display_Path_Name),
               Host));
      end if;
   end Project_Path;

   -----------------------
   -- Project_Directory --
   -----------------------

   function Project_Directory
     (Project : Project_Type;
      Host    : String := "") return VFS.Virtual_File is
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
            Src     : String_List_Id :=
                        Projects_Table (Project) (View).Source_Dirs;
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
      Has_VCS   : Boolean := False) return GNAT.Strings.String_List_Access
   is
      Iter    : Imported_Project_Iterator := Start (Project, Recursive);
      Count   : Natural := 0;
      P       : Project_Type;
      Sources : GNAT.Strings.String_List_Access;
      Result  : GNAT.Strings.String_List_Access;
      Src     : String_List_Id;
      Index   : Natural := 1;
      Current_Dir : constant String := Get_Current_Dir;
   begin
      loop
         P := Current (Iter);
         exit when P = No_Project;
         Count := Count
           + Length (Project.View_Tree,
                     Projects_Table (Project)(Get_View (P)).Source_Dirs);
         Next (Iter);
      end loop;

      Iter := Start (Project, Recursive);
      Sources := new GNAT.Strings.String_List (1 .. Count);

      loop
         P := Current (Iter);
         exit when P = No_Project;

         if not Has_VCS
           or else Get_Attribute_Value (P, VCS_Kind_Attribute) /= ""
         then
            Src := Projects_Table (Project) (Get_View (P)).Source_Dirs;

            while Src /= Nil_String loop
               Sources (Index) := new String'
                 (Name_As_Directory
                    (Normalize_Pathname
                       (Get_String (String_Elements (P) (Src).Display_Value),
                        Current_Dir,
                        Resolve_Links => False)));
               Index := Index + 1;
               Src   := String_Elements (P) (Src).Next;
            end loop;
         end if;

         Next (Iter);
      end loop;

      if Index < Sources'Last then
         Result := new GNAT.Strings.String_List'(Sources (1 .. Index - 1));
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
     (Project : Project_Type; Recursive : Boolean) return String is
   begin
      --  ??? The project parser doesn't cache the non-recursive version
      if not Recursive then
         if Project.Data.Non_Recursive_Include_Path = null then
            Project.Data.Non_Recursive_Include_Path := new String'
              (Prj.Env.Ada_Include_Path
                 (Get_View (Project), Project.View_Tree, Recursive));
         end if;

         return Project.Data.Non_Recursive_Include_Path.all;
      end if;

      return Prj.Env.Ada_Include_Path
        (Get_View (Project), Project.View_Tree, Recursive);
   end Include_Path;

   -----------------
   -- Object_Path --
   -----------------

   function Object_Path
     (Project             : Project_Type;
      Recursive           : Boolean;
      Including_Libraries : Boolean := True) return String
   is
      View : constant Project_Id := Get_View (Project);
   begin
      if View = Prj.No_Project then
         return "";

      elsif Recursive then
         return Prj.Env.Ada_Objects_Path
           (View, Project.View_Tree, Including_Libraries).all;

      elsif Including_Libraries
        and then Projects_Table (Project)(View).Library
        and then Projects_Table (Project)(View).Library_ALI_Dir /= No_Path
      then
         if Projects_Table (Project)(View).Display_Object_Dir = No_Path then
            return Get_String
              (Projects_Table (Project)(View).Library_ALI_Dir);
         else
            return Get_String
              (Projects_Table (Project)(View).Display_Object_Dir)
              & Path_Separator & Get_String
              (Projects_Table (Project)(View).Library_ALI_Dir);
         end if;
      elsif Projects_Table (Project)(View).Display_Object_Dir /= No_Path then
         return Get_String
                  (Projects_Table (Project)(View).Display_Object_Dir);

      else
         return "";
      end if;
   end Object_Path;

   --------------------------
   -- Direct_Sources_Count --
   --------------------------

   function Direct_Sources_Count (Project : Project_Type) return Natural is
   begin
      if Get_View (Project) = Prj.No_Project then
         return 0;
      else
         return Length (Project.View_Tree,
                        Projects_Table (Project)(Get_View (Project)).Sources);
      end if;
   end Direct_Sources_Count;

   ------------
   -- Create --
   ------------

   function Create
     (Base_Name       : Glib.UTF8_String;
      Project         : Projects.Project_Type;
      Use_Source_Path : Boolean := True;
      Use_Object_Path : Boolean := True) return VFS.Virtual_File
   is
      Full : constant String := Get_Full_Path_From_File
        (Project_Registry (Get_Registry (Project)), Base_Name,
         Use_Source_Path, Use_Object_Path, Project);
   begin
      if Full = "" then
         return VFS.No_File;
      else
         return VFS.Create (Full);
      end if;
   end Create;

   ---------------------
   -- Get_Source_File --
   ---------------------

   function Get_Source_File
     (Project : Project_Type; Index : Positive) return VFS.Virtual_File is
   begin
      if Index <= Project.Data.Files'Last then
         return Project.Data.Files (Index);
      else
         return VFS.No_File;
      end if;
   end Get_Source_File;

   ----------------------
   -- Get_Source_Files --
   ----------------------

   function Get_Source_Files
     (Project   : Project_Type;
      Recursive : Boolean) return VFS.File_Array_Access
   is
      Count   : Natural;
      Index   : Natural := 1;
      P       : Project_Type;
      Sources : File_Array_Access;

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

         return new File_Array'(Sources (Sources'First .. Index - 1));
      end;
   end Get_Source_Files;

   -----------------------
   -- Check_Suffix_List --
   -----------------------

   procedure Check_Suffix_List
     (Tree     : Prj.Project_Tree_Ref;
      Filename : String;
      Langs    : String_List_Id;
      List     : in out Array_Element_Id;
      Len      : out Natural)
   is
      Candidate     : Array_Element_Id := No_Array_Element;
      Candidate_Len : Natural := 0;
      Lang          : Name_Id;
      L             : String_List_Id;
   begin
      while List /= No_Array_Element loop
         Lang := Tree.Array_Elements.Table (List).Index;

         Get_Name_String (Tree.Array_Elements.Table (List).Value.Value);

         --  We first need to check the naming schemes for the supported
         --  languages (in case they redefine some of the predefined naming
         --  schemes, such as .h for c++ files). If this is not found in the
         --  list of supported languages, then return any match we had.

         declare
            Ext : String := Name_Buffer (1 .. Name_Len);
         begin
            Canonical_Case_File_Name (Ext);

            L := Langs;
            if Suffix_Matches (Filename, Ext) then
               while L /= Nil_String loop
                  if Tree.String_Elements.Table (L).Value = Lang then
                     Len := Name_Len;
                     return;
                  end if;

                  L := Tree.String_Elements.Table (L).Next;
               end loop;

               Candidate     := List;
               Candidate_Len := Name_Len;
            end if;
         end;

         List := Tree.Array_Elements.Table (List).Next;
      end loop;

      List := Candidate;
      Len  := Candidate_Len;
   end Check_Suffix_List;

   ------------------------------------------
   -- Get_Unit_Part_And_Name_From_Filename --
   ------------------------------------------

   procedure Get_Unit_Part_And_Name_From_Filename
     (Filename  : Glib.UTF8_String;
      Project   : Project_Type;
      Part      : out Unit_Part;
      Unit_Name : out Name_Id;
      Lang      : out Name_Id)
   is
      View   : Project_Id;
      Naming : Naming_Data;
      F      : String := Filename;
      Arr    : Array_Element_Id;
      Len    : Natural;
      File   : Name_Id;
      Langs  : String_List_Id;
   begin
      if Project = No_Project then
         Naming := Standard_Naming_Data;
      else
         View := Get_View (Project);
         Naming := Projects_Table (Project)(View).Naming;
      end if;

      Canonical_Case_File_Name (F);

      --  Check Ada exceptions

      File := Get_String (F);

      Arr := Check_Full_File (Project.View_Tree, File, Naming.Bodies);

      if Arr = No_Array_Element then
         Arr := Check_Full_File (Project.View_Tree, File, Naming.Specs);

         if Arr /= No_Array_Element then
            Part      := Unit_Spec;
            Unit_Name := Array_Elements (Project)(Arr).Index;
            Lang      := Name_Ada;
            return;
         end if;

      else
         Part      := Unit_Body;
         Unit_Name := Array_Elements (Project)(Arr).Index;
         Lang      := Name_Ada;
         return;
      end if;

      --  Check exceptions for other languages.
      --  No notion of unit here, so no other file name

      Arr := Check_Full_File
        (Project.View_Tree, File, Naming.Implementation_Exceptions);

      if Arr = No_Array_Element then
         Arr := Check_Full_File
           (Project.View_Tree, File, Naming.Specification_Exceptions);

         if Arr /= No_Array_Element then
            Part      := Unit_Spec;
            Lang      := Array_Elements (Project)(Arr).Index;
            Unit_Name := Get_String (F);
            return;
         end if;

      else
         Part      := Unit_Body;
         Unit_Name := Get_String (F);
         Lang      := Array_Elements (Project)(Arr).Index;
         return;
      end if;

      --  Check standard extensions. The index in this table is the language

      if Project /= No_Project then
         Langs := Get_Attribute_Value (Project, Languages_Attribute).Values;
      else
         Langs := Nil_String;
      end if;

      Arr := Naming.Spec_Suffix;
      Check_Suffix_List (Project.View_Tree, F, Langs, Arr, Len);
      if Arr /= No_Array_Element then
         Part      := Unit_Spec;
         Unit_Name := Get_String (F (F'First .. F'Last - Len));
         Lang      := Array_Elements (Project)(Arr).Index;
         return;
      end if;

      Arr := Naming.Body_Suffix;
      Check_Suffix_List (Project.View_Tree, F, Langs, Arr, Len);
      if Arr /= No_Array_Element then
         Part      := Unit_Body;
         Unit_Name := Get_String (F (F'First .. F'Last - Len));
         Lang      := Array_Elements (Project)(Arr).Index;
         return;
      end if;

      --  Test the separate suffix (for Ada files)

      declare
         Suffix : constant String := Get_String (Naming.Separate_Suffix);
      begin
         if Suffix_Matches (F, Suffix) then
            Part      := Unit_Separate;
            Lang      := Name_Ada;
            Unit_Name := Get_String (F (F'First .. F'Last - Suffix'Length));
            return;
         end if;
      end;

      --  Special case for the default GNAT extensions, since whatever the user
      --  naming scheme, the runtime always has the same naming scheme

      if GNAT.Directory_Operations.File_Extension (F) = ".ads" then
         Part      := Unit_Spec;
         Unit_Name := Get_String (Base_Name (Filename, ".ads"));
         Lang      := Name_Ada;
         return;

      elsif GNAT.Directory_Operations.File_Extension (F) = ".adb" then
         Part      := Unit_Spec;
         Unit_Name := Get_String (Base_Name (Filename, ".ads"));
         Lang      := Name_Ada;
         return;
      end if;

      --  Whatever. This should be unreachable code anyway

      Lang      := No_Name;
      Part      := Unit_Separate;
      Unit_Name := Get_String (Base_Name (Filename));
   end Get_Unit_Part_And_Name_From_Filename;

   ---------------------------------
   -- Get_Unit_Part_From_Filename --
   ---------------------------------

   function Get_Unit_Part_From_Filename
     (Project : Project_Type; Filename : VFS.Virtual_File) return Unit_Part
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
     (Project : Project_Type; Filename : VFS.Virtual_File) return String
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
      Language                 : String) return String
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
      Language                 : Name_Id) return String
   is
      Arr             : Array_Element_Id := No_Array_Element;
      Unit            : Name_Id;
      View            : Project_Id;
      Value           : Variable_Value;
      Unit_Name_Cased : String := Unit_Name;

      function Has_Predefined_Prefix (S : String) return Boolean;
      --  Return True is S has a name that starts like a predefined unit
      --  (e.g. a.b, which should be replaced by a~b)

      ---------------------------
      -- Has_Predefined_Prefix --
      ---------------------------

      function Has_Predefined_Prefix (S : String) return Boolean is
         C : constant Character := S (S'First);
      begin
         return S (S'First + 1) = '.'
           and then (C = 'a' or else C = 'g' or else C = 'i' or else C = 's');
      end Has_Predefined_Prefix;

   begin
      --  Standard GNAT naming scheme
      --  ??? This isn't language independent, what if other languages have
      --  similar requirements

      if Check_Predefined_Library and then Language = Name_Ada then
         case Part is
            when Unit_Body =>
               return Substitute_Dot (Unit_Name, "-") & ".adb";
            when Unit_Spec =>
               return Substitute_Dot (Unit_Name, "-") & ".ads";
            when others =>
               Assert (Me, False, "Unexpected Unit_Part");
               return "";
         end case;

      --  The project naming scheme
      else
         View := Get_View (Project);
         Name_Len := Unit_Name'Length;
         Name_Buffer (1 .. Name_Len) := Unit_Name;
         Unit := Name_Find;

         --  Check Ada exceptions

         case Part is
            when Unit_Body | Unit_Separate =>
               Value := Value_Of
                 (Index    => Unit,
                  In_Array => Projects_Table (Project)(View).Naming.Bodies,
                  In_Tree  => Project.View_Tree);

            when Unit_Spec =>
               Value := Value_Of
                 (Index    => Unit,
                  In_Array => Projects_Table (Project)(View).Naming.Specs,
                  In_Tree  => Project.View_Tree);
         end case;

         if Value /= Nil_Variable_Value then
            return Get_String (Value.Value);
         end if;

         --  Otherwise test the standard naming scheme

         case Projects_Table (Project)(View).Naming.Casing is
            when All_Lower_Case =>
               Fixed.Translate
                 (Source  => Unit_Name_Cased,
                  Mapping => Lower_Case_Map);

            when All_Upper_Case =>
               Fixed.Translate
                 (Source  => Unit_Name_Cased,
                  Mapping => Upper_Case_Map);

            when others =>
               null;
         end case;

         case Part is
            when Unit_Body =>
               Arr := Projects_Table (Project)(View).Naming.Body_Suffix;

            when Unit_Separate =>
               declare
                  N : constant String := Unit_Name_Cased & Get_String
                    (Name_Id
                      (Projects_Table (Project)(View).Naming.Separate_Suffix));
               begin
                  if not File_Must_Exist
                    or else Get_Project_From_File
                      (Project.Data.Registry.all, N, False) = Project
                  then
                     return N;
                  end if;
               end;

               return "";

            when Unit_Spec =>
               Arr := Projects_Table (Project)(View).Naming.Spec_Suffix;
         end case;

         declare
            Dot_Replacement : constant String := Get_String
              (Name_Id
                (Projects_Table
                 (Project)(Get_View (Project)).Naming.Dot_Replacement));
            Uname           : String := Substitute_Dot
              (Unit_Name_Cased, Dot_Replacement);

         begin
            --  Handle properly special naming such as a.b -> a~b

            if Language = Name_Ada
              and then Unit_Name_Cased'Length > 2
              and then Has_Predefined_Prefix (Unit_Name_Cased)
            then
               Uname (Uname'First + 1) := '~';
            end if;

            while Arr /= No_Array_Element loop
               if Array_Elements (Project)(Arr).Index = Language then
                  declare
                     N : constant String := Uname
                      & Get_String (Array_Elements (Project)(Arr).Value.Value);
                  begin
                     if not File_Must_Exist
                       or else Get_Project_From_File
                         (Project.Data.Registry.all, N, False) = Project
                     then
                        return N;
                     end if;
                  end;
               end if;

               Arr := Array_Elements (Project)(Arr).Next;
            end loop;
         end;
      end if;

      return "";
   end Get_Filename_From_Unit;

   ------------------------
   -- Delete_File_Suffix --
   ------------------------

   function Delete_File_Suffix
     (Filename : String; Project : Project_Type) return Natural
   is
      View  : constant Project_Id := Get_View (Project);
      Arr   : Array_Element_Id;
      Len   : Natural;
      Langs : String_List_Id;
   begin
      --  View will be null when called from the project wizard

      if View /= Prj.No_Project then
         Langs := Get_Attribute_Value (Project, Languages_Attribute).Values;

         Arr := Projects_Table (Project)(View).Naming.Spec_Suffix;
         Check_Suffix_List (Project.View_Tree, Filename, Langs, Arr, Len);
         if Arr /= No_Array_Element then
            return Filename'Last - Len;
         end if;

         Arr := Projects_Table (Project)(View).Naming.Body_Suffix;
         Check_Suffix_List (Project.View_Tree, Filename, Langs, Arr, Len);
         if Arr /= No_Array_Element then
            return Filename'Last - Len;
         end if;
      end if;

      --  Check the default naming scheme as well ? Otherwise, it might happen
      --  that a project has its own naming scheme, but still references files
      --  in the runtime with the default naming scheme.

      declare
         Ext : constant String :=
                 GNAT.Directory_Operations.File_Extension (Filename);
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
     (Project : Project_Type; Source_Filename : Virtual_File) return String
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
         N    : constant String := Get_Filename_From_Unit
           (Project, Unit, Part, Language => Lang);
      begin
         if N /= "" then
            return N;

         elsif Lang = Name_Ada then
            --  Default to the GNAT naming scheme (for runtime files)
            declare
               N2 : constant String := Get_Filename_From_Unit
                 (Project, Unit, Part, Check_Predefined_Library => True,
                  Language => Lang);
            begin
               if N2 /= "" then
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
     (Project   : Project_Type;
      Attribute : Attribute_Pkg;
      Default   : String := "";
      Index     : String := "") return String
   is
      View  : constant Project_Id := Get_View (Project);
      Value : Variable_Value;
   begin
      if Project = No_Project or else View = Prj.No_Project then
         return Default;
      end if;

      --  Special case for the naming scheme, since we need to get access to
      --  the default registered values for foreign languages

      if Attribute = Spec_Suffix_Attribute
        or else Attribute = Specification_Suffix_Attribute
      then
         Value := Value_Of
           (Index    => Get_String (Index),
            In_Array => Projects_Table (Project)(View).Naming.Spec_Suffix,
            In_Tree  => Project.View_Tree);

      elsif Attribute = Impl_Suffix_Attribute
        or else Attribute = Implementation_Suffix_Attribute
      then
         Value := Value_Of
           (Index    => Get_String (Index),
            In_Array => Projects_Table (Project)(View).Naming.Body_Suffix,
            In_Tree  => Project.View_Tree);

      elsif Attribute = Separate_Suffix_Attribute then
         return Get_String
                  (Projects_Table (Project)(View).Naming.Separate_Suffix);

      elsif Attribute = Casing_Attribute then
         return Prj.Image (Projects_Table (Project)(View).Naming.Casing);

      elsif Attribute = Dot_Replacement_Attribute then
         return Get_String
                  (Projects_Table (Project)(View).Naming.Dot_Replacement);

      elsif Attribute = Old_Implementation_Attribute then
         Value := Value_Of
          (Index    => Get_String (Index),
           In_Array => Projects_Table (Project)(View).Naming.Bodies,
           In_Tree  => Project.View_Tree);

      elsif Attribute = Old_Specification_Attribute then
         Value := Value_Of
          (Index    => Get_String (Index),
           In_Array => Projects_Table (Project)(View).Naming.Specs,
           In_Tree  => Project.View_Tree);

      else
         Value := Get_Attribute_Value (Project, Attribute, Index);
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

   function Get_Attribute_Value
     (Project   : Project_Type;
      Attribute : Attribute_Pkg) return Associative_Array
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
            In_Packages =>
              Projects_Table (Project)(Project_View).Decl.Packages,
            In_Tree => Project.View_Tree);
         if Pkg = No_Package then
            return (1 .. 0 => (No_Name, Nil_Variable_Value));
         end if;
         Arr := Packages (Project)(Pkg).Decl.Arrays;

      else
         Arr := Projects_Table (Project)(Project_View).Decl.Arrays;
      end if;

      N := Get_String (Attribute_Name);
      Elem := Value_Of (N,
                        In_Arrays => Arr,
                        In_Tree   => Project.View_Tree);

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

   function Get_Attribute_Value
     (Project   : Project_Type;
      Attribute : Attribute_Pkg;
      Index     : String := "") return GNAT.OS_Lib.Argument_List
   is
      Value : constant Variable_Value := Get_Attribute_Value
        (Project, Attribute, Index);
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
   -- Is_Main_File --
   ------------------

   function Is_Main_File
     (Project : Project_Type; File : String) return Boolean
   is
      Value : Argument_List := Get_Attribute_Value
        (Project, Attribute => Main_Attribute);
   begin
      if Is_Case_Sensitive (Build_Server) then
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

   function Executables_Directory (Project : Project_Type) return String is
   begin
      if Project = No_Project or else Get_View (Project) = Prj.No_Project then
         return "";

      else
         declare
            Exec : constant String := Get_String
             (Name_Id
              (Projects_Table (Project)(Get_View (Project)).Display_Exec_Dir));
         begin
            if Exec /= "" then
               return Name_As_Directory (Exec);
            else
               return Name_As_Directory
                 (Object_Path (Project, Recursive => False));
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

         Current := Parent_Project (Current);
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

         Start := Parent_Project (Start);
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
        (Project_Registry (Get_Registry (Project)));
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

      function External_Default (Var : Project_Node_Id)
         return Name_Id;
      --  Return the default value for the variable. Var must be a variable
      --  declaration.
      --  ??? This doesn't support cases where the default value is defined as
      --  an expression.

      ----------------
      -- Count_Vars --
      ----------------

      function Count_Vars return Natural is
         Count : Natural := 0;

         function Cb (Variable : Project_Node_Id; Prj : Project_Node_Id)
            return Boolean;
         --  Increment the total number of variables

         --------
         -- Cb --
         --------

         function Cb (Variable : Project_Node_Id; Prj : Project_Node_Id)
            return Boolean
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
         Expr : Project_Node_Id := Expression_Of (Var, Project.Tree);
      begin
         Expr := First_Term   (Expr, Project.Tree);
         Expr := Current_Term (Expr, Project.Tree);

         if Kind_Of (Expr, Project.Tree) = N_External_Value then
            Expr := External_Default_Of (Expr, Project.Tree);

            if Expr = Empty_Node then
               return No_Name;
            end if;

            if Kind_Of (Expr, Project.Tree) /= N_Literal_String then
               Expr := First_Term (Expr, Project.Tree);
               Assert (Me, Next_Term (Expr, Project.Tree) = Empty_Node,
                       "Default value cannot be a concatenation");

               Expr := Current_Term (Expr, Project.Tree);
               Assert (Me, Kind_Of (Expr, Project.Tree) = N_Literal_String,
                       "Default value can only be literal string");
            end if;

            return String_Value_Of (Expr, Project.Tree);
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
      if Prj.Ext.Value_Of (Var.Name) = No_Name then
         if Var.Default /= No_Name then
            Prj.Ext.Add (N, External_Default (Var));
         else
            Get_Name_String
              (String_Value_Of
                 (First_Literal_String (Var.String_Type, Tree), Tree));
            Prj.Ext.Add (N, Name_Buffer (Name_Buffer'First .. Name_Len));
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

   function Get_Paths_Type (Project : Project_Type)
      return Paths_Type_Information is
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
      --  change when the view changes

      Free (Project.Data.Non_Recursive_Include_Path);

      Reset (Project.Data.Directories);
      Unchecked_Free (Project.Data.Files);
   end Reset;

   --------------
   -- Get_View --
   --------------

   function Get_View
     (Tree : Prj.Project_Tree_Ref; Name : Name_Id) return Prj.Project_Id is
   begin
      for J in
        Tree.Projects.Table'First .. Project_Table.Last (Tree.Projects)
      loop
         if Tree.Projects.Table (J).Name = Name then
            return J;
         end if;
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

   ---------------------
   -- Check_Full_File --
   ---------------------

   function Check_Full_File
     (Tree : Prj.Project_Tree_Ref;
      File : Name_Id;
      List : Array_Element_Id) return Array_Element_Id
   is
      Prefix : Array_Element_Id := List;
      Str    : String_List_Id;
   begin
      while Prefix /= No_Array_Element loop
         case Tree.Array_Elements.Table (Prefix).Value.Kind is
            when Undefined =>
               null;

            --  Naming exceptions for languages other than Ada
            when Prj.List =>
               Str := Tree.Array_Elements.Table (Prefix).Value.Values;
               while Str /= Nil_String loop
                  if Tree.String_Elements.Table (Str).Value = File then
                     return Prefix;
                  end if;
                  Str := Tree.String_Elements.Table (Str).Next;
               end loop;

            --  Naming exceptions for Ada
            when Single =>
               if Tree.Array_Elements.Table (Prefix).Value.Value = File then
                  return Prefix;
               end if;
         end case;

         Prefix := Tree.Array_Elements.Table (Prefix).Next;
      end loop;
      return No_Array_Element;
   end Check_Full_File;

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
      P : Project_Type;

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
      return Abstract_Registry'Class is
   begin
      return Abstract_Registry'Class (Project.Data.Registry.all);
   end Get_Registry;

   ------------------
   -- Get_Switches --
   ------------------

   procedure Get_Switches
     (Project          : Project_Type;
      In_Pkg           : String;
      File             : VFS.Virtual_File;
      Language         : String;
      Value            : out Variable_Value;
      Is_Default_Value : out Boolean) is
   begin
      Value := Nil_Variable_Value;

      --  Do we have some file-specific switches ?
      if Project /= No_Project and then File /= VFS.No_File then
         Value := Get_Attribute_Value
           (Project        => Project,
            Attribute      =>
              Attribute_Pkg (In_Pkg & '#' & Get_String (Name_Switches)),
            Index          => Base_Name (File));

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

   --------------
   -- Value_Of --
   --------------

   function Value_Of (Var : Scenario_Variable) return String is
   begin
      return Get_String
        (Prj.Ext.Value_Of (Var.Name, With_Default => Var.Default));
   end Value_Of;

   --------------------
   -- Enum_Values_Of --
   --------------------

   function Enum_Values_Of
     (Var : Scenario_Variable; Registery : Abstract_Registry'Class)
      return String_List_Utils.String_List.List
   is
      Values : String_List_Utils.String_List.List;
      Tree   : constant Prj.Tree.Project_Node_Tree_Ref :=
                 Get_Tree (Project_Registry (Registery));
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

      List : String_List_Utils.String_List.List :=
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

   --------------------
   -- Parent_Project --
   --------------------

   function Parent_Project (Project : Project_Type) return Project_Type is
      Extend : constant Project_Node_Id := Extended_Project_Of
        (Project_Declaration_Of (Project.Node, Project.Tree),
         Project.Tree);
   begin
      if Extend = Empty_Node then
         return No_Project;
      else
         return Get_Project_From_Name
           (Project.Data.Registry.all,
            Prj.Tree.Name_Of (Extend, Project.Tree));
      end if;
   end Parent_Project;

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

   ---------------
   -- Set_Value --
   ---------------

   procedure Set_Value (Var : Scenario_Variable; Value : String) is
   begin
      Prj.Ext.Add (External_Reference_Of (Var), Value);
   end Set_Value;

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
     (Project : Project_Type; File : String) return String
   is
      Base         : constant String := Base_Name (File);
      Default_Exec : constant String := Base
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
               Index => File, Default => "");
         begin
            if From_Project = "" then
               --  Check whether the file is a special naming scheme for an
               --  Ada unit. If this is the case, the name of the unit is the
               --  name of the executable

               declare
                  Base_Id : constant Name_Id := Get_String (Base);
                  Args    : constant Associative_Array := Get_Attribute_Value
                    (Project, Attribute => Implementation_Attribute);
               begin
                  for A in Args'Range loop
                     if Args (A).Value.Value = Base_Id then
                        return Get_String (Args (A).Index);
                     end if;
                  end loop;
               end;

               --  Else use the default
               return Default_Exec;
            end if;

            return From_Project;
         end;
      end if;
   end Get_Executable_Name;

   ------------------------------
   -- Directory_Contains_Files --
   ------------------------------

   function Directory_Contains_Files
     (Project   : Project_Type;
      Directory : String) return Boolean
   is
      Info : constant Directory_Info := Get
        (Project.Data.Directories, Name_As_Directory (Directory));
   begin
      return Info.Has_Files;
   end Directory_Contains_Files;

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
      Source_Files : VFS.File_Array_Access) is
   begin
      if Project.Data.Files /= null then
         Unchecked_Free (Project.Data.Files);
      end if;

      Project.Data.Files := Source_Files;
   end Set_Source_Files;

end Projects;
