-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                    Copyright (C) 2002-2003                        --
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

with Ada.Characters.Handling;   use Ada.Characters.Handling;
with Ada.Exceptions;            use Ada.Exceptions;
with Ada.Text_IO;               use Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with Basic_Types;               use Basic_Types;
with File_Utils;                use File_Utils;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with Namet;                     use Namet;
with Osint;                     use Osint;
with OS_Utils;                  use OS_Utils;
with Prj.Env;                   use Prj, Prj.Env;
with Prj.Ext;
with Prj.PP;                    use Prj.PP;
with Prj.Tree;                  use Prj.Tree;
with Prj.Util;                  use Prj.Util;
with Projects.Graphs;           use Projects.Graphs;
with Projects.Editor;           use Projects.Editor;
with Projects.Registry;         use Projects.Registry;
with Snames;                    use Snames;
with String_Hash;
with String_Utils;              use String_Utils;
with Traces;                    use Traces;
with Types;                     use Types;

package body Projects is

   Me    : constant Debug_Handle := Create ("Projects");
   Debug : constant Debug_Handle := Create ("Projects.Debug");

   type Name_Id_Array_Access is access Name_Id_Array;

   type Directory_Info is record
      Has_Files  : Boolean;
   end record;
   No_Directory_Info : constant Directory_Info := (Has_Files => False);

   procedure Do_Nothing (Dep : in out Directory_Info);

   package Directory_Htable is new String_Hash
     (Data_Type => Directory_Info,
      Free_Data => Do_Nothing,
      Null_Ptr  => No_Directory_Info);
   use Directory_Htable.String_Hash_Table;

   procedure Do_Nothing (Bool : in out Boolean);
   package Boolean_Htable is new String_Hash
     (Data_Type => Boolean,
      Free_Data => Do_Nothing,
      Null_Ptr  => False);

   type Project_Type_Data is record
      View : Prj.Project_Id;

      Modified : Boolean := False;
      --  True if the project has been modified by the user, and not saved
      --  yet.

      Paths_Type : Paths_Type_Information := From_Pref;
      --  True if the paths in the project file should be stored as relative
      --  paths.

      Normalized : Boolean := False;
      --  True if the project has been normalized.

      Imported_Projects  : Name_Id_Array_Access;
      Importing_Projects : Name_Id_Array_Access;
      --  Sorted list of imported projects (Cache for
      --  Imported_Project_Iterator)

      Directories : Directory_Htable.String_Hash_Table.HTable;
      --  Information for the various directories of the project

      Non_Recursive_Include_Path : GNAT.OS_Lib.String_Access;
      --  The include path for this project.

      Registry   : Project_Registry_Access;
      --  Needed so that we can return other projects like imported projects

      View_Is_Complete : Boolean := False;
      --  True if the view for the project was correctly computed

      Is_Default : Boolean := False;
      --  True if the current project is the default project (ie is not
      --  associated with a file on the disk)
   end record;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Name_Id_Array, Name_Id_Array_Access);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Project_Registry'Class, Project_Registry_Access);

   function Source_Files_Count
     (Project : Project_Type; Recursive : Boolean) return Natural;
   --  Return the number of source files in the projects returned by Iter.
   --  On exit, Iter is reset to the beginning.

   function Get_View (Name : Name_Id) return Prj.Project_Id;
   --  Return the project view for the project Name

   function Check_Full_File
     (File : Name_Id; List : Array_Element_Id) return Array_Element_Id;
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
     (Filename : String;
      Langs    : String_List_Id;
      List     : in out Array_Element_Id;
      Len      : out Natural);
   pragma Inline (Check_Suffix_List);
   --  Check in List whether any suffix matches Filename.
   --  Len is set to the length of the suffix, and List to the matching item
   --  (or No_Array_Element if there is no match)

   function Project_Imports
     (Parent           : Project_Type;
      Child            : Project_Type;
      Include_Extended : Boolean := False) return Boolean;
   --  Return True if Parent imports directly Child.
   --  if Parents or Child is No_Project, True is returned.
   --  If Include_Extended is true, then True is also returned if Child is an
   --  extended project of Parent

   function Substitute_Dot
     (Unit_Name : String; Dot_Replacement : String) return String;
   --  Replace the '.' in unit_name with Dot_Replacement

   procedure Compute_Importing_Projects
     (Root_Project : Project_Type; Project : Project_Type);
   --  Compute the list of all projects that import, possibly indirectly,
   --  Project.

   ----------------
   -- Do_Nothing --
   ----------------

   procedure Do_Nothing (Dep : in out Directory_Info) is
      pragma Unreferenced (Dep);
   begin
      null;
   end Do_Nothing;

   procedure Do_Nothing (Bool : in out Boolean) is
      pragma Unreferenced (Bool);
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

   procedure Save_Project
     (Project       : Project_Type;
      Report_Error  : Error_Report := null)
   is
      File : Ada.Text_IO.File_Type;

      procedure Internal_Write_Char (C : Character);
      procedure Internal_Write_Str (S : String);
      procedure Internal_Write_Eol;

      procedure Internal_Write_Char (C : Character) is
      begin
         Put (File, C);
      end Internal_Write_Char;

      procedure Internal_Write_Str (S : String) is
      begin
         Put (File, S);
      end Internal_Write_Str;

      procedure Internal_Write_Eol is
      begin
         Put (File, ASCII.LF);
      end Internal_Write_Eol;

   begin
      if not Is_Regular_File (Project_Path (Project))
        or else Project_Modified (Project)
      then
         declare
            Filename : constant String := Project_Path (Project);
            Dirname  : constant String := Dir_Name (Filename);

         begin
            Trace (Me, "Save_Project: Creating new file " & Filename);

            begin
               Make_Dir_Recursive (Dirname);
            exception
               when Directory_Error =>
                  Trace (Me, "Couldn't create directory " & Dirname);

                  if Report_Error /= null then
                     Report_Error ("Couldn't create directory " & Dirname);
                  end if;

                  return;
            end;

            Create (File, Mode => Out_File, Name => Filename);
            Pretty_Print
              (Project => Project,
               Eliminate_Empty_Case_Constructions => True,
               W_Char => Internal_Write_Char'Unrestricted_Access,
               W_Eol  => Internal_Write_Eol'Unrestricted_Access,
               W_Str  => Internal_Write_Str'Unrestricted_Access);
            Close (File);

            Set_Project_Modified (Project, False);
            Set_Is_Default (Project, False);

         exception
            when Ada.Text_IO.Name_Error =>
               Trace (Me, "Couldn't create " & Filename);

               if Report_Error /= null then
                  Report_Error ("Couldn't create file " & Filename);
               end if;
         end;
      end if;
   end Save_Project;

   ------------------
   -- Project_Name --
   ------------------

   function Project_Name (Project : Project_Type) return String is
   begin
      if Project = No_Project or else Is_Default (Project) then
         return "default";
      else
         return Get_String (Prj.Tree.Name_Of (Project.Node));
      end if;
   end Project_Name;

   ------------------
   -- Project_Name --
   ------------------

   function Project_Name (Project : Project_Type) return Name_Id is
   begin
      if Project = No_Project or else Is_Default (Project) then
         return Name_Default;
      else
         return Prj.Tree.Name_Of (Project.Node);
      end if;
   end Project_Name;

   ------------------
   -- Project_Path --
   ------------------

   function Project_Path (Project : Project_Type) return String is
   begin
      if Project = No_Project or else Is_Default (Project) then
         return "";
      else
         return Get_String (Path_Name_Of (Project.Node));
      end if;
   end Project_Path;

   -----------------------
   -- Project_Directory --
   -----------------------

   function Project_Directory (Project : Project_Type) return String is
   begin
      if Project = No_Project then
         return "";
      else
         return Name_As_Directory (Get_String (Directory_Of (Project.Node)));
      end if;
   end Project_Directory;

   -----------------
   -- Source_Dirs --
   -----------------

   function Source_Dirs (Project : Project_Type) return Name_Id_Array is
      View    : constant Project_Id := Get_View (Project);
   begin
      if View = Prj.No_Project then
         return (1 .. 0 => No_Name);

      else
         declare
            Src     : String_List_Id := Prj.Projects.Table (View).Source_Dirs;
            Count   : constant Natural    := Length (Src);
            Sources : Name_Id_Array (1 .. Count);
         begin
            for C in Sources'Range loop
               Sources (C) := String_Elements.Table (Src).Value;
               Src         := String_Elements.Table (Src).Next;
            end loop;
            return Sources;
         end;
      end if;
   end Source_Dirs;

   -----------------
   -- Source_Dirs --
   -----------------

   function Source_Dirs
     (Project : Project_Type; Recursive : Boolean)
      return Basic_Types.String_Array_Access
   is
      Iter    : Imported_Project_Iterator := Start (Project, Recursive);
      Count   : Natural := 0;
      P       : Project_Type;
      Sources : String_Array_Access;
      Src     : String_List_Id;
      Index   : Natural := 1;
   begin
      loop
         P := Current (Iter);
         exit when P = No_Project;
         Count := Count
           + Length (Prj.Projects.Table (Get_View (P)).Source_Dirs);
         Next (Iter);
      end loop;

      Iter := Start (Project, Recursive);
      Sources := new String_Array (1 .. Count);

      loop
         P := Current (Iter);
         exit when P = No_Project;

         Src := Prj.Projects.Table (Get_View (P)).Source_Dirs;

         while Src /= Nil_String loop
            Sources (Index) := new String'
              (Name_As_Directory
                 (Normalize_Pathname
                    (Get_String (String_Elements.Table (Src).Value))));
            Index := Index + 1;
            Src   := String_Elements.Table (Src).Next;
         end loop;

         Next (Iter);
      end loop;

      return Sources;
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
              (Prj.Env.Ada_Include_Path (Get_View (Project), Recursive));
         end if;

         return Project.Data.Non_Recursive_Include_Path.all;
      end if;

      return Prj.Env.Ada_Include_Path (Get_View (Project), Recursive);
   end Include_Path;

   -----------------
   -- Object_Path --
   -----------------

   function Object_Path
     (Project : Project_Type; Recursive : Boolean) return String
   is
      View : constant Project_Id := Get_View (Project);
   begin
      if View = Prj.No_Project then
         return "";

      elsif Recursive then
         return Prj.Env.Ada_Objects_Path (View).all;
      elsif Prj.Projects.Table (View).Object_Directory /= No_Name then
         return Get_String (Prj.Projects.Table (View).Object_Directory);
      else
         return "";
      end if;
   end Object_Path;

   ------------------------
   -- Source_Files_Count --
   ------------------------

   function Source_Files_Count
     (Project : Project_Type; Recursive : Boolean) return Natural
   is
      Iter : Imported_Project_Iterator := Start (Project, Recursive);
      Count : Natural := 0;
      P    : Project_Type;
   begin
      loop
         P := Current (Iter);
         exit when P = No_Project;

         Count := Count + Direct_Sources_Count (P);
         Next (Iter);
      end loop;
      return Count;
   end Source_Files_Count;

   --------------------------
   -- Direct_Sources_Count --
   --------------------------

   function Direct_Sources_Count (Project : Project_Type) return Natural is
   begin
      return Length (Prj.Projects.Table (Get_View (Project)).Sources);
   end Direct_Sources_Count;

   ----------------------
   -- Get_Source_Files --
   ----------------------

   function Get_Source_Files
     (Project            : Project_Type;
      Recursive          : Boolean;
      Full_Path          : Boolean := True;
      Normalized         : Boolean := True;
      Matching_Languages : Name_Id_Array := All_Languages)
      return Basic_Types.String_Array_Access
   is
      Src     : String_List_Id;
      Count   : constant Natural := Source_Files_Count (Project, Recursive);
      Sources : String_Array_Access;
      Index   : Natural := 1;
      View    : Project_Id;
      Iter    : Imported_Project_Iterator;
      P       : Project_Type;
      Seen    : Boolean_Htable.String_Hash_Table.HTable;
      use Boolean_Htable.String_Hash_Table;

   begin
      Reset (Seen);

      Iter := Start (Project, Recursive);
      Sources := new String_Array (1 .. Count);

      loop
         P := Current (Iter);
         exit when P = No_Project;

         View := Get_View (P);
         Src  := Prj.Projects.Table (View).Sources;

         while Src /= Nil_String loop
            Get_Name_String (String_Elements.Table (Src).Value);
            if Language_Matches
              (Project.Data.Registry.all,
               Name_Buffer (1 .. Name_Len),
               Matching_Languages)
            then
               if not Get (Seen, Name_Buffer (1 .. Name_Len))  then
                  if Full_Path then
                     declare
                        File : constant String := Name_Buffer (1 .. Name_Len);
                     begin
                        Get_Full_Path_From_File
                          (Project.Data.Registry.all, File,
                           Use_Source_Path => True, Use_Object_Path => False);

                        --  Sometimes the file cannot be found in the project
                        --  registry, and Get_Full_Path_From_File above sets an
                        --  empty name in Name_Buffer.
                        --  For example, this happens when there are broken
                        --  links in the sources directory. In this case, we
                        --  set the file name with the base name, so that other
                        --  operations (such as search in project files for
                        --  examples) can go on.

                        if Name_Len <= 0 then
                           Name_Len := File'Length;
                           Name_Buffer (1 .. Name_Len) := File;
                        end if;

                        Set (Seen, File, True);
                     end;

                     if Normalized then
                        declare
                           Full : constant String := Normalize_Pathname
                             (Name_Buffer (1 .. Name_Len),
                              Resolve_Links => True);
                        begin
                           Name_Len := Full'Length;
                           Name_Buffer (1 .. Name_Len) := Full;
                        end;
                     end if;
                  else
                     Set (Seen, Name_Buffer (1 .. Name_Len), True);
                  end if;

                  Sources (Index) := new String'(Name_Buffer (1 .. Name_Len));
                  Index := Index + 1;
               end if;
            end if;

            Src := String_Elements.Table (Src).Next;
         end loop;

         Next (Iter);
      end loop;

      Reset (Seen);

      --  Shrink the array if needed
      if Index <= Sources'Last then
         declare
            S : constant String_Array_Access := new String_Array'
              (Sources (Sources'First .. Index - 1));
         begin
            Basic_Types.Unchecked_Free (Sources);
            Sources := S;
         end;
      end if;

      return Sources;
   end Get_Source_Files;

   ----------------------
   -- Get_Source_Files --
   ----------------------

   function Get_Source_Files
     (Project : Project_Type; Recursive : Boolean) return Name_Id_Array
   is
      Src     : String_List_Id;
      Count   : constant Natural := Source_Files_Count (Project, Recursive);
      Iter    : Imported_Project_Iterator := Start (Project, Recursive);
      Index   : Natural := 1;
      P       : Project_Type;
      Sources : Name_Id_Array (1 .. Count);

   begin
      loop
         P := Current (Iter);
         exit when P = No_Project;

         Src := Prj.Projects.Table (Get_View (P)).Sources;

         while Src /= Nil_String loop
            Sources (Index) := String_Elements.Table (Src).Value;
            Index := Index + 1;
            Src := String_Elements.Table (Src).Next;
         end loop;

         Next (Iter);
      end loop;

      return Sources;
   end Get_Source_Files;

   -----------------------
   -- Check_Suffix_List --
   -----------------------

   procedure Check_Suffix_List
     (Filename           : String;
      Langs              : String_List_Id;
      List               : in out Array_Element_Id;
      Len                : out Natural)
   is
      Candidate     : Array_Element_Id := No_Array_Element;
      Candidate_Len : Natural := 0;
      Lang : Name_Id;
      L    : String_List_Id;
   begin
      while List /= No_Array_Element loop
         Lang := Array_Elements.Table (List).Index;

         --  We first need to check the naming schemes for the supported
         --  languages (in case they redefine some of the predefined naming
         --  schemes, such as .h for c++ files). In this is not found in the
         --  list of supported languages, then return any match we had

         L     := Langs;
         while L /= Nil_String loop
            Get_Name_String (Array_Elements.Table (List).Value.Value);
            if Suffix_Matches (Filename, Name_Buffer (1 .. Name_Len)) then
               if String_Elements.Table (L).Value = Lang then
                  Len := Name_Len;
                  return;
               end if;

               Candidate     := List;
               Candidate_Len := Name_Len;
            end if;

            L := String_Elements.Table (L).Next;
         end loop;

         List := Array_Elements.Table (List).Next;
      end loop;

      List := Candidate;
      Len  := Candidate_Len;
   end Check_Suffix_List;

   ------------------------------------------
   -- Get_Unit_Part_And_Name_From_Filename --
   ------------------------------------------

   procedure Get_Unit_Part_And_Name_From_Filename
     (Filename  : String;
      Project   : Project_Type;
      Part      : out Unit_Part;
      Unit_Name : out Name_Id;
      Lang      : out Name_Id)
   is
      View   : constant Project_Id := Get_View (Project);
      Naming : constant Naming_Data := Prj.Projects.Table (View).Naming;
      F    : String := Filename;
      Arr  : Array_Element_Id;
      Len  : Natural;
      File : Name_Id;
      Langs : String_List_Id;
   begin
      Canonical_Case_File_Name (F);

      --  Check Ada exceptions

      File := Get_String (F);

      Arr := Check_Full_File
        (File, Prj.Projects.Table (View).Naming.Bodies);

      if Arr = No_Array_Element then
         Arr := Check_Full_File (File, Naming.Specs);

         if Arr /= No_Array_Element then
            Part      := Unit_Spec;
            Unit_Name := Array_Elements.Table (Arr).Index;
            Lang      := Name_Ada;
            return;
         end if;

      else
         Part      := Unit_Body;
         Unit_Name := Array_Elements.Table (Arr).Index;
         Lang      := Name_Ada;
         return;
      end if;

      --  Check exceptions for other languages.
      --  No notion of unit here, so no other file name

      Arr := Check_Full_File (File, Naming.Implementation_Exceptions);

      if Arr = No_Array_Element then
         Arr := Check_Full_File (File, Naming.Specification_Exceptions);

         if Arr /= No_Array_Element then
            Part      := Unit_Spec;
            Lang      := Array_Elements.Table (Arr).Index;
            Unit_Name := Get_String (Filename);
            return;
         end if;

      else
         Part      := Unit_Body;
         Unit_Name := Get_String (Filename);
         Lang      := Array_Elements.Table (Arr).Index;
         return;
      end if;

      --  Check standard extensions. The index in this table is the language

      Langs := Get_Attribute_Value (Project, Languages_Attribute).Values;

      Arr := Naming.Spec_Suffix;
      Check_Suffix_List (F, Langs, Arr, Len);
      if Arr /= No_Array_Element then
         Part      := Unit_Spec;
         Unit_Name := Get_String (F (F'First .. F'Last - Len));
         Lang      := Array_Elements.Table (Arr).Index;
         return;
      end if;

      Arr := Naming.Body_Suffix;
      Check_Suffix_List (F, Langs, Arr, Len);
      if Arr /= No_Array_Element then
         Part      := Unit_Body;
         Unit_Name := Get_String (F (F'First .. F'Last - Len));
         Lang      := Array_Elements.Table (Arr).Index;
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
      Unit_Name := Get_String (Filename);
   end Get_Unit_Part_And_Name_From_Filename;

   ---------------------------------
   -- Get_Unit_Part_From_Filename --
   ---------------------------------

   function Get_Unit_Part_From_Filename
     (Project : Project_Type; Filename : String) return Unit_Part
   is
      Unit : Unit_Part;
      Name, Lang : Name_Id;
   begin
      Get_Unit_Part_And_Name_From_Filename
        (Filename, Project, Unit, Name, Lang);
      return Unit;
   end Get_Unit_Part_From_Filename;

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
     (Project   : Project_Type := No_Project;
      Unit_Name : String;
      Part      : Unit_Part) return String
   is
      Arr  : Array_Element_Id := No_Array_Element;
      Unit : Name_Id;
      View : Project_Id;
      Value : Variable_Value;

   begin
      --  Standard GNAT naming scheme
      if Project = No_Project then
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
                 (Index => Unit,
                  In_Array => Prj.Projects.Table (View).Naming.Bodies);

            when Unit_Spec =>
               Value := Value_Of
                 (Index => Unit,
                  In_Array => Prj.Projects.Table (View).Naming.Specs);
         end case;

         if Value /= Nil_Variable_Value then
            return Get_String (Value.Value);
         end if;

         --  Otherwise test the standard naming scheme

         case Part is
            when Unit_Body =>
               Arr := Prj.Projects.Table (View).Naming.Body_Suffix;

            when Unit_Separate =>
               declare
                  N : constant String := Unit_Name & Get_String
                    (Prj.Projects.Table (View).Naming.Separate_Suffix);
               begin
                  if Get_Project_From_File
                    (Project.Data.Registry.all, N, False) = Project
                  then
                     return N;
                  end if;
               end;
               return "";

            when Unit_Spec =>
               Arr := Prj.Projects.Table (View).Naming.Spec_Suffix;
         end case;

         declare
            Dot_Replacement : constant String := Get_String
              (Prj.Projects.Table (Get_View (Project)).Naming.Dot_Replacement);
            Uname : constant String := Substitute_Dot
              (Unit_Name, Dot_Replacement);
         begin
            while Arr /= No_Array_Element loop
               if Array_Elements.Table (Arr).Index = Name_Ada then
                  declare
                     N : constant String := Uname
                       & Get_String (Array_Elements.Table (Arr).Value.Value);
                  begin
                     if Get_Project_From_File
                       (Project.Data.Registry.all, N, False) = Project
                     then
                        return N;
                     end if;
                  end;
               end if;

               Arr := Array_Elements.Table (Arr).Next;
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
      View : constant Project_Id := Get_View (Project);
      Arr  : Array_Element_Id;
      Len  : Natural;
      Langs : String_List_Id;
   begin
      --  View will be null when called from the project wizard

      if View /= Prj.No_Project then
         Langs := Get_Attribute_Value (Project, Languages_Attribute).Values;

         Arr := Prj.Projects.Table (View).Naming.Spec_Suffix;
         Check_Suffix_List (Filename, Langs, Arr, Len);
         if Arr /= No_Array_Element then
            return Filename'Last - Len;
         end if;

         Arr := Prj.Projects.Table (View).Naming.Body_Suffix;
         Check_Suffix_List (Filename, Langs, Arr, Len);
         if Arr /= No_Array_Element then
            return Filename'Last - Len;
         end if;
      end if;

      --  Check the default naming scheme as well ? Otherwise, it might happen
      --  that a project has its own naming scheme, but still references files
      --  in the runtime with the default naming scheme.

      if GNAT.Directory_Operations.File_Extension (Filename) = ".ads" then
         return Filename'Last - 4;
      elsif GNAT.Directory_Operations.File_Extension (Filename) = ".adb" then
         return Filename'Last - 4;
      end if;

      return Filename'Last;
   end Delete_File_Suffix;

   ---------------------
   -- Other_File_Name --
   ---------------------

   function Other_File_Name
     (Project : Project_Type; Source_Filename : String) return String
   is
      File       : constant String := Base_Name (Source_Filename);
      Unit, Part : Unit_Part;
      Name, Lang : Name_Id;
      P          : Project_Type;
   begin
      Get_Unit_Part_And_Name_From_Filename
        (File, Project, Unit, Name, Lang);

      case Unit is
         when Unit_Spec                 => Part := Unit_Body;
         when Unit_Body | Unit_Separate => Part := Unit_Spec;
      end case;

      Get_Name_String (Name);
      declare
         File : constant String := Name_Buffer (1 .. Name_Len);
      begin
         --  Search in all extended projects as well, since the other file
         --  might not be redefined in Project itself. Start from the lowest
         --  project.
         P := Extending_Project (Project, Recurse => True);
         while P /= No_Project loop
            declare
               N : constant String := Get_Filename_From_Unit (P, File, Part);
            begin
               if N /= "" then
                  return N;
               end if;
            end;

            P := Parent_Project (P);
         end loop;

         return File;
      end;
   end Other_File_Name;

   -------------------------
   -- Get_Attribute_Value --
   -------------------------

   function Get_Attribute_Value
     (Project        : Project_Type;
      Attribute      : Attribute_Pkg;
      Default        : String := "";
      Index          : String := "") return String
   is
      Value : Variable_Value;
      View  : constant Project_Id := Get_View (Project);
   begin
      if Project = No_Project or else View = Prj.No_Project then
         return Default;
      end if;

      --  Special case for the naming scheme, since we need to get access to
      --  the default registered values for foreign languages

      if Attribute = Spec_Suffix_Attribute then
         Value := Value_Of
           (Index    => Get_String (Index),
            In_Array => Prj.Projects.Table (View).Naming.Spec_Suffix);

      elsif Attribute = Impl_Suffix_Attribute then
         Value := Value_Of
           (Index    => Get_String (Index),
            In_Array => Prj.Projects.Table (View).Naming.Body_Suffix);

      elsif Attribute = Specification_Suffix_Attribute then
         Value := Value_Of
           (Index    => Get_String (Index),
            In_Array => Prj.Projects.Table (View).Naming.Specs);

      elsif Attribute = Implementation_Suffix_Attribute then
         Value := Value_Of
           (Index    => Get_String (Index),
            In_Array => Prj.Projects.Table (View).Naming.Bodies);

      elsif Attribute = Separate_Suffix_Attribute then
         return Get_String (Prj.Projects.Table (View).Naming.Separate_Suffix);

      elsif Attribute = Casing_Attribute then
         return Prj.Image (Prj.Projects.Table (View).Naming.Casing);

      elsif Attribute = Dot_Replacement_Attribute then
         return Get_String (Prj.Projects.Table (View).Naming.Dot_Replacement);

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

   -------------------------
   -- Get_Attribute_Value --
   -------------------------

   function Get_Attribute_Value
     (Project        : Project_Type;
      Attribute      : Attribute_Pkg) return Associative_Array
   is
      Sep : constant Natural := Split_Package (Attribute);
      Pkg_Name : constant String :=
        String (Attribute (Attribute'First .. Sep - 1));
      Attribute_Name : constant String :=
        String (Attribute (Sep + 1 .. Attribute'Last));
      Pkg : Package_Id := No_Package;
      Arr : Array_Id;
      Elem, Elem2 : Array_Element_Id;
      N    : Name_Id;
      Project_View : constant Project_Id := Get_View (Project);
      Count : Natural := 0;
   begin
      if Project_View = Prj.No_Project then
         return (1 .. 0 => (No_Name, Nil_Variable_Value));
      end if;

      if Pkg_Name /= "" then
         Pkg := Value_Of
           (Get_String (Pkg_Name),
            In_Packages => Prj.Projects.Table (Project_View).Decl.Packages);
         if Pkg = No_Package then
            return (1 .. 0 => (No_Name, Nil_Variable_Value));
         end if;
         Arr := Packages.Table (Pkg).Decl.Arrays;
      else
         Arr := Prj.Projects.Table (Project_View).Decl.Arrays;
      end if;

      N := Get_String (Attribute_Name);
      Elem := Value_Of (N, In_Arrays => Arr);

      Elem2 := Elem;
      while Elem2 /= No_Array_Element loop
         Count := Count + 1;
         Elem2 := Array_Elements.Table (Elem2).Next;
      end loop;

      declare
         Result : Associative_Array (1 .. Count);
      begin
         Count := Result'First;

         while Elem /= No_Array_Element loop
            Result (Count) := (Index => Array_Elements.Table (Elem).Index,
                               Value => Array_Elements.Table (Elem).Value);
            Count := Count + 1;
            Elem := Array_Elements.Table (Elem).Next;
         end loop;

         return Result;
      end;
   end Get_Attribute_Value;

   -------------------------
   -- Get_Attribute_Value --
   -------------------------

   function Get_Attribute_Value
     (Project        : Project_Type;
      Attribute      : Attribute_Pkg;
      Index          : String := "") return GNAT.OS_Lib.Argument_List
   is
      No_Value : Argument_List (1 .. 0);
      Value    : constant Variable_Value := Get_Attribute_Value
        (Project, Attribute, Index);
      Val : String_List_Id;
   begin
      case Value.Kind is
         when Undefined =>
            return No_Value;

         when Single =>
            Trace (Me, "Attribute " & String (Attribute) & " is not a list");
            return No_Value;

         when List =>
            declare
               Num    : Natural := Length (Value.Values);
               Result : Argument_List (1 .. Num);
            begin
               Val := Value.Values;
               Num := Result'First;

               while Val /= Nil_String loop
                  Result (Num) := new String'
                    (Get_String (String_Elements.Table (Val).Value));
                  Num := Num + 1;
                  Val := String_Elements.Table (Val).Next;
               end loop;

               return Result;
            end;
      end case;
   end Get_Attribute_Value;

   -------------------
   -- Get_Languages --
   -------------------

   function Get_Languages
     (Project : Project_Type; Recursive : Boolean := False)
      return GNAT.OS_Lib.Argument_List
   is
      Iter : Imported_Project_Iterator := Start (Project, Recursive);
      Num_Languages : Natural := 0;
      Val : Variable_Value;
      Value : String_List_Id;
      Found : Boolean := False;
      P : Project_Type;
   begin
      loop
         P := Current (Iter);
         exit when P = No_Project;

         Num_Languages := Num_Languages + Length
           (Get_Attribute_Value (P, Languages_Attribute).Values);
         Next (Iter);
      end loop;

      Iter := Start (Project, Recursive);

      declare
         Lang  : Argument_List (1 .. Num_Languages);
         Index : Natural := Lang'First;
      begin
         loop
            P := Current (Iter);
            exit when P = No_Project;

            Val := Get_Attribute_Value (P, Languages_Attribute);
            Value := Val.Values;
            Found := False;

            while Value /= Nil_String loop
               declare
                  Str : constant String :=
                    Get_String (String_Elements.Table (Value).Value);
               begin
                  for L in Lang'First .. Index - 1 loop
                     if Lang (L).all = Str then
                        Found := True;
                        exit;
                     end if;
                  end loop;

                  if not Found then
                     Lang (Index) := new String'(Str);
                     Index := Index + 1;
                  end if;
               end;

               Value := String_Elements.Table (Value).Next;
            end loop;

            Next (Iter);
         end loop;

         if Index = Lang'First then
            return (1 => new String'(Ada_String));
         else
            return Lang (Lang'First .. Index - 1);
         end if;
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
      if Filenames_Are_Case_Sensitive then
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
      Exec : constant String := Get_Attribute_Value
        (Project, Attribute => Exec_Dir_Attribute);
   begin
      if Exec /= "" then
         return Name_As_Directory (Exec);
      else
         return Name_As_Directory
           (Object_Path (Project, Recursive => False));
      end if;
   end Executables_Directory;

   -----------
   -- Start --
   -----------

   function Start
     (Root_Project : Project_Type;
      Recursive    : Boolean := True;
      Direct_Only  : Boolean := False;
      Include_Extended : Boolean := True)
      return Imported_Project_Iterator
   is
      Iter : Imported_Project_Iterator;
   begin
      Assert (Me, Root_Project.Data /= null,
              "Start: Uninitialized project passed as argument");

      if Root_Project.Data.Imported_Projects = null then
         Root_Project.Data.Imported_Projects := new Name_Id_Array'
           (Topological_Sort (Root_Project.Node));
         Trace (Me, "Start: compute deps for "
                & Project_Name (Root_Project));

         if Active (Debug) then
            Trace (Debug, "Start: " & Project_Name (Root_Project));
            for N in Root_Project.Data.Imported_Projects'Range loop
               Trace (Debug, "    => "
                      & Get_String (Root_Project.Data.Imported_Projects (N)));
            end loop;
         end if;
      end if;

      if Recursive then
         Iter := Imported_Project_Iterator'
           (Root          => Root_Project,
            Direct_Only   => Direct_Only,
            Importing     => False,
            Current_Cache => No_Project,
            Include_Extended => Include_Extended,
            Current       => Root_Project.Data.Imported_Projects'Last + 1);
         Next (Iter);
         return Iter;
      else
         return Imported_Project_Iterator'
           (Root          => Root_Project,
            Direct_Only   => Direct_Only,
            Importing     => False,
            Current_Cache => No_Project,
            Include_Extended => Include_Extended,
            Current       => Root_Project.Data.Imported_Projects'First);
      end if;
   end Start;

   ---------------------
   -- Project_Imports --
   ---------------------

   function Project_Imports
     (Parent           : Project_Type;
      Child            : Project_Type;
      Include_Extended : Boolean := False) return Boolean
   is
      With_Clause : Project_Node_Id;
      Extended    : Project_Node_Id;
   begin
      Assert (Me, Child /= No_Project, "Project_Imports: no child provided");

      if Parent = No_Project then
         return True;
      end if;

      With_Clause := First_With_Clause_Of (Parent.Node);
      while With_Clause /= Empty_Node loop
         if Project_Node_Of (With_Clause) = Child.Node then
            return True;
         end if;

         With_Clause := Next_With_Clause_Of (With_Clause);
      end loop;

      --  Handling for extending projects ?

      if Include_Extended then
         Extended := Extended_Project_Of
           (Project_Declaration_Of (Parent.Node));
         if Extended = Child.Node then
            return True;
         end if;
      end if;

      return False;
   end Project_Imports;

   --------------------------------
   -- Compute_Importing_Projects --
   --------------------------------

   procedure Compute_Importing_Projects
     (Root_Project : Project_Type;
      Project      : Project_Type)
   is
      type Boolean_Array is array (Positive range <>) of Boolean;
      Imported : Name_Id_Array_Access renames
        Root_Project.Data.Imported_Projects;
      Current  : Project_Type;
      Start    : Project_Type;
      Include  : Boolean_Array (Imported'Range) := (others => False);
      Name     : Name_Id;
      Index    : Integer;
      Parent   : Project_Type;
      Decl, N   : Project_Node_Id;
      Importing : Name_Id_Array_Access;

      procedure Merge_Project (P : Project_Type);
      --  Merge the imported projects of P with the ones for Project

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
         Decl := Project_Declaration_Of (N);
         exit when Extending_Project_Of (Decl) = Empty_Node;
         N := Extending_Project_Of (Decl);
      end loop;

      Current := Get_Project_From_Name
        (Project.Data.Registry.all, Prj.Tree.Name_Of (N));
      Start := Current;

      loop
         Index := Imported'Last;

         --  We first start by the lowest possible project, then go up to the
         --  root project. Note that no project that appears before Project can
         --  import it, so we can save some time.

         Name := Prj.Tree.Name_Of (Current.Node);
         while Index >= Imported'First loop
            exit when Name = Imported (Index);
            Index := Index - 1;
         end loop;

         Include (Index) := True;
         Index := Index - 1;

         while Index >= Imported'First loop
            Parent := Get_Project_From_Name
              (Current.Data.Registry.all, Imported (Index));

            --  Avoid processing a project twice
            --  ??? We still process twice the projects that do not
            --  import Project
            if not Include (Index)
              and then Project_Imports
              (Parent, Child => Current, Include_Extended => False)
            then
               Compute_Importing_Projects (Root_Project, Parent);
               Merge_Project (Parent);
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
      when E : others =>
         Trace (Me, "Unexpected exception " & Exception_Information (E));
   end Compute_Importing_Projects;

   ---------------------------------
   -- Find_All_Projects_Importing --
   ---------------------------------

   function Find_All_Projects_Importing
     (Root_Project : Project_Type;
      Project      : Project_Type;
      Include_Self : Boolean := False;
      Direct_Only  : Boolean := False) return Imported_Project_Iterator
   is
      Iter : Imported_Project_Iterator;
   begin
      if Project = No_Project then
         return Start (Root_Project, Recursive => True);
      end if;

      if Project.Data.Importing_Projects = null then
         if Root_Project.Data.Imported_Projects = null then
            Root_Project.Data.Imported_Projects := new Name_Id_Array'
              (Topological_Sort (Root_Project.Node));
            Trace (Me, "Start: recomputing dependencies for "
                   & Project_Name (Root_Project));
         end if;

         Compute_Importing_Projects (Root_Project, Project);
      end if;

      Iter := Imported_Project_Iterator'
        (Root          => Project,
         Direct_Only   => Direct_Only,
         Importing     => True,
         Current_Cache => No_Project,
         Include_Extended => True,   --  ??? Should this be configurable
         Current       => Project.Data.Importing_Projects'Last + 1);

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

   function Current (Iterator : Imported_Project_Iterator)
      return Project_Type
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

   ----------
   -- Next --
   ----------

   procedure Next (Iterator : in out Imported_Project_Iterator) is
   begin
      Iterator.Current_Cache := No_Project;
      Iterator.Current := Iterator.Current - 1;

      if Iterator.Direct_Only then
         if Iterator.Importing then
            while Iterator.Current >=
              Iterator.Root.Data.Importing_Projects'First
              and then not Project_Imports
                 (Current (Iterator), Iterator.Root, Iterator.Include_Extended)
            loop
               Iterator.Current := Iterator.Current - 1;
            end loop;

         else
            while Iterator.Current >=
              Iterator.Root.Data.Imported_Projects'First
              and then not Project_Imports
                 (Iterator.Root, Current (Iterator), Iterator.Include_Extended)
            loop
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
         Expr : Project_Node_Id := Expression_Of (Var);
      begin
         Expr := First_Term   (Expr);
         Expr := Current_Term (Expr);

         if Kind_Of (Expr) = N_External_Value then
            Expr := External_Default_Of (Expr);

            if Expr = Empty_Node then
               return No_Name;
            end if;

            if Kind_Of (Expr) /= N_Literal_String then
               Expr := First_Term (Expr);
               Assert (Me, Next_Term (Expr) = Empty_Node,
                       "Default value cannot be a concatenation");

               Expr := Current_Term (Expr);
               Assert (Me, Kind_Of (Expr) = N_Literal_String,
                       "Default value can only be literal string");
            end if;

            return String_Value_Of (Expr);
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
         V : constant Name_Id := External_Reference_Of (Var);
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
            String_Type => String_Type_Of (Var));
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

   procedure Ensure_External_Value (Var : Scenario_Variable) is
      N : constant String := External_Reference_Of (Var);
   begin
      if Prj.Ext.Value_Of (Var.Name) = No_Name then
         if Var.Default /= No_Name then
            Prj.Ext.Add (N, External_Default (Var));
         else
            Get_Name_String
              (String_Value_Of (First_Literal_String (Var.String_Type)));
            Prj.Ext.Add
              (N, Name_Buffer (Name_Buffer'First .. Name_Len));
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
      Project.Data.View       := Prj.No_Project;
      --  No need to reset Project.Data.Imported_Projects, since this doesn't
      --  change when the view changes

      Free (Project.Data.Non_Recursive_Include_Path);

      Reset (Project.Data.Directories);
   end Reset;

   --------------
   -- Get_View --
   --------------

   function Get_View (Name : Name_Id) return Prj.Project_Id is
   begin
      for J in Prj.Projects.Table'First .. Prj.Projects.Last loop
         if Prj.Projects.Table (J).Name = Name then
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
         Trace (Me, "Get_View: Project is No_Project");
         return Prj.No_Project;
      elsif Project.Data.View = Prj.No_Project then
         Project.Data.View := Get_View (Prj.Tree.Name_Of (Project.Node));
      end if;

      if Project.Data.View = Prj.No_Project then
         Trace (Me, "Get_View: No view computed for "
                & Project_Name (Project));
      end if;
      return Project.Data.View;
   end Get_View;

   ---------------------
   -- Check_Full_File --
   ---------------------

   function Check_Full_File
     (File : Name_Id; List : Array_Element_Id) return Array_Element_Id
   is
      Prefix : Array_Element_Id := List;
      Str    : String_List_Id;
   begin
      while Prefix /= No_Array_Element loop
         case Array_Elements.Table (Prefix).Value.Kind is
            when Undefined =>
               null;

            --  Naming exceptions for languages other than Ada
            when Prj.List =>
               Str := Array_Elements.Table (Prefix).Value.Values;
               while Str /= Nil_String loop
                  if String_Elements.Table (Str).Value = File then
                     return Prefix;
                  end if;
                  Str := String_Elements.Table (Str).Next;
               end loop;

            --  Naming exceptions for Ada
            when Single =>
               if Array_Elements.Table (Prefix).Value.Value = File then
                  return Prefix;
               end if;
         end case;

         Prefix := Array_Elements.Table (Prefix).Next;
      end loop;
      return No_Array_Element;
   end Check_Full_File;

   ----------------------
   -- Create_From_Node --
   ----------------------

   procedure Create_From_Node
     (Project  : out Project_Type;
      Registry : Abstract_Registry'Class;
      Node     : Prj.Tree.Project_Node_Id) is
   begin
      Assert (Me, Registry in Project_Registry'Class,
              "Invalid type for Registry");
      Project.Node := Node;

      if Project.Data = null then
         Project.Data := new Project_Type_Data;
      end if;

      Project.Data.Registry := new Project_Registry'Class'
        (Project_Registry'Class (Registry));
      Project.Data.View := Prj.No_Project;

      Trace (Me, "Create_From_Node: "  & Project_Name (Project));
   end Create_From_Node;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Project : in out Project_Type) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Project_Type_Data, Project_Type_Data_Access);
   begin
      Unchecked_Free (Project.Data.Imported_Projects);
      Unchecked_Free (Project.Data.Importing_Projects);
      Unchecked_Free (Project.Data.Registry);
      Unchecked_Free (Project.Data);
      Project := No_Project;
   end Destroy;

   --------------------------------------------
   -- For_Each_External_Variable_Declaration --
   --------------------------------------------

   procedure For_Each_External_Variable_Declaration
     (Project   : Project_Type;
      Recursive : Boolean;
      Callback  : External_Variable_Callback)
   is
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
            Var := First_Variable_Of (Pkg);
            while Var /= Empty_Node loop
               if Kind_Of (Var) = N_Typed_Variable_Declaration
                 and then Is_External_Variable (Var)
                 and then not Callback (Var, Prj)
               then
                  exit;
               end if;

               Var := Next_Variable (Var);
            end loop;

            if Pkg = Prj then
               Pkg := First_Package_Of (Prj);
            else
               Pkg := Next_Package_In_Project (Pkg);
            end if;
         end loop;
      end Process_Prj;

      Iterator : Imported_Project_Iterator := Start (Project, Recursive);
      P : Project_Type;
   begin
      loop
         P := Current (Iterator);
         exit when P = No_Project;

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
      File             : String;
      Language         : Types.Name_Id;
      Value            : out Variable_Value;
      Is_Default_Value : out Boolean) is
   begin
      --  Do we have some file-specific switches ?
      if File /= "" then
         Value := Get_Attribute_Value
           (Project        => Project,
            Attribute      =>
              Attribute_Pkg (In_Pkg & '#' & Get_String (Name_Switches)),
            Index          => File);

         if Value /= Nil_Variable_Value then
            Is_Default_Value := False;
            return;
         end if;
      end if;

      Value := Get_Attribute_Value
        (Project        => Project,
         Attribute      =>
           Attribute_Pkg (In_Pkg & '#' & Get_String (Name_Default_Switches)),
         Index          => Get_String (Language));

      Is_Default_Value := True;
   end Get_Switches;

   --------------------------
   -- Is_External_Variable --
   --------------------------

   function Is_External_Variable (Var : Project_Node_Id) return Boolean is
   begin
      return Kind_Of (Current_Term (First_Term (Expression_Of (Var))))
        = N_External_Value;
   end Is_External_Variable;

   ---------------------------
   -- External_Reference_Of --
   ---------------------------

   function External_Reference_Of (Var : Project_Node_Id) return Name_Id
   is
      Expr : Project_Node_Id := Expression_Of (Var);
   begin
      Expr := First_Term   (Expr);
      Expr := Current_Term (Expr);

      if Kind_Of (Expr) = N_External_Value then
         Expr := External_Reference_Of (Expr);
         return String_Value_Of (Expr);
      else
         return No_Name;
      end if;
   end External_Reference_Of;

   ----------------
   -- Is_Default --
   ----------------

   function Is_Default (Project : Project_Type) return Boolean is
   begin
      return Project.Data.Is_Default;
   end Is_Default;

   --------------------
   -- Set_Is_Default --
   --------------------

   procedure Set_Is_Default (Project : Project_Type; Default : Boolean) is
   begin
      Project.Data.Is_Default := Default;
   end Set_Is_Default;

   --------------
   -- Value_Of --
   --------------

   function Value_Of (Var : Scenario_Variable) return String is
   begin
      return Get_String
        (Prj.Ext.Value_Of (Var.Name, With_Default => Var.Default));
   end Value_Of;

   ----------------
   -- Get_String --
   ----------------

   function Get_String (Id : Types.Name_Id) return String is
   begin
      if Id = No_Name then
         return "";
      end if;

      return Get_Name_String (Id);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
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
        (Project_Declaration_Of (Project.Node));
   begin
      if Extend = Empty_Node then
         return No_Project;
      else
         return Get_Project_From_Name
           (Project.Data.Registry.all, Prj.Tree.Name_Of (Extend));
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
     (Project : Project_Type; Recurse : Boolean := False)
      return Project_Type
   is
      Extend : Project_Node_Id;
   begin
      if Recurse then
         Extend := Project.Node;
         while Extending_Project_Of
           (Project_Declaration_Of (Extend)) /= Empty_Node
         loop
            Extend := Extending_Project_Of
              (Project_Declaration_Of (Extend));
         end loop;
      else
         Extend := Extending_Project_Of
           (Project_Declaration_Of (Project.Node));
      end if;

      if Extend = Empty_Node then
         return No_Project;
      else
         return Get_Project_From_Name
           (Project.Data.Registry.all, Prj.Tree.Name_Of (Extend));
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
      return Project.Data.Is_Default
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

   function Build (Package_Name, Attribute_Name : String)
      return Attribute_Pkg is
   begin
      return Attribute_Pkg (Package_Name & '#' & Attribute_Name);
   end Build;

   -------------------------
   -- Get_Executable_Name --
   -------------------------

   function Get_Executable_Name
     (Project : Project_Type; File : String) return String
   is
      Base : constant String := Base_Name (File);
   begin
      if Project = No_Project then
         --  Simply remove the current extension, since we don't have any
         --  information on the file itself.
         for Index in reverse Base'Range loop
            if Base (Index) = '.' then
               return Base (Base'First .. Index - 1);
            end if;
         end loop;

         return Base;
      else
         declare
            Default_Exec : constant String := Base
              (Base'First .. Delete_File_Suffix (Base, Project));
            From_Project : constant String := Get_Attribute_Value
              (Project, Executable_Attribute,
               Index => File, Default => Default_Exec);
         begin
            --  If the executable name was uninitialized
            if From_Project = "" then
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

end Projects;
