-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                 Copyright (C) 2006-2008, AdaCore                  --
--                                                                   --
-- GPS is Free  software;  you can redistribute it and/or modify  it --
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

with Ada.Calendar;              use Ada.Calendar;
with GNAT.Strings;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with GPS.Kernel.Console;        use GPS.Kernel.Console;
with GPS.Kernel.Contexts;       use GPS.Kernel.Contexts;
with GPS.Kernel.Project;        use GPS.Kernel.Project;
with GPS.Kernel.Standard_Hooks; use GPS.Kernel.Standard_Hooks;
with GPS.Location_View;         use GPS.Location_View;
with Default_Preferences.Enums;

with Language;                  use Language;
with Projects;                  use Projects;
with Projects.Registry;         use Projects.Registry;
with Language.Unknown;          use Language.Unknown;
with Language.Tree;             use Language.Tree;
with Language_Handlers;         use Language_Handlers;
with String_Utils;              use String_Utils;
with Code_Coverage;             use Code_Coverage;
with Code_Analysis_GUI;
with Code_Coverage.Gcov;
with Code_Coverage.Xcov;

package body Coverage_GUI is

   package Coverage_Toolchain_Preferences is
      new Default_Preferences.Enums.Generics (Coverage_Toolchain_Kinds);

   Coverage_Toolchain_Preference : Coverage_Toolchain_Preferences.Preference;

   ---------------------------
   -- Add_Gcov_Project_Info --
   ---------------------------

   procedure Add_Gcov_Project_Info
     (Kernel   : Kernel_Handle;
      Prj_Node : Project_Access)
   is
      Src_Files : File_Array_Access;
      Src_File  : Virtual_File;
      Cov_File  : Virtual_File;
   begin
      --  get every source files of the project
      --  check if they are associated with gcov info
      --  load their info
      Src_Files := Get_Source_Files (Prj_Node.Name, Recursive => False);

      for J in Src_Files'First .. Src_Files'Last loop
         Src_File := Src_Files (J);
         Cov_File := Find_Gcov_File (Kernel, Src_File);

         if Is_Regular_File (Cov_File) then
            Add_Gcov_File_Info (Kernel, Src_File, Cov_File, Prj_Node);
         else
            GPS.Kernel.Console.Insert
              (Kernel,
               -"Could not find coverage file " & Full_Name (Cov_File).all);

            declare
               File_Node : constant Code_Analysis.File_Access :=
                             Get_Or_Create (Prj_Node, Src_File);
            begin
               Set_Error (File_Node, File_Not_Found);
            end;
         end if;
      end loop;

      Compute_Project_Coverage (Prj_Node);
   exception
      when E : others => Trace (Exception_Handle, E);
   end Add_Gcov_Project_Info;

   ------------------------
   -- Add_Gcov_File_Info --
   ------------------------

   procedure Add_Gcov_File_Info
     (Kernel       : Kernel_Handle;
      Src_File     : Virtual_File;
      Cov_File     : GNATCOLL.VFS.Virtual_File;
      Project_Node : Project_Access)
   is
      File_Contents : GNAT.Strings.String_Access;
      File_Node     : constant Code_Analysis.File_Access
        := Get_Or_Create (Project_Node, Src_File);
      Handler       : constant Language_Handler
        := Get_Language_Handler (Kernel);
   begin
      if File_Time_Stamp (Src_File) > File_Time_Stamp (Cov_File) then
         GPS.Kernel.Console.Insert
           (Kernel, Base_Name (Src_File) &
         (-" has been modified since GCOV information were generated.") &
         (-" Skipped."),
            Mode => GPS.Kernel.Console.Error);
         Set_Error (File_Node, File_Out_Of_Date);
      else
         declare
            Contents : String_Access := Read_File (Cov_File);
            Last     : Integer;
            CR_Found : Boolean;
         begin
            Strip_CR (Contents.all, Last, CR_Found);

            if CR_Found then
               File_Contents := new String'(Contents (Contents'First .. Last));
               Free (Contents);
            else
               File_Contents := Contents;
            end if;
         end;

         case Current_Coverage_Tool is
            when Gcov =>
               Code_Coverage.Gcov.Add_File_Info (File_Node, File_Contents);

            when Xcov =>
               Code_Coverage.Xcov.Add_File_Info (File_Node, File_Contents);
         end case;

         --  Check for project runs info
         if File_Node.Analysis_Data.Coverage_Data.Is_Valid and then
           Project_Node.Analysis_Data.Coverage_Data = null
         then
            Project_Node.Analysis_Data.Coverage_Data := new Project_Coverage;
            Project_Coverage
              (Project_Node.Analysis_Data.Coverage_Data.all).Status := Valid;
            Get_Runs_Info_From_File
              (File_Contents,
               Project_Coverage
                 (Project_Node.Analysis_Data.Coverage_Data.all).Runs,
               Project_Coverage
                 (Project_Node.Analysis_Data.Coverage_Data.all).Have_Runs);
         end if;

         if File_Node.Analysis_Data.Coverage_Data.Is_Valid then
            declare
               Lang : constant Language_Access :=
                        Get_Language_From_File (Handler, Src_File);
            begin
               if Lang /= Unknown_Lang then
                  Add_Subprogram_Info
                    (File_Node, To_Construct_Tree
                       (Read_File (Src_File).all, Lang));
               end if;
            end;
         end if;

         Free (File_Contents);
      end if;
   end Add_Gcov_File_Info;

   -----------------------------
   -- Clear_Project_Locations --
   -----------------------------

   procedure Clear_Project_Locations
     (Kernel : Kernel_Handle; Project_Node : Project_Access)
   is
      use File_Maps;
      Map_Cur : File_Maps.Cursor := Project_Node.Files.First;
   begin
      for J in 1 .. Integer (Project_Node.Files.Length) loop
         Clear_File_Locations (Kernel, Element (Map_Cur));
         Next (Map_Cur);
      end loop;
   end Clear_Project_Locations;

   --------------------------
   -- Clear_File_Locations --
   --------------------------

   procedure Clear_File_Locations
     (Kernel    : Kernel_Handle;
      File_Node : Code_Analysis.File_Access) is
   begin
      Remove_Location_Category (Kernel, Uncovered_Category, File_Node.Name);
      Remove_Location_Category
        (Kernel, Partially_Covered_Category, File_Node.Name);
   end Clear_File_Locations;

   --------------------------------
   -- Clear_Subprogram_Locations --
   --------------------------------

   procedure Clear_Subprogram_Locations
     (Kernel    : Kernel_Handle;
      File_Node : Code_Analysis.File_Access;
      Subp_Node : Subprogram_Access) is
   begin
      if File_Node.Analysis_Data.Coverage_Data.Is_Valid then
         for J in Subp_Node.Start .. Subp_Node.Stop loop
            if File_Node.Lines (J) /= Null_Line and then
              File_Node.Lines (J).Analysis_Data.Coverage_Data.Coverage = 0 then
               Remove_Location_Category
                 (Kernel,
                  Uncovered_Category,
                  File_Node.Name,
                  File_Node.Lines (J).Number);
               Remove_Location_Category
                 (Kernel,
                  Partially_Covered_Category,
                  File_Node.Name,
                  File_Node.Lines (J).Number);
            end if;
         end loop;
      end if;
   end Clear_Subprogram_Locations;

   -----------------------------------
   -- Add_File_Coverage_Annotations --
   -----------------------------------

   procedure Add_File_Coverage_Annotations
     (Kernel    : Kernel_Handle;
      File_Node : Code_Analysis.File_Access)
   is
      Line_Info : Line_Information_Data;
   begin
      Code_Analysis_GUI.Initialize_Graphics (Kernel);

      if File_Node.Analysis_Data.Coverage_Data.Is_Valid then
         Line_Info  := new Line_Information_Array (File_Node.Lines'Range);

         for J in File_Node.Lines'Range loop
            if File_Node.Lines (J) /= Null_Line then
               Line_Info (J) := Line_Coverage_Info
                 (Line_Coverage'Class
                    (File_Node.Lines (J).Analysis_Data.Coverage_Data.all),
                  Binary_Coverage_Mode);
            else
               Line_Info (J).Text := new String'(" ");
            end if;
         end loop;

         Create_Line_Information_Column
           (Kernel, File_Node.Name, CodeAnalysis_Cst);
         Add_Line_Information
           (Kernel, File_Node.Name, CodeAnalysis_Cst, Line_Info);
         Unchecked_Free (Line_Info);
      end if;
   end Add_File_Coverage_Annotations;

   --------------------------------------
   -- Remove_File_Coverage_Annotations --
   --------------------------------------

   procedure Remove_File_Coverage_Annotations
     (Kernel    : Kernel_Handle;
      File_Node : Code_Analysis.File_Access) is
   begin
      Remove_Line_Information_Column
        (Kernel, File_Node.Name, CodeAnalysis_Cst);
   exception
      when E : others => Trace (Exception_Handle, E);
   end Remove_File_Coverage_Annotations;

   -------------------------------
   -- List_File_Uncovered_Lines --
   -------------------------------

   procedure List_File_Uncovered_Lines
     (Kernel    : Kernel_Handle;
      File_Node : Code_Analysis.File_Access)
   is
      File_Added : Boolean := False;
   begin
      if File_Node.Analysis_Data.Coverage_Data.Is_Valid then
         for J in File_Node.Lines'Range loop
            if File_Node.Lines (J) /= Null_Line then
               Line_Coverage'Class
                 (File_Node.Lines
                    (J).Analysis_Data.Coverage_Data.all).
                 Add_Location_If_Uncovered
                 (Kernel,
                  File_Node.Name,
                  J,
                  File_Node.Lines (J).Contents,
                  File_Added);
            end if;
         end loop;

         if not File_Added then
            GPS.Kernel.Console.Insert
              (Kernel, -"There is no uncovered line in " &
               Base_Name (File_Node.Name));
         end if;
      else
         GPS.Kernel.Console.Insert
           (Kernel, -"There is no Gcov information associated with " &
            Base_Name (File_Node.Name),
            Mode => GPS.Kernel.Console.Info);
      end if;
   end List_File_Uncovered_Lines;

   ----------------------------------
   -- List_Project_Uncovered_Lines --
   ----------------------------------

   procedure List_Project_Uncovered_Lines
     (Kernel : Kernel_Handle; Project_Node : Project_Access)
   is
      use File_Maps;
      Map_Cur  : File_Maps.Cursor := Project_Node.Files.First;
      Sort_Arr : Code_Analysis.File_Array
        (1 .. Integer (Project_Node.Files.Length));
   begin
      for J in Sort_Arr'Range loop
         Sort_Arr (J) := Element (Map_Cur);
         Next (Map_Cur);
      end loop;

      Sort_Files (Sort_Arr);

      for J in Sort_Arr'Range loop
         if Sort_Arr (J).Analysis_Data.Coverage_Data /= null then
            List_File_Uncovered_Lines (Kernel, Sort_Arr (J));
         end if;
      end loop;
   end List_Project_Uncovered_Lines;

   --------------------------------------
   -- Add_Project_Coverage_Annotations --
   --------------------------------------

   procedure Add_Project_Coverage_Annotations
     (Kernel : Kernel_Handle; Project_Node : Project_Access)
   is
      use File_Maps;
      Map_Cur  : File_Maps.Cursor := Project_Node.Files.First;
      Sort_Arr : Code_Analysis.File_Array
        (1 .. Integer (Project_Node.Files.Length));
   begin
      for J in Sort_Arr'Range loop
         Sort_Arr (J) := Element (Map_Cur);
         Next (Map_Cur);
      end loop;

      Sort_Files (Sort_Arr);

      for J in Sort_Arr'Range loop
         if Sort_Arr (J).Analysis_Data.Coverage_Data /= null then
            Add_File_Coverage_Annotations (Kernel, Sort_Arr (J));
         end if;
      end loop;
   end Add_Project_Coverage_Annotations;

   -----------------------------------------
   -- Remove_Project_Coverage_Annotations --
   -----------------------------------------

   procedure Remove_Project_Coverage_Annotations
     (Kernel : Kernel_Handle; Project_Node : Project_Access)
   is
      use File_Maps;
      Map_Cur : File_Maps.Cursor := Project_Node.Files.First;
   begin
      for J in 1 .. Integer (Project_Node.Files.Length) loop
         Remove_File_Coverage_Annotations (Kernel, Element (Map_Cur));
         Next (Map_Cur);
      end loop;
   end Remove_Project_Coverage_Annotations;

   -------------------------------------
   -- List_Subprogram_Uncovered_Lines --
   -------------------------------------

   procedure List_Subprogram_Uncovered_Lines
     (Kernel    : Kernel_Handle;
      File_Node : Code_Analysis.File_Access;
      Subp_Node : Subprogram_Access)
   is
      Added : Boolean := False;
   begin
      if File_Node.Analysis_Data.Coverage_Data.Is_Valid then
         for J in Subp_Node.Start .. Subp_Node.Stop loop
            if File_Node.Lines (J) /= Null_Line then
               Line_Coverage'Class
                 (File_Node.Lines (J).Analysis_Data.Coverage_Data.all).
                 Add_Location_If_Uncovered
                   (Kernel,
                    File_Node.Name,
                    J,
                    File_Node.Lines (J).Contents,
                    Added);
            end if;
         end loop;
      else
         GPS.Kernel.Console.Insert
           (Kernel, -"There is no Gcov information associated with " &
            Base_Name (File_Node.Name),
            Mode => GPS.Kernel.Console.Error);
      end if;
   end List_Subprogram_Uncovered_Lines;

   -----------------------------------
   -- Show_All_Coverage_Information --
   -----------------------------------

   procedure Show_All_Coverage_Information
     (Kernel   : Kernel_Handle;
      Projects : Code_Analysis_Tree)
   is
      use Project_Maps;
      Map_Cur  : Project_Maps.Cursor := Projects.First;
      Sort_Arr : Project_Array (1 .. Integer (Projects.Length));
   begin

      for J in Sort_Arr'Range loop
         Sort_Arr (J) := Element (Map_Cur);
         Next (Map_Cur);
      end loop;

      Sort_Projects (Sort_Arr);

      for J in Sort_Arr'Range loop
         List_Project_Uncovered_Lines (Kernel, Sort_Arr (J));
         Add_Project_Coverage_Annotations (Kernel, Sort_Arr (J));
      end loop;
   end Show_All_Coverage_Information;

   -----------------------------------
   -- Hide_All_Coverage_Information --
   -----------------------------------

   procedure Hide_All_Coverage_Information
   (Kernel   : Kernel_Handle;
    Projects : Code_Analysis_Tree)
   is
      use Project_Maps;
      Map_Cur : Project_Maps.Cursor := Projects.First;
   begin
      for J in 1 .. Integer (Projects.Length) loop
         Clear_Project_Locations (Kernel, Element (Map_Cur));
         Remove_Project_Coverage_Annotations (Kernel, Element (Map_Cur));
         Next (Map_Cur);
      end loop;
   end Hide_All_Coverage_Information;

   --------------------
   -- Find_Gcov_File --
   --------------------

   function Find_Gcov_File
     (Kernel : Kernel_Handle;
      Source : GNATCOLL.VFS.Virtual_File) return GNATCOLL.VFS.Virtual_File
   is
      Gcov_Root : String_Access;
      Result    : GNATCOLL.VFS.Virtual_File;
   begin
      case Current_Coverage_Tool is
         when Gcov =>
            Gcov_Root := Getenv ("GCOV_ROOT");

            if Gcov_Root /= null
              and then Gcov_Root.all = ""
            then
               --  If GCOV_ROOT is set but empty, look for files in the object
               --  directory of the root project.
               Free (Gcov_Root);
            end if;

            if Gcov_Root = null then
               --  Look for the gcov file in the object directory of the root
               --  project.
               return Create
                 (Object_Path
                    (Get_Root_Project
                       (Get_Registry (Kernel).all), False, False)
                  & Directory_Separator & Base_Name (Source)
                  & Gcov_Extension_Cst);

            else
               --  Look for the gcov file in the path pointed by GCOV_ROOT
               Result := Create
                 (Gcov_Root.all & Directory_Separator &
                  Base_Name (Source) & Gcov_Extension_Cst);

               Free (Gcov_Root);
               return Result;
            end if;

         when Xcov =>
            return Create
              (Object_Path
                 (Get_Root_Project
                    (Get_Registry (Kernel).all), False, False)
               & Directory_Separator & Base_Name (Source)
               & Xcov_Extension_Cst);
      end case;
   end Find_Gcov_File;

   --------------------
   -- Have_Gcov_Info --
   --------------------

   function Have_Gcov_Info
     (Projects : Code_Analysis_Tree;
      Context  : Selection_Context) return Boolean
   is
      Prj_Node : Code_Analysis.Project_Access;
   begin
      Prj_Node := Get_Or_Create (Projects, Project_Information (Context));

      if Has_File_Information (Context) then
         declare
            File_Node : Code_Analysis.File_Access;
         begin
            File_Node := Get_Or_Create
              (Prj_Node, File_Information (Context));

            if File_Node.Analysis_Data.Coverage_Data /= null
              and then File_Node.Analysis_Data.Coverage_Data.Is_Valid
            then
               return True;
            end if;
         end;

      else
         if Prj_Node.Analysis_Data.Coverage_Data /= null
           and then Prj_Node.Analysis_Data.Coverage_Data.Is_Valid
         then
            return True;
         end if;
      end if;

      return False;
   end Have_Gcov_Info;

   ---------------------------
   -- Current_Coverage_Tool --
   ---------------------------

   function Current_Coverage_Tool return Coverage_Toolchain_Kinds is
   begin
      return Coverage_Toolchain_Preference.Get_Pref;
   end Current_Coverage_Tool;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
   begin
      Coverage_Toolchain_Preference :=
        Coverage_Toolchain_Preferences.Create
          (Kernel.Get_Preferences,
           "coverage-toolchain",
           "Coverage toolchain",
           "Coverage Analysis",
           "",
           Gcov);
   end Register_Module;

end Coverage_GUI;
