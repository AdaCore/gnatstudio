------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2006-2017, AdaCore                     --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

with Ada.Calendar;                    use Ada.Calendar;
with Ada.Strings.Unbounded;           use Ada.Strings.Unbounded;
with GNAT.Strings;
with GNAT.OS_Lib;                     use GNAT.OS_Lib;

with GNATCOLL.Projects;               use GNATCOLL.Projects;
with GPS.Kernel.Hooks;                use GPS.Kernel.Hooks;
with GPS.Kernel.Locations;            use GPS.Kernel.Locations;
with GPS.Kernel.Messages;             use GPS.Kernel.Messages;
with GPS.Kernel.Project;              use GPS.Kernel.Project;
with Default_Preferences.Enums;

with Language;                        use Language;
with Language.Abstract_Language_Tree; use Language.Abstract_Language_Tree;
with Language.Unknown;                use Language.Unknown;
with Language_Handlers;               use Language_Handlers;
with String_Utils;                    use String_Utils;
with Code_Coverage;                   use Code_Coverage;
with Code_Coverage.Gcov;
with Code_Coverage.GNATcov;
with GPS.Editors; use GPS.Editors;
with GPS.Editors.Line_Information;    use GPS.Editors.Line_Information;

package body Coverage_GUI is

   package Coverage_Toolchain_Preferences is
      new Default_Preferences.Enums.Generics (Coverage_Toolchain_Kinds);

   Coverage_Toolchain_Preference : Coverage_Toolchain_Preferences.Preference;

   Space_String : constant Unbounded_String := To_Unbounded_String (" ");

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
      Src_Files := Prj_Node.Name.Source_Files (Recursive => False);

      for J in Src_Files'First .. Src_Files'Last loop
         Src_File := Src_Files (J);
         Cov_File := Find_Gcov_File (Kernel, Src_File);

         if Is_Regular_File (Cov_File) then
            Add_Gcov_File_Info (Kernel, Src_File, Cov_File, Prj_Node);
         else
            Kernel.Insert
              (-"Could not find coverage file " &
               Display_Full_Name (Cov_File));

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
      when GNATCOLL.VFS.VFS_Invalid_File_Error =>
         null;
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
         Kernel.Insert
           (Display_Base_Name (Src_File) &
            (-" has been modified since GCOV information were generated.") &
            (-" Skipped."),
            Mode => GPS.Kernel.Error);
         Set_Error (File_Node, File_Out_Of_Date);
      else
         declare
            Contents : GNAT.Strings.String_Access := Read_File (Cov_File);
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

            when GNATcov =>
               Code_Coverage.GNATcov.Add_File_Info (File_Node, File_Contents);
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
                  declare
                     Tree : aliased Semantic_Tree'Class :=
                              Kernel.Get_Abstract_Tree_For_File
                                ("COV", File_Node.Name);
                  begin
                     Add_Subprogram_Info
                       (File_Node, Tree'Access);
                  end;
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
      Get_Messages_Container (Kernel).Remove_File
        (Coverage_GUI.Uncovered_Category,
         File_Node.Name,
         Coverage_Message_Flags);
      Get_Messages_Container (Kernel).Remove_File
        (Coverage_GUI.Partially_Covered_Category,
         File_Node.Name,
         Coverage_Message_Flags);
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
              File_Node.Lines (J).Analysis_Data.Coverage_Data.Coverage = 0
            then
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
   begin
      if File_Node.Analysis_Data.Coverage_Data.Is_Valid then
         declare
            Line_Info : Line_Information_Array (File_Node.Lines'Range);
         begin
            for J in File_Node.Lines'Range loop
               if File_Node.Lines (J) /= Null_Line then
                  declare
                     Line_Cov : Line_Coverage'Class renames
                       Line_Coverage'Class
                         (File_Node.Lines (J).Analysis_Data.Coverage_Data.all);
                  begin
                     Line_Info (J) := Line_Coverage_Info
                       (Line_Cov'Access,
                        Kernel,
                        Binary_Coverage_Mode);
                  end;
               else
                  Line_Info (J).Text := Space_String;
               end if;
            end loop;

            Create_Line_Information_Column
              (Kernel, File_Node.Name, CodeAnalysis_Cst);
            Add_Line_Information
              (Kernel, File_Node.Name, CodeAnalysis_Cst, Line_Info);
         end;
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
   end Remove_File_Coverage_Annotations;

   -------------------------------
   -- List_File_Uncovered_Lines --
   -------------------------------

   procedure List_File_Uncovered_Lines
     (Kernel    : Kernel_Handle;
      File_Node : Code_Analysis.File_Access;
      Quiet     : Boolean;
      Allow_Auto_Jump_To_First : Boolean)
   is
      pragma Unreferenced (Quiet);

      File_Added : Boolean := False;
   begin
      Clear_File_Locations (Kernel, File_Node);

      if File_Node.Analysis_Data.Coverage_Data.Is_Valid then
         for J in File_Node.Lines'Range loop
            if File_Node.Lines (J) /= Null_Line then
               Add_Location_If_Uncovered
                 (Line_Coverage'Class
                    (File_Node.Lines (J).Analysis_Data.Coverage_Data.all),
                  Kernel,
                  File_Node.Name,
                  J,
                  File_Node.Lines (J).Contents,
                  File_Added,
                  Allow_Auto_Jump_To_First => Allow_Auto_Jump_To_First);
            end if;
         end loop;

         if not File_Added then
            Kernel.Insert
              (-"There is no uncovered line in " &
               Display_Base_Name (File_Node.Name));
         end if;
      else
         Kernel.Insert
           (-"There is no Gcov information associated with " &
            Display_Base_Name (File_Node.Name));
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
            List_File_Uncovered_Lines
              (Kernel, Sort_Arr (J), False,
               Allow_Auto_Jump_To_First => True);
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
                    Added,
                    Allow_Auto_Jump_To_First => False);
            end if;
         end loop;
      else
         Kernel.Insert
           (-"There is no Gcov information associated with " &
            Display_Base_Name (File_Node.Name),
            Mode => GPS.Kernel.Error);
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
      Gcov_Root_Env : GNAT.Strings.String_Access;
      Gcov_Root     : GNATCOLL.VFS.Virtual_File;
   begin
      case Current_Coverage_Tool is
         when Gcov =>
            Gcov_Root_Env := Getenv ("GCOV_ROOT");

            if Gcov_Root_Env /= null
              and then Gcov_Root_Env.all = ""
            then
               --  If GCOV_ROOT is set but empty, look for files in the object
               --  directory of the root project.
               Free (Gcov_Root_Env);
               Gcov_Root_Env := null;
            end if;

            if Gcov_Root_Env = null then
               --  Look for the gcov file in the object directory of the root
               --  project.
               Gcov_Root := Get_Project (Kernel).Object_Dir;
            else
               --  Look for the gcov file in the path pointed by GCOV_ROOT
               Gcov_Root := Create (+Gcov_Root_Env.all);
               Free (Gcov_Root_Env);
            end if;

            if Gcov_Root = No_File then
               Kernel.Insert
                 (-"Could not determine directory for GCOV files: make sure" &
                  " that the root project has an object directory, or that" &
                  " the environment variable GCOV_ROOT is set.",
                  Mode => Error);
            end if;

            return Create_From_Dir
              (Gcov_Root,
               Base_Name (Source) & Gcov_Extension_Cst);

         when GNATcov =>
            return Create_From_Dir
              (Object_Dir (Get_Registry (Kernel).Tree.Root_Project),
               Base_Name (Source) & GNATcov_Extension_Cst);
      end case;
   end Find_Gcov_File;

   --------------------
   -- Have_Gcov_Info --
   --------------------

   function Have_Gcov_Info
     (Projects : Code_Analysis_Tree;
      Project  : Project_Type;
      File     : GNATCOLL.VFS.Virtual_File := GNATCOLL.VFS.No_File)
      return Boolean
   is
      Prj_Node : Code_Analysis.Project_Access;
   begin
      Prj_Node := Get_Or_Create (Projects, Project);

      if File /= No_File then
         declare
            File_Node : Code_Analysis.File_Access;
         begin
            File_Node := Get_Or_Create (Prj_Node, File);

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
      Coverage_Toolchain_Preference := Coverage_Toolchain_Preferences.Create
          (Kernel.Get_Preferences,
           Name  => "Coverage-Toolchain",
           Label => "Coverage toolchain",
           Path  => "Coverage Analysis",
           Doc   => -"Select the toolchain to perform coverage analysis.",
           Default => Gcov);
   end Register_Module;

end Coverage_GUI;
