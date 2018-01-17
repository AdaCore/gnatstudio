------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2006-2018, AdaCore                     --
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

with Ada.Command_Line;         use Ada.Command_Line;
with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Calendar;             use Ada.Calendar;

with Code_Analysis;            use Code_Analysis;
with Code_Analysis_Dump;       use Code_Analysis_Dump;
with Code_Coverage;            use Code_Coverage;
with Code_Coverage.Gcov;       use Code_Coverage.Gcov;

with Language;                 use Language;
with Language.Tree;            use Language.Tree;
with Language.Ada;             use Language.Ada;
with GNAT.Strings;             use GNAT.Strings;
with GNATCOLL.Projects;        use GNATCOLL.Projects;
with GNATCOLL.Symbols;         use GNATCOLL.Symbols;
with GNATCOLL.VFS;             use GNATCOLL.VFS;
with Glib;                     use Glib;
with Gtk.Main;                 use Gtk.Main;
with Gtk.Window;               use Gtk.Window;
with Gtk.Box;                  use Gtk.Box;
with Gtk.Tree_View;            use Gtk.Tree_View;
with Gtk.Tree_Store;           use Gtk.Tree_Store;
with Gtk.Tree_Model;           use Gtk.Tree_Model;
with Gtk.Tree_View_Column;     use Gtk.Tree_View_Column;
with Gtk.Cell_Renderer_Text;   use Gtk.Cell_Renderer_Text;

procedure Code_Analysis_Test is

   Symbols : constant Symbol_Table_Access := Allocate;

   procedure Print_Usage;
   --  Print the correct usage of the program to the standard output

   function Build_Structure (Projects     : Code_Analysis_Tree;
                             File_Name    : String;
                             Project_File : String)
                             return Project_Access;
   --  Build a code_analysis structure from a gcov file

   procedure Build_Display_Destroy (Projects     : Code_Analysis_Tree;
                                    File_Name    : String;
                                    Project_File : String);
   --  reads a gcov output file, built from a given source file name, builds a
   --  Code_Analysis tree structure from it, displays it on the standard output
   --  and cleanly quits
   --  Correct usage: code_analysis_test source_file build_display_destroy
   --  Example:
   --  $ code_analysis_test main.adb build_display_destroy
   --  Project Dummy_Project_Name 22 / 24 1 call(s)
   --    File main.adb 22 / 24
   --      Subprogram Read_File 10 / 10 1 call(s)
   --      Subprogram Main 12 / 14 2 call(s)
   --        Line 1
   --        Line 2
   --        Line 3 4 execution(s)
   --        Line 4 warning: line never executed

   procedure Benchmark (Projects     : in out Code_Analysis_Tree;
                        File_Name    : String;
                        Project_File : String;
                        Project_Num  : String;
                        File_Num     : String);
   --  builds a big Code_Analysis tree structure and outputs the time needed to
   --  build, perform one request, and destroy the structure

   procedure Treeview (Projects     : in out Code_Analysis_Tree;
                       File_Name    : String;
                       Project_File : String);
   --  builds a code_analysis structure and display it in a Gtk_Tree_View

   -----------------
   -- Print_Usage --
   -----------------

   procedure Print_Usage is
   begin
      Put_Line
        ("Usage: code_analysis_test cov_file prj_file tst_name tst_name...");
      Put_Line
        ("Available tests are: build_display_destroy, benchmark, treeview");
      Put_Line
        ("For ``benchmark'' add <num of projects> <num of files per project>");

   end Print_Usage;

   ---------------------
   -- Build_Structure --
   ---------------------

   function Build_Structure (Projects     : Code_Analysis_Tree;
                             File_Name    : String;
                             Project_File : String)
                             return Project_Access
   is
      procedure Project_Error (Msg : String);

      procedure Project_Error (Msg : String) is
      begin
         Put_Line ("Error loading project: " & Msg);
      end Project_Error;

      Src_File_Name : Virtual_File;
      Cov_File_Name : Virtual_File;
      File_Contents : GNAT.Strings.String_Access;
      Tree          : constant Project_Tree_Access := new Project_Tree;
      Project_Node  : Project_Access;
      File_Node     : Code_Analysis.File_Access;
      P_File       : constant Virtual_File := Create_From_Base (+Project_File);
   begin
      Cov_File_Name := Create (+File_Name);
      Src_File_Name := Create
        (+File_Name (File_Name'First .. File_Name'Last - 5));

      Tree.Load
        (Root_Project_Path  => P_File,
         Errors             => Project_Error'Unrestricted_Access);
      Project_Node  := Get_Or_Create (Projects, Tree.Root_Project);
      File_Contents := Read_File (Cov_File_Name);
      File_Node     := Get_Or_Create (Project_Node, Src_File_Name);
      File_Node.Analysis_Data.Coverage_Data := new File_Coverage;
      Add_File_Info (File_Node, File_Contents);

      -------------------------
      -- Add_Subprogram_Info --
      -------------------------

      if File_Node.Analysis_Data.Coverage_Data.Is_Valid then
         Project_Node.Analysis_Data.Coverage_Data := new Project_Coverage;
            Get_Runs_Info_From_File
              (File_Contents,
               Project_Coverage
                 (Project_Node.Analysis_Data.Coverage_Data.all).Runs,
               Project_Coverage
                 (Project_Node.Analysis_Data.Coverage_Data.all).Have_Runs);

         if File_Node.Analysis_Data.Coverage_Data.Is_Valid then
            Add_Subprogram_Info
              (File_Node, To_Construct_Tree
                 (Src_File_Name, Read_File (Src_File_Name).all, Ada_Lang));
         end if;

         Compute_Project_Coverage (Project_Node);
      end if;

      Free (File_Contents);
      return Project_Node;
   end Build_Structure;

   ---------------------------
   -- Build_Display_Destroy --
   ---------------------------

   procedure Build_Display_Destroy (Projects     : Code_Analysis_Tree;
                                    File_Name    : String;
                                    Project_File : String)
   is
      Project_Node  : Project_Access;
      pragma Unreferenced (Project_Node);
   begin
      Project_Node  := Build_Structure (Projects, File_Name, Project_File);
      Dump_Text (Projects);
   end Build_Display_Destroy;

   ---------------
   -- Benchmark --
   ---------------

   procedure Benchmark (Projects     : in out Code_Analysis_Tree;
                        File_Name    : String;
                        Project_File : String;
                        Project_Num  : String;
                        File_Num     : String) is
      use Project_Maps;
      VFS_File_Name : Virtual_File;
      Cov_File_Name : Virtual_File;
      File_Contents : GNAT.Strings.String_Access;
      File_Node     : Code_Analysis.File_Access;
      Tree          : constant Project_Tree_Access := new Project_Tree;
      Project_Node  : Project_Access;
      Time_Before   : Time;
      Time_After    : Time;
      Measure       : Duration;
      Success       : Import_Project_Error;
      Timeout       : exception;
      Create_Max    : constant Duration := 13.0;
      Request_Max   : constant Duration := 2.0;
      Destroy_Max   : constant Duration := 2.0;
      --  ??? I make the supposition that users dont want to wait more than 2s
      --  for these operations and so we would have to add a waiting dialog
      --  (filling task bar) the creation operation tracking

      function Build_Msg (S : String; Value, Time : Duration) return String;
      --  Build a message used when raising Timeout exception

      function Build_Msg
        (S : String; Value, Time : Duration) return String is
      begin
         return S & Duration'Image (Value) & "s" & ASCII.LF & "Timeout set to:"
           & Duration'Image (Time);
      end Build_Msg;

   begin

      Time_Before := Clock;
      Tree.Load_Empty_Project;

      for J in 0 .. Integer'Value (Project_Num) loop
         Success := Tree.Add_Imported_Project
           (Tree.Root_Project,
            GNATCOLL.VFS.Create
              (+(Project_File (Project_File'First .. Project_File'Last - 4)
                 & "_" & Integer'Image (J) (2) & ".gpr")));
         if Success /= Import_Project_Error'(Success) then
            raise Timeout with "Error importing project" & J'Img;
         end if;
         Project_Node  := Get_Or_Create (Projects, Tree.Root_Project);

         for JJ in 0 .. Integer'Value (File_Num) loop
            Cov_File_Name := Create (+File_Name);
            VFS_File_Name := Create (+File_Name
                                     (File_Name'First .. File_Name'Last - 5)
                                     & (+Integer'Image (JJ)));
            File_Contents := Read_File (Cov_File_Name);
            File_Node     := Get_Or_Create (Project_Node, VFS_File_Name);
            Add_File_Info (File_Node, File_Contents);
            Free (File_Contents);
         end loop;
      end loop;

      Time_After := Clock;
      Measure    := Time_After - Time_Before;
      Put_Line ("Creation time   :" & Duration'Image (Measure) & "s");

      if Measure > Create_Max then
         raise Timeout with Build_Msg ("Creation alarm:", Measure, Create_Max);
      end if;

      Success := Tree.Add_Imported_Project
        (Tree.Root_Project,
         Create
          (+(Project_File (Project_File'First .. Project_File'Last - 4)
             & "_" & Integer'Image (5) (2) & ".gpr")));

      if Success /= Import_Project_Error'(Success) then
         raise Timeout with "Error importing project for requests";
      end if;

      Time_Before   := Clock;
      Project_Node  := Element (Projects.Find (Tree.Root_Project));
      VFS_File_Name := Create (+File_Name
                                     (File_Name'First .. File_Name'Last - 5)
                                     & (+Integer'Image (50)));
      File_Node     := Get_Or_Create (Project_Node, VFS_File_Name);
      Put_Line ("Request result   :");
      Dump_Line (File_Node.Lines (534));
      Time_After    := Clock;
      Measure       := Time_After - Time_Before;
      Put_Line ("Request time    :"
                & Duration'Image (Measure)
                & "s");

      if Measure > Request_Max then
         raise Timeout
           with Build_Msg ("Request alarm:", Measure, Request_Max);
      end if;

      Time_Before := Clock;
      Free_Code_Analysis (Projects);
      Time_After  := Clock;
      Measure     := Time_After - Time_Before;
      Put_Line ("Destruction time:" & Duration'Image (Measure) & "s");

      if Measure > Destroy_Max then
         raise Timeout
           with Build_Msg ("Destruction alarm:", Measure, Destroy_Max);
      end if;
   end Benchmark;

   --------------
   -- Treeview --
   --------------

   Node_Col_Count : constant Guint := 1;
   --  Number of columns needed to store the node information in a
   --  Gtk_Tree_Model
   Cov_Col_Count  : constant Guint := 2;
   --  Number of columns needed to store the coverage information in a
   --  Gtk_Tree_Model

   procedure Treeview (Projects     : in out Code_Analysis_Tree;
                       File_Name    : String;
                       Project_File : String) is
      Project_Node  : Project_Access;
      Window        : Gtk_Window;
      Container     : Gtk_Box;
      Tree_View     : Gtk_Tree_View;
      Tree_Store    : Gtk_Tree_Store;
      Types_Array   : GType_Array (1 .. Node_Col_Count + Cov_Col_Count);
      --  Iter          : Gtk_Tree_Iter;
      --  ??? will be decommented when gui will be separated from module
      --  [G608-020]
      Tree_Col      : Gtk_Tree_View_Column;
      Text_Render   : Gtk_Cell_Renderer_Text;
      Num_Col       : Gint;
      pragma Unreferenced (Num_Col, Project_Node);
   begin
      Project_Node  := Build_Structure
        (Projects, File_Name, Project_File);
      Init;
      Gtk_New (Window);
      Set_Title (Window, "Analysis report");
      Gtk_New_Vbox (Container);
      Add (Window, Container);

      for J in 1 .. Node_Col_Count + Cov_Col_Count loop
         Types_Array (J) := GType_String;
      end loop;
      --  1 text column to display the nodes
      --  2 text columns to display the coverage info

      Gtk_New (Tree_Store, Types_Array);
      Gtk_New (Tree_View, Tree_Store);

      -----------------
      -- Node column --
      -----------------

      Gtk_New (Tree_Col);
      Gtk_New (Text_Render);
      Pack_Start (Tree_Col, Text_Render, True);
      Num_Col := Append_Column (Tree_View, Tree_Col);
      Add_Attribute (Tree_Col, Text_Render, "text", 0);
      Set_Title (Tree_Col, "Entities");

      ----------------------
      -- Coverage columns --
      ----------------------

      Gtk_New (Tree_Col);
      Num_Col := Append_Column (Tree_View, Tree_Col);

      for J in 1 .. Cov_Col_Count loop
         Gtk_New (Text_Render);
         Pack_Start (Tree_Col, Text_Render, True);
         Add_Attribute (Tree_Col, Text_Render, "text", Gint (J));
      end loop;

      Set_Title (Tree_Col, "Coverage");
      Pack_Start (Container, Tree_View);
      --  Iter := Get_Iter_First (Gtk_Tree_Model (Tree_Store));
      --  Fill_Iter (Tree_Store, Iter, Projects, False, );
      --  ??? Could be fixed when gui will be separated from module [G608-020]
      Show_All (Window);
      Main;
      Free_Code_Analysis (Projects);
   end Treeview;

   Projects  : Code_Analysis_Tree := new Project_Maps.Map;
   Arg_Count : constant Natural := 3;
begin
   Set_Symbols (Ada_Lang, Symbols);

   if Argument_Count < Arg_Count then
      Put_Line ("error: missing arguments");
      Print_Usage;
      return;
   end if;

   if Argument (Arg_Count) = "build_display_destroy" then
      Build_Display_Destroy (Projects, Argument (1), Argument (2));
   elsif Argument (Arg_Count) = "benchmark" then
      Benchmark (Projects,
                 Argument (1),
                 Argument (2),
                 Argument (4),
                 Argument (5));
   elsif Argument (Arg_Count) = "treeview" then
      Treeview (Projects, Argument (1), Argument (2));
   else
      Print_Usage;
      return;
   end if;

   if Projects /= null then
      Free_Code_Analysis (Projects);
   end if;
end Code_Analysis_Test;
