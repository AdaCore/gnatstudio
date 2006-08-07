-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2006                         --
--                              AdaCore                              --
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

with Ada.Command_Line;   use Ada.Command_Line;
with Ada.Text_IO;        use Ada.Text_IO;
with Ada.Calendar;       use Ada.Calendar;

with Code_Analysis;      use Code_Analysis;
with Code_Analysis_Dump; use Code_Analysis_Dump;
with Code_Coverage;      use Code_Coverage;

with GNAT.OS_Lib;        use GNAT.OS_Lib;
with VFS;                use VFS;

procedure Code_Analysis_Test is

   procedure Print_Usage;
   --  Print the correct usage of the program to the standard output

   function Build_Structure (File_Name : String) return Project_Access;
   --  Build a code_analysis structure from a gcov file

   procedure Build_Display_Destroy (File_Name : String);
   --  reads a gcov output file, built from a given source file name, builds a
   --  Code_Analysis tree structure from it, displays it on the standard output
   --  and cleanly quits
   --  Correct usage: code_analysis_test source_file build_display_destroy
   --  Example:
   --  $ code_analysis_test main.adb build_display_destroy
   --  Project Dummy_Project_Name
   --    File main.adb
   --      Subprogram main__read_file 0 / 0 1 call(s)
   --      Subprogram main 0 / 0 2 call(s)
   --      Subprogram main___clean 0 / 0 1 call(s)
   --      Subprogram main__read_file___clean 0 / 0 1 call(s)
   --        Line 1
   --        Line 2
   --        Line 3 4 execution(s)
   --        Line 4 warning: line never executed

   procedure Benchmark (File_Name : String);
   --  builds a big Code_Analysis tree structure and outputs the time needed to
   --  build, perform one request, and destroy the structure

--     procedure Treeview (File_Name : String);
   --  builds a code_analysis structure and display it in a Gtk tree view

   -----------------
   -- Print_Usage --
   -----------------

   procedure Print_Usage is
   begin
      Put_Line
        ("Usage: code_analysis_test source_file test_name test_name...");
      Put_Line
        ("Available tests are: build_display_destroy, benchmark,treeview");
   end Print_Usage;

   ---------------------
   -- Build_Structure --
   ---------------------

   function Build_Structure (File_Name : String) return Project_Access is
      VFS_File_Name : VFS.Virtual_File;
      Cov_File_Name : VFS.Virtual_File;
      File_Contents : GNAT.OS_Lib.String_Access;
      Project_Name  : VFS.Virtual_File;
      Project_Node  : Project_Access;
      File_Node     : Code_Analysis.File_Access;
   begin
      VFS_File_Name := Create (File_Name);
      Cov_File_Name := Create (File_Name & ".gcov");
      Project_Name  := Get_Project_From_File (VFS_File_Name, 1);
      Project_Node  := Get_Or_Create (Project_Name);
      File_Contents := Read_File (Cov_File_Name);
      File_Node     := Get_Or_Create (Project_Node, VFS_File_Name);
      Add_Subprograms (File_Node, File_Contents);
      Add_Lines (File_Node, File_Contents);
      Free (File_Contents);
      return Project_Node;
   end Build_Structure;

   ---------------------------
   -- Build_Display_Destroy --
   ---------------------------

   procedure Build_Display_Destroy (File_Name : String) is
      Project_Node  : Project_Access;
   begin
      Project_Node  := Build_Structure (File_Name);
      Dump_Text;
      Free_Project (Project_Node);
   end Build_Display_Destroy;

   ---------------
   -- Benchmark --
   ---------------

   procedure Benchmark (File_Name : String) is
      use Project_Maps;
      Line_Node     : Line_Access;
      VFS_File_Name : VFS.Virtual_File;
      Cov_File_Name : VFS.Virtual_File;
      File_Contents : GNAT.OS_Lib.String_Access;
      File_Node     : Code_Analysis.File_Access;
      Project_Name  : VFS.Virtual_File;
      Project_Node  : Project_Access;
      Time_Before   : Time;
      Time_After    : Time;
      Measure       : Duration;
      Timeout       : exception;
      Create_Max    : constant Duration := 13.0;
      --  ??? Currently observed on bonn : 3.3s
      Request_Max   : constant Duration := 2.0;
      --  ??? Currently observed on bonn : 0.00013s
      Destroy_Max   : constant Duration := 2.0;
      --  ??? Currently observed on bonn : 0.1s
      --  ??? I make the supposition that users dont want to wait more than 2s
      --  for these operations and so we would have to add a waiting dialog
      --  (filling task bar) the creation operation tracking
      Analysis_Tree : Code_Analysis_Tree;
      Cursor_Tree   : Cursor;

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

      for J in 1 .. 10 loop
         Project_Name  := Get_Project_From_File (VFS_File_Name, J);
         Project_Node  := Get_Or_Create (Project_Name);

         for JJ in 0 .. 99 loop
            VFS_File_Name := Create (File_Name & Integer'Image (JJ));
            Cov_File_Name := Create (File_Name & ".gcov");
            File_Contents := Read_File (Cov_File_Name);
            File_Node     := Get_Or_Create (Project_Node, VFS_File_Name);
            Add_Subprograms (File_Node, File_Contents);
            Add_Lines (File_Node, File_Contents);
            Free (File_Contents);
         end loop;
      end loop;

      Time_After    := Clock;
      Measure       := Time_After - Time_Before;
      Put_Line ("Creation time   :" & Duration'Image (Measure) & "s");

      if Measure > Create_Max then
         raise Timeout with Build_Msg ("Creation alarm:", Measure, Create_Max);
      end if;

      Time_Before := Clock;
      Project_Name  := Get_Project_From_File (VFS_File_Name, 5);
      Project_Node  := Get_Or_Create (Project_Name);
      VFS_File_Name := Create (File_Name & Integer'Image (50));
      File_Node     := Get_Or_Create (Project_Node, VFS_File_Name);
      Line_Node     := Get_Or_Create (File_Node, Line_Id (534));
      Put_Line ("Request result   :");
      Dump_Line (Line_Node);
      Time_After    := Clock;
      Measure       := Time_After - Time_Before;
      Put_Line ("Request time    :"
                & Duration'Image (Measure)
                & "s");

      if Measure > Request_Max then
         raise Timeout
           with Build_Msg ("Request alarm:", Measure, Request_Max);
      end if;

      Analysis_Tree := Get_Tree;
      Cursor_Tree   := Analysis_Tree.First;
      Time_Before   := Clock;

      loop
         exit when Cursor_Tree = No_Element;
         Project_Node := Element (Cursor_Tree);
         Next (Cursor_Tree);
         Free_Project (Project_Node);
      end loop;

      Time_After    := Clock;
      Measure        := Time_After - Time_Before;
      Put_Line ("Destruction time:" & Duration'Image (Measure) & "s");

      if Measure > Destroy_Max then
         raise Timeout
           with Build_Msg ("Destruction alarm:", Measure, Destroy_Max);
      end if;
   end Benchmark;

   --------------
   -- Treeview --
   --------------

--     procedure Treeview (File_Name : String) is
--        Project_Node  : Project_Access;
--     begin
--        Project_Node  := Build_Structure (File_Name);
--        Dump_Text;
--        Free_Project (Project_Node);
--     end Treeview;
begin
   if Argument_Count < 2 then
      Put_Line ("error: missing arguments");
      Print_Usage;
      return;
   end if;

   for J in 2 .. Argument_Count loop
      if Argument (J) = "build_display_destroy" then
         Build_Display_Destroy (Argument (1));
      elsif Argument (J) = "benchmark" then
         Benchmark (Argument (1));
      elsif Argument (J) = "treeview" then
         Benchmark (Argument (1));
      else
         Print_Usage;
         return;
      end if;
   end loop;
end Code_Analysis_Test;
