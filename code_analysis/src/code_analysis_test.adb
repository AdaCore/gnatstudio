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

with Code_Analysis;      use Code_Analysis;
with Code_Analysis_Dump; use Code_Analysis_Dump;
with Code_Coverage;      use Code_Coverage;

with GNAT.OS_Lib;        use GNAT.OS_Lib;
with VFS;                use VFS;

--  Code_Analysis_Test reads a gcov output file, built from the given source
--  file name, builds a Code_Analysis tree structure from it, displays it
--  on the standard output, and cleanly quits
--  So a correct usage is : $ code_analysis_test source_file_name
--  Example :
--  $ code_analysis_test main.adb
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

procedure Code_Analysis_Test is

   procedure Print_Usage;
   --  Print the correct usage of the program to the standard output

   -----------------
   -- Print_Usage --
   -----------------

   procedure Print_Usage is
   begin
      Put_Line ("Usage is : $ code_analysis_test source_file_name");
   end Print_Usage;

   VFS_File_Name : VFS.Virtual_File;
   Cov_File_Name : VFS.Virtual_File;
   File_Contents : GNAT.OS_Lib.String_Access;
   Project_Name  : VFS.Virtual_File;
   Project_Node  : Project_Access;
   File_Node     : Code_Analysis.File_Access;
begin
   if Argument_Count > 1 then
      Put_Line ("error: too many arguments");
      Print_Usage;
      return;
   elsif Argument_Count < 1 then
      Put_Line ("error: missing one argument");
      Print_Usage;
      return;
   end if;

   VFS_File_Name := Create (Argument (1));
   Cov_File_Name := Create (Argument (1) & ".gcov");
   Project_Name  := Get_Project_From_File (VFS_File_Name);
   Project_Node  := Get_Or_Create (Project_Name);
   File_Contents := Read_File (Cov_File_Name);

   declare
      Dummy  : Code_Analysis.File_Access;
      pragma Unreferenced (Dummy);
   begin
      Dummy  := Get_Or_Create (Project_Node, VFS_File_Name);
   end;

   File_Node := Get_Or_Create (Get_Or_Create (Project_Name), VFS_File_Name);
   Add_Subprograms (File_Node, File_Contents);
   Add_Lines (File_Node, File_Contents);
   Dump_Text;
   Free (File_Contents);
   Free_Project (Project_Node);
end Code_Analysis_Test;
