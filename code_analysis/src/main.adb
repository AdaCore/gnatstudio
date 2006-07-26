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

with Code_Analysis;      use Code_Analysis;
with Code_Analysis_Dump; use Code_Analysis_Dump;
with Code_Coverage;      use Code_Coverage;

with GNAT.OS_Lib;        use GNAT.OS_Lib;
with VFS;                use VFS;

procedure Main is
   VFS_File_Name : constant VFS.Virtual_File := VFS.Create ("dummy_main.adb");
   Cov_File_Name : constant VFS.Virtual_File := VFS.Create
     ("/bonn.a/descarpe/src/gps/code_analysis/main.adb.gcov");
   File_Contents : GNAT.OS_Lib.String_Access;
   Project_Name  : VFS.Virtual_File;
   Project_Node  : Project_Access;
   File_Node     : Code_Analysis.File_Access;
begin
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
end Main;
