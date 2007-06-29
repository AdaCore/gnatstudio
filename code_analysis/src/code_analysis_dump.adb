-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                  Copyright (C) 2006-2007, AdaCore                 --
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

with Ada.Text_IO;   use Ada.Text_IO;

with Code_Coverage; use Code_Coverage;
with VFS;           use VFS;
with Projects;      use Projects;

package body Code_Analysis_Dump is

   ---------------
   -- Dump_Text --
   ---------------

   procedure Dump_Text (Projects : Code_Analysis_Tree) is
   begin
      Projects.Iterate (Dump_Project'Access);
   end Dump_Text;

   ------------------
   -- Dump_Project --
   ------------------

   procedure Dump_Project (Cursor : Project_Maps.Cursor) is
      Project_Node : constant Project_Access := Project_Maps.Element (Cursor);
   begin
      Put ("Project " & Project_Name (Project_Node.Name));

      if Project_Node.Analysis_Data.Coverage_Data /= null then
         Dump_Prj_Coverage (Project_Node.Analysis_Data.Coverage_Data);
      end if;

      New_Line;
      Project_Node.Files.Iterate (Dump_File'Access);
   end Dump_Project;

   ---------------
   -- Dump_File --
   ---------------

   procedure Dump_File (Cursor : File_Maps.Cursor) is
      File_Node : constant Code_Analysis.File_Access :=
                    File_Maps.Element (Cursor);
   begin
      Put ("  File " & VFS.Base_Name (File_Node.Name));

      if File_Node.Analysis_Data.Coverage_Data /= null then
         Dump_Node_Coverage (File_Node.Analysis_Data.Coverage_Data);
      end if;

      New_Line;
      File_Node.Subprograms.Iterate (Dump_Subprogram'Access);

      for J in 1 .. File_Node.Lines'Length loop
         Dump_Line (File_Node.Lines (J));
      end loop;
   end Dump_File;

   ---------------------
   -- Dump_Subprogram --
   ---------------------

   procedure Dump_Subprogram (Cursor : Subprogram_Maps.Cursor) is
      Sub_Node : constant Subprogram_Access
        := Subprogram_Maps.Element (Cursor);
   begin
      Put ("    Subprogram " & Sub_Node.Name.all);

      if Sub_Node.Analysis_Data.Coverage_Data /= null then
         Dump_Subp_Coverage (Sub_Node.Analysis_Data.Coverage_Data);
      end if;

      New_Line;
   end Dump_Subprogram;

   ---------------
   -- Dump_Line --
   ---------------

   procedure Dump_Line (Line_Node : Code_Analysis.Line) is
   begin
      Put ("      Line");

      if Line_Node /= Null_Line then
         Put (Natural'Image (Line_Node.Number));
      end if;

      if Line_Node.Analysis_Data.Coverage_Data /= null then
         Dump_Line_Coverage (Line_Node.Analysis_Data.Coverage_Data);
      end if;

      New_Line;
   end Dump_Line;

end Code_Analysis_Dump;
