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

with Ada.Text_IO; use Ada.Text_IO;
with Code_Coverage; use Code_Coverage;

package body Code_Analysis_Dump is

   ---------------
   -- Dump_Text --
   ---------------

   procedure Dump_Text is
   begin
      Projects.Iterate (Dump_Project'Access);
   end Dump_Text;

   ------------------
   -- Dump_Project --
   ------------------

   procedure Dump_Project (C : Project_Maps.Cursor) is
      P_A : constant Project_Access := Project_Maps.Element (C);
   begin
      Put (" O¦ Project " & P_A.Name.all);

      if P_A.Analysis_Data.Coverage_Data /= null then
         Dump_Node_Coverage (P_A.Analysis_Data.Coverage_Data);
      end if;

      New_Line;
      P_A.Files.Iterate (Dump_File'Access);
   end Dump_Project;

   ---------------
   -- Dump_File --
   ---------------

   procedure Dump_File (C : File_Maps.Cursor) is
      F_A : constant Code_Analysis.File_Access := File_Maps.Element (C);
   begin
      Put ("   o¦ File " & F_A.Name.all);

      if F_A.Analysis_Data.Coverage_Data /= null then
         Dump_Node_Coverage (F_A.Analysis_Data.Coverage_Data);
      end if;

      New_Line;
      F_A.Subprograms.Iterate (Dump_Subprogram'Access);

      for I in 1 .. F_A.Lines'Length
      loop
         Dump_Line (F_A.Lines (I));
      end loop;

   end Dump_File;

   ---------------------
   -- Dump_Subprogram --
   ---------------------

   procedure Dump_Subprogram (C : Subprogram_Maps.Cursor) is
      S_A : constant Subprogram_Access := Subprogram_Maps.Element (C);
   begin
      Put ("     °¦ Subprogram " & S_A.Name.all);

      if S_A.Analysis_Data.Coverage_Data /= null then
         Dump_Subp_Coverage (S_A.Analysis_Data.Coverage_Data);
      end if;

      New_Line;
   end Dump_Subprogram;

   ---------------
   -- Dump_Line --
   ---------------

   procedure Dump_Line (L_A : Line_Access) is
   begin
      Put ("       ·¦ Line" & Natural'Image (L_A.Number));

      if L_A.Analysis_Data.Coverage_Data /= null then
         Dump_Line_Coverage (L_A.Analysis_Data.Coverage_Data);
      end if;

      New_Line;
   end Dump_Line;

end Code_Analysis_Dump;
