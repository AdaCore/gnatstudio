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
      Put (" O¦ Project " & Get_Id (P_A));
      if Get_Analysis (P_A).Coverage_Data /= null then
         Dump_Coverage (Get_Analysis (P_A).Coverage_Data);
      end if;
      Put (" : ");
      Get_Map (P_A).Iterate (Dump_Children_Names'Access);
      Put (ASCII.lf);
      Get_Map (P_A).Iterate (Dump_File'Access);
   end Dump_Project;

   ---------------
   -- Dump_File --
   ---------------

   procedure Dump_File (C : File_Maps.Cursor) is
      F_A : constant Code_Analysis.File_Access := File_Maps.Element (C);
   begin
      Put ("   o¦ File " & Get_Id (F_A));
      if Get_Analysis (F_A).Coverage_Data /= null then
         Dump_Coverage (Get_Analysis (F_A).Coverage_Data);
      end if;
      Put (" : ");
      Get_Map (F_A).Iterate (Dump_Children_Names'Access);
      Put (ASCII.lf);
      Get_Map (F_A).Iterate (Dump_Subprogram'Access);
   end Dump_File;

   ---------------------
   -- Dump_Subprogram --
   ---------------------

   procedure Dump_Subprogram (C : Subprogram_Maps.Cursor) is
      S_A : constant Subprogram_Access := Subprogram_Maps.Element (C);
   begin
      Put ("     °¦ Subprogram " & Get_Id (S_A));
      if Get_Analysis (S_A).Coverage_Data /= null then
         Dump_Coverage (Get_Analysis (S_A).Coverage_Data);
      end if;
      Put (" : ");
      Get_Map (S_A).Iterate (Dump_Children_Names'Access);
      Put (ASCII.lf);
      Get_Map (S_A).Iterate (Dump_Line'Access);
   end Dump_Subprogram;

   ---------------
   -- Dump_Line --
   ---------------

   procedure Dump_Line (C : Line_Maps.Cursor) is
      L_A : constant Line_Access := Line_Maps.Element (C);
   begin
      Put ("       ·¦ Line " & Integer'Image (Get_Id (L_A)));
      if Get_Analysis (L_A).Coverage_Data /= null then
         Dump_Coverage (Get_Analysis (L_A).Coverage_Data);
      end if;
      Put (" : ");
      Put (ASCII.lf);
   end Dump_Line;

   ------------------------
   -- Dump_Children_Name --
   ------------------------

   procedure Dump_Children_Names (C : File_Maps.Cursor) is
      F_A : constant Code_Analysis.File_Access := File_Maps.Element (C);
   begin
      Put (Get_Id (F_A) & "; ");
   end Dump_Children_Names;

   ------------------------
   -- Dump_Children_Name --
   ------------------------

   procedure Dump_Children_Names (C : Subprogram_Maps.Cursor) is
      S_A : constant Code_Analysis.Subprogram_Access
        := Subprogram_Maps.Element (C);
   begin
      Put (Get_Id (S_A) & "; ");
   end Dump_Children_Names;

   ------------------------
   -- Dump_Children_Name --
   ------------------------

   procedure Dump_Children_Names (C : Line_Maps.Cursor) is
      L_A : constant Code_Analysis.Line_Access
        := Line_Maps.Element (C);
   begin
      Put (Integer'Image (Get_Id (L_A)) & "; ");
   end Dump_Children_Names;

end Code_Analysis_Dump;
