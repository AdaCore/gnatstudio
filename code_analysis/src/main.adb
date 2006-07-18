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

with Code_Analysis; use Code_Analysis;
with Ada.Text_IO; use Ada.Text_IO;
with Code_Analysis_Dump; use Code_Analysis_Dump;

procedure Main is
   P : constant Project_Access := Get_Or_Create_Project ("trucages.txt");

   F1 : Code_Analysis.File_Access;
   F2 : Code_Analysis.File_Access;

   S1 : Subprogram_Access;
   S2 : Subprogram_Access;

   L1 : Line_Access;
   L2 : Line_Access;

   A2 : Analysis_Access;
begin

   F1 := Get_Or_Create_File (P, "fichier1.txt");
   F2 := Get_Or_Create_File (P, "fichier2.txt");
   Put_Line (" * Created File :" & Get_Id (F1));
   Put_Line (" * Created File :" & Get_Id (F2));

   S1 := Get_Or_Create_Subprogram (F1, "Subprog1.txt");
   S2 := Get_Or_Create_Subprogram (F2, "Subprog2.txt");
   Put_Line (" * Created Subprogram :" & Get_Id (S1));
   Put_Line (" * Created Subprogram :" & Get_Id (S2));

   A2 := Get_Analysis (S2);
   A2.Coverage_Data := new Coverage;
   A2.Coverage_Data.Total_Child_Count := 42;
   Put_Line (" · Subprogram " & Get_Id (S2) & " coverage data" & Integer'Image
             (A2.Coverage_Data.Total_Child_Count));

   L1 := Get_Or_Create_Line (S1, 1);
   L2 := Get_Or_Create_Line (S1, 2);

   Put_Line (" * Created Line :" & Integer'Image (Get_Id (L1)));
   Put_Line (" * Created Line :" & Integer'Image (Get_Id (L2)));

   Put_Line (" * ******");

   Dump_Text;

   Free_Project (P);
end Main;
