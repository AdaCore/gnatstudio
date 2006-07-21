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
with Code_Analysis_Dump; use Code_Analysis_Dump;
with Code_Coverage; use Code_Coverage;

with Ada.Text_IO; use Ada.Text_IO;
with GNAT.OS_Lib; use GNAT.OS_Lib;

procedure Main is

   procedure Manual_Test;
   --  a basic test of building, text dumping and destroying a Code_Analysis
   --  tree structure.

   function Read_File (File : String) return String_Access;

   procedure Iterate_Add_Lines (C             : in out Subprogram_Maps.Cursor;
                                File_Contents : String_Access);
   -----------------
   -- Manual_Test --
   -----------------

   procedure Manual_Test is
      P_Name : Project_Id;
      P      : Project_Access;
      F1_Name : File_Id;
      F2_Name : File_Id;
      F1 : Code_Analysis.File_Access;
      F2 : Code_Analysis.File_Access;
      S1_Name : Subprogram_Id;
      S2_Name : Subprogram_Id;
      S1 : Subprogram_Access;
      S2 : Subprogram_Access;
      L1 : Line_Access;
      L2 : Line_Access;
   begin
      P_Name := new String'("Projet 1");
      F1_Name := new String'("fichier1.txt");
      F2_Name := new String'("fichier2.txt");
      S1_Name := new String'("Subprog1");
      S2_Name := new String'("Subprog2");
      P := Get_Or_Create (P_Name);
      Put_Line (" * ******");
      F1 := Get_Or_Create (P, F1_Name);
      F2 := Get_Or_Create (P, F2_Name);
      Put_Line (" * Created File :" & F1.Name.all);
      Put_Line (" * Created File :" & F2.Name.all);
      S1 := Get_Or_Create (F1, S1_Name);
      S2 := Get_Or_Create (F2, S2_Name);
      Put_Line (" * Created Subprogram :" & S1.Name.all);
      Put_Line (" * Created Subprogram :" & S2.Name.all);
      S2.Analysis_Data.Coverage_Data := new Coverage;
      S2.Analysis_Data.Coverage_Data.Total_Child_Count := 42;
      Put_Line (" · Subprogram "
                & S2.Name.all
                & " coverage data"
                & Integer'Image
                (S2.Analysis_Data.Coverage_Data.Total_Child_Count));
      L1 := Get_Or_Create (S1, 1);
      L2 := Get_Or_Create (S1, 2);
      L1.Analysis_Data.Coverage_Data := new Coverage;
      L1.Analysis_Data.Coverage_Data.Total_Child_Count := 3;
      Put_Line (" * Created Line :" & Integer'Image (L1.Number));
      Put_Line (" * Created Line :" & Integer'Image (L2.Number));
      Put_Line (" * ******");

      Dump_Text;

      Free_Project (P);
   end Manual_Test;

   ---------------
   -- Read_File --
   ---------------

   function Read_File (File : String) return String_Access is
      FD           : File_Descriptor := Invalid_FD;
      Buffer       : String_Access;
      Length       : Integer;
      Dummy_Length : Integer;
      pragma Unreferenced (Dummy_Length);

   begin
      FD := Open_Read (File, Fmode => Binary);
      if FD = Invalid_FD then
         Put_Line ("Couldn't open " & File);
         return null;
      end if;
      Length := Integer (File_Length (FD));
      Buffer := new String (1 .. Length);
      Dummy_Length := Read (FD, Buffer.all'Address, Length);
      Close (FD);
      return Buffer;
   end Read_File;

   -----------------------
   -- Iterate_Add_Lines --
   -----------------------

   procedure Iterate_Add_Lines (C             : in out Subprogram_Maps.Cursor;
                                File_Contents : String_Access) is
      use Subprogram_Maps;
   begin
      loop
         Add_Lines (Element (C), File_Contents);
         Subprogram_Maps.Next (C);
         exit when C = No_Element;
      end loop;
   end Iterate_Add_Lines;

   Source_File_Name : constant File_Id := new String'("main.adb");
   File_Contents : String_Access;
   Project_Name  : Project_Id;
   Project_Node  : Project_Access;
   File_Node     : Code_Analysis.File_Access;
   C             : Subprogram_Maps.Cursor;
begin

   Manual_Test;

   New_Line;
   Put_Line (" * ******");
   New_Line;

   Project_Name  := Get_Project_From_File (Source_File_Name);
   Project_Node  := Get_Or_Create (Project_Name);
   File_Contents := Read_File (Source_File_Name.all & ".gcov");
   declare
      Dummy  : Code_Analysis.File_Access;
      pragma Unreferenced (Dummy);
   begin
      Dummy  := Get_Or_Create (Project_Node, Source_File_Name);
   end;
   File_Node := Get_Or_Create (Get_Or_Create (Project_Name), Source_File_Name);
   Add_Subprograms (File_Node, File_Contents);
   C := File_Node.Subprograms.First;
   Iterate_Add_Lines (C, File_Contents);

   Dump_Text;
   New_Line;

   Free (File_Contents);
   Free_Project (Project_Node);
end Main;
