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
with GNAT.Regpat; use GNAT.Regpat;

package body Code_Coverage is

   ---------------------------
   -- Get_Project_From_File --
   ---------------------------

   function Get_Project_From_File (F_I : File_Id) return Project_Id is
      pragma Unreferenced (F_I);
      P_I : constant Project_Id := new String'("Carotte");
   begin
      return P_I;
   end Get_Project_From_File;

   ---------------------
   -- Add_Subprograms --
   ---------------------

   procedure Add_Subprograms
     (F_A : Code_Analysis.File_Access; File_Contents : String_Access) is
      Regexp  : constant Pattern_Matcher
        := Compile ("^f.*_.*_(.+) called \d+", Multiple_Lines);
      Matches : Match_Array (0 .. 1);
      Current : Natural;
      S_I     : Subprogram_Id;
      Dummy   : Subprogram_Access;
      pragma Unreferenced (Dummy);
   begin
      Current    := File_Contents'First;
      loop
         Match (Regexp, File_Contents.all, Matches, Current);
         exit when Matches (0) = No_Match;
         S_I     := new String'(File_Contents
                               (Matches (1).First .. Matches (1).Last));
         Dummy   := Get_Or_Create (F_A, S_I);
         Current := Matches (0).Last + 1;
      end loop;
   end Add_Subprograms;

   ---------------
   -- Add_Lines --
   ---------------

   procedure Add_Lines
     (S_A : Subprogram_Access; File_Contents : String_Access) is
      Regexp  : constant Pattern_Matcher
        := Compile ("^ *(\d+|#####): *(\d+):.*$", Multiple_Lines);
      Matches : Match_Array (0 .. 2);
      Current : Natural;
      L_I     : Line_Id;
      L_A     : Line_Access;
   begin
      Current   := File_Contents'First;
      loop
         Match (Regexp, File_Contents.all, Matches, Current);
         exit when Matches (0) = No_Match;
         L_I    := Integer'Value
           (File_Contents (Matches (2).First .. Matches (2).Last));
         L_A    := Get_Or_Create (S_A, L_I);
         L_A.Analysis_Data.Coverage_Data := new Coverage;
         if File_Contents
           (Matches (1).First .. Matches (1).Last) /= "#####" then
            L_A.Analysis_Data.Coverage_Data.Total_Child_Count := Integer'Value
              (File_Contents (Matches (1).First .. Matches (1).Last));
         else
            L_A.Analysis_Data.Coverage_Data.Total_Child_Count := 0;
         end if;
         Current := Matches (0).Last + 1;
      end loop;
   end Add_Lines;

   -------------------
   -- Dump_Corevage --
   -------------------

   procedure Dump_Coverage (C_A : Coverage_Access) is
   begin
      Put (Integer'Image (C_A.Covered_Child_Count)
           & " /"
           & Integer'Image (C_A.Total_Child_Count));
   end Dump_Coverage;

   ------------------------
   -- Dump_Line_Coverage --
   ------------------------

   procedure Dump_Line_Coverage (C_A : Coverage_Access) is
   begin
      if C_A.Total_Child_Count = 0 then
         Put (" /!\ Never executed");
      else
         Put (" executed"
              & Integer'Image (C_A.Total_Child_Count)
              & " times");
      end if;
   end Dump_Line_Coverage;

end Code_Coverage;
