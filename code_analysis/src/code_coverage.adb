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

   function Get_Project_From_File (File : VFS.Virtual_File)
                                   return VFS.Virtual_File is
      pragma Unreferenced (File);
      Project : constant VFS.Virtual_File := VFS.Create ("Dummy_Project_Name");
   begin
      return Project;
   end Get_Project_From_File;

   ---------------------
   -- Add_Subprograms --
   ---------------------

   procedure Add_Subprograms
     (File_Node     : Code_Analysis.File_Access;
      File_Contents : String_Access) is
      Regexp_1   : constant Pattern_Matcher
        := Compile ("^function (_ada_)?(\w+)([.]\d+)? called (\d+)"
           , Multiple_Lines);
      Matches_1  : Match_Array (0 .. 4);
      Current    : Natural;
      Subprogram : Subprogram_Id;
      Sub_Node   : Subprogram_Access;
   begin
      Current    := File_Contents'First;

      loop
         Match (Regexp_1, File_Contents.all, Matches_1, Current);
         exit when Matches_1 (0) = No_Match;

         Subprogram     := new String'(File_Contents (
           Matches_1 (2).First .. Matches_1 (2).Last));
         Sub_Node     := Get_Or_Create (File_Node, Subprogram);
         Sub_Node.Analysis_Data.Coverage_Data := new Subprogram_Coverage;
         Subprogram_Coverage (Sub_Node.Analysis_Data.Coverage_Data.all).Called
           := Natural'Value
             (File_Contents (Matches_1 (4).First .. Matches_1 (4).Last));
         Current := Matches_1 (0).Last + 1;
      end loop;
   end Add_Subprograms;

   ---------------
   -- Add_Lines --
   ---------------

   procedure Add_Lines
     (File_Node     : Code_Analysis.File_Access;
      File_Contents : String_Access) is
      Regexp     : constant Pattern_Matcher
        := Compile ("^ +(\d+|#####|-): *(\d+):.*$", Multiple_Lines);
      Matches    : Match_Array (0 .. 2);
      Current    : Natural;
      Line_Count : Natural := 0;
      Line       : Line_Id;
      Line_Node  : Line_Access;
   begin
      for C in File_Contents'First .. File_Contents'Last
      loop
         if File_Contents (C) = ASCII.LF then
            Line_Count := Line_Count + 1;
            if Line_Count = 5 then
               Current := C;
            end if;
         end if;
      end loop;

      File_Node.Lines := new Line_Array
        (1 .. Line_Count - Natural (File_Node.Subprograms.Length) - 5);
      --  Create a Line_Array with exactly the number of elements corresponding
      --  to the number of code lines in the original source code file.

      loop
         Match (Regexp, File_Contents.all, Matches, Current);
         exit when Matches (0) = No_Match;
         Line    := Natural'Value
           (File_Contents (Matches (2).First .. Matches (2).Last));
         Line_Node := Get_Or_Create (File_Node, Line);

         case File_Contents (Matches (1).First) is
            when '#' => Line_Node.Analysis_Data.Coverage_Data := new Coverage;
               Line_Node.Analysis_Data.Coverage_Data.Covered := 0;
            when '-' => null;
            when others =>
               Line_Node.Analysis_Data.Coverage_Data := new Coverage;
               Line_Node.Analysis_Data.Coverage_Data.Covered := Natural'Value
                 (File_Contents (Matches (1).First .. Matches (1).Last));
         end case;

         Current := Matches (0).Last + 1;
      end loop;
   end Add_Lines;

   ------------------------
   -- Dump_Node_Coverage --
   ------------------------

   procedure Dump_Node_Coverage (Coverage : Coverage_Access) is
   begin
      Put (Natural'Image (Coverage.Covered)
           & " /"
           & Natural'Image (Node_Coverage (Coverage.all).Children));
   end Dump_Node_Coverage;

   ------------------------
   -- Dump_Line_Coverage --
   ------------------------

   procedure Dump_Line_Coverage (Coverage : Coverage_Access) is
   begin
      if Coverage.Covered = 0 then
         Put (" /!\ Never executed /!\");
      else
         Put (Natural'Image (Coverage.Covered) & " execution(s)");
      end if;
   end Dump_Line_Coverage;

   ------------------------
   -- Dump_Subp_Coverage --
   ------------------------

   procedure Dump_Subp_Coverage (Coverage : Coverage_Access) is
   begin
      Dump_Node_Coverage (Coverage);

      if Subprogram_Coverage (Coverage.all).Called = 0 then
         Put (" /!\ Never called /!\");
      else
         Put (Natural'Image (Subprogram_Coverage (Coverage.all).Called)
              & " call(s)");
      end if;
   end Dump_Subp_Coverage;

end Code_Coverage;
