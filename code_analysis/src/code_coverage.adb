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

   --------------------
   -- Read_Gcov_Info --
   --------------------

   procedure Read_Gcov_Info
     (File_Node     : Code_Analysis.File_Access;
      File_Contents : String_Access;
      Line_Count    : out Natural;
      Covered_Lines : out Natural)
   is
      Subp_Regexp   : constant Pattern_Matcher :=
                        Compile ("^function (\w+)([.]\d+)? called (\d+)",
                                 Multiple_Lines);
      Subp_Matches  : Match_Array (0 .. 3);
      Current       : Natural;
      Subprogram    : String_Access;
      Subp_Node     : Subprogram_Access;
      Line_Regexp   : constant Pattern_Matcher :=
                        Compile ("^ +(\d+|#####|-): *(\d+):", Multiple_Lines);
      Line_Matches  : Match_Array (0 .. 2);
      Line          : Natural;
      Line_Node     : Line_Access;
      Bad_Gcov_File : exception;

   begin
      --  ??? should have one or several CE/exception handlers in this code,
      --  in case matching do not work "as expected"

      Current       := File_Contents'First;
      Line_Count    := 0;
      Covered_Lines := 0;

      loop
         Match (Subp_Regexp, File_Contents.all, Subp_Matches, Current);
         exit when Subp_Matches (0) = No_Match;

         Subprogram := new String'(File_Contents (
           Subp_Matches (1).First .. Subp_Matches (1).Last));
         Subp_Node  := Get_Or_Create (File_Node, Subprogram);
         Match (Line_Regexp,
                File_Contents.all,
                Line_Matches,
                Subp_Matches (3).Last);

         if Line_Matches (0) = No_Match then
            raise Bad_Gcov_File with "Gcov file has bad format after "
              & Subprogram.all
              & " subprogram body declaration.";
         end if;

         Subp_Node.Body_Line := Natural'Value
           (File_Contents (Line_Matches (2).First .. Line_Matches (2).Last));
         Subp_Node.Analysis_Data.Coverage_Data := new Subprogram_Coverage;
         Subprogram_Coverage (Subp_Node.Analysis_Data.Coverage_Data.all).Called
           := Natural'Value
             (File_Contents (Subp_Matches (3).First .. Subp_Matches (3).Last));
         Current := Subp_Matches (3).Last + 1;
      end loop;

      for C in File_Contents'First .. File_Contents'Last loop
         if File_Contents (C) = ASCII.LF then
            Line_Count := Line_Count + 1;

            if Line_Count = 5 then
               Current := C;
            end if;
         end if;
      end loop;

      Line_Count := Line_Count - (Natural (File_Node.Subprograms.Length) + 5);
      File_Node.Lines := new Line_Array (1 .. Line_Count);
      --  Create a Line_Array with exactly the number of elements corresponding
      --  to the number of code lines in the original source code file.

      loop
         Match (Line_Regexp, File_Contents.all, Line_Matches, Current);
         exit when Line_Matches (0) = No_Match;
         Line    := Natural'Value
           (File_Contents (Line_Matches (2).First .. Line_Matches (2).Last));
         Line_Node := Get_Or_Create (File_Node, Line);

         case File_Contents (Line_Matches (1).First) is
            when '#' => Line_Node.Analysis_Data.Coverage_Data := new Coverage;
               Line_Node.Analysis_Data.Coverage_Data.Covered := 0;
            when '-' => null;
            when others =>
               Line_Node.Analysis_Data.Coverage_Data := new Coverage;
               Line_Node.Analysis_Data.Coverage_Data.Covered := Natural'Value
                 (File_Contents
                    (Line_Matches (1).First .. Line_Matches (1).Last));
               Covered_Lines := Covered_Lines + 1;
         end case;

         Current := Line_Matches (0).Last + 1;
      end loop;
   end Read_Gcov_Info;

   ------------------------------
   -- Compute_Project_Coverage --
   ------------------------------

   procedure Compute_Project_Coverage (Project_Node : in out Project_Access) is
      Cur       : File_Maps.Cursor;
      File_Node : Code_Analysis.File_Access;
      use File_Maps;

   begin
      Cur := Project_Node.Files.First;

      if Project_Node.Analysis_Data.Coverage_Data = null then
         Project_Node.Analysis_Data.Coverage_Data := new Node_Coverage;
      end if;

      declare
         Data : constant access Node_Coverage :=
                  Node_Coverage
                    (Project_Node.Analysis_Data.Coverage_Data.all)'Access;
      begin
         loop
            exit when Cur = No_Element;
            File_Node := Element (Cur);

            if File_Node.Analysis_Data.Coverage_Data /= null then
               Data.Children := Data.Children +
                 Node_Coverage
                   (File_Node.Analysis_Data.Coverage_Data.all).Children;
               Data.Covered := Data.Covered +
                 File_Node.Analysis_Data.Coverage_Data.Covered;
            end if;

            Next (Cur);
         end loop;
      end;
   end Compute_Project_Coverage;

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
         Put (" warning: line never executed");
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
         Put (" warning: subprogram never called");
      else
         Put (Natural'Image (Subprogram_Coverage (Coverage.all).Called)
              & " call(s)");
      end if;
   end Dump_Subp_Coverage;

   ------------------------
   -- Line_Coverage_Info --
   ------------------------

   function Line_Coverage_Info (Coverage : Coverage_Access)
                                return String_Access is
   begin
      case Coverage.Covered is
         when 0 => return new String'(" never executed ");
         when 1 => return new String'(" 1 execution ");
         when others =>
            return new String'(Natural'Image (Coverage.Covered)
                               & " executions ");
      end case;
   end Line_Coverage_Info;

   ---------------
   -- Fill_Iter --
   ---------------

   procedure Fill_Iter
     (Tree_Store : in out Gtk_Tree_Store;
      Iter       : in out Gtk_Tree_Iter;
      Coverage   : Coverage_Access)
   is
      function Txt_Lig (Lig_Count : Natural) return String;
      --  ???

      function Txt_Sub (Coverage  : Coverage_Access) return String;
      --  ???

      Cov_Txt   : constant String  := Natural'Image (Coverage.Covered);
      Lig_Count : constant Natural := Node_Coverage (Coverage.all).Children;

      function Txt_Lig (Lig_Count : Natural) return String is
      begin
         if Lig_Count = 1 then
            return " line (";
         else
            return " lines (";
         end if;
      end Txt_Lig;

      function Txt_Sub (Coverage : Coverage_Access) return String is

         function Txt_Cal (Cal_Count : Natural) return String;
         --  ???

         function Txt_Cal (Cal_Count : Natural) return String is
         begin
            if Cal_Count = 1 then
               return " time";
            else
               return " times";
            end if;
         end Txt_Cal;

      begin
         if Coverage.all in Subprogram_Coverage'Class then
            declare
               Cal_Count : constant Natural :=
                             Subprogram_Coverage (Coverage.all).Called;
            begin
               return String'(", called"
                              & Natural'Image (Cal_Count)
                              & Txt_Cal (Cal_Count));
            end;
         else
            return "";
         end if;
      end Txt_Sub;

   begin
      --  ??? Should use String_Utils.Image instead of 'Image, and same
      --  elsewhere in this package

      Set (Tree_Store, Iter, Cov_Col,
         Natural'Image (Lig_Count)
         & Txt_Lig (Lig_Count)
         & Cov_Txt (Cov_Txt'First + 1 .. Cov_Txt'Last)
         & " not covered)"
         & Txt_Sub (Coverage));
   end Fill_Iter;

end Code_Coverage;
