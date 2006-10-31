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

with Ada.Text_IO;       use Ada.Text_IO;
with GNAT.Regpat;       use GNAT.Regpat;
with String_Utils;      use String_Utils;
with Ada.Strings;       use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with VFS;               use VFS;
with Glib;

package body Code_Coverage is

   Int_Image_Padding : constant Positive := 5;
   --  Size of padding wanted with String_Utils.Image

   --------------------
   -- Read_Gcov_Info --
   --------------------

   procedure Read_Gcov_Info
     (File_Node     : Code_Analysis.File_Access;
      File_Contents : String_Access;
      Lines_Count   : out Natural;
      Not_Cov_Count : out Natural)
   is
      Subp_Regexp       : constant Pattern_Matcher :=
                             Compile ("^function (\w+)([.]\d+)? called (\d+)",
                                      Multiple_Lines);
      Subp_Matches      : Match_Array (0 .. 3);
      Current           : Natural;
      Subprogram        : String_Access;
      Subp_Node         : Subprogram_Access;
      Line_Regexp       : constant Pattern_Matcher :=
                             Compile ("^ +(\d+|#####): *(\d+):(.*$)",
                                      Multiple_Lines);
      Line_Matches      : Match_Array (0 .. 3);
      Last_Line_Regexp  : constant Pattern_Matcher :=
                             Compile ("^ +-: *(\d+):", Multiple_Lines);
      Last_Line_Matches : Match_Array (0 .. 1);
      Line_Num          : Natural;
      Bad_Gcov_File     : exception;

   begin
      --  ??? should have one or several CE/exception handlers in this code,
      --  in case matching do not work "as expected"

      Current       := File_Contents'First;
      Lines_Count   := 0;
      Not_Cov_Count := 0;

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

      Current := File_Contents'Last;
      Current := Index
        (File_Contents.all, (1 => ASCII.LF), Current, Backward);

      if Current = File_Contents'Last then
         if Current > 0 then
            Current := Index
              (File_Contents.all, (1 => ASCII.LF), Current - 1, Backward);
            if Current = 0 then
               raise Bad_Gcov_File with VFS.Base_Name (File_Node.Name)
                 & ".gcov is not a valid Gcov file."
                 & " No last source code line found.";
            end if;
         else
            raise Bad_Gcov_File with VFS.Base_Name (File_Node.Name)
              & ".gcov is not a valid Gcov file";
         end if;
      end if;

      loop
         Match
           (Last_Line_Regexp, File_Contents.all, Last_Line_Matches, Current);

         exit when Last_Line_Matches (0) /= No_Match;

         if Current > 0 then
            Current := Index
              (File_Contents.all, (1 => ASCII.LF), Current - 1, Backward);

            if Current = 0 then
               raise Bad_Gcov_File with VFS.Base_Name (File_Node.Name)
                 & ".gcov is not a valid Gcov file."
                 & " No last source code line found.";
            end if;
         else
            raise Bad_Gcov_File with VFS.Base_Name (File_Node.Name)
              & ".gcov is not a valid Gcov file";
         end if;
      end loop;

      Lines_Count := Natural'Value
        (File_Contents
           (Last_Line_Matches (1).First .. Last_Line_Matches (1).Last));
      File_Node.Lines := new Line_Array (1 .. Lines_Count);
      --  Create a Line_Array with exactly the number of elements corresponding
      --  to the number of code lines in the original source code file.

      File_Node.Lines.all := (others => Null_Line);
      Current := File_Contents'First;

      loop
         Match (Line_Regexp, File_Contents.all, Line_Matches, Current);

         exit when Line_Matches (0) = No_Match;

         Line_Num := Natural'Value
           (File_Contents (Line_Matches (2).First .. Line_Matches (2).Last));
         File_Node.Lines (Line_Num).Number := Line_Num;

         case File_Contents (Line_Matches (1).First) is
            when '#' => File_Node.Lines (Line_Num).Analysis_Data.Coverage_Data
                 := new Coverage;
               File_Node.Lines (Line_Num).Analysis_Data.Coverage_Data.Coverage
                 := 0;
               File_Node.Lines (Line_Num).Contents := new String'(File_Contents
                    (Line_Matches (3).First .. Line_Matches (3).Last));
               Not_Cov_Count := Not_Cov_Count + 1;
            when others =>
               File_Node.Lines (Line_Num).Analysis_Data.Coverage_Data
                 := new Coverage;
               File_Node.Lines (Line_Num).Analysis_Data.Coverage_Data.Coverage
                 := Natural'Value
                 (File_Contents
                    (Line_Matches (1).First .. Line_Matches (1).Last));
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
         Data.Coverage := 0;
         Data.Children := 0;

         loop
            exit when Cur = No_Element;
            File_Node := Element (Cur);
            Next (Cur);

            if File_Node.Analysis_Data.Coverage_Data /= null then
               Data.Children := Data.Children +
                 Node_Coverage
                   (File_Node.Analysis_Data.Coverage_Data.all).Children;
               Data.Coverage := Data.Coverage +
                 File_Node.Analysis_Data.Coverage_Data.Coverage;
            end if;
         end loop;
      end;
   end Compute_Project_Coverage;

   ------------------------
   -- Dump_Node_Coverage --
   ------------------------

   procedure Dump_Node_Coverage (Coverage : Coverage_Access) is
   begin
      Put (Natural'Image (Coverage.Coverage)
           & " /"
           & Natural'Image (Node_Coverage (Coverage.all).Children));
   end Dump_Node_Coverage;

   ------------------------
   -- Dump_Line_Coverage --
   ------------------------

   procedure Dump_Line_Coverage (Coverage : Coverage_Access) is
   begin
      if Coverage.Coverage = 0 then
         Put (" warning: line never executed");
      else
         Put (Natural'Image (Coverage.Coverage) & " execution(s)");
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
                                return String_Access
   is
      Pango_Markup_To_Open_1 : constant String
        := "<span size=""small"" foreground=""";
      Pango_Markup_To_Open_2 : constant String := """>";
      Pango_Markup_To_Close : constant String := " </span>";
   begin
      case Coverage.Coverage is
         when 0 => return new String'(Pango_Markup_To_Open_1
                                      & "red"
                                      & Pango_Markup_To_Open_2
                                      & " not covered "
                                      & Pango_Markup_To_Close);
         when 1 => return new String'(Pango_Markup_To_Open_1
                                      & "black"
                                      & Pango_Markup_To_Open_2
                                      & " 1 time "
                                      & Pango_Markup_To_Close);
         when others =>
            return new String'(Pango_Markup_To_Open_1
              & "black"
              & Pango_Markup_To_Open_2
              & Image (Coverage.Coverage, Int_Image_Padding)
              & " times"
              & Pango_Markup_To_Close);
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
      --  Returns in a String the children count coverage info used to
      --  fill the Gtk_Tree_Store of a coverage report

      function Txt_Sub (Coverage  : Coverage_Access) return String;
      --  Returns in a String the Subprograms specific coverage info used to
      --  fill the Gtk_Tree_Store of a coverage report

      Cov_Txt   : constant String  := Natural'Image (Coverage.Coverage);
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
         --  Used to distinguish wether the Subprogram had already been called
         --  once or more in order to have a clean display

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
      Set (Tree_Store, Iter, Cov_Col,
           Image (Lig_Count, Int_Image_Padding)
           & Txt_Lig (Lig_Count)
           & Cov_Txt (Cov_Txt'First + 1 .. Cov_Txt'Last)
           & " not covered)"
           & Txt_Sub (Coverage));
      Set (Tree_Store, Iter, Sort_Col, Glib.Gint (Coverage.Coverage));
   end Fill_Iter;

end Code_Coverage;
