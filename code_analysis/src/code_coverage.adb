-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2006-2007                      --
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

with Ada.Text_IO;            use Ada.Text_IO;
with Ada.Strings;            use Ada.Strings;
with Ada.Strings.Fixed;      use Ada.Strings.Fixed;
with GNAT.Regpat;            use GNAT.Regpat;
with Glib;
with String_Utils;           use String_Utils;
with VFS;                    use VFS;
with Language;               use Language;
with Language.Tree;          use Language.Tree;

package body Code_Coverage is

   Int_Image_Padding : constant Positive := 5;
   --  Size of padding wanted with String_Utils.Image

   -------------------
   -- Add_File_Info --
   -------------------

   procedure Add_File_Info
     (File_Node     : Code_Analysis.File_Access;
      File_Contents : String_Access;
      Lines_Count   : out Natural;
      Not_Cov_Count : out Natural)
   is
      Current           : Natural;
      Line_Regexp       : constant Pattern_Matcher :=
                             Compile ("^ +(\d+|#####): *(\d+):(.*$)",
                                      Multiple_Lines);
      Line_Matches      : Match_Array (0 .. 3);
      Last_Line_Regexp  : constant Pattern_Matcher :=
                             Compile ("^ +(\d+|-): *(\d+):", Multiple_Lines);
      Last_Line_Matches : Match_Array (0 .. 2);
      Line_Num          : Natural;
      Bad_Gcov_File     : exception;
   begin
      Not_Cov_Count := 0;

      --------------------------------------
      -- Determination of the line number --
      --------------------------------------

      Current := File_Contents'Last;
      Current := Index (File_Contents.all, (1 => ASCII.LF), Current, Backward);

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
           (Last_Line_Matches (2).First .. Last_Line_Matches (2).Last));

      -------------------------------
      -- line coverage information --
      -------------------------------

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
   end Add_File_Info;

   -----------------------------
   -- Get_Runs_Info_From_File --
   -----------------------------

   function Get_Runs_Info_From_File
     (File_Node     : Code_Analysis.File_Access;
      File_Contents : String_Access) return Positive
   is
      Current       : Natural;
      Runs_Regexp   : constant Pattern_Matcher :=
                            Compile ("^ +-: +0:Runs:(\d+)", Multiple_Lines);
      Runs_Matches  : Match_Array (0 .. 1);
      Bad_Gcov_File : exception;
   begin

      Current := File_Contents'First;

      for J in 1 .. 4 loop
         Current := Index
           (File_Contents.all, (1 => ASCII.LF), Current, Forward);
      end loop;

      Match (Runs_Regexp, File_Contents.all, Runs_Matches, Current);

      if Runs_Matches (0) = No_Match then
         raise Bad_Gcov_File with VFS.Base_Name (File_Node.Name)
           & ".gcov is not a valid Gcov file."
           & " No runs quantity information found.";
      end if;

      return Positive'Value
        (File_Contents (Runs_Matches (1).First .. Runs_Matches (1).Last));
   end Get_Runs_Info_From_File;

   -------------------------
   -- Add_Subprogram_Info --
   -------------------------

   procedure Add_Subprogram_Info
     (Data_File : Structured_File_Access;
      File_Node : Code_Analysis.File_Access)
   is
      Tree      : constant Construct_Tree := Get_Full_Tree (Data_File);
      Node      : Construct_Tree_Iterator := First (Tree);
      Node_Info : Simple_Construct_Information;
      Subp_Node : Subprogram_Access;
      Subp_Name : String_Access;
   begin
      loop
         Node_Info := Get_Construct (Node);

         if Node_Info.Category in Subprogram_Category then
            Subp_Name := new String'(Node_Info.Name.all);
            Subp_Node := Get_Or_Create (File_Node, Subp_Name);
            Subp_Node.Line   := Node_Info.Sloc_Entity.Line;
            Subp_Node.Column := Node_Info.Sloc_Entity.Column;
            Subp_Node.Start  := Node_Info.Sloc_Start.Line;
            Subp_Node.Stop   := Node_Info.Sloc_End.Line;
            Subp_Node.Analysis_Data.Coverage_Data := new Subprogram_Coverage'
              (Coverage => 0,
               Called   => 99, -- ??? intended crazy value
               Children =>
                 Node_Info.Sloc_End.Line - Node_Info.Sloc_Start.Line);

            if Node_Info.Sloc_End.Line - Node_Info.Sloc_Start.Line = 0 then
               Subprogram_Coverage
                 (Subp_Node.Analysis_Data.Coverage_Data.all).Children := 1;
            end if;

            for J in Node_Info.Sloc_Start.Line .. Node_Info.Sloc_End.Line loop
               if File_Node.Lines (J).Analysis_Data.Coverage_Data /= null then
                  if File_Node.Lines (J).Analysis_Data.Coverage_Data.Coverage =
                    0 then
                     Subp_Node.Analysis_Data.Coverage_Data.Coverage :=
                       Subp_Node.Analysis_Data.Coverage_Data.Coverage + 1;
                  end if;
               end if;
            end loop;
         end if;

         Node := Next (Tree, Node);

         exit when Node = Null_Construct_Tree_Iterator;
      end loop;
   end Add_Subprogram_Info;

   ------------------------------
   -- Compute_Project_Coverage --
   ------------------------------

   procedure Compute_Project_Coverage (Project_Node : Project_Access) is
      use File_Maps;
      Cur       : File_Maps.Cursor;
      File_Node : Code_Analysis.File_Access;
   begin
      Cur := Project_Node.Files.First;

      if Project_Node.Analysis_Data.Coverage_Data = null then
         Project_Node.Analysis_Data.Coverage_Data := new Subprogram_Coverage;
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

         --  if nothing had been computed, then Project_Node must not have a
         --  Coverage_Data attached to him
         if Data.Children = 0 then
            Unchecked_Free (Project_Node.Analysis_Data.Coverage_Data);
         end if;
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

      Cov_Txt     : constant String  := Natural'Image (Coverage.Coverage);
      Lig_Count   : constant Natural := Node_Coverage (Coverage.all).Children;
      Cov_Percent : constant Natural :=
                      (Lig_Count - Coverage.Coverage) * 100 / Lig_Count;

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
      Set (Tree_Store, Iter, Cov_Sort, Glib.Gint (Coverage.Coverage));
      Set (Tree_Store, Iter, Cov_Bar_Val, Glib.Gint (Cov_Percent));
      Set (Tree_Store, Iter, Cov_Bar_Txt,
           Image (Cov_Percent, Int_Image_Padding)
           & " %");
   end Fill_Iter;

end Code_Coverage;
