-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                  Copyright (C) 2006-2008, AdaCore                 --
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
with Ada.Strings;       use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with GNAT.Regpat;       use GNAT.Regpat;
with Glib;
with GPS.Intl;          use GPS.Intl;
with String_Utils;      use String_Utils;
with GNATCOLL.Utils;    use GNATCOLL.Utils;
with GNATCOLL.VFS;      use GNATCOLL.VFS;
with Language;          use Language;
with Code_Analysis_GUI; use Code_Analysis_GUI;

package body Code_Coverage is

   Int_Image_Pad : constant Positive := 5;
   --  Size of padding wanted with GNATCOLL.Utils.Image
   Int_Char_Pad  : constant Character := ' ';
   --  Character used to pad in GNATCOLL.Utils.Image

   ---------------
   -- Set_Error --
   ---------------

   procedure Set_Error
     (File_Node  : Code_Analysis.File_Access;
      Error_Code : Coverage_Status) is
   begin
      if File_Node.Analysis_Data.Coverage_Data = null then
         File_Node.Analysis_Data.Coverage_Data := new Coverage;
      end if;

      File_Node.Analysis_Data.Coverage_Data.Status := Error_Code;

      if File_Node.Lines = null then
         --  Set an empty line array in order to make File_Node a "finished"
         --  Code_Analysis node
         File_Node.Lines := new Line_Array (1 .. 1);
         File_Node.Lines.all := (others => Null_Line);
      end if;
   end Set_Error;

   -------------------
   -- Add_File_Info --
   -------------------

   procedure Add_File_Info
     (File_Node     : Code_Analysis.File_Access;
      File_Contents : String_Access)
   is
      Current           : Natural;
      Line_Regexp       : constant Pattern_Matcher := Compile
        ("^ +(\d+|#####): *(\d+):(.*$)", Multiple_Lines);
      Line_Matches      : Match_Array (0 .. 3);
      Last_Line_Regexp  : constant Pattern_Matcher := Compile
        ("^ +(\d+|#####|-): *(\d+):", Multiple_Lines);
      Last_Line_Matches : Match_Array (0 .. 2);
      Line_Num          : Natural;
      Lines_Count       : Natural := 0;
      Not_Cov_Count     : Natural := 0;
   begin
      if File_Node.Analysis_Data.Coverage_Data = null then
         File_Node.Analysis_Data.Coverage_Data := new Node_Coverage;
      end if;

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
               Set_Error (File_Node, File_Corrupted);
               --  The .gcov file is not a valid Gcov file.
               --  No last source code line found.
               return;
            end if;
         else
            Set_Error (File_Node, File_Empty);
            --  the .gcov file is not a valid Gcov file.
            return;
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
               Set_Error (File_Node, File_Corrupted);
               --  The .gcov file is not a valid Gcov file.
               --  No last source code line found.
               return;
            end if;
         else
            Set_Error (File_Node, File_Corrupted);
            --  the .gcov file is not a valid Gcov file.
            return;
         end if;
      end loop;

      -------------------------------
      -- line coverage information --
      -------------------------------

      File_Node.Lines := new Line_Array (1 .. Natural'Value (File_Contents
        (Last_Line_Matches (2).First .. Last_Line_Matches (2).Last)));
      --  Create a Line_Array with exactly the number of elements corresponding
      --  to the number of code lines in the original source code file.

      File_Node.Lines.all := (others => Null_Line);
      Current := File_Contents'First;

      loop
         Match (Line_Regexp, File_Contents.all, Line_Matches, Current);

         exit when Line_Matches (0) = No_Match;

         Lines_Count := Lines_Count + 1;
         Line_Num := Natural'Value
           (File_Contents (Line_Matches (2).First .. Line_Matches (2).Last));
         File_Node.Lines (Line_Num).Number := Line_Num;
         File_Node.Lines (Line_Num).Analysis_Data.Coverage_Data :=
           new Coverage;
         File_Node.Lines (Line_Num).Analysis_Data.Coverage_Data.Status :=
           Valid;

         case File_Contents (Line_Matches (1).First) is
            when '#' =>
               File_Node.Lines (Line_Num).Analysis_Data.Coverage_Data.Coverage
                 := 0;
               File_Node.Lines (Line_Num).Contents := new String'
                 (File_Contents
                    (Line_Matches (3).First .. Line_Matches (3).Last));
               Not_Cov_Count := Not_Cov_Count + 1;
            when others =>
               File_Node.Lines (Line_Num).Analysis_Data.Coverage_Data.Coverage
                 := Natural'Value
                 (File_Contents
                    (Line_Matches (1).First .. Line_Matches (1).Last));
         end case;

         Current := Line_Matches (0).Last + 1;
      end loop;

      Node_Coverage (File_Node.Analysis_Data.Coverage_Data.all).Children :=
        Lines_Count;
      File_Node.Analysis_Data.Coverage_Data.Coverage := Not_Cov_Count;
      File_Node.Analysis_Data.Coverage_Data.Status := Valid;
   end Add_File_Info;

   -----------------------------
   -- Get_Runs_Info_From_File --
   -----------------------------

   procedure Get_Runs_Info_From_File
     (File_Contents : String_Access;
      Prj_Runs      : out Positive;
      Have_Runs     : out Boolean)
   is
      Current       : Natural;
      Runs_Regexp   : constant Pattern_Matcher :=
                            Compile ("^ +-: +0:Runs:(\d+)", Multiple_Lines);
      Runs_Matches  : Match_Array (0 .. 1);
   begin

      Current := File_Contents'First;

      for J in 1 .. 4 loop
         Current := Index
           (File_Contents.all, (1 => ASCII.LF), Current, Forward);
      end loop;

      Match (Runs_Regexp, File_Contents.all, Runs_Matches, Current);

      if Runs_Matches (0) = No_Match then
         Have_Runs := False;
         return;
         --  The .gcov have no runs count information
      end if;

      Prj_Runs  := Positive'Value
        (File_Contents (Runs_Matches (1).First .. Runs_Matches (1).Last));
      Have_Runs := True;
   end Get_Runs_Info_From_File;

   -------------------------
   -- Add_Subprogram_Info --
   -------------------------

   procedure Add_Subprogram_Info
     (File_Node : Code_Analysis.File_Access;
      Tree      : Construct_Tree)
   is
      Node       : Construct_Tree_Iterator := First (Tree);
      Node_Info  : access Simple_Construct_Information;
      Subp_Node  : Subprogram_Access;
      Subp_Name  : String_Access;
      Subp_Cov   : access Subprogram_Coverage := null;
      Line_Count : Natural := 0;
   begin
      loop
         Node_Info := Get_Construct (Node);

         if Node_Info.Category in Subprogram_Category then
            for J in Node_Info.Sloc_Start.Line .. Node_Info.Sloc_End.Line loop
               if File_Node.Lines (J).Analysis_Data.Coverage_Data /= null then
                  if Subp_Cov = null then
                     Subp_Name := new String'(Node_Info.Name.all);
                     Subp_Node := Get_Or_Create (File_Node, Subp_Name);
                     Subp_Node.Line   := Node_Info.Sloc_Entity.Line;
                     Subp_Node.Column := Node_Info.Sloc_Entity.Column;
                     Subp_Node.Start  := Node_Info.Sloc_Start.Line;
                     Subp_Node.Stop   := Node_Info.Sloc_End.Line;
                     Subp_Node.Analysis_Data.Coverage_Data := new
                       Subprogram_Coverage'
                         (Coverage => 0,
                          Status   => Valid,
                          Called   => File_Node.Lines
                            (J).Analysis_Data.Coverage_Data.Coverage,
                          Children => 1);
                     --  ??? Here we make the hypothesis that the 1st
                     --  executed line of the subprogram was executed
                     --  excatly one time by subprogram calls
                     --  It fits with GCC 4.1 series
                     Subp_Cov := Subprogram_Coverage
                       (Subp_Node.Analysis_Data.Coverage_Data.all)'Access;
                  else
                     Subp_Cov.Children := Subp_Cov.Children + 1;
                  end if;

                  if File_Node.Lines (J).Analysis_Data.Coverage_Data.Coverage =
                    0 then
                     Subp_Cov.Coverage := Subp_Cov.Coverage + 1;
                  end if;
               end if;
            end loop;

            if Subp_Cov /= null then
               Line_Count := Line_Count + Subp_Cov.Children;
               Subp_Cov := null;
            end if;
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
         return;
         --  An Add_File_Info should have set up Runs/Called info
      end if;

      declare
         Data : constant access Node_Coverage := Node_Coverage
           (Project_Node.Analysis_Data.Coverage_Data.all)'Access;
      begin
         Data.Coverage := 0;
         Data.Children := 0;

         loop
            exit when Cur = No_Element;
            File_Node := Element (Cur);
            Next (Cur);

            if File_Node.Analysis_Data.Coverage_Data /= null and then
              File_Node.Analysis_Data.Coverage_Data.Status = Valid then
               Data.Children := Data.Children + Node_Coverage
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

   -----------------------
   -- Dump_Prj_Coverage --
   -----------------------

   procedure Dump_Prj_Coverage (Coverage : Coverage_Access) is
   begin
      Dump_Node_Coverage (Coverage);

      if Project_Coverage (Coverage.all).Have_Runs then
         Put (Natural'Image (Project_Coverage (Coverage.all).Runs)
              & " run(s)");
      end if;
   end Dump_Prj_Coverage;

   -----------------------
   -- XML_Dump_Coverage --
   -----------------------

   procedure XML_Dump_Coverage (Coverage : Coverage_Access; Loc : Node_Ptr) is
   begin
      if Coverage /= null then
         Set_Attribute (Loc, "status", Status_Message (Coverage.Status));

         if Coverage.Status = Valid then
            Set_Attribute (Loc, "coverage", Natural'Image (Coverage.Coverage));

            if Coverage.all in Node_Coverage'Class then
               Set_Attribute (Loc, "children", Natural'Image
                              (Node_Coverage (Coverage.all).Children));
               if Coverage.all in Subprogram_Coverage'Class then
                  Set_Attribute (Loc, "called", Natural'Image
                                 (Subprogram_Coverage (Coverage.all).Called));
               elsif Coverage.all in Project_Coverage'Class then
                  if Project_Coverage (Coverage.all).Have_Runs then
                     Set_Attribute (Loc, "runs", Natural'Image
                                    (Project_Coverage (Coverage.all).Runs));
                  end if;
               end if;
            end if;
         end if;
      end if;
   end XML_Dump_Coverage;

   ------------------------
   -- XML_Parse_Coverage --
   ------------------------

   procedure XML_Parse_Coverage
     (Coverage : in out Coverage_Access;
      Loc      : Node_Ptr)
   is
      Txt_Status : constant String := Get_Attribute (Loc, "status");
      Status     : Coverage_Status;
   begin
      if Txt_Status /= "" then
         Status := Status_Value (Txt_Status);

         if Status = Valid then
            if Loc.Tag.all = "Line" then
               Coverage := new Code_Analysis.Coverage;
            else
               if Loc.Tag.all = "Subprogram" then
                  declare
                     Txt_Called : constant String :=
                                    Get_Attribute (Loc, "called");
                  begin
                     Coverage := new Subprogram_Coverage;

                     if Txt_Called /= "" then
                        Subprogram_Coverage (Coverage.all).Called :=
                          Natural'Value (Txt_Called);
                     end if;
                  end;
               elsif Loc.Tag.all = "File" then
                  Coverage := new Node_Coverage;
               elsif Loc.Tag.all = "Project" then
                  declare
                     Txt_Runs : constant String := Get_Attribute (Loc, "runs");
                  begin
                     Coverage := new Project_Coverage;

                     if Txt_Runs /= "" then
                        Project_Coverage (Coverage.all).Have_Runs := True;
                        Project_Coverage (Coverage.all).Runs :=
                          Natural'Value (Txt_Runs);
                     end if;
                  end;
               end if;

               Node_Coverage (Coverage.all).Children :=
                 Natural'Value (Get_Attribute (Loc, "children"));
            end if;

            Coverage.Coverage := Natural'Value
              (Get_Attribute (Loc, "coverage"));
         else
            Coverage := new Code_Analysis.Coverage;
         end if;

         Coverage.Status := Status;
      end if;
   end XML_Parse_Coverage;

   --------------------------------------
   -- First_Project_With_Coverage_Data --
   --------------------------------------

   function First_Project_With_Coverage_Data
     (Projects : Code_Analysis_Tree) return Project_Type
   is
      use Project_Maps;
      Prj_Node : Code_Analysis.Project_Access;
      Prj_Cur  : Project_Maps.Cursor := Projects.First;
   begin
      if Prj_Cur /= No_Element then
         Prj_Node := Element (Prj_Cur);
      else
         return No_Project;
      end if;

      loop
         exit when Prj_Cur = No_Element;
         Prj_Node := Element (Prj_Cur);
         exit when Prj_Node.Analysis_Data.Coverage_Data /= null;
         Next (Prj_Cur);
      end loop;

      if Prj_Cur /= No_Element then
         return Prj_Node.Name;
      else
         return No_Project;
      end if;
   end First_Project_With_Coverage_Data;

   ------------------------
   -- Line_Coverage_Info --
   ------------------------

   function Line_Coverage_Info
     (Coverage : Coverage_Access;
      Bin_Mode : Boolean := False) return Line_Information_Record
   is
      Pango_Markup_To_Open_1 : constant String := "<span foreground=""";
      Pango_Markup_To_Open_2 : constant String := """>";
      Pango_Markup_To_Close  : constant String := "</span>";

      Result : Line_Information_Record;

   begin
      if Bin_Mode then
         case Coverage.Coverage is
         when 0 =>
            Result.Image := Uncovered_Line_Pixbuf;
            Result.Tooltip_Text := new String'
              (-"The code for this line has not been executed.");
         when others =>
            Result.Image := Covered_Line_Pixbuf;
            Result.Tooltip_Text := new String'
              (-"The code for this line has been executed.");
         end case;
      else
         case Coverage.Coverage is
         when 0 =>
            Result.Text := new
              String'(Pango_Markup_To_Open_1
                      & "red"
                      & Pango_Markup_To_Open_2
                      & "#"
                      & Pango_Markup_To_Close);
         when 1 =>
            Result.Text := new
              String'(Pango_Markup_To_Open_1
                      & "black"
                      & Pango_Markup_To_Open_2
                      & (-" 1 time ")
                      & Pango_Markup_To_Close);
         when others =>
            Result.Text := new
              String'
                (Pango_Markup_To_Open_1
                 & "black"
                 & Pango_Markup_To_Open_2
                 & Image (Coverage.Coverage)
                 & " times"
                 & Pango_Markup_To_Close);
         end case;
      end if;

      return Result;
   end Line_Coverage_Info;

   ---------------
   -- Fill_Iter --
   ---------------

   procedure Fill_Iter
     (Tree_Store : Gtk_Tree_Store;
      Iter       : Gtk_Tree_Iter;
      Coverage   : Coverage_Access;
      Bin_Mode   : Boolean := False)
   is
      function Txt_Lig (Lig_Count : Natural) return String;
      --  Returns in a String the children count coverage info used to
      --  fill the Gtk_Tree_Store of a coverage report

      function Txt_Sub (Coverage  : Coverage_Access) return String;
      --  Returns in a String the Subprograms specific coverage info used to
      --  fill the Gtk_Tree_Store of a coverage report

      -------------
      -- Txt_Lig --
      -------------

      function Txt_Lig (Lig_Count : Natural) return String is
      begin
         if Lig_Count = 1 then
            return -" line (";
         else
            return -" lines (";
         end if;
      end Txt_Lig;

      -------------
      -- Txt_Sub --
      -------------

      function Txt_Sub (Coverage : Coverage_Access) return String is

         function Txt_Cal (Cal_Count : Natural) return String;
         --  Used to distinguish wether the Subprogram had already been called
         --  once or more in order to have a clean display

         -------------
         -- Txt_Cal --
         -------------

         function Txt_Cal (Cal_Count : Natural) return String is
         begin
            if Cal_Count = 1 then
               return -" time";
            else
               return -" times";
            end if;
         end Txt_Cal;

      begin
         if Bin_Mode then
            return "";
         end if;

         if Coverage.all in Subprogram_Coverage'Class then
            declare
               Cal_Count : constant Natural :=
                             Subprogram_Coverage (Coverage.all).Called;
            begin
               return String'(-", called" & Natural'Image (Cal_Count)
                              & Txt_Cal (Cal_Count));
            end;
         elsif Coverage.all in Project_Coverage'Class and then
           Project_Coverage (Coverage.all).Have_Runs then

            declare
               Run_Count : constant Natural :=
                             Project_Coverage (Coverage.all).Runs;
            begin
               return String'(-", ran" & Natural'Image (Run_Count)
                              & Txt_Cal (Run_Count));
            end;
         else
            return "";
         end if;
      end Txt_Sub;

   begin
      if Coverage.Status = Valid then
         declare
            Cov_Txt     : constant String :=
                            Natural'Image (Coverage.Coverage);
            Lig_Count   : constant Natural :=
                            Node_Coverage (Coverage.all).Children;
            Cov_Percent : Natural;
         begin
            if Lig_Count = 0 then
               Cov_Percent := 100;
            else
               Cov_Percent := (Lig_Count - Coverage.Coverage) * 100
                 / Lig_Count;
            end if;

            Set (Tree_Store, Iter, Cov_Col,
                 Image (Lig_Count)
                 & Txt_Lig (Lig_Count)
                 & Cov_Txt (Cov_Txt'First + 1 .. Cov_Txt'Last)
                 & (-" not covered)")
                 & Txt_Sub (Coverage));
            Set (Tree_Store, Iter, Cov_Sort, Glib.Gint (Coverage.Coverage));
            Set (Tree_Store, Iter, Cov_Bar_Val, Glib.Gint (Cov_Percent));
            Set (Tree_Store, Iter, Cov_Bar_Txt,
                 Image (Cov_Percent, Int_Image_Pad, Padding => Int_Char_Pad) &
                 " %");
         end;

      else
         Set (Tree_Store, Iter, Cov_Col,
              " " & Status_Message (Coverage.Status));
         Set (Tree_Store, Iter, Cov_Sort, Glib.Gint (0));
         Set (Tree_Store, Iter, Cov_Bar_Val, Glib.Gint (0));
         Set (Tree_Store, Iter, Cov_Bar_Txt, -"n/a");
      end if;
   end Fill_Iter;

   --------------------
   -- Status_Message --
   --------------------

   function Status_Message
     (Status : Coverage_Status) return String is
   begin
      case Status is
         when File_Not_Found   => return -"Gcov file not found";
         when File_Out_Of_Date => return -"Gcov file out-of-date";
         when File_Corrupted   => return -"Gcov file corrupted";
         when File_Empty       => return -"Gcov file empty";
         when Valid            => return -"Gcov file valid";
         when Undeterminated   => return -"Status undeterminated";
      end case;
   end Status_Message;

   ------------------
   -- Status_Value --
   ------------------

   function Status_Value
     (Status : String) return Coverage_Status is
   begin
      if    Status = -"Gcov file not found"   then
         return File_Not_Found;
      elsif Status = -"Gcov file out-of-date" then
         return File_Out_Of_Date;
      elsif Status = -"Gcov file corrupted"   then
         return File_Corrupted;
      elsif Status = -"Gcov file empty"       then
         return File_Empty;
      elsif Status = -"Gcov file valid"       then
         return Valid;
      elsif Status = -"Status undeterminated" then
         return Undeterminated;
      end if;

      return Undeterminated;
   end Status_Value;

end Code_Coverage;
