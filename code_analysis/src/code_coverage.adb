------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2006-2017, AdaCore                     --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings;           use Ada.Strings;
with Ada.Strings.Fixed;     use Ada.Strings.Fixed;
with GNAT.Regpat;           use GNAT.Regpat;
with Glib;
with Glib.Values;
with Glib_Values_Utils;     use Glib_Values_Utils;

with GPS.Intl;              use GPS.Intl;
with String_Utils;          use String_Utils;
with GNATCOLL.Symbols;      use GNATCOLL.Symbols;
with GNATCOLL.Utils;        use GNATCOLL.Utils;
with GNATCOLL.VFS;          use GNATCOLL.VFS;
with GNATCOLL.Traces;       use GNATCOLL.Traces;
with Language;              use Language;
with Code_Analysis_GUI;     use Code_Analysis_GUI;
with Code_Coverage.Gcov;    use Code_Coverage.Gcov;
with Code_Coverage.GNATcov; use Code_Coverage.GNATcov;
with Coverage_GUI;

package body Code_Coverage is

   Me : constant Trace_Handle := Create ("CODE_COVERAGE", GNATCOLL.Traces.On);

   Int_Image_Pad : constant Positive := 5;
   --  Size of padding wanted with GNATCOLL.Utils.Image
   Int_Char_Pad  : constant Character := ' ';
   --  Character used to pad in GNATCOLL.Utils.Image

   ---------------
   -- Set_Error --
   ---------------

   procedure Set_Error
     (File_Node  : Code_Analysis.File_Access;
      Error_Code : File_Coverage_Status)
   is
   begin
      if File_Node.Analysis_Data.Coverage_Data = null then
         File_Node.Analysis_Data.Coverage_Data := new File_Coverage;
      end if;

      File_Coverage
        (File_Node.Analysis_Data.Coverage_Data.all).Status := Error_Code;

      if File_Node.Lines = null then
         --  Set an empty line array in order to make File_Node a "finished"
         --  Code_Analysis node
         File_Node.Lines := new Line_Array (1 .. 1);
         File_Node.Lines.all := (others => Null_Line);
      end if;
   end Set_Error;

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

      begin
         Prj_Runs  := Positive'Value
           (File_Contents (Runs_Matches (1).First .. Runs_Matches (1).Last));
      exception
         when Constraint_Error =>
            Have_Runs := False;
            return;
      end;

      Have_Runs := True;
   end Get_Runs_Info_From_File;

   -------------------------
   -- Add_Subprogram_Info --
   -------------------------

   procedure Add_Subprogram_Info
     (File_Node : Code_Analysis.File_Access;
      Tree      : not null access Semantic_Tree'Class)
   is
      Current    : Semantic_Tree_Iterator'Class := Tree.Root_Iterator;
      Subp_Node  : Subprogram_Access;
      Subp_Name  : String_Access;
      Subp_Cov   : access Subprogram_Coverage := null;
      Line_Count : Natural := 0;

      procedure Update_Subprogram_Info_From_Line_Info
        (Line_Info : Code_Analysis.Line;
         Node      : Semantic_Node'Class);
      --  Update the subprogram coverage information related to the given Node
      --  from the given Line's coverage information.

      -------------------------------------------
      -- Update_Subprogram_Info_From_Line_Info --
      -------------------------------------------

      procedure Update_Subprogram_Info_From_Line_Info
        (Line_Info : Code_Analysis.Line;
         Node      : Semantic_Node'Class)
      is
         Line_Coverage_Data : constant Coverage_Access :=
                                Line_Info.Analysis_Data.Coverage_Data;
      begin
         if Line_Coverage_Data /= null then

            --   Create the subprogram information if not created yet
            if Subp_Cov = null then
               Subp_Name := new String'(Get (Node.Name).all);
               Subp_Node := Get_Or_Create
                 (File_Node,
                  Key => Subp_Name.all
                  & ' ' & Get (Node.Profile).all);
               Subp_Node.Name   := Subp_Name;
               Subp_Node.Line   := Node.Sloc_Def.Line;
               Subp_Node.Column := Integer (Node.Sloc_Def.Column);
               Subp_Node.Start  := Node.Sloc_Start.Line;
               Subp_Node.Stop   := Node.Sloc_End.Line;
               Subp_Node.Analysis_Data.Coverage_Data := new
                 Subprogram_Coverage'
                   (Coverage => 0,
                    Status   => Valid,
                    Called   => Line_Coverage_Data.Coverage,
                    Children => 1);
               --  ??? Here we make the hypothesis that the 1st
               --  executed line of the subprogram was executed
               --  exactly once by subprogram calls
               --  It fits with GCC 4.1 series
               Subp_Cov := Subprogram_Coverage
                 (Subp_Node.Analysis_Data.Coverage_Data.all)'Access;
            elsif not Line_Coverage_Data.Is_Exempted then
               --  Do not consider an exempted line as a child of the
               --  subprogram node.
               Subp_Cov.Children := Subp_Cov.Children + 1;
            end if;

            --  Do not take into account exempted lines for the coverage
            --  percentage.
            if not Line_Coverage_Data.Is_Exempted
              and then Line_Coverage_Data.Coverage = 0
            then
               Subp_Cov.Coverage := Subp_Cov.Coverage + 1;
            end if;
         end if;
      end Update_Subprogram_Info_From_Line_Info;

   begin
      while Has_Element (Current) loop
         declare
            Node       : constant Semantic_Node'Class := Element (Current);
            Start_Line : constant Natural := Node.Sloc_Start.Line;
            End_Line   : constant Natural := Node.Sloc_End.Line;
         begin
            if Node.Category in Subprogram_Category then
               for J in Start_Line .. End_Line loop
                  if J not in File_Node.Lines'Range then
                     --  This can occur only the Constructs information is
                     --  invalid. In this case, we want to log the error but
                     --  keep going, so that other information (such as project
                     --  totals, or the processing of other files) is still
                     --  reported to the user.

                     Trace (Me, +Full_Name (File_Node.Name) &
                              ": invalid construct at line" & Start_Line'Img);
                  else
                     Update_Subprogram_Info_From_Line_Info
                       (Line_Info => File_Node.Lines (J),
                        Node      => Node);
                  end if;
               end loop;

               if Subp_Cov /= null then
                  Line_Count := Line_Count + Subp_Cov.Children;
                  Subp_Cov := null;
               end if;
            end if;
         end;

         Next (Current);
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

            if File_Node.Analysis_Data.Coverage_Data /= null
              and then File_Node.Analysis_Data.Coverage_Data.Is_Valid
            then
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
         if Coverage.Is_Valid then
            Set_Attribute (Loc, "coverage", Natural'Image (Coverage.Coverage));
         end if;

         if Coverage.all in GNATcov_Line_Coverage'Class then
            Set_Attribute
              (Loc,
               "status",
               GNATcov_Line_Coverage_Status'Image
                 (GNATcov_Line_Coverage (Coverage.all).Status));

         elsif Coverage.all in Gcov_Line_Coverage'Class then
            Set_Attribute
              (Loc,
               "status",
               Gcov_Line_Coverage_Status'Image
                 (Gcov_Line_Coverage (Coverage.all).Status));

         elsif Coverage.all in Node_Coverage'Class then
            if Coverage.Is_Valid then
               Set_Attribute (Loc, "children", Natural'Image
                              (Node_Coverage (Coverage.all).Children));
            end if;

            if Coverage.all in Subprogram_Coverage'Class then
               Set_Attribute
                 (Loc,
                  "status",
                  Coverage_Status'Image
                    (Subprogram_Coverage (Coverage.all).Status));

               if Coverage.Is_Valid then
                  Set_Attribute (Loc, "called", Natural'Image
                                 (Subprogram_Coverage (Coverage.all).Called));
               end if;

            elsif Coverage.all in File_Coverage'Class then
               Set_Attribute
                 (Loc,
                  "status",
                  File_Coverage_Status'Image
                    (File_Coverage (Coverage.all).Status));

            elsif Coverage.all in Project_Coverage'Class then
               Set_Attribute
                 (Loc,
                  "status",
                  Coverage_Status'Image
                    (Project_Coverage (Coverage.all).Status));

               if Coverage.Is_Valid then
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
      function Status_Value
        (Status : String) return Coverage_Status;
      --  Return the coverage status associated with an error message

      function Status_Value
        (Status : String) return File_Coverage_Status;
      --  Return the coverage status associated with an error message

      function Status_Value
        (Status : String) return GNATcov_Line_Coverage_Status;
      --  Return the coverage status associated with an error message

      function Status_Value
        (Status : String) return Gcov_Line_Coverage_Status;
      --  Return the coverage status associated with an error message

      ------------------
      -- Status_Value --
      ------------------

      function Status_Value
        (Status : String) return Coverage_Status is
      begin
         return Coverage_Status'Value (Status);

      exception
         when Constraint_Error =>
            return Undetermined;
      end Status_Value;

      function Status_Value
        (Status : String) return File_Coverage_Status is
      begin
         return File_Coverage_Status'Value (Status);

      exception
         when Constraint_Error =>
            return Undetermined;
      end Status_Value;

      function Status_Value
        (Status : String) return GNATcov_Line_Coverage_Status is
      begin
         return GNATcov_Line_Coverage_Status'Value (Status);

      exception
         when Constraint_Error =>
            return Undetermined;
      end Status_Value;

      function Status_Value
        (Status : String) return Gcov_Line_Coverage_Status is
      begin
         return Gcov_Line_Coverage_Status'Value (Status);

      exception
         when Constraint_Error =>
            return Undetermined;
      end Status_Value;

      Txt_Status : constant String := Get_Attribute (Loc, "status");

   begin
      if Txt_Status /= "" then
         if Loc.Tag.all = "Line" then
            case Coverage_GUI.Current_Coverage_Tool is
               when Coverage_GUI.Gcov =>
                  Coverage := new Gcov_Line_Coverage;
                  Gcov_Line_Coverage (Coverage.all).Status :=
                    Status_Value (Txt_Status);

               when Coverage_GUI.GNATcov =>
                  Coverage := new GNATcov_Line_Coverage;
                  GNATcov_Line_Coverage (Coverage.all).Status :=
                    Status_Value (Txt_Status);
            end case;

         elsif Loc.Tag.all = "Subprogram" then
            Coverage := new Subprogram_Coverage;
            Subprogram_Coverage (Coverage.all).Status :=
              Status_Value (Txt_Status);

            if Coverage.Is_Valid then
               Node_Coverage (Coverage.all).Children :=
                 Natural'Value (Get_Attribute (Loc, "children"));

               declare
                  Txt_Called : constant String :=
                    Get_Attribute (Loc, "called");
               begin
                  if Txt_Called /= "" then
                     Subprogram_Coverage (Coverage.all).Called :=
                       Natural'Value (Txt_Called);
                  end if;
               end;
            end if;

         elsif Loc.Tag.all = "File" then
            Coverage := new File_Coverage;
            File_Coverage (Coverage.all).Status := Status_Value (Txt_Status);

            if Coverage.Is_Valid then
               Node_Coverage (Coverage.all).Children :=
                 Natural'Value (Get_Attribute (Loc, "children"));
            end if;

         elsif Loc.Tag.all = "Project" then
            Coverage := new Project_Coverage;
            Project_Coverage (Coverage.all).Status :=
              Status_Value (Txt_Status);

            if Coverage.Is_Valid then
               Node_Coverage (Coverage.all).Children :=
                 Natural'Value (Get_Attribute (Loc, "children"));

               declare
                  Txt_Runs : constant String := Get_Attribute (Loc, "runs");
               begin
                  if Txt_Runs /= "" then
                     Project_Coverage (Coverage.all).Have_Runs := True;
                     Project_Coverage (Coverage.all).Runs :=
                       Natural'Value (Txt_Runs);
                  end if;
               end;
            end if;
         end if;

         if Coverage.Is_Valid then
            Coverage.Coverage :=
              Natural'Value (Get_Attribute (Loc, "coverage"));
         end if;
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
           Project_Coverage (Coverage.all).Have_Runs
         then

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

      Values  : Glib.Values.GValue_Array (1 .. 4);
      Columns : constant Columns_Array (Values'Range) :=
        (Cov_Col, Cov_Sort, Cov_Bar_Txt, Cov_Bar_Val);

   begin
      if Coverage.Is_Valid then
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

            Values :=
              (1 => As_String
                 (Image (Lig_Count) & Txt_Lig (Lig_Count) &
                    Cov_Txt (Cov_Txt'First + 1 .. Cov_Txt'Last) &
                  (-" not covered)") & Txt_Sub (Coverage)),
               2 => As_Int (Glib.Gint (Coverage.Coverage)),
               3 => As_String
                 (Image (Cov_Percent, Int_Image_Pad,
                  Padding => Int_Char_Pad) & " %"),
               4 => As_Int (Glib.Gint (Cov_Percent)));
         end;

      else
         Values :=
           (1 => As_String (" undetermined"),
            --  & Status_Message (Coverage.Status));
            2 => As_Int    (0),
            3 => As_String ("n/a"),
            4 => As_Int    (0));
      end if;

      Set_And_Clear (Tree_Store, Iter, Columns, Values);
   end Fill_Iter;

end Code_Coverage;
