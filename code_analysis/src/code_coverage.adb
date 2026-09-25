------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2006-2026, AdaCore                     --
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

with Ada.Text_IO;       use Ada.Text_IO;
with Ada.Strings;       use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with GNAT.Regpat;       use GNAT.Regpat;
with Glib;
with Glib.Convert;
with Glib.Values;
with Glib_Values_Utils; use Glib_Values_Utils;

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

   Me : constant Trace_Handle :=
     Create ("GPS.CODE_ANALYSIS.CODE_COVERAGE", GNATCOLL.Traces.On);

   Int_Image_Pad : constant Positive := 5;
   --  Size of padding wanted with GNATCOLL.Utils.Image
   Int_Char_Pad  : constant Character := ' ';
   --  Character used to pad in GNATCOLL.Utils.Image

   package ASU renames Ada.Strings.Unbounded;
   use type ASU.Unbounded_String;

   function Unit_Label
     (Metric : Coverage_Metric; With_Name : Boolean) return String;
   --  The noun naming what Metric counts, singular when it counts exactly
   --  one: "lines" for our own line-based figure, "obligations" for a
   --  criterion reported by the coverage tool, prefixed with that criterion's
   --  name when With_Name is set ("MC/DC obligations").

   function Metric_Label (Metric : Coverage_Metric) return String;
   --  The short label naming Metric's criterion, as displayed inside the
   --  progress bar of the coverage report, e.g. "MC/DC" or "lines"

   function Percentage (Metric : Coverage_Metric) return Natural;
   --  The percentage of Metric's obligations that are covered. 100 when
   --  Metric counts no obligation at all.

   function Uncovered (Metric : Coverage_Metric) return Natural;
   --  The number of Metric's obligations that are not covered

   function Coverage_Tooltip
     (Node : Node_Coverage'Class; Shown : Coverage_Metric) return String;
   --  The full coverage breakdown for Node: the coverage level the tool
   --  announced, every figure it reported, our own line-based figure, and
   --  which of them Shown is. A figure that does not account for every one
   --  of Node's files also names the files it leaves out.

   procedure Dump_Metrics (Node : Node_Coverage'Class; Loc : Node_Ptr);
   --  Add to Loc the coverage figures reported by the coverage tool, as a
   --  "metrics" attribute of the form "statement=4,4,1;MC/DC=1,2,1" (one
   --  "name=covered,total,files" per criterion, followed by the
   --  '|'-separated base names of the files that criterion omits, if any),
   --  plus the announced coverage level. Both are omitted when the tool
   --  reported no figure at all, so a report saved by a newer GNAT Studio
   --  still loads in an older one.

   procedure Parse_Metrics (Node : in out Node_Coverage'Class; Loc : Node_Ptr);
   --  Read back what Dump_Metrics wrote. Tolerates the absence of the
   --  attributes, so a report saved by an older GNAT Studio still loads.

   ----------------
   -- Unit_Label --
   ----------------

   function Unit_Label
     (Metric : Coverage_Metric; With_Name : Boolean) return String
   is
      Singular : constant Boolean := Metric.Total = 1;
   begin
      if Metric.Kind = Line_Metric then
         return (if Singular then -"line" else -"lines");
      end if;

      return
        (if With_Name then Metric_Label (Metric) & " " else "")
        & (if Singular then -"obligation" else -"obligations");
   end Unit_Label;

   ------------------
   -- Metric_Label --
   ------------------

   function Metric_Label (Metric : Coverage_Metric) return String is
   begin
      --  An unrecognized criterion is only known by the name the tool printed

      return
        (if Metric.Kind = Unknown_Metric
         then ASU.To_String (Metric.Name)
         else Metric_Kind_Name (Metric.Kind));
   end Metric_Label;

   ----------------
   -- Percentage --
   ----------------

   function Percentage (Metric : Coverage_Metric) return Natural is
   begin
      --  Covered can only exceed Total in a report that was saved corrupted:
      --  do not let that crash the whole view

      if Metric.Total = 0 or else Metric.Covered >= Metric.Total then
         return 100;
      else
         return Metric.Covered * 100 / Metric.Total;
      end if;
   end Percentage;

   ---------------
   -- Uncovered --
   ---------------

   function Uncovered (Metric : Coverage_Metric) return Natural is
   begin
      return
        (if Metric.Total >= Metric.Covered
         then Metric.Total - Metric.Covered
         else 0);
   end Uncovered;

   ----------------------
   -- Coverage_Tooltip --
   ----------------------

   function Coverage_Tooltip
     (Node : Node_Coverage'Class; Shown : Coverage_Metric) return String
   is
      Max_Named_Files : constant := 5;
      --  How many of the files a partial figure omits the tooltip names
      --  before falling back on counting the remainder

      Result : ASU.Unbounded_String;

      procedure Add (Metric : Coverage_Metric);
      --  Append one line describing Metric to Result

      ---------
      -- Add --
      ---------

      procedure Add (Metric : Coverage_Metric) is
      begin
         ASU.Append
           (Result,
            (if Result = ASU.Null_Unbounded_String
             then ""
             else (1 => ASCII.LF))
            & Metric_Label (Metric)
            & ": "
            & Image (Metric.Covered)
            & (-" of ")
            & Image (Metric.Total)
            & " "
            & Unit_Label (Metric, False)
            & (-" covered (")
            & Image (Percentage (Metric))
            & " %)");

         --  Warn about a figure that does not account for every file, so that
         --  a partial total is never shown silently, and name the files it
         --  leaves out: a count alone does not tell which they are

         if Metric.Nb_Files < Node.Nb_Files then
            ASU.Append
              (Result,
               (-", from")
               & Natural'Image (Metric.Nb_Files)
               & (-" of")
               & Natural'Image (Node.Nb_Files)
               & (-" files"));

            if not Metric.Missing.Is_Empty then
               ASU.Append (Result, -" (not reported by ");

               for J in Metric.Missing.First_Index .. Metric.Missing.Last_Index
               loop
                  --  A project may hold a great many files: name the first
                  --  few and count the rest, so the tooltip stays readable

                  if J > Max_Named_Files then
                     ASU.Append
                       (Result,
                        (-", and")
                        & Natural'Image
                            (Natural (Metric.Missing.Length) - Max_Named_Files)
                        & (-" more"));
                     exit;
                  end if;

                  if J > Metric.Missing.First_Index then
                     ASU.Append (Result, ", ");
                  end if;

                  ASU.Append (Result, ASU.To_String (Metric.Missing (J)));
               end loop;

               ASU.Append (Result, ")");
            end if;
         end if;

         if Metric.Kind = Shown.Kind then
            ASU.Append (Result, -"  <- shown in the % column");
         end if;
      end Add;

   begin
      if Node.Level /= ASU.Null_Unbounded_String then
         ASU.Append
           (Result, (-"Coverage level: ") & ASU.To_String (Node.Level));
      end if;

      for Metric of Node.Metrics loop
         Add (Metric);
      end loop;

      Add (Line_Metric_Of (Node));

      --  Set_Tooltip_Column feeds the string to the tooltip as markup

      return Glib.Convert.Escape_Text (ASU.To_String (Result));
   end Coverage_Tooltip;

   ---------------
   -- Set_Error --
   ---------------

   procedure Set_Error
     (File_Node : Code_Analysis.File_Access; Error_Code : File_Coverage_Status)
   is
   begin
      if File_Node.Analysis_Data.Coverage_Data = null then
         File_Node.Analysis_Data.Coverage_Data := new File_Coverage;
      end if;

      File_Coverage (File_Node.Analysis_Data.Coverage_Data.all).Status :=
        Error_Code;

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
      Current      : Natural;
      Runs_Regexp  : constant Pattern_Matcher :=
        Compile ("^ +-: +0:Runs:(\d+)", Multiple_Lines);
      Runs_Matches : Match_Array (0 .. 1);
   begin

      Current := File_Contents'First;

      for J in 1 .. 4 loop
         Current :=
           Index (File_Contents.all, (1 => ASCII.LF), Current, Forward);
      end loop;

      Match (Runs_Regexp, File_Contents.all, Runs_Matches, Current);

      if Runs_Matches (0) = No_Match then
         Have_Runs := False;
         return;
      --  The .gcov have no runs count information

      end if;

      begin
         Prj_Runs :=
           Positive'Value
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
        (Line_Info : Code_Analysis.Line; Node : Semantic_Node'Class);
      --  Update the subprogram coverage information related to the given Node
      --  from the given Line's coverage information.

      -------------------------------------------
      -- Update_Subprogram_Info_From_Line_Info --
      -------------------------------------------

      procedure Update_Subprogram_Info_From_Line_Info
        (Line_Info : Code_Analysis.Line; Node : Semantic_Node'Class)
      is
         Line_Coverage_Data : constant Coverage_Access :=
           Line_Info.Analysis_Data.Coverage_Data;
      begin
         if Line_Coverage_Data /= null then

            --   Create the subprogram information if not created yet
            if Subp_Cov = null then
               Subp_Name := new String'(Get (Node.Name).all);
               Subp_Node :=
                 Get_Or_Create
                   (File_Node,
                    Key => Subp_Name.all & ' ' & Get (Node.Profile).all);
               Subp_Node.Name := Subp_Name;
               Subp_Node.Line := Node.Sloc_Def.Line;
               Subp_Node.Column := Integer (Node.Sloc_Def.Column);
               Subp_Node.Start := Node.Sloc_Start.Line;
               Subp_Node.Stop := Node.Sloc_End.Line;
               --  A subprogram's figure stays line-based: GNATcoverage
               --  reports no per-subprogram obligation count, and none can
               --  be derived from the per-line data.

               Subp_Node.Analysis_Data.Coverage_Data :=
                 new Subprogram_Coverage'
                   (Coverage => 0,
                    Status   => Valid,
                    Called   => Line_Coverage_Data.Coverage,
                    Children => 1,
                    others   => <>);
               --  ??? Here we make the hypothesis that the 1st
               --  executed line of the subprogram was executed
               --  exactly once by subprogram calls
               --  It fits with GCC 4.1 series
               Subp_Cov :=
                 Subprogram_Coverage
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

                     Trace
                       (Me,
                        +Full_Name (File_Node.Name)
                        & ": invalid construct at line"
                        & Start_Line'Img);
                  else
                     Update_Subprogram_Info_From_Line_Info
                       (Line_Info => File_Node.Lines (J), Node => Node);
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
         Data : constant access Node_Coverage :=
           Node_Coverage (Project_Node.Analysis_Data.Coverage_Data.all)'Access;

         Level_Seen : Boolean := False;
         Same_Level : Boolean := True;

         procedure Merge (Metric : Coverage_Metric);
         --  Add Metric's counts to the project's figure for the same
         --  criterion, creating that figure if this is the first file to
         --  report it

         procedure Note_Omission (Node : Node_Coverage'Class; Name : String);
         --  Record Name as a file omitted from every project figure that
         --  Node did not report

         -----------
         -- Merge --
         -----------

         procedure Merge (Metric : Coverage_Metric) is
         begin
            for Total of Data.Metrics loop
               if Total.Kind = Metric.Kind
                 and then
                   (Metric.Kind /= Unknown_Metric
                    or else Total.Name = Metric.Name)
               then
                  Total.Covered := Total.Covered + Metric.Covered;
                  Total.Total := Total.Total + Metric.Total;
                  Total.Nb_Files := Total.Nb_Files + Metric.Nb_Files;
                  return;
               end if;
            end loop;

            Data.Metrics.Append (Metric);
         end Merge;

         -------------------
         -- Note_Omission --
         -------------------

         procedure Note_Omission (Node : Node_Coverage'Class; Name : String) is
            function Reported (Total : Coverage_Metric) return Boolean;
            --  Whether Node reported the criterion Total stands for

            --------------
            -- Reported --
            --------------

            function Reported (Total : Coverage_Metric) return Boolean is
            begin
               for Metric of Node.Metrics loop
                  if Metric.Kind = Total.Kind
                    and then
                      (Total.Kind /= Unknown_Metric
                       or else Metric.Name = Total.Name)
                  then
                     return True;
                  end if;
               end loop;

               return False;
            end Reported;

         begin
            for Total of Data.Metrics loop
               if not Reported (Total) then
                  Total.Missing.Append (ASU.To_Unbounded_String (Name));
               end if;
            end loop;
         end Note_Omission;

      begin
         Data.Coverage := 0;
         Data.Children := 0;
         Data.Metrics.Clear;
         Data.Level := ASU.Null_Unbounded_String;
         Data.Nb_Files := 0;

         loop
            exit when Cur = No_Element;
            File_Node := Element (Cur);
            Next (Cur);

            if File_Node.Analysis_Data.Coverage_Data /= null
              and then File_Node.Analysis_Data.Coverage_Data.Is_Valid
            then
               declare
                  File_Data : constant access Node_Coverage :=
                    Node_Coverage
                      (File_Node.Analysis_Data.Coverage_Data.all)'Access;
               begin
                  Data.Children := Data.Children + File_Data.Children;
                  Data.Coverage := Data.Coverage + File_Data.Coverage;
                  Data.Nb_Files := Data.Nb_Files + 1;

                  --  GNATcoverage reports absolute obligation counts, so
                  --  summing them per criterion is exact; averaging the
                  --  percentages would not have been. Every valid file counts
                  --  towards Nb_Files, including one that reported no figure
                  --  at all: a criterion only drives the project's percentage
                  --  when all of them reported it, so the project's total
                  --  never silently leaves a file out.

                  if not File_Data.Metrics.Is_Empty then
                     for Metric of File_Data.Metrics loop
                        Merge (Metric);
                     end loop;

                     if not Level_Seen then
                        Data.Level := File_Data.Level;
                        Level_Seen := True;
                     elsif Data.Level /= File_Data.Level then
                        Same_Level := False;
                     end if;
                  end if;
               end;
            end if;
         end loop;

         --  A second pass, now that every criterion any file reported is
         --  known: name the files each partial figure leaves out, so that the
         --  tooltip can say which they are rather than merely how many.

         Cur := Project_Node.Files.First;

         loop
            exit when Cur = No_Element;
            File_Node := Element (Cur);
            Next (Cur);

            if File_Node.Analysis_Data.Coverage_Data /= null
              and then File_Node.Analysis_Data.Coverage_Data.Is_Valid
            then
               Note_Omission
                 (Node_Coverage (File_Node.Analysis_Data.Coverage_Data.all),
                  Display_Base_Name (File_Node.Name));
            end if;
         end loop;

         --  A project whose files were analyzed at differing levels has no
         --  level of its own to inherit: leave it unset and let
         --  Displayed_Metric arbitrate between the criteria instead.

         if not Same_Level then
            Data.Level := ASU.Null_Unbounded_String;
         end if;
      end;
   end Compute_Project_Coverage;

   ------------------------
   -- Dump_Node_Coverage --
   ------------------------

   procedure Dump_Node_Coverage (Coverage : Coverage_Access) is
   begin
      Put
        (Natural'Image (Coverage.Coverage)
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
         Put
           (Natural'Image (Subprogram_Coverage (Coverage.all).Called)
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
         Put
           (Natural'Image (Project_Coverage (Coverage.all).Runs) & " run(s)");
      end if;
   end Dump_Prj_Coverage;

   ------------------
   -- Dump_Metrics --
   ------------------

   procedure Dump_Metrics (Node : Node_Coverage'Class; Loc : Node_Ptr) is
      Metrics : ASU.Unbounded_String;
   begin
      if Node.Metrics.Is_Empty then
         return;
      end if;

      for Metric of Node.Metrics loop
         if Metrics /= ASU.Null_Unbounded_String then
            ASU.Append (Metrics, ";");
         end if;

         ASU.Append
           (Metrics,
            ASU.To_String (Metric.Name)
            & "="
            & Image (Metric.Covered)
            & ","
            & Image (Metric.Total)
            & ","
            & Image (Metric.Nb_Files));

         for J in Metric.Missing.First_Index .. Metric.Missing.Last_Index loop
            ASU.Append
              (Metrics,
               (if J = Metric.Missing.First_Index then "," else "|")
               & ASU.To_String (Metric.Missing (J)));
         end loop;
      end loop;

      Set_Attribute_S (Loc, "metrics", ASU.To_String (Metrics));
      Set_Attribute_S (Loc, "metric_files", Image (Node.Nb_Files));

      if Node.Level /= ASU.Null_Unbounded_String then
         Set_Attribute_S (Loc, "level", ASU.To_String (Node.Level));
      end if;
   end Dump_Metrics;

   -------------------
   -- Parse_Metrics --
   -------------------

   procedure Parse_Metrics (Node : in out Node_Coverage'Class; Loc : Node_Ptr)
   is
      Txt_Metrics : constant String := Get_Attribute_S (Loc, "metrics");
      First       : Positive;
      Last        : Natural;

      function Split_Names (Names : String) return Name_Vectors.Vector;
      --  The '|'-separated file names of Names, in the order they were
      --  written. Empty when Names is.

      -----------------
      -- Split_Names --
      -----------------

      function Split_Names (Names : String) return Name_Vectors.Vector is
         Result : Name_Vectors.Vector;
         From   : Positive := Names'First;
         Bar    : Natural;
      begin
         while From <= Names'Last loop
            Bar := Index (Names, "|", From);
            Result.Append
              (ASU.To_Unbounded_String
                 (Names (From .. (if Bar = 0 then Names'Last else Bar - 1))));
            exit when Bar = 0;
            From := Bar + 1;
         end loop;

         return Result;
      end Split_Names;

   begin
      if Txt_Metrics = "" then
         return;
      end if;

      First := Txt_Metrics'First;

      loop
         Last := Index (Txt_Metrics, ";", First);

         declare
            Item  : constant String :=
              Txt_Metrics
                (First .. (if Last = 0 then Txt_Metrics'Last else Last - 1));
            Eq    : constant Natural := Index (Item, "=");
            Comma : constant Natural :=
              (if Eq = 0 then 0 else Index (Item, ",", Eq + 1));
         begin
            --  Silently drop an entry we cannot make sense of rather than
            --  give up on the whole report

            if Comma /= 0 then
               declare
                  Name    : constant String := Item (Item'First .. Eq - 1);
                  Comma_2 : constant Natural := Index (Item, ",", Comma + 1);
                  Comma_3 : constant Natural :=
                    (if Comma_2 = 0
                     then 0
                     else Index (Item, ",", Comma_2 + 1));
               begin
                  Node.Metrics.Append
                    (Coverage_Metric'
                       (Kind     => Metric_Kind_From_Name (Name),
                        Name     => ASU.To_Unbounded_String (Name),
                        Covered  => Natural'Value (Item (Eq + 1 .. Comma - 1)),
                        Total    =>
                          Natural'Value
                            (Item
                               (Comma
                                + 1
                                ..
                                  (if Comma_2 = 0
                                   then Item'Last
                                   else Comma_2 - 1))),
                        Nb_Files =>
                          (if Comma_2 = 0
                           then 1
                           else
                             Natural'Value
                               (Item
                                  (Comma_2
                                   + 1
                                   ..
                                     (if Comma_3 = 0
                                      then Item'Last
                                      else Comma_3 - 1)))),
                        Missing  =>
                          (if Comma_3 = 0
                           then Name_Vectors.Empty_Vector
                           else
                             Split_Names (Item (Comma_3 + 1 .. Item'Last)))));
               end;
            end if;
         end;

         exit when Last = 0 or else Last = Txt_Metrics'Last;
         First := Last + 1;
      end loop;

      Node.Level := ASU.To_Unbounded_String (Get_Attribute_S (Loc, "level"));

      declare
         Txt_Files : constant String := Get_Attribute_S (Loc, "metric_files");
      begin
         Node.Nb_Files :=
           (if Txt_Files = "" then 1 else Natural'Value (Txt_Files));
      end;

   exception
      when Constraint_Error =>
         --  A malformed attribute must not prevent the report from loading

         Node.Metrics.Clear;
         Node.Level := ASU.Null_Unbounded_String;
         Node.Nb_Files := 0;
   end Parse_Metrics;

   -----------------------
   -- XML_Dump_Coverage --
   -----------------------

   procedure XML_Dump_Coverage (Coverage : Coverage_Access; Loc : Node_Ptr) is
   begin
      if Coverage /= null then
         if Coverage.Is_Valid then
            Set_Attribute_S
              (Loc, "coverage", Natural'Image (Coverage.Coverage));
         end if;

         if Coverage.all in GNATcov_Line_Coverage'Class then
            Set_Attribute_S
              (Loc,
               "status",
               GNATcov_Line_Coverage_Status'Image
                 (GNATcov_Line_Coverage (Coverage.all).Status));

         elsif Coverage.all in Gcov_Line_Coverage'Class then
            Set_Attribute_S
              (Loc,
               "status",
               Gcov_Line_Coverage_Status'Image
                 (Gcov_Line_Coverage (Coverage.all).Status));

         elsif Coverage.all in Node_Coverage'Class then
            if Coverage.Is_Valid then
               Set_Attribute_S
                 (Loc,
                  "children",
                  Natural'Image (Node_Coverage (Coverage.all).Children));
               Dump_Metrics (Node_Coverage (Coverage.all), Loc);
            end if;

            if Coverage.all in Subprogram_Coverage'Class then
               Set_Attribute_S
                 (Loc,
                  "status",
                  Coverage_Status'Image
                    (Subprogram_Coverage (Coverage.all).Status));

               if Coverage.Is_Valid then
                  Set_Attribute_S
                    (Loc,
                     "called",
                     Natural'Image
                       (Subprogram_Coverage (Coverage.all).Called));
               end if;

            elsif Coverage.all in File_Coverage'Class then
               Set_Attribute_S
                 (Loc,
                  "status",
                  File_Coverage_Status'Image
                    (File_Coverage (Coverage.all).Status));

            elsif Coverage.all in Project_Coverage'Class then
               Set_Attribute_S
                 (Loc,
                  "status",
                  Coverage_Status'Image
                    (Project_Coverage (Coverage.all).Status));

               if Coverage.Is_Valid then
                  if Project_Coverage (Coverage.all).Have_Runs then
                     Set_Attribute_S
                       (Loc,
                        "runs",
                        Natural'Image (Project_Coverage (Coverage.all).Runs));
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
     (Coverage : in out Coverage_Access; Loc : Node_Ptr)
   is
      function Status_Value (Status : String) return Coverage_Status;
      --  Return the coverage status associated with an error message

      function Status_Value (Status : String) return File_Coverage_Status;
      --  Return the coverage status associated with an error message

      function Status_Value
        (Status : String) return GNATcov_Line_Coverage_Status;
      --  Return the coverage status associated with an error message

      function Status_Value (Status : String) return Gcov_Line_Coverage_Status;
      --  Return the coverage status associated with an error message

      ------------------
      -- Status_Value --
      ------------------

      function Status_Value (Status : String) return Coverage_Status is
      begin
         return Coverage_Status'Value (Status);

      exception
         when Constraint_Error =>
            return Undetermined;
      end Status_Value;

      function Status_Value (Status : String) return File_Coverage_Status is
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

      function Status_Value (Status : String) return Gcov_Line_Coverage_Status
      is
      begin
         return Gcov_Line_Coverage_Status'Value (Status);

      exception
         when Constraint_Error =>
            return Undetermined;
      end Status_Value;

      Txt_Status : constant String := Get_Attribute_S (Loc, "status");

   begin
      if Txt_Status /= "" then
         if Loc.Tag.all = "Line" then
            case Coverage_GUI.Current_Coverage_Tool is
               when Coverage_GUI.Gcov    =>
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
                 Natural'Value (Get_Attribute_S (Loc, "children"));

               declare
                  Txt_Called : constant String :=
                    Get_Attribute_S (Loc, "called");
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
                 Natural'Value (Get_Attribute_S (Loc, "children"));
            end if;

         elsif Loc.Tag.all = "Project" then
            Coverage := new Project_Coverage;
            Project_Coverage (Coverage.all).Status :=
              Status_Value (Txt_Status);

            if Coverage.Is_Valid then
               Node_Coverage (Coverage.all).Children :=
                 Natural'Value (Get_Attribute_S (Loc, "children"));

               declare
                  Txt_Runs : constant String := Get_Attribute_S (Loc, "runs");
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
              Natural'Value (Get_Attribute_S (Loc, "coverage"));

            if Coverage.all in Node_Coverage'Class then
               Parse_Metrics (Node_Coverage (Coverage.all), Loc);
            end if;
         end if;
      end if;
   end XML_Parse_Coverage;

   --------------------------------------
   -- First_Project_With_Coverage_Data --
   --------------------------------------

   function First_Project_With_Coverage_Data
     (Projects : Code_Analysis_Tree) return Project_Type
   is
      pragma Annotate (CodePeer, Skip_Analysis);
      --  Shut down false positives

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
         return Prj_Node.View.Get_Project_Type;
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
      function Txt_Sub (Coverage : Coverage_Access) return String;
      --  Returns in a String the Subprograms specific coverage info used to
      --  fill the Gtk_Tree_Store of a coverage report

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
               return
                 String'
                   (-", called"
                    & Natural'Image (Cal_Count)
                    & Txt_Cal (Cal_Count));
            end;
         elsif Coverage.all in Project_Coverage'Class
           and then Project_Coverage (Coverage.all).Have_Runs
         then

            declare
               Run_Count : constant Natural :=
                 Project_Coverage (Coverage.all).Runs;
            begin
               return
                 String'
                   (-", ran"
                    & Natural'Image (Run_Count)
                    & Txt_Cal (Run_Count));
            end;
         else
            return "";
         end if;
      end Txt_Sub;

      Values  : Glib.Values.GValue_Array (1 .. 6);
      Columns : constant Columns_Array (Values'Range) :=
        (Cov_Col,
         Cov_Sort,
         Cov_Bar_Txt,
         Cov_Bar_Val,
         Cov_Bar_Label,
         Cov_Tooltip);

   begin
      if Coverage.Is_Valid then
         declare
            Node        : Node_Coverage'Class renames
              Node_Coverage'Class (Coverage.all);
            Metric      : constant Coverage_Metric := Displayed_Metric (Node);
            Not_Cov     : constant Natural := Uncovered (Metric);
            Cov_Percent : constant Natural := Percentage (Metric);
         begin
            Values :=
              (1 =>
                 As_String
                   (Image (Metric.Total)
                    & " "
                    & Unit_Label (Metric, True)
                    & " ("
                    & Image (Not_Cov)
                    & (-" not covered)")
                    & Txt_Sub (Coverage)),
               2 => As_Int (Glib.Gint (Not_Cov)),
               3 =>
                 As_String
                   (Image (Cov_Percent, Int_Image_Pad, Padding => Int_Char_Pad)
                    & " %"),
               4 => As_Int (Glib.Gint (Cov_Percent)),
               5 => As_String (Metric_Label (Metric)),
               6 => As_String (Coverage_Tooltip (Node, Metric)));
         end;

      else
         Values :=
           (1 => As_String (Coverage.Print_Status),
            2 => As_Int (0),
            3 => As_String (String'("n/a")),
            4 => As_Int (0),
            5 => As_String (String'("")),
            6 => As_String (Coverage.Print_Status));
      end if;

      Set_And_Clear (Tree_Store, Iter, Columns, Values);
   end Fill_Iter;

end Code_Coverage;
