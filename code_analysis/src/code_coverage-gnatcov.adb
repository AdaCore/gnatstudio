------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2008-2012, AdaCore                     --
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

with Ada.Strings;                use Ada.Strings;
with Ada.Strings.Fixed;          use Ada.Strings.Fixed;
with GNAT.Regpat;                use GNAT.Regpat;
with GNATCOLL.Traces;
with GPS.Intl;                   use GPS.Intl;
with GPS.Kernel.Messages;        use GPS.Kernel.Messages;
with GPS.Kernel.Messages.Simple; use GPS.Kernel.Messages.Simple;
with GPS.Kernel.Styles;          use GPS.Kernel.Styles;
with GPS.Styles;                 use GPS.Styles;
with GPS.Styles.UI;              use GPS.Styles.UI;
with Traces;
with Coverage_GUI;               use Coverage_GUI;

package body Code_Coverage.GNATcov is

   GNATcov_Code_Coverage_Module_Trace : constant Traces.Debug_Handle
     := GNATCOLL.Traces.Create
       ("GNATCOV_CODE_COVERAGE_MODULE", GNATCOLL.Traces.On);

   -------------------
   -- Add_File_Info --
   -------------------

   procedure Add_File_Info
     (File_Node     : Code_Analysis.File_Access;
      File_Contents : String_Access)
   is
      Current           : Natural;
      Line_Regexp       : constant Pattern_Matcher := Compile
        ("^ +(\d+) ([-!?+>v*]):(.*$)", Multiple_Lines);
      Line_Matches      : Match_Array (0 .. 3);
      Last_Line_Regexp  : constant Pattern_Matcher := Compile
        ("^ +(\d+) (.):", Multiple_Lines);
      Last_Line_Matches : Match_Array (0 .. 2);
      Line_Num          : Natural;
      Lines_Count       : Natural := 0;
      Not_Cov_Count     : Natural := 0;
      Line_Coverage     : GNATcov_Line_Coverage_Access;
   begin
      if File_Node.Analysis_Data.Coverage_Data = null then
         File_Node.Analysis_Data.Coverage_Data := new File_Coverage;
      end if;

      --  Determination of the line number

      Current := File_Contents'Last;
      Current := Index (File_Contents.all, (1 => ASCII.LF), Current, Backward);

      if Current = File_Contents'Last then
         if Current > 0 then
            Current := Index
              (File_Contents.all, (1 => ASCII.LF), Current - 1, Backward);

            if Current = 0 then
               Set_Error (File_Node, File_Corrupted);
               --  The .xcov file is not a valid Xcov file.
               --  No last source code line found.
               return;
            end if;
         else
            Set_Error (File_Node, File_Empty);
            --  the .xcov file is not a valid Xcov file.
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
               --  The .xcov file is not a valid Xcov file.
               --  No last source code line found.
               return;
            end if;
         else
            Set_Error (File_Node, File_Corrupted);
            --  the .xcov file is not a valid Xcov file.
            return;
         end if;
      end loop;

      --  Parsing of the line coverage information

      File_Node.Lines := new Line_Array (1 .. Natural'Value (File_Contents
        (Last_Line_Matches (1).First .. Last_Line_Matches (1).Last)));
      --  Create a Line_Array with exactly the number of elements corresponding
      --  to the number of code lines in the original source code file.

      File_Node.Lines.all := (others => Null_Line);
      Current := File_Contents'First;

      --  Skip first two lines: file name and coverage statistic information

      Current := Index (File_Contents.all, (1 => ASCII.LF), Current + 1);
      Current := Index (File_Contents.all, (1 => ASCII.LF), Current + 1);
      Current := Current + 1;

      loop
         Match (Line_Regexp, File_Contents.all, Line_Matches, Current);

         exit when Line_Matches (0) = No_Match;

         Lines_Count := Lines_Count + 1;
         Line_Num := Natural'Value
           (File_Contents (Line_Matches (1).First .. Line_Matches (1).Last));
         File_Node.Lines (Line_Num).Number := Line_Num;
         Line_Coverage := new GNATcov_Line_Coverage;
         File_Node.Lines (Line_Num).Analysis_Data.Coverage_Data :=
           Coverage_Access (Line_Coverage);
         Line_Coverage.Status := No_Code;
         Line_Coverage.Coverage := 0;

         case File_Contents (Line_Matches (2).First) is
            when '-' =>
               Line_Coverage.Status   := Not_Covered;
               Not_Cov_Count := Not_Cov_Count + 1;

            when '!' =>
               Line_Coverage.Status := Partially_Covered;

            when '?' =>
               Line_Coverage.Status := Branch_Partially_Covered;

            when '+' =>
               Line_Coverage.Status := Covered_No_Branch;

            when '>' =>
               Line_Coverage.Status := Branch_Taken;

            when 'v' =>
               Line_Coverage.Status := Branch_Fallthrough;

            when '*' =>
               Line_Coverage.Status := Branch_Covered;

            when others =>
               GNATCOLL.Traces.Trace
                 (GNATcov_Code_Coverage_Module_Trace,
                  "unexpected character: "
                  & File_Contents (Line_Matches (2).First));
               pragma Assert (False);
         end case;

         --  Set Coverage to 1 for partially and fully covered lines

         if Line_Coverage.Status in GNATcov_Partially_Covered
           or else Line_Coverage.Status in GNATcov_Fully_Covered
         then
            Line_Coverage.Coverage := 1;
         end if;

         --  For uncovered and partially covered lines also store text
         --  of the corresponding line.

         if Line_Coverage.Status = Not_Covered
           or else Line_Coverage.Status in GNATcov_Partially_Covered
         then
            File_Node.Lines (Line_Num).Contents := new String'
              (File_Contents
                 (Line_Matches (3).First .. Line_Matches (3).Last));
         end if;

         Current := Line_Matches (0).Last + 1;
      end loop;

      Node_Coverage (File_Node.Analysis_Data.Coverage_Data.all).Children :=
        Lines_Count;
      File_Node.Analysis_Data.Coverage_Data.Coverage := Not_Cov_Count;
      File_Coverage (File_Node.Analysis_Data.Coverage_Data.all).Status :=
        Valid;
   end Add_File_Info;

   -------------------------------
   -- Add_Location_If_Uncovered --
   -------------------------------

   overriding procedure Add_Location_If_Uncovered
     (Coverage    : GNATcov_Line_Coverage;
      Kernel      : GPS.Kernel.Kernel_Handle;
      File        : GNATCOLL.VFS.Virtual_File;
      Line_Number : Positive;
      Line_Text   : String_Access;
      Added       : in out Boolean)
   is
      Message : Simple_Message_Access;
   begin
      if Coverage.Status = Not_Covered then
         Added := True;
         Message :=
           Create_Simple_Message
             (Get_Messages_Container (Kernel),
              Uncovered_Category,
              File,
              Line_Number,
              1,
              Line_Text.all,
              0,
              Coverage_Message_Flags);
         Message.Set_Highlighting
           (Get_Or_Create_Style_Copy
              (Kernel,
               Get_Name (Builder_Styles (Warnings)) & '/' & Uncovered_Category,
               Builder_Styles (Warnings)));

      elsif Coverage.Status in GNATcov_Partially_Covered then
         Added := True;
         Message :=
           Create_Simple_Message
             (Get_Messages_Container (Kernel),
              Partially_Covered_Category,
              File,
              Line_Number,
              1,
              Line_Text.all,
              0,
              Coverage_Message_Flags);
         Message.Set_Highlighting
           (Get_Or_Create_Style_Copy
              (Kernel,
               Get_Name (Builder_Styles (Warnings))
               & '/' & Partially_Covered_Category,
               Builder_Styles (Warnings)));
      end if;
   end Add_Location_If_Uncovered;

   ------------------------
   -- Line_Coverage_Info --
   ------------------------

   overriding function Line_Coverage_Info
     (Coverage : GNATcov_Line_Coverage;
      Bin_Mode : Boolean := False)
      return GPS.Editors.Line_Information.Line_Information_Record
   is
      pragma Unreferenced (Bin_Mode);
      --  It is used by the gcov plugin.

      Pango_Markup_To_Open_1 : constant String := "<span background=""";
      Pango_Markup_To_Open_2 : constant String := """>";
      Pango_Markup_To_Close  : constant String := "</span>";

      Not_Covered_Color       : constant String := "red";
      Partially_Covered_Color : constant String := "orange";
      Fully_Covered_Color     : constant String := "green";

      Result : GPS.Editors.Line_Information.Line_Information_Record;

   begin
      case Coverage.Status is
         when No_Code =>
            null;

         when Not_Covered =>
            Result.Text := new String'
              (Pango_Markup_To_Open_1 & Not_Covered_Color
               & Pango_Markup_To_Open_2 & "-" & Pango_Markup_To_Close);
            Result.Tooltip_Text := new String'
              (-"The code for this line has not been executed.");

         when Partially_Covered =>
            Result.Text := new String'
              (Pango_Markup_To_Open_1 & Partially_Covered_Color
               & Pango_Markup_To_Open_2 & "!" & Pango_Markup_To_Close);
            Result.Tooltip_Text := new String'
              (-"The code for this line has been partially executed.");

         when Branch_Partially_Covered =>
            Result.Text := new String'
              (Pango_Markup_To_Open_1 & Partially_Covered_Color
               & Pango_Markup_To_Open_2 & "?" & Pango_Markup_To_Close);
            Result.Tooltip_Text := new String'
              (-("The code for this line has been executed,"
               & " but not all decisions taken"));

         when Covered_No_Branch =>
            Result.Text := new String'
              (Pango_Markup_To_Open_1 & Fully_Covered_Color
               & Pango_Markup_To_Open_2 & "+" & Pango_Markup_To_Close);
            Result.Tooltip_Text := new String'
              (-"The code for this line has been executed, no branches");

         when Branch_Taken =>
            Result.Text := new String'
              (Pango_Markup_To_Open_1 & Partially_Covered_Color
               & Pango_Markup_To_Open_2 & ">" & Pango_Markup_To_Close);
            Result.Tooltip_Text := new String'
              (-("The code for this line has been executed,"
               & " branch taken"));

         when Branch_Fallthrough =>
            Result.Text := new String'
              (Pango_Markup_To_Open_1 & Partially_Covered_Color
               & Pango_Markup_To_Open_2 & "v" & Pango_Markup_To_Close);
            Result.Tooltip_Text := new String'
              (-("The code for this line has been executed,"
               & " branch fallthrough"));

         when Branch_Covered =>
            Result.Text := new String'
              (Pango_Markup_To_Open_1 & Fully_Covered_Color
               & Pango_Markup_To_Open_2 & "*" & Pango_Markup_To_Close);
            Result.Tooltip_Text := new String'
              (-"The code for this line has been executed, branch covered");

         when Undetermined =>
            Result.Text := new String'(-"Undetermined");
      end case;

      return Result;
   end Line_Coverage_Info;

   --------------
   -- Is_Valid --
   --------------

   overriding function Is_Valid
     (Self : GNATcov_Line_Coverage) return Boolean is
   begin
      return Self.Status /= Undetermined;
   end Is_Valid;

end Code_Coverage.GNATcov;
