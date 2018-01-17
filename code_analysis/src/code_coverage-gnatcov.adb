------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2008-2018, AdaCore                     --
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

with Ada.Strings;                  use Ada.Strings;
with Ada.Strings.Fixed;            use Ada.Strings.Fixed;
with Ada.Strings.Unbounded;        use Ada.Strings.Unbounded;

with GNAT.Regpat;                  use GNAT.Regpat;
with GNATCOLL.Traces;              use GNATCOLL.Traces;
with GPS.Editors;                  use GPS.Editors;
with GPS.Editors.Line_Information; use GPS.Editors.Line_Information;
with GPS.Intl;                     use GPS.Intl;
with GPS.Kernel.Messages;          use GPS.Kernel.Messages;
with GPS.Kernel.Messages.Simple;   use GPS.Kernel.Messages.Simple;
with Coverage_GUI;                 use Coverage_GUI;

package body Code_Coverage.GNATcov is

   GNATcov_Code_Coverage_Module_Trace : constant Trace_Handle
     := GNATCOLL.Traces.Create
       ("GNATCOV_CODE_COVERAGE_MODULE", GNATCOLL.Traces.On);

   type Coverage_Status_Char_Type is
     array (GNATcov_Line_Coverage_Status) of Character;
   Coverage_Status_Char : constant Coverage_Status_Char_Type :=
     (Undetermined             => ' ',
      No_Code                  => ' ',
      Not_Covered              => '-',
      Partially_Covered        => '!',
      Branch_Taken             => '>',
      Branch_Fallthrough       => 'v',
      Exempted_Violated        => '*',
      Exempted_Not_Violated    => '#',
      Covered_No_Branch        => '+');

   type Cst_String_Access is access constant String;
   type Coverage_Status_Color_Type is
     array (GNATcov_Line_Coverage_Status) of Cst_String_Access;
   Not_Covered_Color           : aliased constant String := "red";
   Partially_Covered_Color     : aliased constant String := "orange";
   Fully_Covered_Color         : aliased constant String := "green";
   Exempted_Violated_Color     : aliased constant String := "#b895f4";
   Exempted_Not_Violated_Color : aliased constant String := "#95b9f4";

   Coverage_Status_Color : constant Coverage_Status_Color_Type :=
     (Not_Covered                 => Not_Covered_Color'Access,
      GNATcov_Partially_Covered   => Partially_Covered_Color'Access,
      Covered_No_Branch           => Fully_Covered_Color'Access,
      Exempted_Violated           => Exempted_Violated_Color'Access,
      Exempted_Not_Violated       => Exempted_Not_Violated_Color'Access,
      others                      => null);

   function Coverage_Verbose_Message
     (Coverage : GNATcov_Line_Coverage)
      return String;
   --  Return an localized message that describe why some line is not covered.
   --  Return an empty string if there is no coverage issue.

   procedure Process_Detailed_Messages
     (Coverage : GNATcov_Line_Coverage;
      Process  : not null access procedure (Item : GNATcov_Item_Coverage));
   --  Call Process for each detailed message in Coverage. If there is no
   --  detailed message, process a default one.

   procedure Remove_Inlined_Detailed_Messages
     (Self : in out Detail_Messages_Command);
   --  Remove all detailed messages associated to this command

   -------------------
   -- Add_File_Info --
   -------------------

   procedure Add_File_Info
     (File_Node     : Code_Analysis.File_Access;
      File_Contents : GNAT.Strings.String_Access)
   is
      Current           : Natural;
      Line_Regexp       : constant Pattern_Matcher := Compile
        ("^ *(\d+) ([-!?+>v*#]):(.*$)", Multiple_Lines);
      Line_Matches      : Match_Array (0 .. 3);
      Last_Line_Regexp  : constant Pattern_Matcher := Compile
        ("^ *(\d+) (.):", Multiple_Lines);
      Last_Line_Matches : Match_Array (0 .. 2);
      Line_Num          : Natural;
      Lines_Count       : Natural := 0;
      Not_Cov_Count     : Natural := 0;
      Line_Coverage     : GNATcov_Line_Coverage_Access;

      Location_Regexp   : constant Pattern_Matcher := Compile
        ("\sat (\d+):(\d+)\s", Multiple_Lines);
      Location_Matches  : Match_Array (0 .. 2);
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

         Line_Num := Natural'Value
           (File_Contents (Line_Matches (1).First .. Line_Matches (1).Last));
         File_Node.Lines (Line_Num).Number := Line_Num;
         Line_Coverage := new GNATcov_Line_Coverage'
           (File     => File_Node.Name,
            Line     => Line_Num,
            Status   => No_Code,
            Coverage => 0,
            others   => <>);
         File_Node.Lines (Line_Num).Analysis_Data.Coverage_Data :=
           Coverage_Access (Line_Coverage);

         case File_Contents (Line_Matches (2).First) is
            when '-' =>
               Line_Coverage.Status   := Not_Covered;
               Not_Cov_Count := Not_Cov_Count + 1;

            when '!' =>
               Line_Coverage.Status := Partially_Covered;
               Not_Cov_Count := Not_Cov_Count + 1;

            when '+' =>
               Line_Coverage.Status := Covered_No_Branch;

            when '>' =>
               Line_Coverage.Status := Branch_Taken;

            when 'v' =>
               Line_Coverage.Status := Branch_Fallthrough;

            when '*' =>
               Line_Coverage.Status := Exempted_Violated;

            when '#' =>
               Line_Coverage.Status := Exempted_Not_Violated;

            when others =>
               Trace
                 (GNATcov_Code_Coverage_Module_Trace,
                  "unexpected character: "
                  & File_Contents (Line_Matches (2).First));
               pragma Assert (False);
         end case;

         --  Do not take into account exempted lines when calculating the total
         --  number of lines.

         if Line_Coverage.Status not in GNATcov_Exempted_Line then
            Lines_Count := Lines_Count + 1;
         end if;

         --  Set Coverage to 1 for fully covered lines

         if Line_Coverage.Status in GNATcov_Fully_Covered then
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

         --  If detailed messages about the previous line follow, add them
         --  to the line coverage information.

         --  TODO??? Yield one separate coverage data per message instead of
         --  one coverage data per line and multiple messages per line.

         declare
            Details_First : Natural := Current + 1;
            Details_Last  : Natural;
            Column        : Basic_Types.Visible_Column_Type;
         begin
            while Details_First <= File_Contents'Last
              and then
                (File_Contents (Details_First) /= ' '
                 and then File_Contents (Details_First) not in '0' .. '9')
            loop
               Details_Last := Index
                 (File_Contents.all, (1 => ASCII.LF), Details_First);
               if Details_Last = 0 then
                  Details_Last := File_Contents'Last;
               end if;

               Current := Details_Last + 1;

               if File_Contents (Details_Last) = ASCII.LF then
                  Details_Last := Details_Last - 1;
               end if;

               Match
                 (Location_Regexp,
                  File_Contents (Details_First .. Details_Last),
                  Location_Matches,
                  Details_First);
               if Location_Matches (0) = No_Match then
                  Column := 0;
               else
                  Column := Basic_Types.Visible_Column_Type'Value
                    (File_Contents
                       (Location_Matches (2).First
                        .. Location_Matches (2).Last));
               end if;

               Line_Coverage.Items.Append
                 ((Column  => Column,
                   Message => To_Unbounded_String
                     (File_Contents (Details_First .. Details_Last))));
               Details_First := Current;
            end loop;
         end;

      end loop;

      Node_Coverage (File_Node.Analysis_Data.Coverage_Data.all).Children :=
        Lines_Count;
      File_Node.Analysis_Data.Coverage_Data.Coverage := Not_Cov_Count;
      File_Coverage (File_Node.Analysis_Data.Coverage_Data.all).Status :=
        Valid;
   end Add_File_Info;

   -------------------------------
   -- Process_Detailed_Messages --
   -------------------------------

   procedure Process_Detailed_Messages
     (Coverage : GNATcov_Line_Coverage;
      Process  : not null access procedure (Item : GNATcov_Item_Coverage))
   is
      Empty : Boolean := True;
   begin
      --  Process each detailed message

      for Item of Coverage.Items loop
         Empty := False;
         Process (Item);
      end loop;

      --  Fallback to a default message if there was no detailed message

      if Empty then
         Process
           ((0, To_Unbounded_String (Coverage_Verbose_Message (Coverage))));
      end if;
   end Process_Detailed_Messages;

   -------------------------------
   -- Add_Location_If_Uncovered --
   -------------------------------

   overriding procedure Add_Location_If_Uncovered
     (Coverage    : GNATcov_Line_Coverage;
      Kernel      : GPS.Kernel.Kernel_Handle;
      File        : GNATCOLL.VFS.Virtual_File;
      Line_Number : Positive;
      Line_Text   : GNAT.Strings.String_Access;
      Added       : in out Boolean;
      Allow_Auto_Jump_To_First : Boolean)
   is
      pragma Unreferenced (Line_Text);

      Coverage_Category : Cst_String_Access;

      procedure Process (Item : GNATcov_Item_Coverage);
      --  Helper to add a message in the location window for the current line

      procedure Process (Item : GNATcov_Item_Coverage)
      is
         Msg : Simple_Message_Access;
      begin
         Msg :=
           Create_Simple_Message
             (Kernel.Get_Messages_Container,
              Coverage_Category.all,
              File,
              Line_Number,
              Item.Column,
              To_String (Item.Message),
              0,
              Coverage_Message_Flags,
              Allow_Auto_Jump_To_First => Allow_Auto_Jump_To_First);
         Msg.Set_Highlighting
           (Analysis_Styles (GNATcov_Style_Categories (Coverage.Status)));
      end Process;

   begin
      if Coverage.Status = Not_Covered then
         Coverage_Category := Uncovered_Category'Unrestricted_Access;
      elsif Coverage.Status in GNATcov_Partially_Covered then
         Coverage_Category := Partially_Covered_Category'Unrestricted_Access;
      else
         return;
      end if;

      Process_Detailed_Messages (Coverage, Process'Access);
      Added := True;
   end Add_Location_If_Uncovered;

   ------------------------
   -- Line_Coverage_Info --
   ------------------------

   overriding function Line_Coverage_Info
     (Coverage : access GNATcov_Line_Coverage;
      Kernel   : GPS.Kernel.Kernel_Handle;
      Bin_Mode : Boolean := False)
      return GPS.Editors.Line_Information.Line_Information_Record
   is
      pragma Unreferenced (Bin_Mode);
      --  It is used by the gcov plugin.

      Pango_Markup_To_Open_1 : constant String := "<span background=""";
      Pango_Markup_To_Open_2 : constant String := """>";
      Pango_Markup_To_Close  : constant String := "</span>";

      Text   : constant String := Coverage_Verbose_Message (Coverage.all);
      Result : GPS.Editors.Line_Information.Line_Information_Record;
   begin
      if Text /= "" then
         Result.Text := To_Unbounded_String
           (Pango_Markup_To_Open_1
            & Coverage_Status_Color (Coverage.Status).all
            & Pango_Markup_To_Open_2
            & Coverage_Status_Char (Coverage.Status)
            & Pango_Markup_To_Close);
         Result.Tooltip_Text := To_Unbounded_String (Text);
         Result.Associated_Command := new Detail_Messages_Command'
           (Commands.Root_Command with
            Line   => Coverage.all'Access,
            Kernel => Kernel,
            Added  => False,
            Marks  => <>);
      end if;

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

   ------------------------------
   -- Coverage_Verbose_Message --
   ------------------------------

   function Coverage_Verbose_Message
     (Coverage : GNATcov_Line_Coverage)
      return String
   is
   begin
      case Coverage.Status is
         when No_Code =>
            return "";

         when Not_Covered =>
            return -"The code for this line has not been executed.";

         when Partially_Covered =>
            return -"The code for this line has been partially executed.";

         when Covered_No_Branch =>
            return -"The code for this line has been executed, no branches";

         when Branch_Taken =>
            return -("The code for this line has been executed "
                     & "branch taken.");

         when Branch_Fallthrough =>
            return -("The code for this line has been executed,"
                     & " branch fallthrough.");

         when Exempted_Violated =>
            return -("The code for this line has been exempted and a "
                     & "violation has occurred.");

         when Exempted_Not_Violated =>
            return -("The code for this line has been exempted but no "
                     & "violation has occurred.");

         when Undetermined =>
            return -"Undetermined.";
      end case;
   end Coverage_Verbose_Message;

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : GPS.Editors.Editor_Mark'Class) return Boolean
   is
      pragma Unreferenced (Left, Right);
   begin
      return False;
   end "=";

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self : access Detail_Messages_Command)
      return Commands.Command_Return_Type
   is
      Coverage : constant GNATcov_Line_Coverage_Access := Self.Line;
      Buffer   : constant Editor_Buffer'Class :=
        Self.Kernel.Get_Buffer_Factory.Get (Coverage.File);

      procedure Process (Item : GNATcov_Item_Coverage);
      --  Helper to add an inline detailed message

      -------------
      -- Process --
      -------------

      procedure Process (Item : GNATcov_Item_Coverage) is
      begin
         --  Put it *after* the corresponding line
         Self.Marks.Append
           (GPS_Editor_Buffer'Class (Buffer).Add_Special_Line
            (Coverage.Line + 1,
                 To_String (Item.Message),
                 Editor_Code_Annotations_Style));
      end Process;

   begin
      --  If the detailed messages are already displayed, remove them. Display
      --  them otherwise.

      if Self.Added then
         Remove_Inlined_Detailed_Messages (Self.all);

      elsif Buffer /= Nil_Editor_Buffer
        and then Buffer in GPS_Editor_Buffer'Class
        and then Coverage.Status /= Covered_No_Branch
      then
         Process_Detailed_Messages
           (GNATcov_Line_Coverage (Coverage.all),
            Process'Access);
         Self.Added := True;
      end if;

      return Commands.Success;
   end Execute;

   --------------------
   -- Primitive_Free --
   --------------------

   overriding procedure Primitive_Free
     (Self : in out Detail_Messages_Command)
   is
   begin
      Remove_Inlined_Detailed_Messages (Self);
   end Primitive_Free;

   --------------------------------------
   -- Remove_Inlined_Detailed_Messages --
   --------------------------------------

   procedure Remove_Inlined_Detailed_Messages
     (Self : in out Detail_Messages_Command)
   is
      Buffer   : constant Editor_Buffer'Class :=
        Self.Kernel.Get_Buffer_Factory.Get (Self.Line.File);
   begin
      for Mark of Self.Marks loop
         GPS_Editor_Buffer'Class (Buffer).Remove_Special_Lines
           (Mark, 1);
      end loop;
      Self.Added := False;
   end Remove_Inlined_Detailed_Messages;

end Code_Coverage.GNATcov;
