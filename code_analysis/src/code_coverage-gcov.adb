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

with Ada.Strings;                use Ada.Strings;
with Ada.Strings.Fixed;          use Ada.Strings.Fixed;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with GNAT.Regpat;                use GNAT.Regpat;

with GPS.Intl;                   use GPS.Intl;
with GPS.Default_Styles;         use GPS.Default_Styles;
with GPS.Kernel.Messages;        use GPS.Kernel.Messages;
with GPS.Kernel.Messages.Simple; use GPS.Kernel.Messages.Simple;
with Code_Analysis_GUI;
--  ??? Why not replace by Code_Coverage_Gui?
with Coverage_GUI;               use Coverage_GUI;
with String_Utils;               use String_Utils;

package body Code_Coverage.Gcov is

   -------------------
   -- Add_File_Info --
   -------------------

   procedure Add_File_Info
     (File_Node     : Code_Analysis.File_Access;
      File_Contents : GNAT.Strings.String_Access)
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

      --  Parsing of the line coverage information

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
           new Gcov_Line_Coverage;
         Gcov_Line_Coverage
           (File_Node.Lines (Line_Num).Analysis_Data.Coverage_Data.all).Status
           := No_Code;

         case File_Contents (Line_Matches (1).First) is
            when '#' =>
               Gcov_Line_Coverage
                 (File_Node.Lines
                    (Line_Num).Analysis_Data.Coverage_Data.all).Status
                 := Not_Covered;
               File_Node.Lines (Line_Num).Analysis_Data.Coverage_Data.Coverage
                 := 0;
               File_Node.Lines (Line_Num).Contents := new String'
                 (File_Contents
                    (Line_Matches (3).First .. Line_Matches (3).Last));
               Not_Cov_Count := Not_Cov_Count + 1;
            when others =>
               Gcov_Line_Coverage
                 (File_Node.Lines
                    (Line_Num).Analysis_Data.Coverage_Data.all).Status
                 := Covered;
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
      File_Coverage (File_Node.Analysis_Data.Coverage_Data.all).Status :=
        Valid;
   end Add_File_Info;

   -------------------------------
   -- Add_Location_If_Uncovered --
   -------------------------------

   overriding procedure Add_Location_If_Uncovered
     (Coverage    : Gcov_Line_Coverage;
      Kernel      : GPS.Kernel.Kernel_Handle;
      File        : GNATCOLL.VFS.Virtual_File;
      Line_Number : Positive;
      Line_Text   : GNAT.Strings.String_Access;
      Added       : in out Boolean;
      Allow_Auto_Jump_To_First : Boolean)
   is
      Message : Simple_Message_Access;

   begin
      if Coverage.Coverage = 0 then
         Added := True;
         Message :=
           Create_Simple_Message
             (Kernel.Get_Messages_Container,
              Uncovered_Category,
              File,
              Line_Number,
              1,
              Line_Text.all,
              0,
              Coverage_Message_Flags,
              Allow_Auto_Jump_To_First => Allow_Auto_Jump_To_First);
         Message.Set_Highlighting (Messages_Styles (High_Importance));
      end if;
   end Add_Location_If_Uncovered;

   ------------------------
   -- Line_Coverage_Info --
   ------------------------

   overriding function Line_Coverage_Info
     (Coverage : access Gcov_Line_Coverage;
      Kernel   : GPS.Kernel.Kernel_Handle;
      Bin_Mode : Boolean := False)
      return GPS.Editors.Line_Information.Line_Information_Record
   is
      pragma Unreferenced (Kernel);

      Pango_Markup_To_Open_1 : constant String := "<span foreground=""";
      Pango_Markup_To_Open_2 : constant String := """>";
      Pango_Markup_To_Close  : constant String := "</span>";

      Result : GPS.Editors.Line_Information.Line_Information_Record;

   begin
      if Bin_Mode then
         case Coverage.Coverage is
         when 0 =>
            Result.Image := Code_Analysis_GUI.Uncovered_Line_Pixbuf;
            Result.Tooltip_Text := To_Unbounded_String
              (-"The code for this line has not been executed.");
         when others =>
            Result.Image := Code_Analysis_GUI.Covered_Line_Pixbuf;
            Result.Tooltip_Text := To_Unbounded_String
              (-"The code for this line has been executed.");
         end case;
      else
         case Coverage.Coverage is
            when 0 =>
               Result.Text := To_Unbounded_String
                 (Pango_Markup_To_Open_1
                  & "red"
                  & Pango_Markup_To_Open_2
                  & "#"
                  & Pango_Markup_To_Close);

            when 1 =>
               Result.Text := To_Unbounded_String
                 (Pango_Markup_To_Open_1
                  & "black"
                  & Pango_Markup_To_Open_2
                  & (-" 1 time ")
                  & Pango_Markup_To_Close);

            when others =>
               Result.Text := To_Unbounded_String
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

   --------------
   -- Is_Valid --
   --------------

   overriding function Is_Valid (Self : Gcov_Line_Coverage) return Boolean is
   begin
      return Self.Status /= Undetermined;
   end Is_Valid;

end Code_Coverage.Gcov;
