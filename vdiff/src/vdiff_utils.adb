-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
--                            ACT-Europe                             --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
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
with Interfaces.C.Strings;
with Glib; use Glib;
with Gdk.Color; use Gdk.Color;
with Pango.Font; use Pango.Font;
with Gtk.Adjustment; use Gtk.Adjustment;
with Glide_Kernel;             use Glide_Kernel;
with Glide_Kernel.Preferences; use Glide_Kernel.Preferences;
with Gtk.Enums; use Gtk.Enums;
with Gtk.Style; use Gtk.Style;
with Vdiff_Pkg; use Vdiff_Pkg;
with String_Utils; use String_Utils;

package body Vdiff_Utils is

   package ICS renames Interfaces.C.Strings;

   ---------------------
   -- Fill_Diff_Lists --
   ---------------------

   procedure Fill_Diff_Lists
     (Kernel : access Kernel_Handle_Record'Class;
      List1  : access Gtk_Clist_Record'Class;
      List2  : access Gtk_Clist_Record'Class;
      File1  : String;
      File2  : String;
      Diff   : Diff_Occurrence_Link)
   is
      Context_Len     : Integer :=
        Integer (Get_Pref (Kernel, Diff_Context_Length));
      --  Number of lines displayed before and after each chunk of differences

      Num_Line_Sep    : constant := 3;
      --  Number of empty lines separating each chunk.

      S               : String (1 .. 8192);
      Last            : Natural;
      Len             : Natural;
      Infile1         : Ada.Text_IO.File_Type;
      Infile2         : Ada.Text_IO.File_Type;
      Texts           : ICS.chars_ptr_array (0 .. 1);
      Row             : Gint;
      Line1           : Natural;
      Line2           : Natural;
      Offset1         : Natural;
      Offset2         : Natural;
      Link            : Diff_Occurrence_Link;
      Default_Style   : Gtk_Style;
      Old_Style       : Gtk_Style;
      Append_Style    : Gtk_Style;
      Remove_Style    : Gtk_Style;
      Change_Style    : Gtk_Style;
      Color           : Gdk_Color;
      Context_Changed : Boolean;
      Desc            : constant Pango_Font_Description :=
        Get_Pref (Kernel, Default_Source_Editor_Font);

      procedure Add_Line
        (List  : access Gtk_Clist_Record'Class;
         Style : Gtk_Style;
         Line  : String := "");
      --  Add a line constaining Line in List, using Style for font/color.

      procedure Read_Line
        (Infile  : File_Type;
         List    : access Gtk_Clist_Record'Class;
         Line    : Natural;
         Style   : Gtk_Style;
         Display : Boolean := True);
      --  Read the next line in Infile and add it in List, with the line
      --  number Line, using Style for font/color.
      --  If Display is True, add the line in the list, otherwise only
      --  read the line in Infile.

      procedure Add_Line
        (List  : access Gtk_Clist_Record'Class;
         Style : Gtk_Style;
         Line  : String := "") is
      begin
         Texts (0) := ICS.Null_Ptr;
         Texts (1) := ICS.New_String (Line);
         Row := Append (List, Texts);
         Set_Cell_Style (List, Row, 0, Style);
         Set_Cell_Style (List, Row, 1, Style);
         ICS.Free (Texts (1));
      end Add_Line;

      procedure Read_Line
        (Infile  : File_Type;
         List    : access Gtk_Clist_Record'Class;
         Line    : Natural;
         Style   : Gtk_Style;
         Display : Boolean := True) is
      begin
         Get_Line (File => Infile, Item => S, Last => Last);

         if Display then
            Texts (0) := ICS.New_String (Image (Line));
            Texts (1) := ICS.New_String (S (1 .. Last));
            Row := Append (List, Texts);
            Set_Cell_Style (List, Row, 0, Style);
            Set_Cell_Style (List, Row, 1, Style);
            ICS.Free (Texts (0));
            ICS.Free (Texts (1));
         end if;
      end Read_Line;

   begin
      if Context_Len = -1 then
         Context_Len := Integer'Last;
      end if;

      --  ??? When are these styles freed ?
      Default_Style := Copy (Get_Style (List1));
      Set_Font_Description (Default_Style, Desc);

      Old_Style     := Copy (Default_Style);
      Append_Style  := Copy (Default_Style);
      Remove_Style  := Copy (Default_Style);
      Change_Style  := Copy (Default_Style);

      --  <preferences>
      Set_Rgb (Color, 50000, 50000, 50000);
      Set_Base (Old_Style, State_Normal, Color);
      Set_Rgb (Color, 0, 56000, 0);
      Set_Base (Append_Style, State_Normal, Color);
      Set_Rgb (Color, 56000, 0, 0);
      Set_Base (Remove_Style, State_Normal, Color);
      Set_Rgb (Color, 0, 40000, 65000);
      Set_Base (Change_Style, State_Normal, Color);

      Open (Infile1, In_File, File1);
      Open (Infile2, In_File, File2);
      Freeze (List1);
      Freeze (List2);

      Line1 := 1;
      Line2 := 1;
      Link := Diff;

      while Link /= null loop
         Context_Changed := False;

         for J in Line1 .. Link.Range1.First - 1 loop
            if J - Line1 < Context_Len
              or else Link.Range1.First - J <= Context_Len
            then
               Read_Line (Infile1, List1, J, Default_Style);
            else
               Read_Line (Infile1, List1, J, Default_Style, False);

               if not Context_Changed then
                  for J in 1 .. Num_Line_Sep loop
                     Add_Line (List1, Default_Style);
                  end loop;

                  Context_Changed := True;
               end if;
            end if;
         end loop;

         Context_Changed := False;

         for J in Line2 .. Link.Range2.First - 1 loop
            if J - Line2 < Context_Len
              or else Link.Range2.First - J <= Context_Len
            then
               Read_Line (Infile2, List2, J, Default_Style);
            else
               Read_Line (Infile2, List2, J, Default_Style, False);

               if not Context_Changed then
                  for J in 1 .. Num_Line_Sep loop
                     Add_Line (List2, Default_Style);
                  end loop;

                  Context_Changed := True;
               end if;
            end if;
         end loop;

         case Link.Action is
            when Append =>
               for J in Link.Range2.First .. Link.Range2.Last - 1 loop
                  Add_Line (List1, Old_Style);
               end loop;

               for J in Link.Range2.First .. Link.Range2.Last - 1 loop
                  Read_Line (Infile2, List2, J, Append_Style);
               end loop;

               Line1 := Link.Range1.First;
               Line2 := Link.Range2.Last;

            when Change =>
               Offset1 := Link.Range1.Last - Link.Range1.First;
               Offset2 := Link.Range2.Last - Link.Range2.First;

               for J in Link.Range1.First .. Link.Range1.Last - 1 loop
                  Read_Line (Infile1, List1, J, Old_Style);
               end loop;

               for J in Link.Range2.First .. Link.Range2.Last - 1 loop
                  Read_Line (Infile2, List2, J, Change_Style);
               end loop;

               if Offset1 < Offset2 then
                  for J in Offset1 .. Offset2 - 1 loop
                     Add_Line (List1, Old_Style);
                  end loop;
               elsif Offset1 > Offset2 then
                  for J in Offset2 .. Offset1 - 1 loop
                     Add_Line (List2, Change_Style);
                  end loop;
               end if;

               Line1 := Link.Range1.Last;
               Line2 := Link.Range2.Last;

            when Delete =>
               for J in Link.Range1.First .. Link.Range1.Last - 1 loop
                  Read_Line (Infile1, List1, J, Old_Style);
               end loop;

               for J in Link.Range1.First .. Link.Range1.Last - 1 loop
                  Add_Line (List2, Remove_Style);
               end loop;

               Line1 := Link.Range1.Last;
               Line2 := Link.Range2.First;
         end case;

         Link := Link.Next;
      end loop;

      --  Complete files with the remaining lines.

      Len := 0;
      while Len < Context_Len and then not End_Of_File (Infile1) loop
         Read_Line (Infile1, List1, Line1, Default_Style);
         Line1 := Line1 + 1;
         Len   := Len + 1;
      end loop;

      Len := 0;
      while Len < Context_Len and then not End_Of_File (Infile2) loop
         Read_Line (Infile2, List2, Line2, Default_Style);
         Line2 := Line2 + 1;
         Len   := Len + 1;
      end loop;

      Thaw (List2);
      Thaw (List1);
      Close (Infile2);
      Close (Infile1);

   exception
      when End_Error =>
         Thaw (List2);
         Thaw (List1);
         Close (Infile2);
         Close (Infile1);
   end Fill_Diff_Lists;

   --------------------
   -- Value1_Changed --
   --------------------

   procedure Value1_Changed
     (Object : access Gtk_Widget_Record'Class)
   is
      Vdiff : constant Vdiff_Access := Vdiff_Access (Object);
   begin
      if Vdiff.Ignore_Value_Changed then
         return;
      end if;

      Vdiff.Ignore_Value_Changed := True;
      Set_Value
        (Get_Vadjustment (Vdiff.Clist2),
         Get_Value (Get_Vadjustment (Vdiff.Clist1)));
      Vdiff.Ignore_Value_Changed := False;
   end Value1_Changed;

   --------------------
   -- Value2_Changed --
   --------------------

   procedure Value2_Changed
     (Object : access Gtk_Widget_Record'Class)
   is
      Vdiff : constant Vdiff_Access := Vdiff_Access (Object);
   begin
      if Vdiff.Ignore_Value_Changed then
         return;
      end if;

      Vdiff.Ignore_Value_Changed := True;
      Set_Value
        (Get_Vadjustment (Vdiff.Clist1),
         Get_Value (Get_Vadjustment (Vdiff.Clist2)));
      Vdiff.Ignore_Value_Changed := False;
   end Value2_Changed;

end Vdiff_Utils;
