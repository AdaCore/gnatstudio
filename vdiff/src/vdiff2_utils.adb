-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2003                       --
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

with Ada.Unchecked_Deallocation;

with Glide_Kernel;              use Glide_Kernel;
with Glide_Kernel.Scripts;      use Glide_Kernel.Scripts;
with Glide_Intl;                use Glide_Intl;

with String_Utils;              use String_Utils;
with Basic_Types;               use Basic_Types;

with GNAT.OS_Lib; use GNAT.OS_Lib;
with Gtkada.Dialogs;            use Gtkada.Dialogs;

package body Vdiff2_Utils is

   procedure Free (Ar : in out String_List);
   procedure Add_Line
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      File  : String;
      Pos   : Natural;
      Style : String := "";
      Line  : String := "");
   --  Add a line constaining Line in editor,at line Pos,
   --  using Style for color.

   procedure Highlight_Line
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      File  : String;
      Pos   : Natural;
      Style : String := "");
   --  Color a line constaining Line in editor,at line Pos,
   --  using Style for color.

   ----------------
   -- Show_Merge --
   ----------------
   procedure Show_Merge
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      Diff   : Diff_Occurrence_Link;
      Merge  : String;
      File1  : String;
      File2  : String;
      File3  : String := "")
   is
   pragma Unreferenced (File3);
      Button : Message_Dialog_Buttons;
      Args_edit       : Argument_List := (1 => new String'(Merge));
      pragma Unreferenced (File1, File2, Diff);
   begin
      if Is_Regular_File (Merge) then
         Button := Message_Dialog
           (Msg         => -"Would you overwrite this file: "& Merge,
            Buttons     => Button_Yes or Button_No,
            Parent      => Get_Main_Window (Kernel));
         if Button = Button_No then
            return;
         end if;
      end if;
      Execute_GPS_Shell_Command (Kernel, "edit", Args_edit);
      Free (Args_edit);
   end Show_Merge;

   ----------------------
   -- Show_Differences --
   ----------------------

   procedure Show_Differences
     (Kernel : access Kernel_Handle_Record'Class;
      Diff   : Diff_Occurrence_Link;
      File1  : String;
      File2  : String;
      File3  : String := "")
   is
   pragma Unreferenced (File3);
      Link            : Diff_Occurrence_Link := Diff;
      Line1           : Natural := 1;
      Line2           : Natural := 1;
      Offset1         : Natural;
      Offset2         : Natural;
      Default_Style   : constant String  := "Defaut_diff";
      Old_Style       : constant String  := "Old_diff";
      Append_Style    : constant String  := "Append_diff";
      Remove_Style    : constant String  := "Remove_diff";
      Change_Style    : constant String  := "Change_diff";
      Default_Color   : constant String  := "#0000FF";
      Old_Color       : constant String  := "#C1C1C1";
      Append_Color    : constant String  := "#32DE0D";
      Remove_Color    : constant String  := "#FF5050";
      Change_Color    : constant String  := "#ECEC00";
      Args_edit       : Argument_List := (1 => new String'(File1));
      Args            : Argument_List := (1 => new String'(Default_Style),
                                          2 => new String'(Default_Color));
   begin
      --  <preferences>
      Execute_GPS_Shell_Command (Kernel, "register_highlighting", Args);
      Free (Args);
      Args := (1 => new String'(Append_Style),
               2 => new String'(Append_Color));
      Execute_GPS_Shell_Command (Kernel, "register_highlighting", Args);
      Free (Args);
      Args := (1 => new String'(Old_Style),
               2 => new String'(Old_Color));
      Execute_GPS_Shell_Command (Kernel, "register_highlighting", Args);
      Free (Args);
      Args := (1 => new String'(Remove_Style),
               2 => new String'(Remove_Color));
      Execute_GPS_Shell_Command (Kernel, "register_highlighting", Args);
      Free (Args);
      Args := (1 => new String'(Change_Style),
               2 => new String'(Change_Color));
      Execute_GPS_Shell_Command (Kernel, "register_highlighting", Args);
      Free (Args);

      Execute_GPS_Shell_Command (Kernel, "edit", Args_edit);
      Free (Args_edit);
      Args_edit := (1 => new String'(File2));
      Execute_GPS_Shell_Command (Kernel, "edit", Args_edit);
      Free (Args_edit);

      while Link /= null loop

         case Link.Action is
            when Append =>
               for J in Link.Range2.First .. Link.Range2.Last - 1 loop
                  Add_Line (Kernel, File1, J, Old_Style);
               end loop;

               for J in Link.Range2.First .. Link.Range2.Last - 1 loop
                  Highlight_Line (Kernel, File2, J, Append_Style);
               end loop;

               Line1 := Link.Range1.First;
               Line2 := Link.Range2.Last;

            when Change =>
               Offset1 := Link.Range1.Last - Link.Range1.First;
               Offset2 := Link.Range2.Last - Link.Range2.First;

               for J in Link.Range1.First .. Link.Range1.Last - 1 loop
                  Highlight_Line (Kernel, File1, J, Old_Style);
               end loop;

               for J in Link.Range2.First .. Link.Range2.Last - 1 loop
                  Highlight_Line (Kernel, File2, J, Change_Style);
               end loop;

               if Offset1 < Offset2 then
                  for J in Offset1 .. Offset2 - 1 loop
                     Add_Line (Kernel, File1, J, Old_Style);
                  end loop;
               elsif Offset1 > Offset2 then
                  for J in Offset2 .. Offset1 - 1 loop
                     Add_Line (Kernel, File2, J, Change_Style);
                  end loop;
               end if;

               Line1 := Link.Range1.Last;
               Line2 := Link.Range2.Last;

            when Delete =>
               for J in Link.Range1.First .. Link.Range1.Last - 1 loop
                  Highlight_Line (Kernel, File1, J, Old_Style);
               end loop;

               for J in Link.Range1.First .. Link.Range1.Last - 1 loop
                  Add_Line (Kernel, File2, J, Remove_Style);
               end loop;

               Line1 := Link.Range1.Last;
               Line2 := Link.Range2.First;

            when others =>
               null;
         end case;

         Link := Link.Next;
      end loop;
      Line1 := Line2;
      Line2 := Line1;
   end Show_Differences;

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Text_Iterator_Access) is
      procedure Free_Data is
        new Ada.Unchecked_Deallocation (Text_Iterator, Text_Iterator_Access);
   begin
      if This = null then
         return;
      end if;

      if This.Next /= null then
         Free (This.Next);
      end if;

      Free (This.New_Line);
      Free (This.Old_Line);
      Free_Data (This);
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Ar : in out String_List) is
   begin
      for A in Ar'Range loop
         Free (Ar (A));
      end loop;
   end Free;

   --------------
   -- Add_Line --
   --------------

   procedure Add_Line
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      File  : String;
      Pos   : Natural;
      Style : String := "";
      Line  : String := "")
   is
      Args_Line : Argument_List := (1 => new String'(File),
                                       2 => new String'(Image (Pos)),
                                       3 => new String'("1"),
                                       4 => new String'(Line & ASCII.LF),
                                       5 => new String'("0"),
                                       6 => new String'("0"));
      Args_Highlight : Argument_List := (1 => new String'(File),
                                            2 => new String'(Style),
                                            3 => new String'(Image (Pos)));
   begin
      Execute_GPS_Shell_Command (Kernel, "replace_text", Args_Line);
      Execute_GPS_Shell_Command (Kernel, "highlight", Args_Highlight);
      Free (Args_Line);
      Free (Args_Highlight);
   end Add_Line;

   --------------------
   -- Highlight_Line --
   --------------------

   procedure Highlight_Line
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      File  : String;
      Pos   : Natural;
      Style : String := "")
   is
      Args_Highlight : Argument_List := (1 => new String'(File),
                                            2 => new String'(Style),
                                            3 => new String'(Image (Pos)));
   begin
      Execute_GPS_Shell_Command (Kernel, "highlight", Args_Highlight);
      Free (Args_Highlight);
   end Highlight_Line;

end Vdiff2_Utils;
