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
with Basic_Types;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with Gtkada.Dialogs;            use Gtkada.Dialogs;

package body Vdiff2_Utils is

   Default_Style : constant String  := "Defaut_diff";
   Old_Style     : constant String  := "Old_diff";
   Append_Style  : constant String  := "Append_diff";
   Remove_Style  : constant String  := "Remove_diff";
   Change_Style  : constant String  := "Change_diff";

   function Add_Line
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      File   : String;
      Pos    : Natural;
      Style  : String := "";
      Number : Natural := 1) return String;

   procedure Add_Line
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      File   : String;
      Pos    : Natural;
      Style  : String := "";
      Number : Natural := 1);
   --  Add a line constaining Line in editor,at line Pos,
   --  using Style for color.

   procedure Highlight_Line
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      File  : String;
      Pos   : Natural;
      Style : String := "";
      Number : Natural := 1);
   --  Color a line constaining Line in editor,at line Pos,
   --  using Style for color.

   procedure Unhighlight_Line
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      File  : String;
      Pos   : Natural;
      Style : String := "");

   function Mark_Diff_Block
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      File  : String;
      Pos   : Natural) return String;

   procedure Register_Highlighting
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class);
   ----------------
   -- Show_Merge --
   ----------------

   procedure Show_Merge
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      Merge  : String;
      Item   : Diff_List_Head)
   is
      pragma Unreferenced (Item);
      Button : Message_Dialog_Buttons;
      Args_edit       : Argument_List := (1 => new String'(Merge));
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
      Basic_Types.Free (Args_edit);
   end Show_Merge;

   ----------------------
   -- Show_Differences --
   ----------------------

   procedure Show_Differences
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      Item   : Diff_List_Head)
   is
      Link      : Diff_Occurrence_Link := Item.List;
      Offset1   : Natural;
      Offset2   : Natural;
      Args_edit : Argument_List := (1 => new String'(Item.File1.all));
   begin
      Register_Highlighting (Kernel);
      Execute_GPS_Shell_Command (Kernel, "edit", Args_edit);
      Basic_Types.Free (Args_edit);
      Args_edit := (1 => new String'(Item.File2.all));
      Execute_GPS_Shell_Command (Kernel, "edit", Args_edit);
      Basic_Types.Free (Args_edit);

      while Link /= null loop
         case Link.Action is
            when Append =>
               Link.Range1.Mark := new String'
                 (Add_Line (Kernel, Item.File1.all,
                               Link.Range1.First, Old_Style,
                               Link.Range2.Last - Link.Range2.First));
               Highlight_Line (Kernel, Item.File2.all, Link.Range2.First,
                                  Append_Style,
                                  Link.Range2.Last - Link.Range2.First);
               Link.Range2.Mark := new String'
                 (Mark_Diff_Block (Kernel, Item.File2.all,
                                      Link.Range2.First));
            when Change =>
               Offset1 := Link.Range1.Last - Link.Range1.First;
               Offset2 := Link.Range2.Last - Link.Range2.First;
               Highlight_Line (Kernel, Item.File1.all, Link.Range1.First,
                                  Old_Style, Offset1);
               Highlight_Line (Kernel, Item.File2.all, Link.Range2.First,
                                  Change_Style, Offset2);

               if Offset1 < Offset2 then
                  Add_Line (Kernel, Item.File1.all,
                               Link.Range1.Last, Old_Style,
                               Offset2 - Offset1);
               elsif Offset1 > Offset2 then
                  Add_Line (Kernel, Item.File2.all,
                               Link.Range2.Last, Change_Style,
                               Offset1 - Offset2);
               end if;
               Link.Range1.Mark := new String'
                 (Mark_Diff_Block (Kernel, Item.File1.all,
                                      Link.Range1.First));
               Link.Range2.Mark := new String'
                 (Mark_Diff_Block (Kernel, Item.File2.all,
                                      Link.Range2.First));

            when Delete =>
               Highlight_Line (Kernel, Item.File1.all,
                                  Link.Range1.First, Old_Style,
                                  Link.Range1.Last - Link.Range1.First);
               Link.Range2.Mark := new String'
                 (Add_Line (Kernel, Item.File2.all,
                               Link.Range2.First, Remove_Style,
                               Link.Range1.Last - Link.Range1.First));
               Link.Range1.Mark := new String'
                 (Mark_Diff_Block (Kernel, Item.File1.all,
                                      Link.Range1.First));
            when others =>
               null;
         end case;
         Link := Link.Next;
      end loop;
   end Show_Differences;


   procedure Show_Differences3
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      Item   : Diff_List_Head)
   is
      Link2     : constant Diff_Occurrence_Link := Item.List;
      Args_edit : Argument_List := (1 => new String'(Item.File1.all));
   begin
      Register_Highlighting (Kernel);
      Execute_GPS_Shell_Command (Kernel, "edit", Args_edit);
      Basic_Types.Free (Args_edit);
      Args_edit := (1 => new String'(Item.File2.all));
      Execute_GPS_Shell_Command (Kernel, "edit", Args_edit);
      Basic_Types.Free (Args_edit);

      if Link2 = null then
         Show_Differences (Kernel, Item);
         return;
      end if;
   end Show_Differences3;
   ----------------------
   -- Hide_Differences --
   ----------------------

   procedure Hide_Differences
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      Item   : Diff_List_Head) is
   begin
      if Item.File1 /= null then
         Unhighlight_Line (Kernel, Item.File1.all, 0, Default_Style);
         Unhighlight_Line (Kernel, Item.File1.all, 0, Old_Style);
         Unhighlight_Line (Kernel, Item.File1.all, 0, Append_Style);
         Unhighlight_Line (Kernel, Item.File1.all, 0, Remove_Style);
         Unhighlight_Line (Kernel, Item.File1.all, 0, Change_Style);
      end if;
      if Item.File2 /= null then
         Unhighlight_Line (Kernel, Item.File2.all, 0, Default_Style);
         Unhighlight_Line (Kernel, Item.File2.all, 0, Old_Style);
         Unhighlight_Line (Kernel, Item.File2.all, 0, Append_Style);
         Unhighlight_Line (Kernel, Item.File2.all, 0, Remove_Style);
         Unhighlight_Line (Kernel, Item.File2.all, 0, Change_Style);
      end if;
      if Item.File3 /= null then
         Unhighlight_Line (Kernel, Item.File3.all, 0, Default_Style);
         Unhighlight_Line (Kernel, Item.File3.all, 0, Old_Style);
         Unhighlight_Line (Kernel, Item.File3.all, 0, Append_Style);
         Unhighlight_Line (Kernel, Item.File3.all, 0, Remove_Style);
         Unhighlight_Line (Kernel, Item.File3.all, 0, Change_Style);
      end if;
   end Hide_Differences;


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

   --------------
   -- Add_Line --
   --------------

   function Add_Line
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      File   : String;
      Pos    : Natural;
      Style  : String := "";
      Number : Natural := 1) return String
   is
      Args_Line : Argument_List := (1 => new String'(File),
                                    2 => new String'(Image (Pos)),
                                    3 => new String'(Image (Number)),
                                    4 => new String'(Style));
      Res : constant String :=  Execute_GPS_Shell_Command
        (Kernel, "add_blank_lines", Args_Line);
   begin
      Basic_Types.Free (Args_Line);
      return Res;
   end Add_Line;

   procedure Add_Line
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      File   : String;
      Pos    : Natural;
      Style  : String := "";
      Number : Natural := 1) is
      Dummy : constant String := Add_Line (Kernel, File, Pos, Style, Number);
      pragma Unreferenced (Dummy);
   begin
      null;
   end Add_Line;

   --------------------
   -- Highlight_Line --
   --------------------

   procedure Highlight_Line
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      File  : String;
      Pos   : Natural;
      Style : String := "";
      Number : Natural := 1)
   is
      Args_Highlight : Argument_List := (1 => new String'(File),
                                         2 => new String'(Style),
                                         3 => null);
   begin
      for J in 1 .. Number loop
         Args_Highlight (3) := new String'(Image (Pos + J - 1));
         Execute_GPS_Shell_Command (Kernel, "highlight", Args_Highlight);
         Free (Args_Highlight (3));
      end loop;
      Basic_Types.Free (Args_Highlight);
   end Highlight_Line;

   ----------------------
   -- Unhighlight_Line --
   ----------------------

   procedure Unhighlight_Line
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      File  : String;
      Pos   : Natural;
      Style : String := "")
   is
      Args_Highlight : Argument_List := (1 => new String'(File),
                                         2 => new String'(Style),
                                         3 => new String'(Image (Pos)));
   begin
      Execute_GPS_Shell_Command (Kernel, "unhighlight", Args_Highlight);
      Basic_Types.Free (Args_Highlight);
   end Unhighlight_Line;

   ---------------------
   -- Mark_Diff_Block --
   ---------------------

   function Mark_Diff_Block
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      File  : String;
      Pos   : Natural) return String
   is
      Args : Argument_List := (1 => new String'(File),
                               2 => new String'(Image (Pos)),
                               3 => new String'("1"));
      Res : constant String := Execute_GPS_Shell_Command
        (Kernel, "create_mark", Args);
   begin
      Basic_Types.Free (Args);
      return Res;
   end Mark_Diff_Block;

   ---------------------------
   -- Register_Highlighting --
   ---------------------------

   procedure Register_Highlighting
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class) is

      Default_Color : constant String  := "#0000FF";
      Old_Color     : constant String  := "#C1C1C1";
      Append_Color  : constant String  := "#32DE0D";
      Remove_Color  : constant String  := "#FF5050";
      Change_Color  : constant String  := "#ECEC00";
      Args          : Argument_List := (1 => new String'(Default_Style),
                                        2 => new String'(Default_Color));
   begin
      --  <preferences>
      Execute_GPS_Shell_Command (Kernel, "register_highlighting", Args);
      Basic_Types.Free (Args);
      Args := (1 => new String'(Append_Style),
               2 => new String'(Append_Color));
      Execute_GPS_Shell_Command (Kernel, "register_highlighting", Args);
      Basic_Types.Free (Args);
      Args := (1 => new String'(Old_Style),
               2 => new String'(Old_Color));
      Execute_GPS_Shell_Command (Kernel, "register_highlighting", Args);
      Basic_Types.Free (Args);
      Args := (1 => new String'(Remove_Style),
               2 => new String'(Remove_Color));
      Execute_GPS_Shell_Command (Kernel, "register_highlighting", Args);
      Basic_Types.Free (Args);
      Args := (1 => new String'(Change_Style),
               2 => new String'(Change_Color));
      Execute_GPS_Shell_Command (Kernel, "register_highlighting", Args);
      Basic_Types.Free (Args);

   end Register_Highlighting;
end Vdiff2_Utils;
