-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2003                         --
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

with Glide_Kernel;         use Glide_Kernel;
with Glide_Kernel.Scripts; use Glide_Kernel.Scripts;
with Glide_Intl;           use Glide_Intl;

with String_Utils;         use String_Utils;
with Basic_Types;
with GNAT.OS_Lib;          use GNAT.OS_Lib;
with Gtkada.Dialogs;       use Gtkada.Dialogs;

with Vdiff2_Module;        use Vdiff2_Module;
with Gdk.Color; use Gdk.Color;
with Glide_Kernel.Preferences; use Glide_Kernel.Preferences;

package body Vdiff2_Utils is

   use Diff_Head_List;
   use Diff_Chunk_List;

   Default_Style : constant String  := "Default_diff";
   Old_Style     : constant String  := "Old_diff";
   Append_Style  : constant String  := "Append_diff";
   Remove_Style  : constant String  := "Remove_diff";
   Change_Style  : constant String  := "Change_diff";

   type   T_VRange is array (1 .. 3) of Diff_Range;
   type   T_VStr  is array (1 .. 3) of String_Access;
   type   T_VOffset is array (1 .. 3) of Natural;

   procedure Free (V : in out T_VStr);
   --  free memory of each element of vector V

   procedure Append
     (Kernel  : access Glide_Kernel.Kernel_Handle_Record'Class;
      VRange  : in out T_VRange;
      VFile   : T_VStr;
      VOffset : T_VOffset;
      VStyle  : T_VStr;
      Ref     : T_Loc;
      Loc     : T_Loc);
   --  Hightlight the Chunk CurrChunk where action is Append
   --  ??? What is CurrChunk

   procedure Show_Diff_Chunk
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      Item   : Diff_Head; CurrChunk : Diff_Chunk_Access);
   --  Hightlight the Chunk CurrChunk
   --  ??? What is CurrChunk

   function Add_Line
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      File   : String;
      Pos    : Natural;
      Style  : String := "";
      Number : Natural := 1) return String;
   --  Add a line constaining Line in editor,at line Pos,
   --  using Style for color
   --  And return corresponding Mark

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
   --  Remove highlighting of line number Pos in file File

   function Mark_Diff_Block
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      File  : String;
      Pos   : Natural) return String;
   --  Return the mark corresponding the beginig of line number Pos

   ----------------
   -- Show_Merge --
   ----------------

   procedure Show_Merge
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      Merge  : String;
      Item   : Diff_Head)
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
      Item   : Diff_Head)
   is
      List      : constant Diff_List := Item.List;
      CurrNode  : Diff_List_Node := First (List);
      CurrChunk : Diff_Chunk_Access := Data (CurrNode);
      --  ??? Do not use Pascal style identifier. Use Mixed_Case instead

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

      while CurrNode /= Diff_Chunk_List.Null_Node loop
         case CurrChunk.Range2.Action is
            when Append =>
               CurrChunk.Range1.Mark := new String'
                 (Add_Line (Kernel, Item.File1.all,
                            CurrChunk.Range1.First, Append_Style,
                            CurrChunk.Range2.Last - CurrChunk.Range2.First));
               Highlight_Line (Kernel, Item.File2.all, CurrChunk.Range2.First,
                               Old_Style,
                               CurrChunk.Range2.Last - CurrChunk.Range2.First);
               CurrChunk.Range2.Mark := new String'
                 (Mark_Diff_Block (Kernel, Item.File2.all,
                                   CurrChunk.Range2.First));

            when Change =>
               Offset1 := CurrChunk.Range1.Last - CurrChunk.Range1.First;
               Offset2 := CurrChunk.Range2.Last - CurrChunk.Range2.First;
               Highlight_Line (Kernel, Item.File1.all, CurrChunk.Range1.First,
                               Change_Style, Offset1);
               Highlight_Line (Kernel, Item.File2.all, CurrChunk.Range2.First,
                               Old_Style, Offset2);

               if Offset1 < Offset2 then
                  Add_Line (Kernel, Item.File1.all,
                            CurrChunk.Range1.Last, Change_Style,
                            Offset2 - Offset1);
               elsif Offset1 > Offset2 then
                  Add_Line (Kernel, Item.File2.all,
                            CurrChunk.Range2.Last, Old_Style,
                            Offset1 - Offset2);
               end if;

               CurrChunk.Range1.Mark := new String'
                 (Mark_Diff_Block (Kernel, Item.File1.all,
                                   CurrChunk.Range1.First));
               CurrChunk.Range2.Mark := new String'
                 (Mark_Diff_Block (Kernel, Item.File2.all,
                                   CurrChunk.Range2.First));

            when Delete =>
               Highlight_Line (Kernel, Item.File1.all,
                               CurrChunk.Range1.First, Remove_Style,
                               CurrChunk.Range1.Last - CurrChunk.Range1.First);
               CurrChunk.Range2.Mark := new String'
                 (Add_Line (Kernel, Item.File2.all,
                            CurrChunk.Range2.First, Old_Style,
                            CurrChunk.Range1.Last - CurrChunk.Range1.First));
               CurrChunk.Range1.Mark := new String'
                 (Mark_Diff_Block (Kernel, Item.File1.all,
                                   CurrChunk.Range1.First));

            when others =>
               null;
         end case;

         CurrNode := Next (CurrNode);
         CurrChunk := Data (CurrNode);
      end loop;

   end Show_Differences;

   ------------------------
   --  Show_Differences3 --
   ------------------------

   procedure Show_Differences3
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      Item   : Diff_Head)
   is
      Args_edit : Argument_List := (1 => new String'(Item.File1.all));
      Res       : Diff_List;
      CurrNode  : Diff_List_Node;
      CurrChunk : Diff_Chunk_Access;

   begin

      if Item.File3 = null then
         Show_Differences (Kernel, Item);
         return;
      end if;

      Res := Simplify (Item.List, Item.Ref_File);
      CurrNode := First (Res);
      Register_Highlighting (Kernel);

      Execute_GPS_Shell_Command (Kernel, "edit", Args_edit);
      Basic_Types.Free (Args_edit);

      Args_edit := (1 => new String'(Item.File2.all));
      Execute_GPS_Shell_Command (Kernel, "edit", Args_edit);
      Basic_Types.Free (Args_edit);

      Args_edit := (1 => new String'(Item.File3.all));
      Execute_GPS_Shell_Command (Kernel, "edit", Args_edit);
      Basic_Types.Free (Args_edit);

      while CurrNode /= Diff_Chunk_List.Null_Node loop
         CurrChunk := Data (CurrNode);
         Show_Diff_Chunk (Kernel, Item, CurrChunk);
         CurrNode := Next (CurrNode);
      end loop;

      Free_List (Res);
   end Show_Differences3;

   -------------
   --  Append --
   -------------

   procedure Append
     (Kernel  : access Glide_Kernel.Kernel_Handle_Record'Class;
      VRange  : in out T_VRange;
      VFile   : T_VStr;
      VOffset : T_VOffset;
      VStyle  : T_VStr;
      Ref     : T_Loc;
      Loc     : T_Loc)
   is
      Other, Other2 : T_Loc := 0;
      Offset_Max   : Natural := 0;
      Tmp : String_Access;

   begin
      for I in 1 .. 3 loop
      --  ??? Do not use I for loop indexing, use J instead
         if I /= Ref and I /= Loc and Other = 0 then
            Other := I;
            if Loc /= 0 then
               Other2 := Loc;
               exit;
            end if;
         end if;

         if I /= Other and Other /= 0 and I /= Ref then
            Other2 := I;
            exit;
         end if;
      end loop;

      for I in 1 .. 3 loop
         if VOffset (I) >= Offset_Max then
            Offset_Max := VOffset (I);
         end if;
      end loop;

      for I in 1 .. 3 loop
         Highlight_Line
           (Kernel, VFile (I).all,
            VRange (I).First,
            VStyle (I).all, VOffset (I));

         if VOffset (I) < Offset_Max then
            Tmp := new String'
              (Add_Line
                 (Kernel, VFile (I).all,
                  VRange (I).Last, VStyle (I).all,
                  Offset_Max - VOffset (I)));
         end if;

         if VRange (I).Action = Delete then
            VRange (I).Mark := Tmp;
         elsif I = Ref then

            if VRange (Other).Action = Append or
              VRange (Other2).Action = Append then
               VRange (I).Mark := Tmp;

            else
               VRange (I).Mark := new String'
                 (Mark_Diff_Block (Kernel, VFile (I).all,
                                   VRange (I).First));
               Free (Tmp);
            end if;
         else
            VRange (I).Mark := new String'
              (Mark_Diff_Block (Kernel, VFile (I).all,
                                VRange (I).First));
            Free (Tmp);
         end if;
      end loop;
   end Append;

   ---------------------
   -- Show_Diff_Chunk --
   ---------------------

   procedure Show_Diff_Chunk
     (Kernel  : access Glide_Kernel.Kernel_Handle_Record'Class;
      Item    : Diff_Head; CurrChunk : Diff_Chunk_Access)
   is
      Other   : T_Loc := 0;
      Other2  : T_Loc := 0;
      Loc     : constant T_Loc := CurrChunk.Location;
      Ref     : constant T_Loc := Item.Ref_File;
      VRange  : T_VRange :=
        (CurrChunk.Range1, CurrChunk.Range2, CurrChunk.Range3);
      VFile   : constant T_VStr :=
        (Item.File1, Item.File2, Item.File3);
      VOffset : constant T_VOffset :=
        ((CurrChunk.Range1.Last - CurrChunk.Range1.First),
         (CurrChunk.Range2.Last - CurrChunk.Range2.First),
         (CurrChunk.Range3.Last - CurrChunk.Range3.First));
      VStyle : T_VStr;

   begin
      for I in 1 .. 3 loop
         if I /= Ref and I /= Loc and Other = 0 then
            Other := I;
            if Loc /= 0 then
               Other2 := Loc;
               exit;
            end if;
         end if;

         if I /= Other and Other /= 0 and I /= Ref then
            Other2 := I;
            exit;
         end if;
      end loop;

      if Loc = 0 then
         if CurrChunk.Conflict then
            if VRange (Other).Action = Change
              and VRange (Other2).Action = Change then
               VStyle (Other2) := new String'(Change_Style);
               VStyle (Ref) := new String'(Old_Style);
               VStyle (Other) := new String'(Change_Style);
               Append (Kernel, VRange, VFile, VOffset, VStyle, Ref, Other);
               Free (VStyle);
            else
               for J in 1 .. 3 loop
                  for K in 1 .. 3 loop
                     if K /= Ref and K /= J then Other2 := K; end if;
                  end loop;

                  if VRange (J).Action = Append then
                     VStyle (J)      := new String'(Append_Style);
                     VStyle (Ref)    := new String'(Old_Style);
                     VStyle (Other2) := new String'(Change_Style);
                     Append (Kernel, VRange, VFile,
                             VOffset, VStyle, Ref, J);
                     Free (VStyle);
                     exit;

                  elsif VRange (J).Action = Delete then
                     VStyle (J)      := new String'(Remove_Style);
                     VStyle (Ref)    := new String'(Old_Style);
                     VStyle (Other2) := new String'(Change_Style);
                     Append (Kernel, VRange, VFile,
                             VOffset, VStyle, Ref, J);
                     Free (VStyle);
                     exit;
                  end if;
               end loop;

            end if;
         else

            if VRange (Other).Action = Append then
               VStyle (Other)  := new String'(Append_Style);
               VStyle (Ref)    := new String'(Old_Style);
               VStyle (Other2) := new String'(Append_Style);
               Append (Kernel, VRange, VFile,
                       VOffset, VStyle, Ref, Other);
            elsif VRange (Other).Action = Delete then
               VStyle (Other)  := new String'(Remove_Style);
               VStyle (Ref)    := new String'(Old_Style);
               VStyle (Other2) := new String'(Remove_Style);
               Append (Kernel, VRange, VFile,
                       VOffset, VStyle, Ref, Other);
            elsif VRange (Other).Action = Change then
               VStyle (Other)  := new String'(Change_Style);
               VStyle (Ref)    := new String'(Old_Style);
               VStyle (Other2) := new String'(Change_Style);
               Append (Kernel, VRange, VFile, VOffset, VStyle, Ref, Other);
            end if;

            Free (VStyle);
         end if;

         CurrChunk.Range1 := VRange (1);
         CurrChunk.Range2 := VRange (2);
         CurrChunk.Range3 := VRange (3);
         return;
      end if;

      case VRange (Loc).Action is
         when Append =>
            VStyle (Loc)   := new String'(Append_Style);
            VStyle (Ref)   := new String'(Old_Style);
            VStyle (Other) := new String'(Default_Style);
            Append (Kernel, VRange, VFile,
                    VOffset, VStyle, Ref, Loc);

         when Change =>
            VStyle (Loc)   := new String'(Change_Style);
            VStyle (Ref)   := new String'(Old_Style);
            VStyle (Other) := new String'(Default_Style);
            Append (Kernel, VRange, VFile, VOffset, VStyle, Ref, Loc);

         when Delete =>
            VStyle (Loc)   := new String'(Remove_Style);
            VStyle (Ref)   := new String'(Old_Style);
            VStyle (Other) := new String'(Default_Style);
            Append (Kernel, VRange, VFile,
                    VOffset, VStyle, Ref, Loc);

         when others =>
            null;
      end case;

      Free (VStyle);
      CurrChunk.Range1 := VRange (1);
      CurrChunk.Range2 := VRange (2);
      CurrChunk.Range3 := VRange (3);

   end Show_Diff_Chunk;

   ----------------------
   -- Hide_Differences --
   ----------------------

   procedure Hide_Differences
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      Item   : Diff_Head) is
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

   procedure Free (V : in out T_VStr) is
   begin
      for J in V'Range loop
         Free (V (J));
      end loop;
   end Free;

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
      Args_Line : Argument_List :=
        (1 => new String'(File),
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
      Args_Highlight : Argument_List :=
        (1 => new String'(File),
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
      Args_Highlight : Argument_List :=
        (1 => new String'(File),
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
      Args : Argument_List :=
        (1 => new String'(File),
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
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
   is
      Default_Color : constant String  :=
        To_String (Get_Pref (Kernel, Diff_Default_Color));
      Old_Color     : constant String  :=
        To_String (Get_Pref (Kernel, Diff_Old_Color));
      Append_Color  : constant String  :=
        To_String (Get_Pref (Kernel, Diff_Append_Color));
      Remove_Color  : constant String  :=
        To_String (Get_Pref (Kernel, Diff_Remove_Color));
      Change_Color  : constant String  :=
        To_String (Get_Pref (Kernel, Diff_Change_Color));
      Args          : Argument_List :=
      (1 => new String'(Default_Style),
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