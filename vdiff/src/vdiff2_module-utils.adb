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

with Glide_Kernel;                      use Glide_Kernel;
with Glide_Intl;                        use Glide_Intl;
with Glide_Kernel.Modules;              use Glide_Kernel.Modules;

with GNAT.OS_Lib;                       use GNAT.OS_Lib;
with Gtkada.MDI;                        use Gtkada.MDI;
with Gtk.Enums;                         use Gtk.Enums;

with Vdiff2_Module;                     use Vdiff2_Module;
with Vdiff2_Module.Utils.Shell_Command; use Vdiff2_Module.Utils.Shell_Command;
with Vdiff2_Module.Utils.Text;          use Vdiff2_Module.Utils.Text;
with Vdiff2_Command_Block;              use Vdiff2_Command_Block;
with Vdiff2_Command_Line;               use Vdiff2_Command_Line;
with Pixmaps_Vdiff2;                    use Pixmaps_Vdiff2;

with Gdk.Pixbuf;                        use Gdk.Pixbuf;
with Gtkada.Dialogs;                    use Gtkada.Dialogs;

with Traces;                            use Traces;
with Ada.Exceptions;                    use Ada.Exceptions;
with Ada.Unchecked_Deallocation;
with Commands;                          use Commands;

package body Vdiff2_Module.Utils is

   use Diff_Head_List;
   use Diff_Chunk_List;

   type   T_VLine_Information is array (1 .. 3) of Line_Information_Data;

   procedure Append
     (Kernel   : access Glide_Kernel.Kernel_Handle_Record'Class;
      VRange   : in out T_VRange;
      VFile    : T_VFile;
      VOffset  : T_VOffset;
      VStyle   : T_VStr;
      Ref      : T_Loc;
      Loc      : T_Loc;
      Info     : T_VLine_Information;
      Conflict : Boolean := False);
   --  Hightlight the Current Chunk given in VRange where action is Append

   procedure Fine_Diff_Block
     (Kernel       : Kernel_Handle;
      Source_File  : Virtual_File;
      Dest_File    : Virtual_File;
      Source_Range : Diff_Range;
      Dest_Range   : Diff_Range := Null_Range);
   --  Highlight Fine change between two diff block

   procedure Fine_Highlight_Line
     (Kernel            : Kernel_Handle;
      Current_Line_Source,
      Current_Line_Dest : String;
      File              : Virtual_File;
      Line              : Natural);
   --  Highlight difference in File between two lines

   procedure Move_Mark (Source, Dest : Diff_List);
   --  Move Source mark on to Dest

   procedure Put_Button
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      Info     : T_VLine_Information;
      Conflict : Boolean;
      Pos      : Natural;
      VRange   : T_VRange;
      VFile    : T_VFile;
      VStyle   : T_VStr;
      Action   : Handler_Action_Line := null);
   --  Put an icon on the column information

   procedure Show_Differences
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      Item   : in out Diff_Head);
   --  Show a result of diff Item

   procedure Show_Diff_Chunk
     (Kernel  : access Glide_Kernel.Kernel_Handle_Record'Class;
      Item    : Diff_Head; Curr_Chunk : Diff_Chunk_Access;
      Info    : T_VLine_Information);
   --  Hightlight the Current Chunk Curr_Chunk

   -------------
   --  Append --
   -------------

   procedure Append
     (Kernel   : access Glide_Kernel.Kernel_Handle_Record'Class;
      VRange   : in out T_VRange;
      VFile    : T_VFile;
      VOffset  : T_VOffset;
      VStyle   : T_VStr;
      Ref      : T_Loc;
      Loc      : T_Loc;
      Info     : T_VLine_Information;
      Conflict : Boolean := False)
   is
      Other, Other2 : T_Loc := 0;
      Offset_Max   : Natural := 0;
      Tmp : String_Access;

   begin
      for J in 1 .. 3 loop
         if J /= Ref and J /= Loc and Other = 0 then
            Other := J;
            if Loc /= 0 then
               Other2 := Loc;
               exit;
            end if;
         end if;

         if J /= Other and Other /= 0 and J /= Ref then
            Other2 := J;
            exit;
         end if;
      end loop;

      for J in 1 .. 3 loop
         if VOffset (J) >= Offset_Max then
            Offset_Max := VOffset (J);
         end if;
      end loop;

      for J in 1 .. 3 loop
         if VOffset (J) > 0 and then J /= Ref then
            Put_Button
              (Kernel, Info, Conflict, J,
               VRange, VFile, VStyle, Move_On_Ref_File'Access);
         elsif J /= Ref then
            Put_Button
              (Kernel, Info, Conflict, J,
               VRange, VFile, VStyle, Delete_From_Ref_File'Access);
         end if;

         Highlight_Line
           (Kernel, VFile (J),
            VRange (J).First,
            VStyle (J).all, VOffset (J));

         if VOffset (J) < Offset_Max then
            Tmp := new String'
              (Add_Line
                 (Kernel, VFile (J),
                  VRange (J).Last, VStyle (J).all,
                  Offset_Max - VOffset (J)));
            VRange (J).Blank_Lines := new String'(Tmp.all);
         end if;

         if VRange (J).Action = Delete then
            if Tmp /= null then
               VRange (J).Mark := new String'(Tmp.all);
            end if;

         elsif J = Ref then

            if VRange (Other).Action = Append or
              VRange (Other2).Action = Append then
               if Tmp /= null then
                  VRange (J).Mark := new String'(Tmp.all);
               end if;
            else
               VRange (J).Mark := new String'
                 (Mark_Diff_Block (Kernel, VFile (J),
                                   VRange (J).First));
               if Tmp /= null then
                  Delete_Mark (Kernel_Handle (Kernel), Tmp.all);
               end if;
            end if;
         else
            if (VRange (Other).Action = Append and then J /= Other) or
              (VRange (Other2).Action = Append and then J /= Other2)
            then
               if Tmp /= null then
                  VRange (J).Mark := new String'(Tmp.all);
               end if;
            else
               VRange (J).Mark := new String'
                 (Mark_Diff_Block (Kernel, VFile (J),
                                   VRange (J).First));
               if Tmp /= null then
                  Delete_Mark (Kernel_Handle (Kernel), Tmp.all);
               end if;
            end if;
         end if;

         Free (Tmp);
      end loop;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Append;

   -------------------------
   -- Fine_Highlight_Line --
   -------------------------

   procedure Fine_Highlight_Line
     (Kernel            : Kernel_Handle;
      Current_Line_Source,
      Current_Line_Dest : String;
      File              : Virtual_File;
      Line              : Natural)
   is
      Hor_List : constant Diff_List := Horizontal_Diff
        (Current_Line_Dest, Current_Line_Source);
      Curr_Node : Diff_Chunk_List.List_Node := First (Hor_List);
      Diff      : Diff_Chunk;
      First     : Natural := 0;
      Last      : Natural := 0;
      Nb_Hghlt_Chr : Natural := 0;
   begin
      if Current_Line_Dest'Length = 0 or Current_Line_Source'Length = 0 then
         return;
      end if;

      while Curr_Node /= Diff_Chunk_List.Null_Node
      loop
         Diff := Data (Curr_Node).all;
         First := Diff.Range1.First;
         Last := Diff.Range1.Last;

         if First = 0 then
            First := 1;
            Last := Last + 1;
         end if;

         if Last = 0 then
            Last := 2;
            First := 1;
         end if;

         if First < Last then
            Nb_Hghlt_Chr := Nb_Hghlt_Chr + (Last - First);
         else
            Nb_Hghlt_Chr := Nb_Hghlt_Chr + (First - Last);
         end if;

         if Nb_Hghlt_Chr > Natural (Current_Line_Dest'Length * 0.40) then
            return;
         end if;

         Curr_Node := Next (Curr_Node);
      end loop;

      Curr_Node := Diff_Chunk_List.First (Hor_List);

      while Curr_Node /= Diff_Chunk_List.Null_Node loop
         Diff := Data (Curr_Node).all;
         First := Diff.Range1.First;
         Last := Diff.Range1.Last;

         if First = 0 then
            First := 1;
            Last := Last + 1;
         end if;

         if Last = 0 then
            Last := 2;
            First := 1;
         end if;

         Highlight_Range
           (Kernel, File, Fine_Change_Style, Line, First, Last);

         Curr_Node := Next (Curr_Node);
      end loop;
   end Fine_Highlight_Line;

   -----------------------
   --  Fine_Diff_Block  --
   -----------------------

   procedure Fine_Diff_Block
     (Kernel       : Kernel_Handle;
      Source_File  : Virtual_File;
      Dest_File    : Virtual_File;
      Source_Range : Diff_Range;
      Dest_Range   : Diff_Range := Null_Range)
   is
      Offset_Dest          : constant Natural :=
        Dest_Range.Last - Dest_Range.First;
      Offset_Source        : constant Natural :=
        Source_Range.Last - Source_Range.First;
      Offset_Min           : Natural;
      Current_Line_Source : String_Access;
      Current_Line_Dest   : String_Access;
      Line                : Natural := Dest_Range.First;

   begin
      if not VDiff2_Module (Vdiff_Module_ID).Enable_Fine_Diff then
         return;
      end if;

      if Offset_Source < Offset_Dest then
         Offset_Min := Offset_Source;
      else
         Offset_Min := Offset_Dest;
      end if;

      if Offset_Min > 0 then
         for J in 1 .. Offset_Min loop

            Current_Line_Dest := new String'
              (Get_Line
                 (Kernel, Dest_File, Line));
            Current_Line_Source := new String'
              (Get_Line
                 (Kernel, Source_File, Source_Range.First + J - 1));
            Fine_Highlight_Line
              (Kernel,
               Current_Line_Source.all,
               Current_Line_Dest.all,
               Dest_File,
               Line);
            Line := Dest_Range.First + J;
            Free (Current_Line_Source);
            Free (Current_Line_Dest);
         end loop;
      end if;
   end Fine_Diff_Block;

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

   ----------------------
   -- Hide_Differences --
   ----------------------

   procedure Hide_Differences
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      Item   : Diff_Head)
   is
      Curr_Node  : Diff_List_Node;
      Curr_Chunk : Diff_Chunk_Access;
      VFile      : constant T_VFile := (Item.File1, Item.File2, Item.File3);

   begin
      Curr_Node := First (Item.List);

      while Curr_Node /= Diff_Chunk_List.Null_Node loop
         Curr_Chunk := Data (Curr_Node);
         Remove_Blank_Lines (Kernel, Curr_Chunk.Range1.Blank_Lines);
         Remove_Blank_Lines (Kernel, Curr_Chunk.Range2.Blank_Lines);
         Remove_Blank_Lines (Kernel, Curr_Chunk.Range3.Blank_Lines);
         Curr_Node := Next (Curr_Node);
      end loop;

      for J in VFile'Range loop
         if VFile (J) /= VFS.No_File then
            Unhighlight_Line (Kernel, VFile (J), 0, Default_Style);
            Unhighlight_Line (Kernel, VFile (J), 0, Old_Style);
            Unhighlight_Line (Kernel, VFile (J), 0, Append_Style);
            Unhighlight_Line (Kernel, VFile (J), 0, Remove_Style);
            Unhighlight_Line (Kernel, VFile (J), 0, Change_Style);
            Remove_Line_Information_Column
              (Kernel, VFile (J), Id_Col_Vdiff);
            Unhighlight_Range (Kernel, VFile (J), Fine_Change_Style);
         end if;
      end loop;
   end Hide_Differences;

   ---------------------
   -- Is_In_Diff_List --
   ---------------------

   function Is_In_Diff_List
     (Selected_File : Virtual_File;
      List          : Diff_Head_List.List) return Diff_Head_List.List_Node
   is
      Curr_Node : Diff_Head_List.List_Node := First (List);
      Diff      : Diff_Head;
   begin
      while Curr_Node /= Diff_Head_List.Null_Node loop
         Diff := Data (Curr_Node);
         exit when Diff.File1 = Selected_File
           or else Diff.File2 = Selected_File
           or else Diff.File3 = Selected_File;
         Curr_Node := Next (Curr_Node);
      end loop;

      return Curr_Node;
   end Is_In_Diff_List;

   -----------------
   --  Move_Mark  --
   -----------------

   procedure Move_Mark (Source, Dest : Diff_List) is
      Curr_Node_Source  : Diff_List_Node := First (Source);
      Curr_Chunk_Source : Diff_Chunk_Access;
      Curr_Node_Dest    : Diff_List_Node := First (Dest);
      Curr_Chunk_Dest   : Diff_Chunk_Access;

   begin
      while Curr_Node_Source /= Diff_Chunk_List.Null_Node
        and then Curr_Node_Dest /= Diff_Chunk_List.Null_Node
      loop
         Curr_Chunk_Source := Data (Curr_Node_Source);
         Curr_Chunk_Dest := Data (Curr_Node_Dest);

         if Curr_Chunk_Source.Range1.Mark /= null then
            Curr_Chunk_Dest.Range1.Mark :=
              new String'(Curr_Chunk_Source.Range1.Mark.all);
         end if;

         if Curr_Chunk_Source.Range2.Mark /= null then
            Curr_Chunk_Dest.Range2.Mark :=
              new String'(Curr_Chunk_Source.Range2.Mark.all);
         end if;

         if Curr_Chunk_Source.Range3.Mark /= null then
            Curr_Chunk_Dest.Range3.Mark :=
              new String'(Curr_Chunk_Source.Range3.Mark.all);
         end if;

         if Curr_Chunk_Source.Range1.Blank_Lines /= null then
            Curr_Chunk_Dest.Range1.Blank_Lines :=
              new String'(Curr_Chunk_Source.Range1.Blank_Lines.all);
         end if;

         if Curr_Chunk_Source.Range2.Blank_Lines /= null then
            Curr_Chunk_Dest.Range2.Blank_Lines :=
              new String'(Curr_Chunk_Source.Range2.Blank_Lines.all);
         end if;

         if Curr_Chunk_Source.Range3.Blank_Lines /= null then
            Curr_Chunk_Dest.Range3.Blank_Lines :=
              new String'(Curr_Chunk_Source.Range3.Blank_Lines.all);
         end if;

         Curr_Node_Source := Next (Curr_Node_Source);
         Curr_Node_Dest := Next (Curr_Node_Dest);
      end loop;
   end Move_Mark;

   -------------------------
   --  Modify_Differences --
   -------------------------

   procedure Modify_Differences
     (Kernel    : access Glide_Kernel.Kernel_Handle_Record'Class;
      Item      : in out Diff_Head;
      Diff_List : Diff_Head_List_Access)
   is
      Curr_Node : Diff_Head_List.List_Node;
   begin
      Curr_Node := Is_In_Diff_List (Item.File1, Diff_List.all);

      if Curr_Node /= Diff_Head_List.Null_Node then
         Show_Differences3 (Kernel, Item);
         Item.Current_Node := First (Item.List);
         Set_Data (Curr_Node, Item);
         Goto_Difference (Kernel_Handle (Kernel), Data (Item.Current_Node));
      end if;
   end Modify_Differences;

   -------------------------
   -- Process_Differences --
   -------------------------

   procedure Process_Differences
     (Kernel    : access Glide_Kernel.Kernel_Handle_Record'Class;
      Item      : Diff_Head;
      Diff_List : Diff_Head_List_Access)
   is
      Item_Local : Diff_Head := Item;
      Button     : Message_Dialog_Buttons;
      pragma Unreferenced (Button);

   begin
      if not VDiff2_Module (Vdiff_Module_ID).Is_Active then
         VDiff_Toolbar (Kernel);
      end if;

      if Is_In_Diff_List
          (Item_Local.File1, Diff_List.all) = Diff_Head_List.Null_Node
        and then Is_In_Diff_List
          (Item_Local.File2, Diff_List.all) = Diff_Head_List.Null_Node
      then
         if Item_Local.File3 /= VFS.No_File
           and then Is_In_Diff_List
             (Item.File3, Diff_List.all) = Diff_Head_List.Null_Node
         then
            Append (Diff_List.all, Item_Local);
            Show_Differences3 (Kernel, Item_Local);
            Init_Prev_Diff_Cmd (Item);
            Goto_Difference (Kernel_Handle (Kernel),
                             Data (Item_Local.Current_Node));
            return;

         elsif Item_Local.File3 = VFS.No_File then
            Append (Diff_List.all, Item_Local);
            Show_Differences3 (Kernel, Item_Local);
            Init_Prev_Diff_Cmd (Item);
            Goto_Difference (Kernel_Handle (Kernel),
                             Data (Item_Local.Current_Node));
            return;
         end if;
         VDiff2_Module (Vdiff_Module_ID).Is_Active :=
           (VDiff2_Module (Vdiff_Module_ID).List_Diff.all /=
              Diff_Head_List.Null_List);
      end if;
      Button := Message_Dialog
        (Msg     => -"One of these files is already used in VDiff",
         Buttons => Button_OK,
         Parent  => Get_Main_Window (Kernel));
   end Process_Differences;

   ------------------
   --  Put_Button  --
   ------------------

   procedure Put_Button
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      Info     : T_VLine_Information;
      Conflict : Boolean;
      Pos      : Natural;
      VRange   : T_VRange;
      VFile    : T_VFile;
      VStyle   : T_VStr;
      Action   : Handler_Action_Line := null)
   is
      Cmd                 : Diff_Command_Line_Access;
      Green_Button_Pixbuf : constant Gdk_Pixbuf :=
        Gdk_New_From_Xpm_Data (green_button_xpm);
      Red_Button_Pixbuf   : constant Gdk_Pixbuf :=
        Gdk_New_From_Xpm_Data (red_button_xpm);
      Line : Natural := VRange (Pos).First - 1;

   begin
      if VStyle (Pos).all /= Default_Style then
         Create
           (Cmd, Kernel_Handle (Kernel),
            VDiff2_Module (Vdiff_Module_ID).List_Diff,
            VFile (Pos), VRange (Pos).First,
            Action);

         if Line <= 0 then
            Line := 1;
         end if;

         if not Conflict then
            Info (Pos)(Line).Image := Green_Button_Pixbuf;
         else
            Info (Pos)(Line).Image := Red_Button_Pixbuf;
         end if;

         Info (Pos)(Line).Associated_Command := Command_Access (Cmd);
      end if;
   end Put_Button;

   ----------------------
   -- Show_Differences --
   ----------------------

   procedure Show_Differences
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      Item   : in out Diff_Head)
   is
      List       : constant Diff_List := Item.List;
      Curr_Node  : Diff_List_Node := First (List);
      Curr_Chunk : Diff_Chunk_Access;
      Offset1    : Natural;
      Offset2    : Natural;
      VStyle     : T_VStr;
      Ref        : T_Loc := Item.Ref_File;
      Other      : T_Loc := 2;

   begin
      Trace (Me, "Show_Differences");

      if Ref = 1 then
         Other := 2;
      elsif Ref = 2 then
         Other := 1;
      else
         Ref := 1;
      end if;

      --  Keep the current window configuration, except we split the current
      --  editor. This doesn't lose the user's current setup, and will be
      --  superceded by the use of MDI_Child groups

      Edit (Kernel, Item.File1);
      Edit (Kernel, Item.File2);
      Split (Get_MDI (Kernel), Orientation_Horizontal);

      Create_Line_Information_Column
        (Kernel, Item.File1, Id_Col_Vdiff, True, True, True);
      Create_Line_Information_Column
        (Kernel, Item.File2, Id_Col_Vdiff, True, True, True);

      while Curr_Node /= Diff_Chunk_List.Null_Node loop
         Curr_Chunk := Data (Curr_Node);

         case Curr_Chunk.Range2.Action is
            when Append =>
               VStyle (Other) := new String'(Append_Style);
               VStyle (Ref)   := new String'(Old_Style);
               Curr_Chunk.Range1.Mark := new String'
                 (Add_Line
                    (Kernel, Item.File1,
                     Curr_Chunk.Range1.First, VStyle (Ref).all,
                     Curr_Chunk.Range2.Last - Curr_Chunk.Range2.First));
               Curr_Chunk.Range1.Blank_Lines :=
                 new String'(Curr_Chunk.Range1.Mark.all);
               Highlight_Line
                 (Kernel, Item.File2, Curr_Chunk.Range2.First,
                  VStyle (Other).all,
                  Curr_Chunk.Range2.Last - Curr_Chunk.Range2.First);
               Curr_Chunk.Range2.Mark := new String'
                 (Mark_Diff_Block (Kernel, Item.File2,
                                   Curr_Chunk.Range2.First));

            when Change =>
               VStyle (Other) := new String'(Change_Style);
               VStyle (Ref)   := new String'(Old_Style);
               Offset1 := Curr_Chunk.Range1.Last - Curr_Chunk.Range1.First;
               Offset2 := Curr_Chunk.Range2.Last - Curr_Chunk.Range2.First;
               Highlight_Line (Kernel, Item.File1, Curr_Chunk.Range1.First,
                               VStyle (Ref).all, Offset1);
               Highlight_Line (Kernel, Item.File2, Curr_Chunk.Range2.First,
                               VStyle (Other).all, Offset2);

               if Offset1 < Offset2 then
                  Curr_Chunk.Range2.Blank_Lines :=
                    new String'
                      (Add_Line
                        (Kernel, Item.File1,
                         Curr_Chunk.Range1.Last, VStyle (Ref).all,
                         Offset2 - Offset1));

               elsif Offset1 > Offset2 then
                  Curr_Chunk.Range2.Blank_Lines :=
                    new String'
                      (Add_Line
                        (Kernel, Item.File2,
                         Curr_Chunk.Range2.Last, VStyle (Other).all,
                         Offset1 - Offset2));
               end if;

               Curr_Chunk.Range1.Mark := new String'
                 (Mark_Diff_Block
                   (Kernel, Item.File1,
                    Curr_Chunk.Range1.First));
               Curr_Chunk.Range2.Mark := new String'
                 (Mark_Diff_Block
                   (Kernel, Item.File2,
                     Curr_Chunk.Range2.First));

               Fine_Diff_Block
                 (Kernel_Handle (Kernel),
                  Item.File1,
                  Item.File2,
                  Curr_Chunk.Range1,
                  Curr_Chunk.Range2);

            when Delete =>
               VStyle (Other) := new String'(Remove_Style);
               VStyle (Ref)   := new String'(Old_Style);
               Highlight_Line
                 (Kernel, Item.File1,
                  Curr_Chunk.Range1.First, VStyle (Ref).all,
                  Curr_Chunk.Range1.Last - Curr_Chunk.Range1.First);
               Curr_Chunk.Range2.Mark := new String'
                 (Add_Line
                   (Kernel, Item.File2,
                    Curr_Chunk.Range2.First, VStyle (Other).all,
                    Curr_Chunk.Range1.Last - Curr_Chunk.Range1.First));
               Curr_Chunk.Range2.Blank_Lines :=
                 new String'(Curr_Chunk.Range2.Mark.all);
               Curr_Chunk.Range1.Mark := new String'
                 (Mark_Diff_Block
                   (Kernel, Item.File1,
                    Curr_Chunk.Range1.First));

            when others =>
               null;
         end case;

         Free (VStyle);
         Curr_Node := Next (Curr_Node);
      end loop;
   end Show_Differences;

   ------------------------
   --  Show_Differences3 --
   ------------------------

   procedure Show_Differences3
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      Item   : in out Diff_Head)
   is
      Res                 : Diff_List;
      Curr_Node           : Diff_List_Node;
      Curr_Chunk          : Diff_Chunk_Access;
      Cmd                 : Diff_Command_Access;
      Info                : T_VLine_Information;

   begin
      Register_Highlighting (Kernel);

      if Item.File3 = VFS.No_File then
         Show_Differences (Kernel, Item);
         return;
      end if;

      Trace (Me, "Show_Differences3");

      --  Keep the current window configuration, except we split the current
      --  editor. This doesn't lose the user's current setup, and will be
      --  superceded by the use of MDI_Child groups

      Edit (Kernel, Item.File2);
      Edit (Kernel, Item.File1);
      Split (Get_MDI (Kernel), Orientation_Horizontal);
      Edit (Kernel, Item.File3);
      Split (Get_MDI (Kernel), Orientation_Horizontal);

      Info (1) := new Line_Information_Array
        (1 .. Get_File_Last_Line (Kernel, Item.File1));
      Info (2) := new Line_Information_Array
        (1 .. Get_File_Last_Line (Kernel, Item.File2));
      Info (3) := new Line_Information_Array
        (1 .. Get_File_Last_Line (Kernel, Item.File3));

      Create_Line_Information_Column
        (Kernel, Item.File1, Id_Col_Vdiff, True, True, True);
      Create_Line_Information_Column
        (Kernel, Item.File2, Id_Col_Vdiff, True, True, True);
      Create_Line_Information_Column
        (Kernel, Item.File3, Id_Col_Vdiff, True, True, True);

      Res := Simplify (Item.List, Item.Ref_File);
      Curr_Node := First (Res);

      while Curr_Node /= Diff_Chunk_List.Null_Node loop
         Curr_Chunk := Data (Curr_Node);
         Show_Diff_Chunk (Kernel, Item, Curr_Chunk, Info);
         Curr_Node := Next (Curr_Node);
      end loop;

      Create
        (Cmd,
         Kernel_Handle (Kernel),
         VDiff2_Module (Vdiff_Module_ID).List_Diff,
         Unhighlight_Difference'Access);

      Add_Line_Information
        (Kernel, Item.File1, Id_Col_Vdiff,
         Info (1));
      Add_Line_Information
        (Kernel, Item.File2, Id_Col_Vdiff,
         Info (2));
      Add_Line_Information
        (Kernel, Item.File3, Id_Col_Vdiff,
         Info (3));
      Move_Mark (Res, Item.List);
      Free_List (Res);
      Free (Res);
      Trace (Me, "End Show_Differences3");

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Show_Differences3;

   ---------------------
   -- Show_Diff_Chunk --
   ---------------------

   procedure Show_Diff_Chunk
     (Kernel     : access Glide_Kernel.Kernel_Handle_Record'Class;
      Item       : Diff_Head;
      Curr_Chunk : Diff_Chunk_Access;
      Info       : T_VLine_Information)
   is
      Other   : T_Loc := 0;
      Other2  : T_Loc := 0;
      Loc     : constant T_Loc := Curr_Chunk.Location;
      Ref     : constant T_Loc := Item.Ref_File;
      VRange  : T_VRange :=
        (Curr_Chunk.Range1, Curr_Chunk.Range2, Curr_Chunk.Range3);
      VFile   : constant T_VFile :=
        (Item.File1, Item.File2, Item.File3);
      VOffset : constant T_VOffset :=
        ((Curr_Chunk.Range1.Last - Curr_Chunk.Range1.First),
         (Curr_Chunk.Range2.Last - Curr_Chunk.Range2.First),
         (Curr_Chunk.Range3.Last - Curr_Chunk.Range3.First));
      VStyle : T_VStr;

   begin
      for J in 1 .. 3 loop
         if J /= Ref and J /= Loc and Other = 0 then
            Other := J;
            if Loc /= 0 then
               Other2 := Loc;
               exit;
            end if;
         end if;

         if J /= Other and Other /= 0 and J /= Ref then
            Other2 := J;
            exit;
         end if;
      end loop;

      if Loc = 0 then
         if Curr_Chunk.Conflict then
            if VRange (Other).Action = Change
              and VRange (Other2).Action = Change then
               VStyle (Other2) := new String'(Change_Style);
               VStyle (Ref)    := new String'(Old_Style);
               VStyle (Other)  := new String'(Change_Style);
               Append
                 (Kernel, VRange, VFile,
                  VOffset, VStyle, Ref, Other, Info, Curr_Chunk.Conflict);

               Fine_Diff_Block
                 (Kernel_Handle (Kernel),
                  VFile (Ref),
                  VFile (Other),
                  VRange (Ref),
                  VRange (Other));

               Fine_Diff_Block
                 (Kernel_Handle (Kernel),
                  VFile (Ref),
                  VFile (Other2),
                  VRange (Ref),
                  VRange (Other2));

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
                             VOffset, VStyle, Ref,
                             J, Info, Curr_Chunk.Conflict);
                     Fine_Diff_Block
                       (Kernel_Handle (Kernel),
                        VFile (Ref),
                        VFile (Other2),
                        VRange (Ref),
                        VRange (Other2));
                     Free (VStyle);
                     exit;

                  elsif VRange (J).Action = Delete then
                     VStyle (J)      := new String'(Remove_Style);
                     VStyle (Ref)    := new String'(Old_Style);
                     VStyle (Other2) := new String'(Change_Style);
                     Append (Kernel, VRange, VFile,
                             VOffset, VStyle, Ref,
                             J, Info, Curr_Chunk.Conflict);
                     Fine_Diff_Block
                       (Kernel_Handle (Kernel),
                        VFile (Ref),
                        VFile (Other2),
                        VRange (Ref),
                        VRange (Other2));
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
                       VOffset, VStyle, Ref,
                       Other, Info, Curr_Chunk.Conflict);
            elsif VRange (Other).Action = Delete then
               VStyle (Other)  := new String'(Remove_Style);
               VStyle (Ref)    := new String'(Old_Style);
               VStyle (Other2) := new String'(Remove_Style);
               Append (Kernel, VRange, VFile,
                       VOffset, VStyle, Ref, Other, Info,
                       Curr_Chunk.Conflict);
            elsif VRange (Other).Action = Change then
               VStyle (Other)  := new String'(Change_Style);
               VStyle (Ref)    := new String'(Old_Style);
               VStyle (Other2) := new String'(Change_Style);
               Append
                 (Kernel, VRange, VFile,
                  VOffset, VStyle, Ref, Other, Info, Curr_Chunk.Conflict);
               Fine_Diff_Block
                 (Kernel_Handle (Kernel),
                  VFile (Ref),
                  VFile (Other),
                  VRange (Ref),
                  VRange (Other));
               Fine_Diff_Block
                 (Kernel_Handle (Kernel),
                  VFile (Ref),
                  VFile (Other2),
                  VRange (Ref),
                  VRange (Other2));
            end if;

            Free (VStyle);
         end if;

         Curr_Chunk.Range1 := VRange (1);
         Curr_Chunk.Range2 := VRange (2);
         Curr_Chunk.Range3 := VRange (3);
         return;
      end if;

      case VRange (Loc).Action is
         when Append =>
            VStyle (Loc)   := new String'(Append_Style);
            VStyle (Ref)   := new String'(Old_Style);
            VStyle (Other) := new String'(Default_Style);
            Append (Kernel, VRange, VFile,
                    VOffset, VStyle, Ref, Loc, Info,
                    Curr_Chunk.Conflict);

         when Change =>
            VStyle (Loc)   := new String'(Change_Style);
            VStyle (Ref)   := new String'(Old_Style);
            VStyle (Other) := new String'(Default_Style);
            Append (Kernel, VRange, VFile, VOffset,
                    VStyle, Ref, Loc, Info,
                    Curr_Chunk.Conflict);
            Fine_Diff_Block
              (Kernel_Handle (Kernel),
               VFile (Ref),
               VFile (Loc),
               VRange (Ref),
               VRange (Loc));

         when Delete =>
            VStyle (Loc)   := new String'(Remove_Style);
            VStyle (Ref)   := new String'(Old_Style);
            VStyle (Other) := new String'(Default_Style);
            Append (Kernel, VRange, VFile,
                    VOffset, VStyle, Ref, Loc, Info,
                    Curr_Chunk.Conflict);

         when others =>
            null;
      end case;

      Free (VStyle);
      Curr_Chunk.Range1 := VRange (1);
      Curr_Chunk.Range2 := VRange (2);
      Curr_Chunk.Range3 := VRange (3);

   end Show_Diff_Chunk;

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

      Edit (Kernel, Create (Merge));
   end Show_Merge;

   -------------------------
   --  Unhighlight_Block  --
   -------------------------

   procedure Unhighlight_Block
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      File   : Virtual_File;
      Range1 : in out Diff_Range;
      Style  : String := "") is
   begin
      for J in Range1.First .. Range1.Last loop
         Unhighlight_Line (Kernel, File, J, Style);
      end loop;

      Remove_Blank_Lines (Kernel, Range1.Blank_Lines);
   end Unhighlight_Block;

   -----------------
   -- Visual_Diff --
   -----------------

   procedure Visual_Diff
     (File1 : Virtual_File;
      File2 : Virtual_File;
      File3 : Virtual_File := VFS.No_File)
   is
      Id     : constant VDiff2_Module := VDiff2_Module (Vdiff_Module_ID);
      Result : Diff_List;
      Button : Message_Dialog_Buttons;
      pragma Unreferenced (Button);
      Item   : Diff_Head;

   begin
      if File3 /= VFS.No_File then
         Result := Diff3 (Id.Kernel, File1, File2, File3);
      else
         Result := Diff (Id.Kernel, File1, File2);
      end if;

      if Result = Diff_Chunk_List.Null_List then
         Button := Message_Dialog
           (Msg         => -"No differences found.",
            Buttons     => Button_OK,
            Parent      => Get_Main_Window (Id.Kernel));
         return;
      end if;

      Item :=
        (List         => Result,
         File1        => File1,
         File2        => File2,
         File3        => File3,
         Current_Node => First (Result),
         Ref_File     => 2,
         Tmp_File     => VFS.No_File);
      Process_Differences (Id.Kernel, Item, Id.List_Diff);
   end Visual_Diff;

   ------------------
   -- Visual_Patch --
   ------------------

   function Visual_Patch
     (Orig_File : VFS.Virtual_File;
      New_File  : VFS.Virtual_File;
      Diff_File : VFS.Virtual_File;
      Revert    : Boolean := False;
      Tmp_File  : VFS.Virtual_File := VFS.No_File) return Boolean
   is
      Id     : constant VDiff2_Module := VDiff2_Module (Vdiff_Module_ID);
      Result : Diff_List;
      Button : Message_Dialog_Buttons;
      pragma Unreferenced (Button);
      Item   : Diff_Head;

   begin
      Result :=
        Diff (Id.Kernel, Orig_File, New_File, Diff_File, Revert);

      if Result = Diff_Chunk_List.Null_List then
         Button := Message_Dialog
           (Msg         => -"No differences found.",
            Buttons     => Button_OK,
            Parent      => Get_Main_Window (Id.Kernel));
         return False;
      end if;

      Item :=
        (List         => Result,
         File1        => Orig_File,
         File2        => New_File,
         File3        => VFS.No_File,
         Current_Node => First (Result),
         Ref_File     => 1,
         Tmp_File     => Tmp_File);
      Process_Differences (Id.Kernel, Item, Id.List_Diff);
      return True;
   end Visual_Patch;

end Vdiff2_Module.Utils;
