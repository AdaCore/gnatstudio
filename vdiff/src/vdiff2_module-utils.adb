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

with Glide_Kernel;             use Glide_Kernel;
with Glide_Kernel.Scripts;     use Glide_Kernel.Scripts;
with Glide_Intl;               use Glide_Intl;
with Glide_Kernel.Preferences; use Glide_Kernel.Preferences;
with Glide_Kernel.Modules;     use Glide_Kernel.Modules;

with String_Utils;             use String_Utils;
with GNAT.OS_Lib;              use GNAT.OS_Lib;
with Basic_Types;

with Vdiff2_Command;           use Vdiff2_Command;
with Vdiff2_Command_Line;      use Vdiff2_Command_Line;
with Pixmaps_Vdiff2;           use Pixmaps_Vdiff2;

with Gdk.Pixbuf;               use Gdk.Pixbuf;
with Gdk.Color;                use Gdk.Color;
with Gtkada.Dialogs;           use Gtkada.Dialogs;

with Traces;                   use Traces;
with Ada.Exceptions;           use Ada.Exceptions;
with Ada.Unchecked_Deallocation;
with Commands;                 use Commands;


package body Vdiff2_Module.Utils is

   use Diff_Head_List;
   use Diff_Chunk_List;

   Me                  : constant Debug_Handle := Create ("VDiff2_Utils");
   Default_Style       : constant String  := "Default_diff";
   Old_Style           : constant String  := "Old_diff";
   Append_Style        : constant String  := "Append_diff";
   Remove_Style        : constant String  := "Remove_diff";
   Change_Style        : constant String  := "Change_diff";
   Id_Col_Vdiff        : constant String  := "Vdiff2_Col_Merge";

   type   T_VLine_Information is array (1 .. 3) of Line_Information_Data;

   function Add_Line
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      File   : Virtual_File;
      Pos    : Natural;
      Style  : String := "";
      Number : Natural := 1) return String;
   --  Add a blank line at line Pos of a given file editor,
   --  using Style for color.
   --  Return corresponding Mark.

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

   function Get_File_Last_Line
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      File : Virtual_File) return Natural;
   --  Return the number of line in file File

   procedure Highlight_Line
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      File   : Virtual_File;
      Pos    : Natural;
      Style  : String := "";
      Number : Natural := 1);
   --  Color a line at line Pos in a given file editor, using Style for color

   function Mark_Diff_Block
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      File  : Virtual_File;
      Pos   : Natural) return String;
   --  Return the mark corresponding the begining of line number Pos

   procedure Move_Mark (Source, Dest : Diff_List);
   --  Move Source mark on to Dest

   procedure Put_Button
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      Info     : T_VLine_Information;
      Conflict : Boolean;
      Pos      : Natural;
      VRange   : T_VRange;
      VFile    : T_VFile;
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

   procedure Unhighlight_Line
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      File  : Virtual_File;
      Pos   : Natural;
      Style : String := "");
   --  Remove highlighting of line number Pos in file File

   --------------
   -- Add_Line --
   --------------

   function Add_Line
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      File   : Virtual_File;
      Pos    : Natural;
      Style  : String := "";
      Number : Natural := 1) return String
   is
      Args_Line : Argument_List :=
        (1 => new String'(Full_Name (File)),
         2 => new String'(Image (Pos)),
         3 => new String'(Image (Number)),
         4 => new String'(Style));
      Res : constant String :=  Execute_GPS_Shell_Command
        (Kernel, "add_blank_lines", Args_Line);

   begin
      Basic_Types.Free (Args_Line);
      return Res;
   end Add_Line;

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
         if VOffset (J) > 0 then
            Put_Button
              (Kernel, Info, Conflict, J,
               VRange, VFile, Move_On_Ref_File'Access);
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
            Trace (Me, "VRange (J).Blank_Lines" & Tmp.all);
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
            end if;
         else
            VRange (J).Mark := new String'
              (Mark_Diff_Block (Kernel, VFile (J),
                                VRange (J).First));

         end if;
         Free (Tmp);
      end loop;
   end Append;

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

   --------------------------
   --  Get_File_Last_Line  --
   --------------------------

   function Get_File_Last_Line
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      File : Virtual_File) return Natural
   is
      Args_Line : constant Argument_List :=
        (1 => new String'(Full_Name (File)));
      Res       : constant String :=  Execute_GPS_Shell_Command
        (Kernel, "get_last_line", Args_Line);
   begin
      return Natural'Value (Res);
   end Get_File_Last_Line;

   ---------------------
   -- Goto_Difference --
   ---------------------

   procedure Goto_Difference
     (Kernel : Kernel_Handle;
      Link : Diff_Chunk_Access)
   is
      Args : Argument_List (1 .. 1);
   begin
      if Link.Range1.Mark /= null then
         Trace (Me, "Execute Goto_Difference1 : " & Link.Range1.Mark.all);
         Args := (1 => new String'(Link.Range1.Mark.all));
         Execute_GPS_Shell_Command (Kernel, "goto_mark", Args);
         Basic_Types.Free (Args);
      end if;
      if Link.Range2.Mark /= null then
         Trace (Me, "Execute Goto_Difference2 : " & Link.Range2.Mark.all);
         Args := (1 => new String'(Link.Range2.Mark.all));
         Execute_GPS_Shell_Command (Kernel, "goto_mark", Args);
         Basic_Types.Free (Args);
      end if;
      if Link.Range3.Mark /= null then
         Args := (1 => new String'(Link.Range3.Mark.all));
         Execute_GPS_Shell_Command (Kernel, "goto_mark", Args);
         Basic_Types.Free (Args);
         Trace (Me, "Execute Goto_Difference3 : " & Link.Range3.Mark.all);
      end if;
   end Goto_Difference;

   ----------------------
   -- Hide_Differences --
   ----------------------

   procedure Hide_Differences
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      Item   : Diff_Head)
   is
      Curr_Node  : Diff_List_Node;
      Curr_Chunk : Diff_Chunk_Access;
      Args : Argument_List (1 .. 1);
   begin
      Curr_Node := First (Item.List);
      while Curr_Node /= Diff_Chunk_List.Null_Node
      loop
         Curr_Chunk := Data (Curr_Node);

         if Curr_Chunk.Range1.Blank_Lines /= null then
            Args (1) := Curr_Chunk.Range1.Blank_Lines;
            Execute_GPS_Shell_Command
              (Kernel, "remove_blank_lines", Args);
            Free (Curr_Chunk.Range1.Blank_Lines);
         end if;

         if Curr_Chunk.Range2.Blank_Lines /= null then
            Args (1) := Curr_Chunk.Range2.Blank_Lines;
            Execute_GPS_Shell_Command
              (Kernel, "remove_blank_lines", Args);
            Free (Curr_Chunk.Range2.Blank_Lines);
         end if;

         if Curr_Chunk.Range3.Blank_Lines /= null then
            Args (1) := Curr_Chunk.Range3.Blank_Lines;
            Execute_GPS_Shell_Command
              (Kernel, "remove_blank_lines", Args);
            Free (Curr_Chunk.Range3.Blank_Lines);
         end if;
         Curr_Node := Next (Curr_Node);
      end loop;

      if Item.File1 /= VFS.No_File then
         Unhighlight_Line (Kernel, Item.File1, 0, Default_Style);
         Unhighlight_Line (Kernel, Item.File1, 0, Old_Style);
         Unhighlight_Line (Kernel, Item.File1, 0, Append_Style);
         Unhighlight_Line (Kernel, Item.File1, 0, Remove_Style);
         Unhighlight_Line (Kernel, Item.File1, 0, Change_Style);
         Remove_Line_Information_Column
           (Kernel, Item.File1, Id_Col_Vdiff);
      end if;

      if Item.File2 /= VFS.No_File then
         Unhighlight_Line (Kernel, Item.File2, 0, Default_Style);
         Unhighlight_Line (Kernel, Item.File2, 0, Old_Style);
         Unhighlight_Line (Kernel, Item.File2, 0, Append_Style);
         Unhighlight_Line (Kernel, Item.File2, 0, Remove_Style);
         Unhighlight_Line (Kernel, Item.File2, 0, Change_Style);
         Remove_Line_Information_Column
           (Kernel, Item.File2, Id_Col_Vdiff);
      end if;

      if Item.File3 /= VFS.No_File then
         Unhighlight_Line (Kernel, Item.File3, 0, Default_Style);
         Unhighlight_Line (Kernel, Item.File3, 0, Old_Style);
         Unhighlight_Line (Kernel, Item.File3, 0, Append_Style);
         Unhighlight_Line (Kernel, Item.File3, 0, Remove_Style);
         Unhighlight_Line (Kernel, Item.File3, 0, Change_Style);
         Remove_Line_Information_Column
           (Kernel, Item.File3, Id_Col_Vdiff);
      end if;

   end Hide_Differences;

   --------------------
   -- Highlight_Line --
   --------------------

   procedure Highlight_Line
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      File  : Virtual_File;
      Pos   : Natural;
      Style : String := "";
      Number : Natural := 1)
   is
      Args_Highlight : Argument_List :=
        (1 => new String'(Full_Name (File)),
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

   ---------------------
   -- Mark_Diff_Block --
   ---------------------

   function Mark_Diff_Block
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      File  : Virtual_File;
      Pos   : Natural) return String
   is
      Args : Argument_List :=
        (1 => new String'(Full_Name (File)),
         2 => new String'(Image (Pos)),
         3 => new String'("1"));
      Res : constant String := Execute_GPS_Shell_Command
        (Kernel, "create_mark", Args);

   begin
      Basic_Types.Free (Args);
      return Res;
   end Mark_Diff_Block;

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

   ------------------
   --  Move_Block  --
   ------------------

   procedure Move_Block
     (Kernel       : Kernel_Handle;
      Source_File  : Virtual_File;
      Dest_File    : Virtual_File;
      Source_Range : Diff_Range;
      Dest_Range   : Diff_Range := Null_Range)
   is
      Offset_Dest       : constant Natural :=
        Dest_Range.Last - Dest_Range.First;
      Offset_Source     : constant Natural :=
        Source_Range.Last - Source_Range.First;
      Args_Get_Chars    : Argument_List :=
        (1 => new String'(Full_Name (Source_File)),
         2 => new String'(Natural'Image (Source_Range.First)),
         3 => new String'(Integer'Image (1)));
      Args_Replace_Text : Argument_List :=
        (1 => new String'(Full_Name (Dest_File)),
         2 => new String'(Natural'Image (Dest_Range.First)),
         3 => new String'(Integer'Image (1)),
         4 => null);
      Current_Line      : String_Access;

   begin
      if Offset_Source > 0 and Offset_Dest > 0 then
         for J in 1 .. Offset_Source loop
            Current_Line := new String'
              (Execute_GPS_Shell_Command
                 (Kernel, "get_chars", Args_Get_Chars));
            Args_Replace_Text (4) := Current_Line;
            Execute_GPS_Shell_Command
              (Kernel, "replace_text", Args_Replace_Text);
            Free (Args_Get_Chars (2));
            Free (Args_Replace_Text (2));
            Free (Args_Replace_Text (4));
            Args_Get_Chars (2) := new String'
              (Natural'Image (Source_Range.First + J));
            Args_Replace_Text (2) := new String'
              (Natural'Image (Dest_Range.First + J));
         end loop;

         if Offset_Source < Offset_Dest then
            Args_Replace_Text (4) := new String'("");
            for J in Offset_Source .. Offset_Dest loop
               Free (Args_Replace_Text (2));
               Args_Replace_Text (2) := new String'
                 (Natural'Image (Dest_Range.First + J));
               Execute_GPS_Shell_Command
                 (Kernel, "replace_text", Args_Replace_Text);
            end loop;
         end if;
      end if;

      Basic_Types.Free (Args_Get_Chars);
      Basic_Types.Free (Args_Replace_Text);
   end Move_Block;

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
      if Is_In_Diff_List
          (Item_Local.File1, Diff_List.all) = Diff_Head_List.Null_Node
        and then Is_In_Diff_List
          (Item_Local.File2, Diff_List.all) = Diff_Head_List.Null_Node
        and then Is_In_Diff_List
          (Item_Local.File3, Diff_List.all) = Diff_Head_List.Null_Node
      then
         Append (Diff_List.all, Item_Local);
         Show_Differences3 (Kernel, Item_Local);
         Goto_Difference (Kernel_Handle (Kernel),
                          Data (Item_Local.Current_Node));
      else
         Button := Message_Dialog
           (Msg     => -"One of these files is already used in VDiff.",
            Buttons => Button_OK,
            Parent  => Get_Main_Window (Kernel));
         return;
      end if;
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
      Action   : Handler_Action_Line := null)
   is
      Cmd : Diff_Command_Line_Access;
      Green_Button_Pixbuf : constant Gdk_Pixbuf :=
        Gdk_New_From_Xpm_Data (green_button_xpm);
      Red_Button_Pixbuf   : constant Gdk_Pixbuf :=
        Gdk_New_From_Xpm_Data (red_button_xpm);

   begin
      Create
        (Cmd, Kernel_Handle (Kernel),
         VDiff2_Module (Vdiff_Module_ID).List_Diff,
         VFile (Pos), VRange (Pos).First,
         Action);

      if not Conflict then
         Info (Pos)(VRange (Pos).First).Image := Green_Button_Pixbuf;
      else
         Info (Pos)(VRange (Pos).First).Image := Red_Button_Pixbuf;
      end if;

      Info (Pos)
        (VRange (Pos).First).Associated_Command := Command_Access (Cmd);
   end Put_Button;

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

   ----------------------
   -- Show_Differences --
   ----------------------
   procedure Show_Differences
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      Item   : in out Diff_Head)
   is
      List      : constant Diff_List := Item.List;
      Curr_Node  : Diff_List_Node := First (List);
      Curr_Chunk : Diff_Chunk_Access;
      Offset1   : Natural;
      Offset2   : Natural;
      Args_edit : Argument_List := (1 => new String'(Full_Name (Item.File1)));

   begin
      Trace (Me, "Show_Differences");
      Execute_GPS_Shell_Command (Kernel, "edit", Args_edit);
      Basic_Types.Free (Args_edit);
      Args_edit := (1 => new String'(Full_Name (Item.File2)));
      Execute_GPS_Shell_Command (Kernel, "edit", Args_edit);
      Basic_Types.Free (Args_edit);

      Create_Line_Information_Column
        (Kernel, Item.File1, Id_Col_Vdiff, True, True, True);
      Create_Line_Information_Column
        (Kernel, Item.File2, Id_Col_Vdiff, True, True, True);

      while Curr_Node /= Diff_Chunk_List.Null_Node loop
         Curr_Chunk := Data (Curr_Node);
         case Curr_Chunk.Range2.Action is
            when Append =>
               Curr_Chunk.Range1.Mark := new String'
                 (Add_Line
                    (Kernel, Item.File1,
                     Curr_Chunk.Range1.First, Append_Style,
                     Curr_Chunk.Range2.Last - Curr_Chunk.Range2.First));
               Curr_Chunk.Range1.Blank_Lines :=
                 new String'(Curr_Chunk.Range1.Mark.all);
               Highlight_Line
                 (Kernel, Item.File2, Curr_Chunk.Range2.First,
                  Old_Style,
                  Curr_Chunk.Range2.Last - Curr_Chunk.Range2.First);
               Curr_Chunk.Range2.Mark := new String'
                 (Mark_Diff_Block (Kernel, Item.File2,
                                   Curr_Chunk.Range2.First));

            when Change =>
               Offset1 := Curr_Chunk.Range1.Last - Curr_Chunk.Range1.First;
               Offset2 := Curr_Chunk.Range2.Last - Curr_Chunk.Range2.First;
               Highlight_Line (Kernel, Item.File1, Curr_Chunk.Range1.First,
                               Change_Style, Offset1);
               Highlight_Line (Kernel, Item.File2, Curr_Chunk.Range2.First,
                               Old_Style, Offset2);

               if Offset1 < Offset2 then
                  Curr_Chunk.Range2.Blank_Lines :=
                    new String'
                      (Add_Line (Kernel, Item.File1,
                            Curr_Chunk.Range1.Last, Change_Style,
                            Offset2 - Offset1));
               elsif Offset1 > Offset2 then
                  Curr_Chunk.Range2.Blank_Lines :=
                    new String'
                      (Add_Line (Kernel, Item.File2,
                                 Curr_Chunk.Range2.Last, Old_Style,
                                 Offset1 - Offset2));
               end if;

               Curr_Chunk.Range1.Mark := new String'
                 (Mark_Diff_Block (Kernel, Item.File1,
                                   Curr_Chunk.Range1.First));
               Curr_Chunk.Range2.Mark := new String'
                 (Mark_Diff_Block (Kernel, Item.File2,
                                   Curr_Chunk.Range2.First));

            when Delete =>
               Highlight_Line
                 (Kernel, Item.File1,
                  Curr_Chunk.Range1.First, Remove_Style,
                  Curr_Chunk.Range1.Last - Curr_Chunk.Range1.First);
               Curr_Chunk.Range2.Mark := new String'
                 (Add_Line (Kernel, Item.File2,
                            Curr_Chunk.Range2.First, Old_Style,
                            Curr_Chunk.Range1.Last - Curr_Chunk.Range1.First));
               Curr_Chunk.Range2.Blank_Lines :=
                 new String'(Curr_Chunk.Range2.Mark.all);
               Curr_Chunk.Range1.Mark := new String'
                 (Mark_Diff_Block (Kernel, Item.File1,
                                   Curr_Chunk.Range1.First));

            when others =>
               null;
         end case;

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
      Args_edit           : Argument_List
        := (1 => new String'(Full_Name (Item.File1)));
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
      Execute_GPS_Shell_Command (Kernel, "edit", Args_edit);
      Basic_Types.Free (Args_edit);

      Args_edit := (1 => new String'(Full_Name (Item.File2)));
      Execute_GPS_Shell_Command (Kernel, "edit", Args_edit);
      Basic_Types.Free (Args_edit);

      Args_edit := (1 => new String'(Full_Name (Item.File3)));
      Execute_GPS_Shell_Command (Kernel, "edit", Args_edit);
      Basic_Types.Free (Args_edit);

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
               VStyle (Ref) := new String'(Old_Style);
               VStyle (Other) := new String'(Change_Style);
               Append
                 (Kernel, VRange, VFile,
                  VOffset, VStyle, Ref, Other, Info, Curr_Chunk.Conflict);
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
                     Free (VStyle);
                     exit;

                  elsif VRange (J).Action = Delete then
                     VStyle (J)      := new String'(Remove_Style);
                     VStyle (Ref)    := new String'(Old_Style);
                     VStyle (Other2) := new String'(Change_Style);
                     Append (Kernel, VRange, VFile,
                             VOffset, VStyle, Ref,
                             J, Info, Curr_Chunk.Conflict);
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
   -- Unhighlight_Line --
   ----------------------

   procedure Unhighlight_Line
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      File  : Virtual_File;
      Pos   : Natural;
      Style : String := "")
   is
      Args_Highlight : Argument_List :=
        (1 => new String'(Full_Name (File)),
         2 => new String'(Style),
         3 => new String'(Image (Pos)));

   begin
      Execute_GPS_Shell_Command (Kernel, "unhighlight", Args_Highlight);
      Basic_Types.Free (Args_Highlight);
   end Unhighlight_Line;

   -------------------------
   --  Unhighlight_Block  --
   -------------------------

   procedure Unhighlight_Block
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      File   : Virtual_File;
      Range1 : in out Diff_Range;
      Style  : String := "")
   is
      Args : Argument_List (1 .. 1);
   begin
      for J in Range1.First .. Range1.Last loop
         Unhighlight_Line (Kernel, File, J, Style);
      end loop;
      if Range1.Blank_Lines /= null then
         Args (1) := Range1.Blank_Lines;
         Execute_GPS_Shell_Command
           (Kernel, "remove_blank_lines", Args);
      end if;
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
      Tmp_File  : VFS.Virtual_File := VFS.No_File) return Boolean is
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
         return false;
      end if;

      Item :=
        (List         => Result,
         File1        => New_File,
         File2        => Orig_File,
         File3        => VFS.No_File,
         Current_Node => First (Result),
         Ref_File     => 2,
         Tmp_File     => Tmp_File);
      Process_Differences (Id.Kernel, Item, Id.List_Diff);
      return true;
   end Visual_Patch;

end Vdiff2_Module.Utils;
