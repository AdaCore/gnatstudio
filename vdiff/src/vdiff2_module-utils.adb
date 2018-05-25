------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2003-2018, AdaCore                     --
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

with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;

with GNATCOLL.Arg_Lists;                use GNATCOLL.Arg_Lists;

with Gtk.Enums;                         use Gtk.Enums;
with Gtkada.Dialogs;                    use Gtkada.Dialogs;
with Gtkada.MDI;                        use Gtkada.MDI;

with Basic_Types;                       use Basic_Types;
with Commands;                          use Commands;
with GPS.Editors.Line_Information;      use GPS.Editors.Line_Information;
with GPS.Editors;                       use GPS.Editors;
with GPS.Intl;                          use GPS.Intl;
with GPS.Kernel.Hooks;                  use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;                    use GPS.Kernel.MDI;
with GPS.Kernel.Messages.Simple;        use GPS.Kernel.Messages.Simple;
with GPS.Kernel.Messages;               use GPS.Kernel.Messages;
with GPS.Kernel.Modules;                use GPS.Kernel.Modules;
with GPS.Kernel.Preferences;            use GPS.Kernel.Preferences;
with GPS.Kernel.Scripts;                use GPS.Kernel.Scripts;
with GPS.Kernel.Style_Manager;          use GPS.Kernel.Style_Manager;
with String_Utils;                      use String_Utils;
with Vdiff2_Command_Line;               use Vdiff2_Command_Line;
with Vdiff2_Module;                     use Vdiff2_Module;
with Vdiff2_Module.Utils.Shell_Command; use Vdiff2_Module.Utils.Shell_Command;
with Vdiff2_Module.Utils.Text;          use Vdiff2_Module.Utils.Text;

package body Vdiff2_Module.Utils is

   use Diff_Head_List;
   use Diff_Chunk_List;

   type T_VLine_Information is array (1 .. 3) of Line_Information_Data;

   GPS_Diff_Noconflict_Symbolic_String : constant Unbounded_String :=
     To_Unbounded_String ("gps-diff-noconflict-symbolic");
   GPS_Diff_Conflict_Symbolic_String   : constant Unbounded_String :=
     To_Unbounded_String ("gps-diff-conflict-symbolic");

   Minus_Sign_String : constant Unbounded_String := To_Unbounded_String ("-");
   Plus_Sign_String  : constant Unbounded_String := To_Unbounded_String ("+");

   procedure Append
     (Kernel   : access GPS.Kernel.Kernel_Handle_Record'Class;
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
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Info     : T_VLine_Information;
      Conflict : Boolean;
      Pos      : Natural;
      VRange   : T_VRange;
      VFile    : T_VFile;
      VStyle   : T_VStr;
      Action   : Handler_Action_Line := null);
   --  Put an icon on the column information

   procedure Show_Differences
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Item   : access Diff_Head);
   --  Show a result of diff Item

   procedure Show_Unified_Differences
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Item   : access Diff_Head);
   --  Show a result of diff Item as an Udiff

   procedure Show_Diff_Chunk
     (Kernel     : access GPS.Kernel.Kernel_Handle_Record'Class;
      Item       : access Diff_Head;
      Curr_Chunk : Diff_Chunk_Access;
      Info       : T_VLine_Information);
   --  Hightlight the Current Chunk Curr_Chunk

   ------------
   -- Append --
   ------------

   procedure Append
     (Kernel   : access GPS.Kernel.Kernel_Handle_Record'Class;
      VRange   : in out T_VRange;
      VFile    : T_VFile;
      VOffset  : T_VOffset;
      VStyle   : T_VStr;
      Ref      : T_Loc;
      Loc      : T_Loc;
      Info     : T_VLine_Information;
      Conflict : Boolean := False)
   is
      Other      : T_Loc := 0;
      Offset_Max : Natural := 0;

   begin
      for J in 1 .. 3 loop
         if J /= Ref and J /= Loc and Other = 0 then
            Other := J;
            if Loc /= 0 then
               exit;
            end if;
         end if;

         if J /= Other and Other /= 0 and J /= Ref then
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
            VRange (J).Blank_Lines_Mark.Replace_Element
              (Add_Line
                 (Kernel,
                  Buffer => GPS_Editor_Buffer
                    (Get_Buffer_Factory (Kernel).Get (File => VFile (J))),
                  Pos    => Editable_Line_Type (VRange (J).Last),
                  Style  => VStyle (J).all,
                  Number => Offset_Max - VOffset (J)));
         end if;
      end loop;

   exception
      when E : others => Trace (Me, E);
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
      Hor_List : Diff_List;
      First    : Natural := 0;
      Last     : Natural := 0;
   begin
      if Current_Line_Dest'Length = 0
        or else Current_Line_Source'Length = 0
      then
         return;
      end if;

      Hor_List :=
        Horizontal_Diff (Kernel, Current_Line_Dest, Current_Line_Source);

      for Diff of Hor_List loop
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
      end loop;
   end Fine_Highlight_Line;

   ---------------------
   -- Fine_Diff_Block --
   ---------------------

   procedure Fine_Diff_Block
     (Kernel       : Kernel_Handle;
      Source_File  : Virtual_File;
      Dest_File    : Virtual_File;
      Source_Range : Diff_Range;
      Dest_Range   : Diff_Range := Null_Range)
   is
      Offset_Dest         : constant Natural :=
                              Dest_Range.Last - Dest_Range.First;
      Offset_Source       : constant Natural :=
                              Source_Range.Last - Source_Range.First;
      Offset_Min          : Natural;
      Current_Line_Source : GNAT.Strings.String_Access;
      Current_Line_Dest   : GNAT.Strings.String_Access;
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
            GNAT.Strings.Free (Current_Line_Source);
            GNAT.Strings.Free (Current_Line_Dest);
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
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Item   : access Diff_Head)
   is

      procedure Remove_Blank_And_Special (D : in out Diff_Range);
      --  Remove blank and special lines associated with D.

      ------------------------------
      -- Remove_Blank_And_Special --
      ------------------------------

      procedure Remove_Blank_And_Special (D : in out Diff_Range) is
      begin
         if not D.Blank_Lines_Mark.Is_Empty then
            Remove_Blank_Lines (Kernel, D.Blank_Lines_Mark.Element);
            D.Blank_Lines_Mark.Clear;
         end if;

         if not D.Special_Lines_Mark.Is_Empty then
            declare
               Buffer : constant Editor_Buffer'Class :=
                 D.Special_Lines_Mark.Element.Location (False).Buffer;
            begin
               if Buffer in GPS_Editor_Buffer'Class then
                  Remove_Special_Lines
                    (GPS_Editor_Buffer'Class (Buffer),
                     D.Special_Lines_Mark.Element,
                     D.Last - D.First);
               end if;
            end;

            D.Special_Lines_Mark.Clear;
         end if;
      end Remove_Blank_And_Special;

   begin
      for Curr_Chunk of Item.List loop
         Remove_Blank_And_Special (Curr_Chunk.Range1);
         Remove_Blank_And_Special (Curr_Chunk.Range2);
         Remove_Blank_And_Special (Curr_Chunk.Range3);
      end loop;

      for J in T_VFile'Range loop
         if Item.Files (J) /= GNATCOLL.VFS.No_File then
            Unhighlight_Line (Kernel, Item.Files (J), 0, Default_Style);
            Unhighlight_Line (Kernel, Item.Files (J), 0, Old_Style);
            Unhighlight_Line (Kernel, Item.Files (J), 0, Append_Style);
            Unhighlight_Line (Kernel, Item.Files (J), 0, Remove_Style);
            Unhighlight_Line (Kernel, Item.Files (J), 0, Change_Style);
            Remove_Line_Information_Column
              (Kernel, Item.Files (J), Id_Col_Vdiff);
            Unhighlight_Range (Kernel, Item.Files (J), Fine_Change_Style);

            Get_Messages_Container (Kernel).Remove_File
              (-"Visual differences", Item.Files (J), Side_And_Locations);
         end if;
      end loop;
   end Hide_Differences;

   ----------------------
   -- Is_In_3Diff_List --
   ----------------------

   function Is_In_3Diff_List
     (Selected_File : GNATCOLL.VFS.Virtual_File;
      List          : Diff_Head_List.Vector) return Boolean
   is
      use Diff_Head_List.Std_Vectors;

      Node : constant Diff_Head_List.Std_Vectors.Cursor :=
               Get_Diff_Node (Selected_File, List);
   begin
      if not Has_Element (Node)
        or else Element (Node) = null
      then
         return False;
      end if;

      declare
         Diff : constant Diff_Head := Element (Node).all;
      begin
         for J in Diff.Files'Range loop
            if Diff.Files (J) = No_File then
               return False;
            end if;
         end loop;
      end;

      return True;
   end Is_In_3Diff_List;

   -------------------
   -- Get_Diff_Node --
   -------------------

   function Get_Diff_Node
     (Selected_File : Virtual_File;
      List          : Diff_Head_List.Vector)
      return Diff_Head_List.Std_Vectors.Cursor
   is
      use Diff_Head_List.Std_Vectors;

      Curr_Node : Diff_Head_List.Std_Vectors.Cursor := List.First;
      Diff      : Diff_Head;
   begin
      while Has_Element (Curr_Node) loop
         Diff := Element (Curr_Node).all;
         exit when Diff.Files (1) = Selected_File
           or else Diff.Files (2) = Selected_File
           or else Diff.Files (3) = Selected_File;
         Next (Curr_Node);
      end loop;

      return Curr_Node;
   end Get_Diff_Node;

   ---------------
   -- Move_Mark --
   ---------------

   procedure Move_Mark (Source, Dest : Diff_List) is
      use Diff_Chunk_List.Std_Vectors;

      Curr_Node_Source  : Diff_List_Node := Source.First;
      Curr_Chunk_Source : Diff_Chunk_Access;
      Curr_Node_Dest    : Diff_List_Node := Dest.First;
      Curr_Chunk_Dest   : Diff_Chunk_Access;

   begin
      while Has_Element (Curr_Node_Source)
        and then Has_Element (Curr_Node_Dest)
      loop
         Curr_Chunk_Source := Element (Curr_Node_Source);
         Curr_Chunk_Dest   := Element (Curr_Node_Dest);

         Curr_Chunk_Dest.Range1.Blank_Lines_Mark :=
           Curr_Chunk_Source.Range1.Blank_Lines_Mark;

         Curr_Chunk_Dest.Range2.Blank_Lines_Mark :=
           Curr_Chunk_Source.Range2.Blank_Lines_Mark;

         Curr_Chunk_Dest.Range3.Blank_Lines_Mark :=
           Curr_Chunk_Source.Range3.Blank_Lines_Mark;

         Next (Curr_Node_Source);
         Next (Curr_Node_Dest);
      end loop;
   end Move_Mark;

   -------------------------
   -- Process_Differences --
   -------------------------

   function Process_Differences
     (Kernel    : access GPS.Kernel.Kernel_Handle_Record'Class;
      Item      : Diff_Head;
      Diff_List : Diff_Head_List_Access) return Diff_Head_Access
   is
      use Diff_Head_List.Std_Vectors;

      Item_Access : Diff_Head_Access;
   begin
      if not Has_Element (Get_Diff_Node (Item.Files (1), Diff_List.all))
        and then not Has_Element
          (Get_Diff_Node (Item.Files (2), Diff_List.all))
      then
         if Item.Files (3) = GNATCOLL.VFS.No_File
           or else not Has_Element
             (Get_Diff_Node (Item.Files (3), Diff_List.all))
         then
            Item_Access := new Diff_Head'(Item);
            Diff_Head_List.Append (Diff_List.all, Item_Access);
            Show_Differences3 (Kernel, Item_Access);
            return Item_Access;
         end if;
      end if;

      Kernel.Insert
        (-"One of these files is already used in VDiff",
         Mode => Info);

      return null;
   end Process_Differences;

   ----------------
   -- Put_Button --
   ----------------

   procedure Put_Button
     (Kernel   : access GPS.Kernel.Kernel_Handle_Record'Class;
      Info     : T_VLine_Information;
      Conflict : Boolean;
      Pos      : Natural;
      VRange   : T_VRange;
      VFile    : T_VFile;
      VStyle   : T_VStr;
      Action   : Handler_Action_Line := null)
   is
      Cmd  : Diff_Command_Line_Access;
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
            Info (Pos)(Line).Image := GPS_Diff_Noconflict_Symbolic_String;

         else
            Info (Pos)(Line).Image := GPS_Diff_Conflict_Symbolic_String;
         end if;

         Info (Pos)(Line).Associated_Command := Command_Access (Cmd);
      end if;
   end Put_Button;

   ----------------------
   -- Show_Differences --
   ----------------------

   procedure Show_Differences
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Item   : access Diff_Head)
   is
      Block : constant Block_Trace_Handle := Create (Me, "Show differences");

      List       : constant Diff_List := Item.List;
      Offset1    : Natural;
      Offset2    : Natural;
      VStyle     : T_VStr;
      Ref        : T_Loc := Item.Ref_File;
      Other      : T_Loc := 2;

      Highlight_File : Virtual_File;
      Modification   : String (1 .. 8);
      The_Range      : Natural := 0;
      Line           : Integer;
      Split_MDI      : Boolean;

      procedure Add_Side_Symbol
        (File       : Virtual_File;
         Line_Start : Natural;
         Line_End   : Natural;
         Symbol     : String);
      --  Add the specified symbol to the side of lines Line_Start to Line_End
      --  in editors for File.

      function Is_Ref_Editor_Opened return Boolean;
      --  Return True if the reference editor is already opened. In this case
      --  we do not need to split the MDI, the reference editor should be
      --  reused where it is currently placed.

      ---------------------
      -- Add_Side_Symbol --
      ---------------------

      procedure Add_Side_Symbol
        (File       : Virtual_File;
         Line_Start : Natural;
         Line_End   : Natural;
         Symbol     : String)
      is
         Infos : Line_Information_Array (Line_Start .. Line_End);
      begin
         for J in Infos'Range loop
            Infos (J).Text := To_Unbounded_String (Symbol);
         end loop;

         Add_Line_Information
           (Kernel,
            File       => File,
            Identifier => Id_Col_Vdiff,
            Info       => Infos);
      end Add_Side_Symbol;

      --------------------------
      -- Is_Ref_Editor_Opened --
      --------------------------

      function Is_Ref_Editor_Opened return Boolean is
         Filename : constant String := +Base_Name (Item.Files (Ref));
         CL       : Arg_List := Create ("MDI.get");
      begin
         Append_Argument (CL, Filename, One_Arg);
         return Execute_GPS_Shell_Command (Kernel, CL) /= "null";
      end Is_Ref_Editor_Opened;

      use Diff_Chunk_List.Std_Vectors;

   begin
      if Ref = 1 then
         Other := 2;
      elsif Ref = 2 then
         Other := 1;
      else
         Ref := 1;
      end if;

      Split_MDI := not Is_Ref_Editor_Opened;

      if Other = 1 then
         Highlight_File := Item.Files (1);
      else
         Highlight_File := Item.Files (2);
      end if;

      --  Keep the current window configuration, except we split the current
      --  editor. This doesn't lose the user's current setup, and will be
      --  superceded by the use of MDI_Child groups

      --  Note: we need to open the "real" editor first, so that it will not
      --  split into its-own column. This way, closing vdiffs won't keep "real"
      --  editor in splitted state.

      if Ref = 1 then
         Edit (Kernel, Item.Files (2));
         Edit (Kernel, Item.Files (1));
      else
         Edit (Kernel, Item.Files (1));
         Edit (Kernel, Item.Files (2));
      end if;

      --  Synchronize the scrollings

      Synchronize_Scrolling (Kernel, Item.Files (1), Item.Files (2));

      if Split_MDI then
         Split
           (Get_MDI (Kernel), Orientation_Horizontal, Mode => Before_Reuse);
      end if;

      Create_Line_Information_Column
        (Kernel,
         File       => Item.Files (1),
         Identifier => Id_Col_Vdiff,
         Every_Line => True);
      Create_Line_Information_Column
        (Kernel,
         File       => Item.Files (2),
         Identifier => Id_Col_Vdiff,
         Every_Line => True);

      for Curr_Chunk of List loop
         case Curr_Chunk.Range2.Action is
            when Append =>
               Modification := "appended";
               The_Range := Curr_Chunk.Range2.Last - Curr_Chunk.Range2.First;

               VStyle (Other) := new String'(Append_Style);
               VStyle (Ref)   := new String'(Old_Style);

               Curr_Chunk.Range1.Blank_Lines_Mark.Replace_Element
                 (Add_Line
                    (Kernel,
                     Buffer => GPS_Editor_Buffer
                       (Get_Buffer_Factory (Kernel).Get (Item.Files (1))),
                     Pos    => Editable_Line_Type (Curr_Chunk.Range1.First),
                     Style  => VStyle (Ref).all,
                     Number => The_Range));

               Highlight_Line
                 (Kernel, Item.Files (2), Curr_Chunk.Range2.First,
                  VStyle (Other).all,
                  The_Range);

               Add_Side_Symbol
                 (Item.Files (2),
                  Curr_Chunk.Range2.First, Curr_Chunk.Range2.Last - 1,
                  "+");

            when Change =>
               Modification := "modified";
               The_Range := Curr_Chunk.Range2.Last - Curr_Chunk.Range2.First;

               VStyle (Other) := new String'(Change_Style);
               VStyle (Ref)   := new String'(Old_Style);
               Offset1 := Curr_Chunk.Range1.Last - Curr_Chunk.Range1.First;
               Offset2 := Curr_Chunk.Range2.Last - Curr_Chunk.Range2.First;
               Highlight_Line (Kernel, Item.Files (1), Curr_Chunk.Range1.First,
                               VStyle (Ref).all, Offset1);
               Highlight_Line (Kernel, Item.Files (2), Curr_Chunk.Range2.First,
                               VStyle (Other).all, Offset2);

               Add_Side_Symbol
                 (Item.Files (2),
                  Curr_Chunk.Range2.First, Curr_Chunk.Range2.Last - 1,
                  "!");
               Add_Side_Symbol
                 (Item.Files (1),
                  Curr_Chunk.Range1.First, Curr_Chunk.Range1.Last - 1,
                  "!");

               if Offset1 < Offset2 then
                  Curr_Chunk.Range2.Blank_Lines_Mark.Replace_Element
                    (Add_Line
                       (Kernel,
                        GPS_Editor_Buffer
                          (Get_Buffer_Factory (Kernel).Get (Item.Files (1))),
                        Pos    => Editable_Line_Type (Curr_Chunk.Range1.Last),
                        Style  => VStyle (Ref).all,
                        Number => Offset2 - Offset1));
               elsif Offset1 > Offset2 then
                  Curr_Chunk.Range2.Blank_Lines_Mark.Replace_Element
                    (Add_Line
                       (Kernel,
                        GPS_Editor_Buffer
                          (Get_Buffer_Factory (Kernel).Get (Item.Files (2))),
                        Pos    => Editable_Line_Type (Curr_Chunk.Range2.Last),
                        Style  => VStyle (Other).all,
                        Number => Offset1 - Offset2));
               end if;

               Fine_Diff_Block
                 (Kernel_Handle (Kernel),
                  Item.Files (1),
                  Item.Files (2),
                  Curr_Chunk.Range1,
                  Curr_Chunk.Range2);

            when Delete =>
               Modification := "removed ";
               The_Range := Curr_Chunk.Range1.Last - Curr_Chunk.Range1.First;

               VStyle (Other) := new String'(Remove_Style);
               VStyle (Ref)   := new String'(Old_Style);
               Highlight_Line
                 (Kernel, Item.Files (1),
                  Curr_Chunk.Range1.First, VStyle (Ref).all,
                  Curr_Chunk.Range1.Last - Curr_Chunk.Range1.First);
               Curr_Chunk.Range2.Blank_Lines_Mark.Replace_Element
                 (Add_Line
                    (Kernel,
                     GPS_Editor_Buffer
                       (Get_Buffer_Factory (Kernel).Get (Item.Files (2))),
                     Pos    => Editable_Line_Type (Curr_Chunk.Range2.First),
                     Style  => VStyle (Other).all,
                     Number =>
                        Curr_Chunk.Range1.Last - Curr_Chunk.Range1.First));

               Add_Side_Symbol
                 (Item.Files (1),
                  Curr_Chunk.Range1.First, Curr_Chunk.Range1.Last - 1,
                  "-");

            when others =>
               null;
         end case;

         --  Insert the diff chunk in the Locations View

         if Other = 1 then
            Line := Curr_Chunk.Range1.First;
         else
            Line := Curr_Chunk.Range2.First;
         end if;

         if The_Range = 1 then
            Create_Simple_Message
              (Get_Messages_Container (Kernel),
               -"Visual differences",
               Highlight_File,
               Line,
               1,
               "1 line " & Modification,
               Unspecified,
               Side_And_Locations);

         else
            Create_Simple_Message
              (Get_Messages_Container (Kernel),
               -"Visual differences",
               Highlight_File,
               Line,
               1,
               Image (The_Range) & " lines " & Modification,
               Unspecified,
               Side_And_Locations);
         end if;

         Free (VStyle);
      end loop;
   end Show_Differences;

   ------------------------------
   -- Show_Unified_Differences --
   ------------------------------

   procedure Show_Unified_Differences
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Item   : access Diff_Head)
   is
      use Ada.Strings.Unbounded;
      use Diff_Chunk_List.Std_Vectors;

      Ref        : constant T_Loc := Item.Ref_File;
      File       : constant Virtual_File := Item.Files (3 - Ref);
      List       : constant Diff_List := Item.List;
      Curr_Chunk : Diff_Chunk_Access;
      Buf        : constant GPS_Editor_Buffer'Class :=
        GPS_Editor_Buffer'Class
          (Get (Get_Buffer_Factory (Kernel).all, File,
           Open_Buffer => True, Open_View => True));
      Refbuf     : constant Editor_Buffer'Class := Get
        (Get_Buffer_Factory (Kernel).all, Item.Files (Ref),
         Open_Buffer => True, Open_View => False);

      function Get_Line
        (Buf : Editor_Buffer'Class; Line : Integer) return String;
      --  Return the contents of Line.
      procedure Show_Deleted;
      procedure Show_Added;
      --  Display the deleted/added lines in the current chunk.

      --------------
      -- Get_Line --
      --------------

      function Get_Line
        (Buf : Editor_Buffer'Class; Line : Integer) return String
      is
         Start : constant Editor_Location'Class :=
           Buf.New_Location_At_Line (Line);
         The_End : constant Editor_Location'Class := Start.End_Of_Line;
      begin
         return Buf.Get_Chars (Start, The_End);
      end Get_Line;

      ------------------
      -- Show_Deleted --
      ------------------

      procedure Show_Deleted
      is
         Original : Unbounded_String;
         Arr      : Line_Information_Data;
         Manager  : constant Style_Manager_Access := Get_Style_Manager
           (Kernel_Handle (Kernel));
      begin
         Arr := new Line_Information_Array
           (Curr_Chunk.Range1.First .. Curr_Chunk.Range1.Last - 1);

         for L in Curr_Chunk.Range1.First ..
           Curr_Chunk.Range1.Last - 1
         loop
            Arr (L).Text := Minus_Sign_String;
            Append (Original, Get_Line (Refbuf, L));
         end loop;

         declare
            --  Strip last ASCII.LF in Original
            O : constant String := To_String (Original);
         begin
            Curr_Chunk.Range1.Special_Lines_Mark.Replace_Element
              (Buf.Add_Special_Line
                 (Start_Line => Curr_Chunk.Range2.First,
                  Text       => O (O'First .. O'Last - 1),
                  Style      => Manager.Get (Remove_Style),
                  Name       => "",
                  Column_Id  => Id_Col_Vdiff,
                  Info       => Arr));
         end;

         Create_Simple_Message
           (Get_Messages_Container (Kernel),
            -"Visual differences",
            File,
            Natural'Max (Curr_Chunk.Range2.First - 1, 1),
            1,
            (if Arr'Length = 1
             then "1 line removed"
             else Image (Arr'Length) & " lines removed"),
            Informational,
            Side_And_Locations);

         Unchecked_Free (Arr);
      end Show_Deleted;

      ----------------
      -- Show_Added --
      ----------------

      procedure Show_Added is
         Arr : Line_Information_Data;
      begin
         Arr := new Line_Information_Array
           (Curr_Chunk.Range2.First .. Curr_Chunk.Range2.Last - 1);

         for L in Arr'Range loop
            Arr (L).Text := Plus_Sign_String;
         end loop;

         Highlight_Line
           (Kernel, Item.Files (2),
            Curr_Chunk.Range2.First,
            Append_Style,
            Curr_Chunk.Range2.Last - Curr_Chunk.Range2.First);

         Buf.Add_File_Information (Id_Col_Vdiff, Arr);

         Create_Simple_Message
           (Get_Messages_Container (Kernel),
            -"Visual differences",
            File,
            Curr_Chunk.Range2.First,
            1,
            (if Arr'Length = 1
             then "1 line added"
             else Image (Arr'Length) & " lines added"),
            Informational,
            Side_And_Locations);

         Unchecked_Free (Arr);
      end Show_Added;

   begin
      Create_Line_Information_Column
        (Kernel, File => Item.Files (3 - Ref), Identifier => Id_Col_Vdiff,
         Every_Line => True);

      for Item of List loop
         Curr_Chunk := Item;

         case Curr_Chunk.Range2.Action is
            when Append =>
               Show_Added;

            when Change =>
               Show_Deleted;
               Show_Added;

            when Delete =>
               Show_Deleted;

            when Nothing => null;
         end case;
      end loop;
   end Show_Unified_Differences;

   -----------------------
   -- Show_Differences3 --
   -----------------------

   procedure Show_Differences3
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Item   : access Diff_Head)
   is
      Block : constant Block_Trace_Handle := Create (Me, "Show differences3");
      Res  : Diff_List;
      Info : T_VLine_Information;

   begin
      Register_Highlighting (Kernel);

      if Item.Files (3) = GNATCOLL.VFS.No_File then
         if Item.Mode = Unified then
            Show_Unified_Differences (Kernel, Item);
         else
            Show_Differences (Kernel, Item);
         end if;

         return;
      end if;

      --  Keep the current window configuration, except we split the current
      --  editor. This doesn't lose the user's current setup, and will be
      --  superceded by the use of MDI_Child groups

      Edit (Kernel, Item.Files (2));
      Edit (Kernel, Item.Files (1));

      Split (Get_MDI (Kernel), Orientation_Horizontal, Mode => Before_Reuse);
      Edit (Kernel, Item.Files (3));
      Split (Get_MDI (Kernel), Orientation_Horizontal, Mode => Before_Reuse);

      --  Synchronize the scrollings

      Synchronize_Scrolling
        (Kernel, Item.Files (1), Item.Files (2), Item.Files (3));

      Info (1) := new Line_Information_Array
        (1 .. Get_File_Last_Line (Kernel, Item.Files (1)));
      Info (2) := new Line_Information_Array
        (1 .. Get_File_Last_Line (Kernel, Item.Files (2)));
      Info (3) := new Line_Information_Array
        (1 .. Get_File_Last_Line (Kernel, Item.Files (3)));

      Create_Line_Information_Column
         (Kernel, Item.Files (1), Id_Col_Vdiff, Every_Line => True);
      Create_Line_Information_Column
         (Kernel, Item.Files (2), Id_Col_Vdiff, Every_Line => True);
      Create_Line_Information_Column
         (Kernel, Item.Files (3), Id_Col_Vdiff, Every_Line => True);

      Res := Simplify (Item.List, Item.Ref_File);

      for Curr_Chunk of Res loop
         Show_Diff_Chunk (Kernel, Item, Curr_Chunk, Info);
      end loop;

      Add_Line_Information
        (Kernel, Item.Files (1), Id_Col_Vdiff, Info => Info (1).all);
      Add_Line_Information
        (Kernel, Item.Files (2), Id_Col_Vdiff, Info => Info (2).all);
      Add_Line_Information
        (Kernel, Item.Files (3), Id_Col_Vdiff, Info => Info (3).all);
      Move_Mark (Res, Item.List);
      Res.Clear;

      Unchecked_Free (Info (1));
      Unchecked_Free (Info (2));
      Unchecked_Free (Info (3));
   end Show_Differences3;

   ---------------------
   -- Show_Diff_Chunk --
   ---------------------

   procedure Show_Diff_Chunk
     (Kernel     : access GPS.Kernel.Kernel_Handle_Record'Class;
      Item       : access Diff_Head;
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
                  (Item.Files (1), Item.Files (2), Item.Files (3));
      VOffset : constant T_VOffset :=
                  ((Curr_Chunk.Range1.Last - Curr_Chunk.Range1.First),
                   (Curr_Chunk.Range2.Last - Curr_Chunk.Range2.First),
                   (Curr_Chunk.Range3.Last - Curr_Chunk.Range3.First));
      VStyle  : T_VStr;

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
              and VRange (Other2).Action = Change
            then
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
                     if K /= Ref and K /= J then
                        Other2 := K;
                     end if;
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
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Merge  : Virtual_File)
   is
      Button : Message_Dialog_Buttons;

   begin
      if Is_Regular_File (Merge) then
         Button := Message_Dialog
           (Msg     => -"Would you overwrite this file: " &
                          Merge.Display_Full_Name,
            Buttons => Button_Yes or Button_No,
            Parent  => Get_Current_Window (Kernel));

         if Button = Button_No then
            return;
         end if;
      end if;

      Edit (Kernel, Merge);
   end Show_Merge;

   -----------------------
   -- Unhighlight_Block --
   -----------------------

   procedure Unhighlight_Block
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      File   : Virtual_File;
      Range1 : in out Diff_Range;
      Style  : String := "") is
   begin
      for J in Range1.First .. Range1.Last loop
         Unhighlight_Line (Kernel, File, J, Style);
      end loop;

      Remove_Blank_Lines (Kernel, Range1.Blank_Lines_Mark.Element);
   end Unhighlight_Block;

   -----------------
   -- Visual_Diff --
   -----------------

   function Visual_Diff
     (Mode  : GPS.Kernel.Preferences.Vdiff_Modes;
      File1 : Virtual_File;
      File2 : Virtual_File;
      File3 : Virtual_File := GNATCOLL.VFS.No_File) return Diff_Head_Access
   is
      Id     : constant VDiff2_Module := VDiff2_Module (Vdiff_Module_ID);
      Kernel : constant Kernel_Handle := Get_Kernel (Id.all);
      Result : Diff_List;
      Item   : Diff_Head;

   begin
      if File3 /= GNATCOLL.VFS.No_File then
         Result := Diff3 (Kernel, File1, File2, File3);
      else
         Result := Diff (Kernel, File1, File2);
      end if;

      if Result.Is_Empty then
         Kernel.Insert (-"No differences found.");
         return null;
      end if;

      Item :=
        (Mode           => Mode,
         List           => Result,
         Files          => (File1, File2, File3),
         Current_Node   => First (Result),
         Ref_File       => 2,
         In_Destruction => False,
         Instances      => <>);

      return Process_Differences (Kernel, Item, Id.List_Diff);
   end Visual_Diff;

   -----------------
   -- Visual_Diff --
   -----------------

   procedure Visual_Diff
     (Mode  : GPS.Kernel.Preferences.Vdiff_Modes;
      File1 : Virtual_File;
      File2 : Virtual_File;
      File3 : Virtual_File := GNATCOLL.VFS.No_File)
   is
      Dummy : constant Diff_Head_Access :=
        Visual_Diff (Mode, File1, File2, File3);
      pragma Unreferenced (Dummy);
   begin
      null;
   end Visual_Diff;

   ------------------
   -- Visual_Patch --
   ------------------

   function Visual_Patch
     (Mode      : GPS.Kernel.Preferences.Vdiff_Modes;
      Orig_File : GNATCOLL.VFS.Virtual_File;
      New_File  : GNATCOLL.VFS.Virtual_File;
      Diff_File : GNATCOLL.VFS.Virtual_File;
      Revert    : Boolean := False) return Diff_Head_Access
   is
      Id     : constant VDiff2_Module := VDiff2_Module (Vdiff_Module_ID);
      Kernel : constant Kernel_Handle := Get_Kernel (Id.all);
      Result : Diff_List;
      Item   : Diff_Head;

   begin
      Result := Diff (Kernel, Orig_File, New_File, Diff_File, Revert);

      if Result.Is_Empty then
         Kernel.Insert (-"No differences found.");
         return null;
      end if;

      Item :=
        (Mode           => Mode,
         List           => Result,
         Files          => (Orig_File, New_File, No_File),
         Current_Node   => First (Result),
         Ref_File       => 1,
         In_Destruction => False,
         Instances      => <>);

      return Process_Differences (Kernel, Item, Id.List_Diff);
   end Visual_Patch;

   ---------------
   -- Get_Vdiff --
   ---------------

   function Get_Vdiff
     (File1 : Virtual_File;
      File2 : Virtual_File := GNATCOLL.VFS.No_File;
      File3 : Virtual_File := GNATCOLL.VFS.No_File) return Diff_Head_Access
   is
      use Diff_Head_List.Std_Vectors;

      Vdiff_List : constant Diff_Head_List_Access := Get_Vdiff_List;
      Vdiff_Node : Diff_Head_List.Std_Vectors.Cursor;
      Vdiff      : Diff_Head_Access;
   begin
      if Vdiff_List = null then
         return null;
      end if;

      Vdiff_Node := Get_Diff_Node (File1, Vdiff_List.all);

      if not Has_Element (Vdiff_Node) then
         return null;
      end if;

      Vdiff := Element (Vdiff_Node);

      if (File2 /= GNATCOLL.VFS.No_File
          and then File2 /= Vdiff.Files (1)
          and then File2 /= Vdiff.Files (2)
          and then File2 /= Vdiff.Files (3))
        or else (File3 /= GNATCOLL.VFS.No_File
          and then File3 /= Vdiff.Files (1)
          and then File3 /= Vdiff.Files (2)
          and then File3 /= Vdiff.Files (3))
      then
         return null;
      end if;

      return Vdiff;
   end Get_Vdiff;

   --------------------
   -- Get_Vdiff_List --
   --------------------

   function Get_Vdiff_List return Diff_Head_List_Access is
   begin
      return VDiff2_Module (Vdiff_Module_ID).List_Diff;
   end Get_Vdiff_List;

end Vdiff2_Module.Utils;
