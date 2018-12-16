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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with GNATCOLL.Projects;     use GNATCOLL.Projects;
with GNAT.Strings;          use GNAT.Strings;
with Gtk.Text_Buffer;       use Gtk.Text_Buffer;
with Interfaces.C;

with Language.Abstract_Language_Tree; use Language.Abstract_Language_Tree;
with Src_Editor_Box;                  use Src_Editor_Box;
with Src_Editor_Module;               use Src_Editor_Module;
with Src_Editor_View;                 use Src_Editor_View;
with Src_Editor_Buffer.Line_Information;
use  Src_Editor_Buffer.Line_Information;

package body Commands.Editor is

   function g_utf8_strlen
     (P : String; Max : Interfaces.C.size_t) return Long_Integer;
   pragma Import (C, g_utf8_strlen);
   --  Return the text size of an UTF8 string

   function Avoid_Move_Cursor
     (Command : access Editor_Command_Type) return Boolean;
   --  Predicate that returns wether the cursor should be moved
   --  for the given command.

   ------------
   -- Create --
   ------------

   procedure Create
     (Item   : out Check_Modified_State;
      Buffer : Source_Buffer) is
   begin
      Item := new Check_Modified_State_Type;
      Item.Buffer := Buffer;
   end Create;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Check_Modified_State_Type) return Command_Return_Type is
   begin
      Set_Last_Status (Command.Buffer, Get_Status (Command.Buffer));
      return Success;
   end Execute;

   --------------------
   -- Primitive_Free --
   --------------------

   overriding procedure Primitive_Free (X : in out Editor_Command_Type) is
   begin
      Free (X.Current_Text);
   end Primitive_Free;

   overriding procedure Primitive_Free
     (X : in out Editor_Replace_Slice_Type) is
   begin
      Free (X.Text_Before);
      Free (X.Text_After);
   end Primitive_Free;

   overriding procedure Primitive_Free
     (X : in out Remove_Blank_Lines_Command_Type) is
   begin
      if not Get_Deleted (X.Mark) then
         Delete_Mark (X.Buffer, X.Mark);
      end if;
   end Primitive_Free;

   ---------------------
   -- Is_Null_Command --
   ---------------------

   function Is_Null_Command (Command : Editor_Command) return Boolean is
   begin
      return (Command = null or else (Command.Current_Text_Size = 0));
   end Is_Null_Command;

   -------------------
   -- Get_Direction --
   -------------------

   function Get_Direction (Command : Editor_Command) return Direction_Type is
   begin
      return Command.Direction;
   end Get_Direction;

   --------------
   -- Get_Mode --
   --------------

   function Get_Mode (Command : Editor_Command) return Editor_Command_Mode is
   begin
      return Command.Edition_Mode;
   end Get_Mode;

   ------------
   -- Create --
   ------------

   procedure Create
     (Item                : out Editor_Command;
      Mode                : Editor_Command_Mode;
      Buffer              : Source_Buffer;
      User_Executed       : Boolean;
      Cursor_Loc, Sel_Loc : Loc_T;
      End_Loc             : Loc_T := (0, 0);
      Direction           : Direction_Type := Forward;
      C                   : Cursor := Nil_Cursor) is
   begin
      Item := new Editor_Command_Type;
      Item.Buffer := Buffer;
      Item.Current_Text := new String (1 .. 512);
      Item.Edition_Mode := Mode;
      Item.User_Executed := User_Executed;
      Item.Locs.Start_Loc := Cursor_Loc;
      Item.Direction := Direction;
      Item.Locs.Start_Sel_Loc := Sel_Loc;
      Item.Locs.End_Loc := End_Loc;
      Item.Locs.End_Sel_Loc := End_Loc;
      Item.Linked_Cursor := Holder (C);
   end Create;

   -----------------------
   -- Avoid_Move_Cursor --
   -----------------------

   function Avoid_Move_Cursor
     (Command : access Editor_Command_Type) return Boolean
   is
   begin
      return Command.Buffer.Avoid_Cursor_Move_On_Changes;
   end Avoid_Move_Cursor;

   --------------
   -- Get_Text --
   --------------

   function Get_Text (Item : Editor_Command) return Basic_Types.UTF8_String is
   begin
      return Item.Current_Text (1 .. Item.Current_Text_Size);
   end Get_Text;

   --------------
   -- Set_Text --
   --------------

   procedure Set_Text
     (Item : Editor_Command;
      Text : Basic_Types.UTF8_String) is
   begin
      while Text'Length > Item.Current_Text_Total_Length loop
         Item.Current_Text_Total_Length := Item.Current_Text_Total_Length * 2;
      end loop;

      Item.Current_Text_Size := Text'Length;

      declare
         New_Current_Text : String (1 .. Item.Current_Text_Total_Length);
      begin
         New_Current_Text (1 .. Item.Current_Text_Size) := Text;
         Free (Item.Current_Text);
         Item.Current_Text := new String'(New_Current_Text);
      end;
   end Set_Text;

   --------------
   -- Add_Text --
   --------------

   procedure Add_Text
     (Item         : Editor_Command;
      UTF8         : Basic_Types.UTF8_String;
      Start_Line   : Editable_Line_Type := 0;
      Start_Column : Character_Offset_Type := 0)
   is
      Text_Length : constant Integer := UTF8'Length;
      First       : Natural := Item.Current_Text'First;
   begin
      while Item.Current_Text_Size + Text_Length
        > Item.Current_Text_Total_Length
      loop
         Item.Current_Text_Total_Length := Item.Current_Text_Total_Length * 2;
      end loop;

      if Item.Current_Text_Total_Length > Item.Current_Text'Length then
         declare
            New_Current_Text : String (1 .. Item.Current_Text_Total_Length);
         begin
            New_Current_Text (1 .. Item.Current_Text_Size) :=
              Item.Current_Text (First .. First + Item.Current_Text_Size - 1);
            Free (Item.Current_Text);
            Item.Current_Text := new String'(New_Current_Text);
            First := Item.Current_Text'First;
         end;
      end if;

      if Item.Edition_Mode = Insertion then
         Item.Current_Text
           (First + Item.Current_Text_Size
              .. First + Item.Current_Text_Size + Text_Length - 1) := UTF8;

      else
         case Item.Direction is
            when Forward | Extended =>
               if Item.Current_Text_Size > 0 then
                  for J in reverse 0 ..  Item.Current_Text_Size - 1 loop
                     Item.Current_Text (First + Text_Length + J)
                       := Item.Current_Text (First + J);
                  end loop;
               end if;

               Item.Current_Text (First .. First + Text_Length - 1) := UTF8;

            when Backward =>
               Item.Current_Text
                 (First + Item.Current_Text_Size
                    .. First + Item.Current_Text_Size
                      + Text_Length - 1) := UTF8;
         end case;
      end if;

      Item.Current_Text_Size := Item.Current_Text_Size + Text_Length;

      if Item.Edition_Mode = Insertion then
         if Start_Line /= 0 then
            Item.Locs.Start_Loc.Line := Start_Line;
         end if;

         if Start_Column /= 0 then
            Item.Locs.Start_Loc.Col := Start_Column;
         end if;
      end if;
   end Add_Text;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Editor_Command_Type) return Command_Return_Type
   is
      First  : constant Natural := Command.Current_Text'First;
      C      : constant Cursor := Command.Linked_Cursor.Element;
      MC_Sync_Save : Cursors_Sync_Type;

      procedure Set_Cursor_Position
        (Loc, Sel_Loc : Loc_T; View : Source_View);
      --  Set the action's cursor at the right place whether it is a multi
      --  cursor or the main cursor

      procedure Set_Cursor_Position
        (Loc, Sel_Loc : Loc_T; View : Source_View)
      is
         Iter : Gtk_Text_Iter;
         Mark : Gtk_Text_Mark;
         Sync : constant Cursors_Sync_Type
           := Get_Cursors_Sync (Command.Buffer);
      begin

         --  The cursor is a multi cursor
         if not C.Is_Main_Cursor then
            if Is_Alive (C) then
               Mark := Get_Mark (C);

               if Mark /= null
                 and then Loc.Line /= 0
               then
                  Command.Buffer.Get_Iter_At_Screen_Position
                    (Iter, Loc.Line, Loc.Col);

                  Command.Buffer.Move_Mark (Mark, Iter);

                  --  Move the selection mark if we have meaningful values
                  --  for End_Sel_Line and End_Sel_Col. The zero check is to
                  --  prevent setting the selection mark when we just built
                  --  the action and don't want the sel mark to move because
                  --  end_sel_line and end_sel_col have not been set yet.

                  Mark := Get_Sel_Mark (C);

                  Command.Buffer.Get_Iter_At_Screen_Position
                    (Iter, Sel_Loc.Line, Sel_Loc.Col);

                  Command.Buffer.Move_Mark (Mark, Iter);
                  Update_MC_Selection (Command.Buffer);
               end if;
            end if;
         else
            Set_Cursor_Position
              (Command.Buffer, Loc.Line, Loc.Col, Internal => True);

            --  Move the selection mark. See comment above
            if Sel_Loc.Line /= 0 then
               Mark := Command.Buffer.Get_Selection_Bound;

               Command.Buffer.Get_Iter_At_Screen_Position
                 (Iter, Sel_Loc.Line, Sel_Loc.Col);

               Command.Buffer.Move_Mark (Mark, Iter);
            end if;

            if View /= null then
               Scroll_To_Cursor_Location (View);
            end if;
         end if;

         Set_Cursors_Sync (Command.Buffer, Sync);
      end Set_Cursor_Position;

      User_Executed : constant Boolean := Command.User_Executed;

      First_Loc : constant Loc_T :=
        (if not User_Executed then
           (if Command.Edition_Mode = Insertion
            then Command.Locs.Start_Loc
            else Command.Locs.End_Loc)
         else
            (if Command.Locs.Start_Loc < Command.Locs.End_Loc
            then Command.Locs.Start_Loc
            else Command.Locs.End_Loc));

      Editor : Source_Editor_Box;
      View   : Source_View;

   begin
      if Command.User_Executed then
         Command.User_Executed := False;

      else
         Editor := Get_Source_Box_From_MDI
           (Find_Editor
              (Kernel  => Get_Kernel (Command.Buffer),
               File    => Command.Buffer.Get_Filename,
               Project => GNATCOLL.Projects.No_Project));
--           (Find_Current_Editor (Get_Kernel (Command.Buffer)));
         if Editor /= null then
            --  Might not have an editor yet when this is called as part of
            --  the initial loading of the file.
            View := Get_View (Editor);
            if View.Get_Buffer /= Gtk_Text_Buffer (Command.Buffer) then
               View := null;
            end if;
         end if;

         MC_Sync_Save := Get_Cursors_Sync (Command.Buffer);

         --  The cursor is a multi cursor
         if Is_Alive (C) then
            Set_Manual_Sync (C);
         end if;

         if not Avoid_Move_Cursor (Command) then
            Set_Cursor_Position (First_Loc, First_Loc, View);
         end if;

         case Command.Edition_Mode is
            when Insertion =>
               Insert
                 (Command.Buffer,
                  First_Loc.Line,
                  First_Loc.Col,
                  Command.Current_Text
                    (First .. First + Command.Current_Text_Size - 1),
                  False);

            when Deletion =>
               Delete
                 (Command.Buffer,
                  First_Loc.Line,
                  First_Loc.Col,
                  Natural
                    (g_utf8_strlen
                         (Command.Current_Text
                              (First .. Command.Current_Text_Size + First - 1),
                          Interfaces.C.size_t (Command.Current_Text_Size))),
                  False);

         end case;

         if not Avoid_Move_Cursor (Command)
           and then Command.Locs.End_Loc /= (0, 0)
         then
            Set_Cursor_Position
              (Command.Locs.End_Loc, Command.Locs.End_Sel_Loc, View);
         end if;

         Set_Cursors_Sync (Command.Buffer, MC_Sync_Save);
      end if;

      Command_Finished (Command, True);

      return Success;
   end Execute;

   ----------
   -- Undo --
   ----------

   overriding function Undo
     (Command : access Editor_Command_Type) return Boolean
   is
      New_Locs : constant Editor_Command_Locations :=
        (Start_Loc     => Command.Locs.End_Loc,
         End_Loc       => Command.Locs.Start_Loc,
         Start_Sel_Loc => Command.Locs.End_Sel_Loc,
         End_Sel_Loc   => Command.Locs.Start_Sel_Loc);
      Old_Locs : constant Editor_Command_Locations := Command.Locs;
   begin

      Command.Edition_Mode :=
        (if Command.Edition_Mode = Insertion then Deletion else Insertion);
      Command.Locs := New_Locs;

      Execute (Command);

      Command.Edition_Mode :=
        (if Command.Edition_Mode = Insertion then Deletion else Insertion);
      Command.Locs := Old_Locs;

      return True;
   end Undo;

   ----------------------
   -- Set_End_Location --
   ----------------------

   procedure Set_End_Location
     (Command : access Editor_Command_Type;
      Position : Gtk_Text_Iter)
   is
      L : Editable_Line_Type;
      C : Character_Offset_Type;
   begin
      Get_Iter_Position (Command.Buffer, Position, L, C);
      Command.Set_End_Location ((L, C), (L, C));
   end Set_End_Location;

   ----------------------
   -- Set_End_Location --
   ----------------------

   procedure Set_End_Location
     (Command : access Editor_Command_Type;
      Cursor_Loc, Sel_Loc : Loc_T)
   is
   begin
      Command.Locs.End_Loc := Cursor_Loc;
      Command.Locs.End_Sel_Loc := Sel_Loc;
   end Set_End_Location;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Editor_Replace_Slice_Type) return Command_Return_Type
   is
      Editor : Source_Editor_Box;
      Sync   : constant Cursors_Sync_Type :=
        Get_Cursors_Sync (Command.Buffer);
   begin
      Set_Manual_Sync (Get_Main_Cursor (Command.Buffer));

      Replace_Slice
        (Command.Buffer,
         Command.Start_Line,
         Command.Start_Column,
         Command.End_Line_Before,
         Command.End_Column_Before,
         Command.Text_After.all,
         False);

      --  If needed, compute Command.End_Line_After, Command.End_Column_After

      if Command.End_Line_After = 0 then
         Forward_Position
           (Command.Buffer,
            Command.Start_Line,
            Command.Start_Column,
            Integer (g_utf8_strlen
                       (Command.Text_After.all, Command.Text_After'Length)),
            Command.End_Line_After,
            Command.End_Column_After);
      end if;

      if Command.Move_Cursor and then Is_Valid_Position
        (Command.Buffer, Command.End_Line_After, Command.End_Column_After)
      then
         Editor := Get_Source_Box_From_MDI
           (Find_Current_Editor (Get_Kernel (Command.Buffer)));

         Set_Cursor_Position
           (Command.Buffer,
            Command.End_Line_After,
            Command.End_Column_After,
            Internal  => True);
         Scroll_To_Cursor_Location (Get_View (Editor));
      end if;

      Command_Finished (Command, True);

      Set_Cursors_Sync (Command.Buffer, Sync);

      return Success;
   end Execute;

   ----------
   -- Undo --
   ----------

   overriding function Undo
     (Command : access Editor_Replace_Slice_Type) return Boolean
   is
      Editor : Source_Editor_Box;
      Sync   : constant Cursors_Sync_Type :=
        Get_Cursors_Sync (Command.Buffer);
   begin
      if not Is_Valid_Position
        (Command.Buffer,
         Command.End_Line_After,
         Command.End_Column_After)
      then
         return True;
      end if;

      Set_Manual_Sync (Get_Main_Cursor (Command.Buffer));

      Replace_Slice
        (Command.Buffer,
         Command.Start_Line,
         Command.Start_Column,
         Command.End_Line_After,
         Command.End_Column_After,
         Command.Text_Before.all,
         False);

      Editor := Get_Source_Box_From_MDI
        (Find_Current_Editor (Get_Kernel (Command.Buffer)));

      if Command.Force_End then
         Set_Cursor_Position
           (Command.Buffer,
            Command.End_Line_Before,
            Command.End_Column_Before,
            Internal  => True);
      else
         Set_Cursor_Position
           (Command.Buffer,
            Command.Start_Line,
            Command.Start_Column,
            Internal  => True);
      end if;

      Scroll_To_Cursor_Location (Get_View (Editor));

      Command_Finished (Command, True);
      Set_Cursors_Sync (Command.Buffer, Sync);
      return True;
   end Undo;

   ------------
   -- Create --
   ------------

   procedure Create
     (Item         : out Editor_Replace_Slice;
      Buffer       : Source_Buffer;
      Start_Line   : Editable_Line_Type;
      Start_Column : Character_Offset_Type;
      End_Line     : Editable_Line_Type;
      End_Column   : Character_Offset_Type;
      Text         : Basic_Types.UTF8_String;
      Force_End    : Boolean := False;
      Move_Cursor  : Boolean := True)
   is
   begin
      Item := new Editor_Replace_Slice_Type;
      Item.Buffer := Buffer;
      Item.Start_Line := Start_Line;
      Item.Start_Column := Start_Column;
      Item.End_Line_Before := End_Line;
      Item.End_Column_Before := End_Column;
      Item.Force_End := Force_End;
      Item.Text_Before := new String'
        (To_String (Get_Text
          (Buffer, Start_Line, Start_Column, End_Line, End_Column)));
      Item.Text_After := new String'(Text);
      Item.Move_Cursor := Move_Cursor;
   end Create;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Remove_Blank_Lines_Command_Type)
      return Command_Return_Type is
   begin
      Remove_Blank_Lines (Command.Buffer, Command.Mark, Command.Number);

      return Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Hide_Editable_Lines_Type)
      return Command_Return_Type is
   begin
      if Blocks_Are_Exact (Command.Buffer) then
         --  Do not actually hide the lines if the block information is not
         --  known to be good.
         Hide_Lines (Command.Buffer, Command.Base_Line, Command.Number);
      end if;

      return Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Unhide_Editable_Lines_Type)
      return Command_Return_Type is
   begin
      Unhide_Lines
        (Command.Buffer,
         Get_Editable_Line (Command.Buffer, Command.Base_Line));

      return Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Update_Async_Record)
      return Commands.Command_Return_Type
   is
      Tree : Semantic_Tree'Class :=
        Command.Kernel.Get_Abstract_Tree_For_File ("EDIT", Command.Filename);

   begin
      Tree.Update_Async;

      return Success;
   end Execute;

   ----------
   -- Name --
   ----------

   overriding function Name
     (Command : access Update_Async_Record) return String
   is
      pragma Unreferenced (Command);
   begin
      return "Semantic tree update";
   end Name;

   ------------------
   -- Debug_String --
   ------------------

   overriding function Debug_String
     (C : Hide_Editable_Lines_Type) return String is
   begin
      return "Hide_Editable_Lines: " & C.Number'Img;
   end Debug_String;

   overriding function Debug_String
     (C : Unhide_Editable_Lines_Type) return String is
   begin
      return "Unhide_Editable_Lines: " & C.Base_Line'Img;
   end Debug_String;

   overriding function Debug_String
     (C : Check_Modified_State_Type) return String
   is
      pragma Unreferenced (C);
   begin
      return "Check_Modified_State";
   end Debug_String;

   overriding function Debug_String
     (C : Editor_Command_Type) return String is
      function Loc_String (L : Loc_T) return String is
         (L.Line'Img & ":" & L.Col'Img);
   begin
      return C.Edition_Mode'Img
        & " " & C.Current_Text (1 .. C.Current_Text_Size) & " - "
        & "START POSITIONS : " & Loc_String (C.Locs.Start_Loc) & " "
        & Loc_String (C.Locs.Start_Sel_Loc) & " END POSITIONS : "
        & Loc_String (C.Locs.End_Loc) & " " & Loc_String (C.Locs.End_Sel_Loc);
   end Debug_String;

   overriding function Debug_String
     (C : Editor_Replace_Slice_Type) return String is
   begin
      return "REPLACE: " & C.Text_Before.all & "->" & C.Text_After.all;
   end Debug_String;

end Commands.Editor;
