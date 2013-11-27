------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2003-2013, AdaCore                     --
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
with GNAT.Strings;              use GNAT.Strings;
with Interfaces.C;

with Src_Editor_Box;            use Src_Editor_Box;
with Src_Editor_Module;         use Src_Editor_Module;
with Src_Editor_View;           use Src_Editor_View;
with Src_Editor_Buffer.Line_Information;
use  Src_Editor_Buffer.Line_Information;
with Src_Editor_Buffer.Multi_Cursors;
use  Src_Editor_Buffer.Multi_Cursors;
with GPS.Kernel.Standard_Hooks; use GPS.Kernel.Standard_Hooks;

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
      Buffer : Source_Buffer;
      Queue  : Command_Queue) is
   begin
      Item := new Check_Modified_State_Type;
      Item.Buffer := Buffer;
      Item.Check_Queue := Queue;
   end Create;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Check_Modified_State_Type) return Command_Return_Type
   is
      New_Status : constant Status_Type := Get_Status (Command.Buffer);
   begin
      if New_Status /= Get_Last_Status (Command.Buffer) then
         Status_Changed (Command.Buffer);
      end if;

      Set_Last_Status (Command.Buffer, New_Status);

      return Success;
   end Execute;

   ----------
   -- Free --
   ----------

   overriding procedure Free (X : in out Editor_Command_Type) is
   begin
      Free (X.Current_Text);
   end Free;

   overriding procedure Free (X : in out Editor_Replace_Slice_Type) is
   begin
      Free (X.Text_Before);
      Free (X.Text_After);
   end Free;

   overriding procedure Free (X : in out Check_Modified_State_Type) is
      pragma Unreferenced (X);
   begin
      null;
   end Free;

   overriding procedure Free (X : in out Remove_Blank_Lines_Command_Type) is
   begin
      if not Get_Deleted (X.Mark) then
         Delete_Mark (X.Buffer, X.Mark);
      end if;
   end Free;

   overriding procedure Free (X : in out Unhide_Editable_Lines_Type) is
   begin
      null;
   end Free;

   overriding procedure Free (X : in out Hide_Editable_Lines_Type) is
      pragma Unreferenced (X);
   begin
      null;
   end Free;

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
      Cursor_Name         : String := "") is
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
      Item.Alternative_Cursor_Name := To_Unbounded_String (Cursor_Name);
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

      if Start_Line /= 0 then
         Item.Locs.Start_Loc.Line := Start_Line;
      end if;

      if Start_Column /= 0 then
         Item.Locs.Start_Loc.Col := Start_Column;
      end if;
   end Add_Text;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Editor_Command_Type) return Command_Return_Type
   is
      First  : constant Natural := Command.Current_Text'First;
      Editor : Source_Editor_Box;
      View   : Source_View;
      Cursor_Name : constant String
        := To_String (Command.Alternative_Cursor_Name);
      Mark : Gtk_Text_Mark;
      MC_Sync_Save : Multi_Cursors_Sync_Type;

      procedure Set_Cursor_Position;
      --  Set the action's cursor at the right place whether it is a multi
      --  cursor or the main cursor

      procedure Set_Cursor_Position is
         Iter : Gtk_Text_Iter;
         Mark : Gtk_Text_Mark;
         Reset_Mode : Boolean := False;
      begin

         if Get_Multi_Cursors_Sync (Command.Buffer).Mode = Auto then
            Set_Multi_Cursors_Manual_Sync (Command.Buffer);
            Reset_Mode := True;
         end if;

         --  The cursor is a multi cursor
         if Cursor_Name /= "" then
            Mark := Command.Buffer.Get_Mark (Cursor_Name);

            if Mark /= null then
               if Command.Locs.End_Loc.Line /= 0 then
                  Command.Buffer.Get_Iter_At_Screen_Position
                    (Iter, Command.Locs.End_Loc.Line,
                     Command.Locs.End_Loc.Col);

                  Command.Buffer.Move_Mark (Mark, Iter);

                  --  Move the selection mark if we have meaningful values
                  --  for End_Sel_Line and End_Sel_Col. The zero check is to
                  --  prevent setting the selection mark when we just built
                  --  the action and don't want the sel mark to move because
                  --  end_sel_line and end_sel_col have not been set yet.

                  Mark := Command.Buffer.Get_Mark
                    (Get_Sel_Mark_Name (Cursor_Name));

                  Command.Buffer.Get_Iter_At_Screen_Position
                    (Iter,
                     Command.Locs.End_Sel_Loc.Line,
                     Command.Locs.End_Sel_Loc.Col);

                  Command.Buffer.Move_Mark (Mark, Iter);
                  Update_MC_Selection (Command.Buffer);
               end if;
            end if;
         else
            Set_Cursor_Position
              (Command.Buffer, Command.Locs.End_Loc.Line,
               Command.Locs.End_Loc.Col, Internal => True);

            --  Move the selection mark. See comment above
            if Command.Locs.End_Sel_Loc.Line /= 0 then
               Mark := Command.Buffer.Get_Selection_Bound;

               Command.Buffer.Get_Iter_At_Screen_Position
                 (Iter,
                  Command.Locs.End_Sel_Loc.Line,
                  Command.Locs.End_Sel_Loc.Col);

               Command.Buffer.Move_Mark (Mark, Iter);
            end if;

            Scroll_To_Cursor_Location (View);
         end if;

         if Reset_Mode then
            Set_Multi_Cursors_Auto_Sync (Command.Buffer);
         end if;

      end Set_Cursor_Position;

      First_Loc : constant Loc_T :=
        (if Command.Locs.Start_Loc < Command.Locs.End_Loc
         then Command.Locs.Start_Loc
         else Command.Locs.End_Loc);

   begin
      if Command.User_Executed then
         Command.User_Executed := False;

      else
         Editor := Get_Source_Box_From_MDI
           (Find_Current_Editor (Get_Kernel (Command.Buffer)));
         View := Get_View (Editor);

         MC_Sync_Save := Get_Multi_Cursors_Sync (Command.Buffer);

         --  The cursor is a multi cursor
         if Cursor_Name /= "" then
            Mark := Command.Buffer.Get_Mark (Cursor_Name);
            if Mark /= null then
               Set_Multi_Cursors_Manual_Sync (Command.Buffer, Mark);
            else
               Set_Multi_Cursors_Manual_Sync (Command.Buffer);
            end if;
         else
            Set_Multi_Cursors_Manual_Sync (Command.Buffer);
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

         if not Avoid_Move_Cursor (Command) then
            Set_Cursor_Position;
         end if;

         Set_Multi_Cursors_Sync (Command.Buffer, MC_Sync_Save);
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

   begin
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

      if Is_Valid_Position
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

      return Success;
   end Execute;

   ----------
   -- Undo --
   ----------

   overriding function Undo
     (Command : access Editor_Replace_Slice_Type) return Boolean
   is
      Editor : Source_Editor_Box;
   begin
      if not Is_Valid_Position
        (Command.Buffer,
         Command.End_Line_After,
         Command.End_Column_After)
      then
         return True;
      end if;

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
      Force_End    : Boolean := False)
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
        (Get_Text (Buffer, Start_Line, Start_Column, End_Line, End_Column));

      Item.Text_After := new String'(Text);
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
      if Get_Constructs_State (Command.Buffer) >= Line_Exact then
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
