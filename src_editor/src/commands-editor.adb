-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2003                         --
--                            ACT-Europe                             --
--                                                                   --
-- GPS is free  software; you can  redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Traces;                   use Traces;
with String_Utils;             use String_Utils;
with Src_Editor_Box;           use Src_Editor_Box;
with Src_Editor_Module;        use Src_Editor_Module;
with Src_Editor_Buffer.Line_Information;
use Src_Editor_Buffer.Line_Information;
with Interfaces.C;

package body Commands.Editor is

   function g_utf8_strlen
     (P : String; Max : Interfaces.C.size_t) return Long_Integer;
   pragma Import (C, g_utf8_strlen);
   --  Return the text size of an UTF8 string.

   Me : constant Debug_Handle := Create ("Commands.Editor");

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

   function Execute
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

   procedure Free (X : in out Editor_Command_Type) is
   begin
      Free (X.Current_Text);
   end Free;

   procedure Free (X : in out Editor_Replace_Slice_Type) is
   begin
      Free (X.Text_Before);
      Free (X.Text_After);
   end Free;

   procedure Free (X : in out Check_Modified_State_Type) is
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
     (Item          : out Editor_Command;
      Mode          : Editor_Command_Mode;
      Buffer        : Source_Buffer;
      User_Executed : Boolean;
      Line          : Editable_Line_Type;
      Column        : Natural;
      Direction     : Direction_Type := Forward;
      Cursor_Line   : Editable_Line_Type := 0;
      Cursor_Column : Natural := 0) is
   begin
      Item := new Editor_Command_Type;
      Item.Buffer := Buffer;
      Item.Current_Text := new String (1 .. 512);
      Item.Edition_Mode := Mode;
      Item.User_Executed := User_Executed;
      Item.Line := Line;
      Item.Column := Column;
      Item.Direction := Direction;
      Item.Cursor_Line := Cursor_Line;
      Item.Cursor_Column := Cursor_Column;
   end Create;

   --------------
   -- Get_Text --
   --------------

   function Get_Text (Item : Editor_Command) return String is
   begin
      return Item.Current_Text (1 .. Item.Current_Text_Size);
   end Get_Text;

   --------------
   -- Set_Text --
   --------------

   procedure Set_Text (Item : Editor_Command; Text : String) is
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
      UTF8         : String;
      Start_Line   : Editable_Line_Type := 0;
      Start_Column : Natural := 0)
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
         Item.Line := Start_Line;
      end if;

      if Start_Column /= 0 then
         Item.Column := Start_Column;
      end if;
   end Add_Text;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Editor_Command_Type) return Command_Return_Type
   is
      First  : constant Natural := Command.Current_Text'First;
      Editor : Source_Editor_Box;
   begin
      if Command.User_Executed then
         Command.User_Executed := False;
      else
         if not Is_Valid_Position
           (Command.Buffer, Command.Line, Command.Column)
         then
            --  This should never happen. If it does, it probably means
            --  that a command with wrong settings has been recorded.

            Trace (Me, "Invalid location: " &
                   Image (Natural (Command.Line)) &
                   ':' & Image (Natural (Command.Column)));
            Command_Finished (Command, True);

            return Success;
         end if;

         Editor := Get_Source_Box_From_MDI
           (Find_Current_Editor (Get_Kernel (Command.Buffer)));

         case Command.Edition_Mode is
            when Insertion =>
               Set_Cursor_Position
                 (Command.Buffer,
                  Command.Line,
                  Command.Column);
               Scroll_To_Cursor_Location (Editor);
               Insert
                 (Command.Buffer,
                  Command.Line,
                  Command.Column,
                  Command.Current_Text
                    (First .. First + Command.Current_Text_Size - 1),
                  False);

               if Command.Direction = Extended then
                  Set_Cursor_Position
                    (Command.Buffer,
                     Command.Cursor_Line,
                     Command.Cursor_Column);
                  Scroll_To_Cursor_Location (Editor);

               elsif Command.Direction = Backward then
                  Set_Cursor_Position
                    (Command.Buffer,
                     Command.Line,
                     Command.Column);
                  Scroll_To_Cursor_Location (Editor);
               end if;

            when Deletion =>
               Delete
                 (Command.Buffer,
                  Command.Line,
                  Command.Column,
                  Natural
                    (g_utf8_strlen
                       (Command.Current_Text
                          (First .. Command.Current_Text_Size + First),
                        Interfaces.C.size_t (Command.Current_Text_Size))),
                  False);

               Set_Cursor_Position
                 (Command.Buffer,
                  Command.Line,
                  Command.Column);
               Scroll_To_Cursor_Location (Editor);
         end case;
      end if;

      Command_Finished (Command, True);

      return Success;
   end Execute;

   ----------
   -- Undo --
   ----------

   function Undo
     (Command : access Editor_Command_Type) return Boolean is
   begin
      if Command.Edition_Mode = Insertion then
         Command.Edition_Mode := Deletion;
      else
         Command.Edition_Mode := Insertion;
      end if;

      Execute (Command);

      if Command.Edition_Mode = Insertion then
         Command.Edition_Mode := Deletion;
      else
         Command.Edition_Mode := Insertion;
      end if;

      return True;
   end Undo;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Editor_Replace_Slice_Type) return Command_Return_Type
   is
      Editor : Source_Editor_Box;

   begin
      if not
        Is_Valid_Position
          (Command.Buffer,
           Command.Start_Line, Command.Start_Column)
        or else not Is_Valid_Position
          (Command.Buffer,
           Command.End_Line_Before, Command.End_Column_Before)
      then
         --  This should never happen. If it does, it probably means
         --  that a command with wrong settings has been recorded.

         Trace (Me, "Invalid location: start:" &
                Image (Natural (Command.Start_Line)) & ':' &
                Image (Natural (Command.Start_Column)) & " end: " &
                Image (Natural (Command.End_Line_Before)) & ':' &
                Image (Natural (Command.End_Column_Before)));
         Command_Finished (Command, True);

         return Success;
      end if;

      Replace_Slice
        (Command.Buffer,
         Command.Start_Line,
         Command.Start_Column,
         Command.End_Line_Before,
         Command.End_Column_Before,
         Command.Text_After.all,
         False);

      --  If needed, compute Command.End_Line_After, Command.End_Column_After.

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

      Editor := Get_Source_Box_From_MDI
        (Find_Current_Editor (Get_Kernel (Command.Buffer)));

      if Is_Valid_Position
        (Command.Buffer, Command.End_Line_After, Command.End_Column_After)
      then
         Set_Cursor_Position
           (Command.Buffer,
            Command.End_Line_After,
            Command.End_Column_After);
         Scroll_To_Cursor_Location (Editor);
      end if;

      Command_Finished (Command, True);

      return Success;
   end Execute;

   ----------
   -- Undo --
   ----------

   function Undo (Command : access Editor_Replace_Slice_Type) return Boolean is
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
            Command.End_Column_Before);
      else
         Set_Cursor_Position
           (Command.Buffer,
            Command.Start_Line,
            Command.Start_Column);
      end if;

      Scroll_To_Cursor_Location (Editor);

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
      Start_Column : Natural;
      End_Line     : Editable_Line_Type;
      End_Column   : Natural;
      Text         : String;
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

   function Execute
     (Command : access Remove_Blank_Lines_Command_Type)
      return Command_Return_Type is
   begin
      Remove_Blank_Lines (Command.Buffer, Command.Mark, Command.Number);

      return Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Hide_Editable_Lines_Type)
      return Command_Return_Type is
   begin
      if Blocks_Valid (Command.Buffer) then
         Hide_Lines (Command.Buffer, Command.Mark, Command.Number);
      end if;

      return Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Unhide_Editable_Lines_Type)
      return Command_Return_Type is
   begin
      if Blocks_Valid (Command.Buffer) then
         Unhide_Lines (Command.Buffer, Command.Mark);
      end if;

      return Success;
   end Execute;

end Commands.Editor;
