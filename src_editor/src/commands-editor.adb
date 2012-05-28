------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2003-2012, AdaCore                     --
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
use Src_Editor_Buffer.Line_Information;
with GPS.Editors;               use GPS.Editors;
with GPS.Kernel.Standard_Hooks; use GPS.Kernel.Standard_Hooks;

package body Commands.Editor is

   function g_utf8_strlen
     (P : String; Max : Interfaces.C.size_t) return Long_Integer;
   pragma Import (C, g_utf8_strlen);
   --  Return the text size of an UTF8 string

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
     (Item          : out Editor_Command;
      Mode          : Editor_Command_Mode;
      Buffer        : Source_Buffer;
      User_Executed : Boolean;
      Line          : Editable_Line_Type;
      Column        : Character_Offset_Type;
      Direction     : Direction_Type := Forward;
      Cursor_Line   : Editable_Line_Type := 0;
      Cursor_Column : Character_Offset_Type := 0) is
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

   function Get_Text (Item : Editor_Command) return UTF8_String is
   begin
      return Item.Current_Text (1 .. Item.Current_Text_Size);
   end Get_Text;

   --------------
   -- Set_Text --
   --------------

   procedure Set_Text (Item : Editor_Command; Text : UTF8_String) is
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
      UTF8         : UTF8_String;
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
         Item.Line := Start_Line;
      end if;

      if Start_Column /= 0 then
         Item.Column := Start_Column;
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
   begin
      if Command.User_Executed then
         Command.User_Executed := False;

      else
         Editor := Get_Source_Box_From_MDI
           (Find_Current_Editor (Get_Kernel (Command.Buffer)));
         View := Get_View (Editor);

         case Command.Edition_Mode is
            when Insertion =>
               if not Avoid_Cursor_Move_On_Changes (Command.Buffer) then
                  Set_Cursor_Position
                    (Command.Buffer,
                     Command.Line,
                     Command.Column,
                     Centering => Minimal,
                     Internal  => True);
                  Scroll_To_Cursor_Location (View);
               end if;

               Insert
                 (Command.Buffer,
                  Command.Line,
                  Command.Column,
                  Command.Current_Text
                    (First .. First + Command.Current_Text_Size - 1),
                  False);

               if not Avoid_Cursor_Move_On_Changes (Command.Buffer) then
                  if Command.Direction = Extended then
                     Set_Cursor_Position
                       (Command.Buffer,
                        Command.Cursor_Line,
                        Command.Cursor_Column,
                        Centering => Minimal,
                        Internal  => True);
                     Scroll_To_Cursor_Location (View);

                  elsif Command.Direction = Backward then
                     Set_Cursor_Position
                       (Command.Buffer,
                        Command.Line,
                        Command.Column,
                        Centering => Minimal,
                        Internal  => True);
                     Scroll_To_Cursor_Location (View);
                  end if;
               end if;

            when Deletion =>
               Delete
                 (Command.Buffer,
                  Command.Line,
                  Command.Column,
                  Natural
                    (g_utf8_strlen
                       (Command.Current_Text
                          (First .. Command.Current_Text_Size + First - 1),
                        Interfaces.C.size_t (Command.Current_Text_Size))),
                  False);

               if not Avoid_Cursor_Move_On_Changes (Command.Buffer) then
                  Set_Cursor_Position
                    (Command.Buffer,
                     Command.Line,
                     Command.Column,
                     Centering => Minimal,
                     Internal  => True);
                  Scroll_To_Cursor_Location (View);
               end if;
         end case;
      end if;

      Command_Finished (Command, True);

      return Success;
   end Execute;

   ----------
   -- Undo --
   ----------

   overriding function Undo
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
            Centering => Minimal,
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
            Centering => Minimal,
            Internal  => True);
      else
         Set_Cursor_Position
           (Command.Buffer,
            Command.Start_Line,
            Command.Start_Column,
            Centering => Minimal,
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
      Text         : UTF8_String;
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
   begin
      return C.Edition_Mode'Img
        & " " & C.Current_Text (1 .. C.Current_Text_Size);
   end Debug_String;

   overriding function Debug_String
     (C : Editor_Replace_Slice_Type) return String is
   begin
      return "REPLACE: " & C.Text_Before.all & "->" & C.Text_After.all;
   end Debug_String;

end Commands.Editor;
