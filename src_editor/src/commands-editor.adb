-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2002                         --
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

with Glib;                              use Glib;
with Gtk.Text_Iter;                     use Gtk.Text_Iter;
with Traces;                            use Traces;
with String_Utils;                      use String_Utils;
with Src_Editor_Box;                    use Src_Editor_Box;
with Src_Editor_Module;                 use Src_Editor_Module;

package body Commands.Editor is

   Me : constant Debug_Handle := Create ("Commands.Editor");

   ------------
   -- Create --
   ------------

   procedure Create
     (Item  : out Check_Modified_State;
      Box   : Source_Editor_Box;
      Queue : Command_Queue) is
   begin
      Item := new Check_Modified_State_Type;
      Item.Box := Box;
      Item.Check_Queue := Queue;
   end Create;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Check_Modified_State_Type) return Boolean is
   begin
      if Get_Position (Command.Check_Queue)
        = Get_Saved_Position (Command.Box)
      then
         Set_Modified_State (Command.Box, False);
      end if;

      return True;
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
      Line          : Integer;
      Column        : Integer;
      Direction     : Direction_Type := Forward) is
   begin
      Item := new Editor_Command_Type;
      Item.Buffer := Buffer;
      Item.Current_Text := new String (1 .. 512);
      Item.Edition_Mode := Mode;
      Item.User_Executed := User_Executed;
      Item.Line := Line;
      Item.Column := Column;
      Item.Direction := Direction;
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
      Text         : String;
      Start_Line   : Integer := -1;
      Start_Column : Integer := -1)
   is
      Text_Length : constant Integer := Text'Length;
      First       : Natural := Item.Current_Text.all'First;
   begin
      while Item.Current_Text_Size + Text_Length
        > Item.Current_Text_Total_Length
      loop
         Item.Current_Text_Total_Length := Item.Current_Text_Total_Length * 2;
      end loop;

      if Item.Current_Text_Total_Length > Item.Current_Text.all'Length then
         declare
            New_Current_Text : String (1 .. Item.Current_Text_Total_Length);
         begin
            New_Current_Text (1 .. Item.Current_Text_Size) :=
              Item.Current_Text (First .. First + Item.Current_Text_Size - 1);
            Free (Item.Current_Text);
            Item.Current_Text := new String'(New_Current_Text);
            First := Item.Current_Text.all'First;
         end;
      end if;

      if Item.Edition_Mode = Insertion then
         Item.Current_Text
           (First + Item.Current_Text_Size
              .. First + Item.Current_Text_Size + Text_Length - 1) := Text;

      else
         if Item.Current_Text_Size > 0 then
            for J in reverse 0 ..  Item.Current_Text_Size - 1 loop
               Item.Current_Text (First + Text_Length + J)
                 := Item.Current_Text (First + J);
            end loop;
         end if;

         Item.Current_Text (First .. First + Text_Length - 1) := Text;
      end if;

      Item.Current_Text_Size := Item.Current_Text_Size + Text_Length;

      if Start_Line /= -1 then
         Item.Line := Start_Line;
      end if;

      if Start_Column /= -1 then
         Item.Column := Start_Column;
      end if;
   end Add_Text;

   -------------
   -- Execute --
   -------------

   function Execute (Command : access Editor_Command_Type) return Boolean is
      First  : constant Natural := Command.Current_Text'First;
      Editor : Source_Editor_Box;
   begin
      if Command.User_Executed then
         Command.User_Executed := False;
      else
         if not Is_Valid_Position
           (Command.Buffer, Gint (Command.Line), Gint (Command.Column))
         then
            --  This should never happen. If it does, it probably means
            --  that a command with wrong settings has been recorded.

            Trace (Me, "Invalid location: " &
                   Image (Command.Line) & ':' & Image (Command.Column));
            Command_Finished (Command, True);

            return True;
         end if;

         Editor := Get_Source_Box_From_MDI
           (Find_Current_Editor (Get_Kernel (Command.Buffer)));

         case Command.Edition_Mode is
            when Insertion =>
               Set_Cursor_Position
                 (Command.Buffer,
                  Gint (Command.Line),
                  Gint (Command.Column));
               Scroll_To_Cursor_Location (Editor);
               Insert
                 (Command.Buffer,
                  Gint (Command.Line),
                  Gint (Command.Column),
                  Command.Current_Text
                    (First .. First + Command.Current_Text_Size - 1),
                  False);

               if Command.Direction = Backward then
                  Set_Cursor_Position
                    (Command.Buffer,
                     Gint (Command.Line),
                     Gint (Command.Column));
                  Scroll_To_Cursor_Location (Editor);
               end if;

            when Deletion =>
               Delete
                 (Command.Buffer,
                  Gint (Command.Line),
                  Gint (Command.Column),
                  Gint (Command.Current_Text_Size),
                  False);

               Set_Cursor_Position
                 (Command.Buffer,
                  Gint (Command.Line),
                  Gint (Command.Column));
               Scroll_To_Cursor_Location (Editor);
         end case;
      end if;

      Command_Finished (Command, True);

      return True;
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
     (Command : access Editor_Replace_Slice_Type) return Boolean
   is
      Iter       : Gtk_Text_Iter;
      Result     : Boolean;
      Editor     : Source_Editor_Box;
   begin
      if not
        Is_Valid_Position
          (Command.Buffer,
           Gint (Command.Start_Line), Gint (Command.Start_Column))
        or else not Is_Valid_Position
          (Command.Buffer,
           Gint (Command.End_Line_Before), Gint (Command.End_Column_Before))
      then
         --  This should never happen. If it does, it probably means
         --  that a command with wrong settings has been recorded.

         Trace (Me, "Invalid location: start:" &
                Image (Command.Start_Line) & ':' &
                Image (Command.Start_Column) & " end: " &
                Image (Command.End_Line_Before) & ':' &
                Image (Command.End_Column_Before));
         Command_Finished (Command, True);

         return True;
      end if;

      Replace_Slice
        (Command.Buffer,
         Gint (Command.Start_Line),
         Gint (Command.Start_Column),
         Gint (Command.End_Line_Before),
         Gint (Command.End_Column_Before),
         Command.Text_After.all,
         False);
      Get_Iter_At_Line_Offset
        (Command.Buffer,
         Iter,
         Gint (Command.Start_Line),
         Gint (Command.Start_Column));

      if Command.End_Line_After = -1 then
         Forward_Chars (Iter, Command.Text_After'Length, Result);
         Command.End_Line_After := Integer (Get_Line (Iter));
         Command.End_Column_After := Integer (Get_Line_Offset (Iter));
      end if;

      Editor := Get_Source_Box_From_MDI
        (Find_Current_Editor (Get_Kernel (Command.Buffer)));
      Set_Cursor_Position
        (Command.Buffer,
         Gint (Command.End_Line_After),
         Gint (Command.End_Column_After));
      Scroll_To_Cursor_Location (Editor);

      Command_Finished (Command, True);

      return True;
   end Execute;

   ----------
   -- Undo --
   ----------

   function Undo (Command : access Editor_Replace_Slice_Type) return Boolean is
      Editor : Source_Editor_Box;
   begin
      if not Is_Valid_Position
        (Command.Buffer,
         Gint (Command.End_Line_After),
         Gint (Command.End_Column_After))
      then
         return True;
      end if;

      Replace_Slice
        (Command.Buffer,
         Gint (Command.Start_Line),
         Gint (Command.Start_Column),
         Gint (Command.End_Line_After),
         Gint (Command.End_Column_After),
         Command.Text_Before.all,
         False);

      Editor := Get_Source_Box_From_MDI
        (Find_Current_Editor (Get_Kernel (Command.Buffer)));

      if Command.Direction = Forward then
         Set_Cursor_Position
           (Command.Buffer,
            Gint (Command.End_Line_Before),
            Gint (Command.End_Column_Before));
      else
         Set_Cursor_Position
           (Command.Buffer,
            Gint (Command.Start_Line),
            Gint (Command.Start_Column));
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
      Start_Line   : Integer;
      Start_Column : Integer;
      End_Line     : Integer;
      End_Column   : Integer;
      Text         : String;
      Direction    : Direction_Type := Backward)
   is
      Iter : Gtk_Text_Iter;
   begin
      Item := new Editor_Replace_Slice_Type;
      Item.Buffer := Buffer;
      Item.Start_Line := Start_Line;
      Item.Start_Column := Start_Column;
      Item.End_Line_Before := End_Line;
      Item.End_Column_Before := End_Column;

      Item.Text_Before := new String'
        (Get_Slice
          (Buffer,
           Gint (Start_Line),
           Gint (Start_Column),
           Gint (End_Line),
           Gint (End_Column)));
      Item.Text_After := new String'(Text);

      Get_Iter_At_Line_Offset
        (Buffer, Iter, Gint (Start_Line), Gint (Start_Column));
      Item.Direction := Direction;
   end Create;

end Commands.Editor;
