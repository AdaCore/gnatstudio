-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                        Copyright (C) 2002                         --
--                            ACT-Europe                             --
--                                                                   --
-- GLIDE is free software; you can redistribute it and/or modify  it --
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

with Glib;        use Glib;

package body Commands.Editor is

   ---------------------
   -- Is_Null_Command --
   ---------------------

   function Is_Null_Command (Command : Editor_Command) return Boolean is
   begin
      return (Command = null or else (Command.Current_Text_Size = 0));
   end Is_Null_Command;

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
      Column        : Integer)
   is
      String_Buffer : String (1 .. 512);
      pragma Warnings (Off, String_Buffer);
   begin
      Item := new Editor_Command_Type;
      Item.Buffer := Buffer;
      Item.Current_Text := new String' (String_Buffer);
      Item.Edition_Mode := Mode;
      Item.User_Executed := User_Executed;
      Item.Line := Line;
      Item.Column := Column;
   end Create;

   --------------
   -- Add_Text --
   --------------

   procedure Add_Text
     (Item         : Editor_Command;
      Text         : String;
      Start_Line   : Integer := -1;
      Start_Column : Integer := -1)
   is
      Text_Length : Integer := Text'Length;
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
            Item.Current_Text := new String' (New_Current_Text);
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

   function Execute
     (Command : access Editor_Command_Type) return Boolean
   is
      First : Natural := Command.Current_Text.all'First;
   begin
      if Command.User_Executed then
         Command.User_Executed := False;
      else
         if Command.Edition_Mode = Insertion then
            Set_Cursor_Position (Command.Buffer,
                                 Gint (Command.Line),
                                 Gint (Command.Column));

            Insert (Command.Buffer,
                    Gint (Command.Line),
                    Gint (Command.Column),
                    Command.Current_Text
                      (First .. First + Command.Current_Text_Size - 1),
                    False);

         elsif Command.Edition_Mode = Deletion then
            Delete (Command.Buffer,
                    Gint (Command.Line),
                    Gint (Command.Column),
                    Gint (Command.Current_Text_Size),
                    False);

            Set_Cursor_Position (Command.Buffer,
                                 Gint (Command.Line),
                                 Gint (Command.Column));
         end if;
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

end Commands.Editor;
