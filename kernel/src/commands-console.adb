------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2012, AdaCore                     --
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

package body Commands.Console is

   use type GNAT.Strings.String_Access;

   ----------
   -- Copy --
   ----------

   function Copy
     (Item : Console_Command_Access) return Console_Command_Access
   is
      Result : Console_Command_Access;
   begin
      Result := new Console_Command;
      Result.Kernel         := Item.Kernel;
      Result.Text           := new String'(Item.Text.all);
      Result.Highlight_Sloc := Item.Highlight_Sloc;
      Result.Add_LF         := Item.Add_LF;
      Result.Mode           := Item.Mode;
      Result.Group_Fail     := Item.Group_Fail;
      return Result;
   end Copy;

   ------------
   -- Create --
   ------------

   procedure Create
     (Item           : out Console_Command_Access;
      Kernel         : Kernel_Handle;
      Text           : String;
      Highlight_Sloc : Boolean := True;
      Add_LF         : Boolean := True;
      Mode           : Message_Type := Info) is
   begin
      Item := new Console_Command;
      Item.Kernel := Kernel;
      Item.Text := new String'(Text);
      Item.Highlight_Sloc := Highlight_Sloc;
      Item.Add_LF := Add_LF;
      Item.Insert_Mode := Mode;
   end Create;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Console_Command) return Command_Return_Type is
   begin
      if Command.Text /= null then
         Insert (Command.Kernel,
                 Command.Text.all,
                 Command.Add_LF,
                 Command.Insert_Mode);
      end if;

      Command_Finished (Command, True);

      return Success;
   end Execute;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Command : in out Console_Command) is
   begin
      GNAT.Strings.Free (Command.Text);
   end Free;

   ----------
   -- Undo --
   ----------

   overriding function Undo
     (Command : access Console_Command) return Boolean
   is
      pragma Unreferenced (Command);
   begin
      return True;
   end Undo;

end Commands.Console;
