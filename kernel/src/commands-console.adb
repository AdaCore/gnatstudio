---------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
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

with Glide_Kernel;         use Glide_Kernel;
with Glide_Kernel.Console; use Glide_Kernel.Console;
with Basic_Types;          use Basic_Types;

package body Commands.Console is

   ----------
   -- Copy --
   ----------

   function Copy
     (Item : Console_Command_Access)
     return Console_Command_Access
   is
      Result : Console_Command_Access;
   begin
      Result := new Console_Command;
      Result.Kernel         := Item.Kernel;
      Result.Text           := new String' (Item.Text.all);
      Result.Highlight_Sloc := Item.Highlight_Sloc;
      Result.Add_LF         := Item.Add_LF;
      Result.Mode           := Item.Mode;
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
      Item.Text := new String' (Text);
      Item.Highlight_Sloc := Highlight_Sloc;
      Item.Add_LF := Add_LF;
      Item.Insert_Mode := Mode;
   end Create;

   -------------
   -- Execute --
   -------------

   function Execute (Command : access Console_Command) return Boolean is
   begin
      if Command.Text /= null then
         Insert (Command.Kernel,
                 Command.Text.all,
                 Command.Highlight_Sloc,
                 Command.Add_LF,
                 Command.Insert_Mode);
      end if;

      Command_Finished (Command, True);

      return True;
   end Execute;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Command : access Console_Command) is
   begin
      Free (Command.Text);
   end Destroy;

end Commands.Console;
