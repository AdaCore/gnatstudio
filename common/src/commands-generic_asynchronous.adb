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

package body Commands.Generic_Asynchronous is

   ----------
   -- Free --
   ----------

   procedure Free (D : in out Generic_Asynchronous_Command) is
   begin
      if D.Data /= null then
         Free (D.Data.all);
         Unchecked_Free (D.Data);
      end if;
   end Free;

   ------------
   -- Create --
   ------------

   procedure Create
     (Command : out Generic_Asynchronous_Command_Access;
      Data    : in Data_Type) is
   begin
      Command := new Generic_Asynchronous_Command;
      Command.Data := new Data_Type'(Data);
   end Create;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Generic_Asynchronous_Command)
      return Command_Return_Type
   is
      Result : Command_Return_Type;

   begin
      if Command.Data = null then
         return Failure;
      end if;

      Iterate (Command.Data.all, Command_Access (Command), Result);

      return Result;
   end Execute;

   ----------
   -- Name --
   ----------

   function Name (Command : access Generic_Asynchronous_Command) return String
   is
      pragma Unreferenced (Command);
   begin
      return Description;
   end Name;

end Commands.Generic_Asynchronous;
