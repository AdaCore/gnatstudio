-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2003-2006                      --
--                              AdaCore                              --
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
         Free (D.Description);
      end if;
   end Free;

   ------------
   -- Create --
   ------------

   procedure Create
     (Command     : out Generic_Asynchronous_Command_Access;
      Description : String;
      Data        : in Data_Type;
      Iterate     : Iteration_Procedure) is
   begin
      Command := new Generic_Asynchronous_Command;
      Command.Data := new Data_Type'(Data);
      Command.Iterate := Iterate;
      Command.Description := new String'(Description);
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

      if Command.Iterate = null then
         return Failure;
      else
         Command.Iterate (Command.Data.all, Command_Access (Command), Result);
      end if;

      return Result;
   end Execute;

   ----------
   -- Name --
   ----------

   function Name
     (Command : access Generic_Asynchronous_Command) return String is
   begin
      if Command.Description = null then
         return "Generic asynchronous command";
      else
         return Command.Description.all;
      end if;
   end Name;

   --------------
   -- Set_Data --
   --------------

   procedure Set_Data
     (Command : access Generic_Asynchronous_Command;
      Data    : Data_Type) is
   begin
      Unchecked_Free (Command.Data);
      Command.Data := new Data_Type'(Data);
   end Set_Data;

   --------------
   -- Get_Data --
   --------------

   function Get_Data (Command : access Generic_Asynchronous_Command)
      return Data_Type is
   begin
      return Command.Data.all;
   end Get_Data;

end Commands.Generic_Asynchronous;
