------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2003-2019, AdaCore                     --
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

package body Commands.Generic_Asynchronous is

   use GNAT.Strings;

   --------------------
   -- Primitive_Free --
   --------------------

   overriding procedure Primitive_Free
     (D : in out Generic_Asynchronous_Command) is
   begin
      if D.Data /= null then
         Free (D.Data.all);
         Unchecked_Free (D.Data);
         Free (D.Description);
      end if;
   end Primitive_Free;

   ------------
   -- Create --
   ------------

   procedure Create
     (Command     : out Generic_Asynchronous_Command_Access;
      Description : String;
      Data        : Data_Type;
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

   overriding function Execute
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

   overriding function Name
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
      if Command.Data /= null then
         Free (Command.Data.all);
         Unchecked_Free (Command.Data);
      end if;
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
