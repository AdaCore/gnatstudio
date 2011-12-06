-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                   Copyright (C) 2011, AdaCore                     --
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

with Commands.Interactive;

package Commands.GNATTest is

   --  Go to test from tested subprogram and backward

   type Go_To_Test_Command_Type is new Commands.Interactive.Interactive_Command
   with record
      To_Test : Boolean;
   end record;

   type Go_To_Test_Command_Access is access all Go_To_Test_Command_Type;

   overriding function Execute
     (Command : access Go_To_Test_Command_Type;
      Context : Commands.Interactive.Interactive_Command_Context)
      return Commands.Command_Return_Type;

   overriding function Name (X : access Go_To_Test_Command_Type) return String;

   overriding procedure Free (X : in out Go_To_Test_Command_Type);

end Commands.GNATTest;
