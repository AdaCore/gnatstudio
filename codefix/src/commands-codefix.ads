-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2003-2004                       --
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

with GPS.Kernel;           use GPS.Kernel;

with Codefix.Errors_Manager; use Codefix.Errors_Manager;
with Codefix_Module;

package Commands.Codefix is

   type Codefix_Command is new Root_Command with record
      Error        : Error_Id;
      Session      : Codefix_Module.Codefix_Session;
      Kernel       : Kernel_Handle;
   end record;

   type Codefix_Add_Command is new Root_Command with record
      Kernel        : Kernel_Handle;
      Session       : Codefix_Module.Codefix_Session;
      Current_Error : Error_Id;
      Errors_Num    : Natural;
      Errors_Fixed  : Natural := 0;
   end record;

   function Execute
     (Command : access Codefix_Command) return Command_Return_Type;
   --  Fix the error recorded in the Codefix_Command.

   function Execute
     (Command : access Codefix_Add_Command) return Command_Return_Type;
   procedure Free (Command : in out Codefix_Add_Command);
   function Progress
     (Command : access Codefix_Add_Command) return Progress_Record;
   function Name (Command : access Codefix_Add_Command) return String;
   --  See Commands for a description of overloaded subprograms.

   function Undo (Command : access Codefix_Command) return Boolean;
   --  Unfix the error recored.

   procedure Free (Command : in out Codefix_Command);
   --  Do not do anyting, as far as nothing has to be freed in Codefix_Command.

end Commands.Codefix;
