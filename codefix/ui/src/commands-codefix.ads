------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2019, AdaCore                     --
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

with GPS.Kernel;             use GPS.Kernel;
with Codefix.Errors_Manager; use Codefix.Errors_Manager;
with Codefix_Module;

package Commands.Codefix is

   type Codefix_Command is new Root_Command with record
      Error             : Error_Id;
      Session           : Codefix_Module.Codefix_Session;
      Kernel            : Kernel_Handle;
      Session_Timestamp : Integer := 0;
   end record;
   overriding function Execute
     (Command : access Codefix_Command) return Command_Return_Type;
   overriding function Undo (Command : access Codefix_Command) return Boolean;

   type Codefix_Add_Command is new Root_Command with record
      Kernel            : Kernel_Handle;
      Session           : Codefix_Module.Codefix_Session;
      Current_Error     : Error_Id;
      Errors_Num        : Natural := 0;
      Errors_Fixed      : Natural := 0;
      Session_Timestamp : Integer := 0;
   end record;
   overriding function Execute
     (Command : access Codefix_Add_Command) return Command_Return_Type;
   overriding function Progress
     (Command : access Codefix_Add_Command) return Progress_Record;
   overriding function Name
     (Command : access Codefix_Add_Command) return String;

end Commands.Codefix;
