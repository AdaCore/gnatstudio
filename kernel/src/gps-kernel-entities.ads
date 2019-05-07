------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2014-2019, AdaCore                     --
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

--  This package provides a number of contextual menus and script commands
--  related to entities. This is the dual of gps-scripts-entities, but
--  these subprograms depend on the various GPS kernel services.

with Commands.Interactive;          use Commands, Commands.Interactive;

package GPS.Kernel.Entities is

   type Find_All_Refs_Command is new Interactive_Command with record
      Locals_Only     : Boolean := False;
      Recurse_Project : Boolean := True;
      Writes_Only     : Boolean := False;
      Reads_Only      : Boolean := False;
   end record;
   overriding function Execute
     (Command : access Find_All_Refs_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

   type Find_Specific_Refs_Command
   is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Find_Specific_Refs_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

   procedure Find_All_Refs
     (Kernel   : Kernel_Handle;
      Entity   : Xref.Root_Entity'Class;
      Implicit : Boolean);
   --  Implement 'find_all_refs' for python API
   --  Temporary, until LSP is not activated

   procedure Register_Module
      (Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register contextual menu and commands

end GPS.Kernel.Entities;
