------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                       Copyright (C) 2014-2018, AdaCore                   --
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
--  Interactive actions that is used by GNAThub module and their registration.

private with Commands.Interactive;

with GNAThub.Module; use GNAThub.Module;

package GNAThub.Actions is

   procedure Register_Actions (Module : not null GNAThub_Module_Id);
   --  Registers interactive commands of GNAThub module.

private

   use Commands;
   use Commands.Interactive;

   type GNAThub_Interactive_Command
     (Module : not null access GNAThub_Module_Id_Record'Class)
      is abstract new Interactive_Command with null record;

   type Display_Command is new GNAThub_Interactive_Command with null record;
   overriding function Execute
     (Self    : access Display_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Called when "Display Data..." menu item is activated

   type Remove_Database_Command is
     new GNAThub_Interactive_Command with null record;
   overriding function Execute
     (Self    : access Remove_Database_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Clean the GNAThub module by deleting the GNAThub database and its
   --  associated messages.

end GNAThub.Actions;
