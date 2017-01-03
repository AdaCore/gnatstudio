------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2004-2017, AdaCore                     --
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

--  This package provides a dialog to help the user choose among the
--  several project wizards provided by GPS

with GPS.Kernel;
with Commands.Interactive;  use Commands, Commands.Interactive;

package Creation_Wizard.Selector is

   function Create_New_Project
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
      return Boolean;
   --  Open a new dialog that lists all the possible wizards provided by
   --  GPS. The user can then immediately start creating a project.
   --  Return True if a project could be created, False if there was an error
   --  or the user cancelled the operation.

   type New_Project_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access New_Project_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Same as above, but suitable for use from a callback.

end Creation_Wizard.Selector;
