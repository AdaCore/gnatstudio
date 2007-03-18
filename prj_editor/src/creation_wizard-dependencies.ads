-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2004-2007                      --
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
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Commands.Interactive;

package Creation_Wizard.Dependencies is

   ---------------------
   -- Wizards support --
   ---------------------

   procedure Add_Project_Dependencies_Page
     (Wiz : access Project_Wizard_Record'Class);
   --  Add a new page to allow editing the dependencies with other projects

   -------------
   -- Actions --
   -------------

   type Project_Dependency_Wizard_Command
     is new Commands.Interactive.Interactive_Command with null record;
   --  Opens and run a wizard for editing the dependencies for a project

private
   function Execute
     (Command : access Project_Dependency_Wizard_Command;
      Context : Commands.Interactive.Interactive_Command_Context)
      return Commands.Command_Return_Type;
   --  See inherited documentation
end Creation_Wizard.Dependencies;
