-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2004                       --
--                             AdaCore                               --
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

with Glide_Kernel;

package Creation_Wizard.Full is

   type Prj_Wizard_Record is new Project_Wizard_Record with private;
   type Prj_Wizard is access all Prj_Wizard_Record'Class;

   procedure Gtk_New
     (Wiz               : out Prj_Wizard;
      Kernel            : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Create a new project wizard.
   --  New pages can be added at will with Add_Page and through XML files.
   --  Default values for the various pages are taken from the project
   --  currently loaded in Kernel.

   procedure Initialize
     (Wiz               : access Prj_Wizard_Record'Class;
      Kernel            : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Internal function for the creation of a new wizard

private
   type Prj_Wizard_Record is new Project_Wizard_Record with null record;
end Creation_Wizard.Full;
