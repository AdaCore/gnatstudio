-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2004                            --
--                            AdaCore                                --
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

--  A wizard to create some .gpr files from a .adp file

with Glide_Kernel;
with Gtk.GEntry;

package Creation_Wizard.Adp is

   type Adp_Wizard_Record is new Wizard_Base_Record with private;
   type Adp_Wizard is access all Adp_Wizard_Record'Class;

   procedure Gtk_New
     (Wiz    : out Adp_Wizard;
      Kernel : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Create a new simple project creation, which creates a set of projects
   --  given a set of object directories and source directories.

   procedure Initialize
     (Wiz    : access Adp_Wizard_Record'Class;
      Kernel : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Internal function for the creation of a new wizard

   procedure Generate_Project
     (Wiz     : access Adp_Wizard_Record;
      Project : in out Projects.Project_Type);
   --  Generate additional attributes for the project, as well as other
   --  projects if needed

private
   type Adp_Wizard_Record is new Wizard_Base_Record with record
      Adp_File_Name : Gtk.GEntry.Gtk_Entry;
   end record;
end Creation_Wizard.Adp;
