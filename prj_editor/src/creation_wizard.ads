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

with Gtk.GEntry;
with Gtk.Check_Button;

with Wizards;
with Glide_Kernel;
with Projects;

package Creation_Wizard is

   type Wizard_Base_Record is abstract new Wizards.Wizard_Record with private;
   type Wizard_Base is access all Wizard_Base_Record'Class;

   procedure Initialize
     (Wiz                 : access Wizard_Base_Record'Class;
      Kernel              : access Glide_Kernel.Kernel_Handle_Record'Class;
      Force_Relative_Dirs : Boolean := False;
      Ask_About_Loading   : Boolean := False);
   --  Initialize a new basic wizard.
   --  It has a single page, to select the name and location of the project
   --  (the user must provide both).
   --  If Force_Relative_Dirs is False, then an extra button is added so that
   --  the user can choose whether paths should be relative or absolute.
   --  If Ask_About_Loading is true, then an extra page is appended at the end
   --  of the wizard, and the user can select whether to load the project
   --  immediately or not. Otherwise, the project is not loaded.

   function Run (Wiz : access Wizard_Base_Record'Class) return String;
   --  Run the wizard and report the directory/name of the project that was
   --  created. The empty string is returned if the wizard was cancelled.
   --  Note that in this mode the wizard is modal.
   --  The wizard is destroyed on exit

   procedure Generate_Project
     (Wiz     : access Wizard_Base_Record;
      Project : in out Projects.Project_Type) is abstract;
   --  This function is called once the project file itself has been created,
   --  so that children of Wizard_Base_Record can add their own setup to
   --  the project.
   --  Project is the project being created, which hasn't been saved on the
   --  disk yet at that point.

private

   type Wizard_Base_Record is abstract new Wizards.Wizard_Record with record
      Project_Name      : Gtk.GEntry.Gtk_Entry;
      Project_Location  : Gtk.GEntry.Gtk_Entry;
      Relative_Paths    : Gtk.Check_Button.Gtk_Check_Button;
      Kernel            : Glide_Kernel.Kernel_Handle;
      Ask_About_Loading : Boolean;
   end record;

end Creation_Wizard;
