-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2003                       --
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
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Gtk.GEntry;
with Gtk.Check_Button;

with Wizards;
with Glide_Kernel;
with Languages_Lists;

package Creation_Wizard is

   type Prj_Wizard_Record is new Wizards.Wizard_Record with private;
   type Prj_Wizard is access all Prj_Wizard_Record'Class;

   procedure Gtk_New
     (Wiz    : out Prj_Wizard;
      Kernel : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Create a new project wizard.
   --  New pages can be added at will with Add_Page
   --  Default values for the various pages are taken from the project
   --  currently loaded in Kernel.

   procedure Initialize
     (Wiz    : access Prj_Wizard_Record'Class;
      Kernel : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Internal function for the creation of a new wizard

   function Run (Wiz : access Prj_Wizard_Record) return String;
   --  Run the wizard and report the directory/name of the project that was
   --  created. The empty string is returned if the wizard was cancelled.
   --  Note that in this mode the wizard is modal.
   --  The wizard is not destroyed on exit, it is your responsability to do
   --  so. This allows you to access the extra pages you might have added to
   --  the wizard with Add_Page.


private

   type Prj_Wizard_Record is new Wizards.Wizard_Record with record
      Project_Name      : Gtk.GEntry.Gtk_Entry;
      Project_Location  : Gtk.GEntry.Gtk_Entry;
      Languages         : Languages_Lists.Languages_List;
      Language_Changed  : Boolean := True;
      Relative_Paths    : Gtk.Check_Button.Gtk_Check_Button;
      Kernel            : Glide_Kernel.Kernel_Handle;
   end record;

end Creation_Wizard;
