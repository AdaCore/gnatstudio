-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                    Copyright (C) 2004-2008, AdaCore               --
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
with GPS.Kernel;
with Projects;
with Gtk.Widget;
with Gtk.Handlers;
with GNATCOLL.VFS;

package Creation_Wizard is

   type Project_Wizard_Page_Record is abstract
      new Wizards.Wizard_Page_Record with null record;
   type Project_Wizard_Page is access all Project_Wizard_Page_Record'Class;

   procedure Generate_Project
     (Page    : access Project_Wizard_Page_Record;
      Kernel  : access GPS.Kernel.Kernel_Handle_Record'Class;
      Scenario_Variables : Projects.Scenario_Variable_Array;
      Project : in out Projects.Project_Type;
      Changed : in out Boolean) is abstract;
   --  This function is called when the user has pressed Finish in the wizard.
   --  It should update the project's attributes as per the settings in the
   --  page.
   --  Changed is set to True if some modification was actually done, left
   --  unchanged otherwise.
   --  Raise Invalid_Project_Page to cancel the generation of the project

   Invalid_Project_Page : exception;

   type Project_Wizard_Record is
      new Wizards.Wizard_Record with private;
   type Project_Wizard is access all Project_Wizard_Record'Class;

   procedure Gtk_New
     (Wiz               : out Project_Wizard;
      Kernel            : access GPS.Kernel.Kernel_Handle_Record'Class;
      Title             : String;
      Show_Toc          : Boolean := True;
      Auto_Save_On_Exit : Boolean := True;
      Project           : Projects.Project_Type := Projects.No_Project);
   --  Create a new project wizard, or edit an existing one if Project is
   --  specified.
   --  All pages added to the wizard must be children of
   --  Project_Wizard_Page_Record.
   --  The project is not loaded automatically on exit;

   procedure Initialize
     (Wiz               : access Project_Wizard_Record'Class;
      Kernel            : access GPS.Kernel.Kernel_Handle_Record'Class;
      Title             : String;
      Show_Toc          : Boolean := True;
      Auto_Save_On_Exit : Boolean := True;
      Project           : Projects.Project_Type := Projects.No_Project);
   --  Initialize a new project wizard.

   function Run
     (Wiz : access Project_Wizard_Record) return GNATCOLL.VFS.Virtual_File;
   --  Display the dialog, let the user interact with it, and return the name
   --  of the project that was created (and not loaded).
   --  The empty string is returned if the user pressed Cancel.

   function Get_Project
     (Wiz : access Project_Wizard_Record'Class) return Projects.Project_Type;
   --  Return the project being modified.
   --  Most of the time, this will be null, except when the wizard is used to
   --  edit an existing project.
   --  Once the wizard has been completed (and before it is being destroyed),
   --  this will return the newly created project.

   type Name_And_Location_Page is new Project_Wizard_Page_Record with private;
   type Name_And_Location_Page_Access
     is access all Name_And_Location_Page'Class;
   --  See inherited documentation.
   --  This page must be the first in a project wizard, and is responsible for
   --  creating the project itself (the parameter it gets passed is No_Project)
   --  This page adds checks to make sure the name of the project is valid.

   function Get_Path_Widget
     (Page : access Name_And_Location_Page) return Gtk.GEntry.Gtk_Entry;
   --  Return the widget that contains the currently set path for the project

   function Get_Name_Widget
     (Page : access Name_And_Location_Page) return Gtk.GEntry.Gtk_Entry;
   --  Return the widget that contains the name of the project

   function Add_Name_And_Location_Page
     (Wiz                 : access Project_Wizard_Record'Class;
      Force_Relative_Dirs : Boolean := False)
      return Name_And_Location_Page_Access;
   --  Add a new page for editing the name and location of a project.
   --  If Force_Relative_Dirs is False, then an extra button is added so that
   --  the user can choose whether paths should be relative or absolute.

   -------------------
   -- Gtk interface --
   -------------------

   package Page_Handlers is new Gtk.Handlers.User_Callback
     (Gtk.Widget.Gtk_Widget_Record, Project_Wizard_Page);

private

   overriding procedure Perform_Finish (Wiz : access Project_Wizard_Record);

   overriding function Is_Complete
     (Page : access Name_And_Location_Page) return String;
   overriding function Create_Content
     (Page : access Name_And_Location_Page;
      Wiz  : access Wizards.Wizard_Record'Class) return Gtk.Widget.Gtk_Widget;
   overriding procedure Generate_Project
     (Page               : access Name_And_Location_Page;
      Kernel             : access GPS.Kernel.Kernel_Handle_Record'Class;
      Scenario_Variables : Projects.Scenario_Variable_Array;
      Project            : in out Projects.Project_Type;
      Changed            : in out Boolean);
   --  See inherited doc

   type Name_And_Location_Page is new Project_Wizard_Page_Record with record
      Project_Name        : Gtk.GEntry.Gtk_Entry;
      Project_Location    : Gtk.GEntry.Gtk_Entry;
      Relative_Paths      : Gtk.Check_Button.Gtk_Check_Button;
      Kernel              : GPS.Kernel.Kernel_Handle;
      Force_Relative_Dirs : Boolean;
   end record;

   type Project_Wizard_Record is new Wizards.Wizard_Record with
      record
         Project : Projects.Project_Type;
         Auto_Save_On_Exit : Boolean;
      end record;

end Creation_Wizard;
