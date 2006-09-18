-----------------------------------------------------------------------
--                          G P S                                    --
--                                                                   --
--                        Copyright (C) 2002-2006                    --
--                            AdaCore                                --
--                                                                   --
-- GPS is free software; you can redistribute it and/or modify  it   --
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

--  <description>
--  This package provides a properties editor for projects. It basically gives
--  access to various aspects of project files, like their name, the list of
--  executables,...
--  </description>

with Gtk.Box;
with Gtk.GEntry;
with GPS.Kernel;
with Projects;
with GNAT.OS_Lib;
with Commands.Interactive;
with Creation_Wizard;
with Wizards;

package Project_Properties is

   procedure Edit_Properties
     (Project : Projects.Project_Type;
      Kernel  : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Edit the properties for Project_View.

   type Project_Properties_Editor_Command
     is new Commands.Interactive.Interactive_Command with null record;
   function Execute
     (Command : access Project_Properties_Editor_Command;
      Context : Commands.Interactive.Interactive_Command_Context)
      return Commands.Command_Return_Type;
   --  Edit the properties of the project in Context. This is meant to be used
   --  as a callback for a contextual menu.
   --  Context.Context must be of type File_Selection_Context

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register the project properties module

   -----------------------
   -- Attribute editors --
   -----------------------

   function Attribute_Editors_Page_Count return Natural;
   --  Return the number of pages required to edit all the attributes of the
   --  project, not including the "General" page.
   --  The latter should always be added by the editors anyway, and thus isn't
   --  taken into account even if some XML attributes should be displayed in
   --  that page..

   function Attribute_Editors_Page_Name (Nth : Integer) return String;
   --  Return the name of the Nth page for editing attributes

   function Attribute_Editors_Page_Box
     (Kernel           : access GPS.Kernel.Kernel_Handle_Record'Class;
      Wiz              : Wizards.Wizard;
      Project          : Projects.Project_Type;
      General_Page_Box : Gtk.Box.Gtk_Box := null;
      Path_Widget      : access Gtk.GEntry.Gtk_Entry_Record'Class;
      Nth_Page         : Integer;
      Context          : String) return Creation_Wizard.Project_Wizard_Page;
   --  Return the nth page for editing attributes.
   --  Return null if the  given page contains no visible attribute.
   --
   --  Project is the project from which the value of the attributes is taken.
   --  If No_Project is specified, the default value for the attributes will be
   --  used.
   --  General_Page_Box is the box associated with the "General" page, so that
   --  new attributes can be put in it.
   --  Path_Widget is the widget that contains the location of the project file
   --
   --  Context is the name of the window that will display this box. For
   --  instance, it is "wizard" for the project wizard. This acts as a filter
   --  over which attributes will be displayed.
   --
   --  Wiz is the wizard in which the editor is displayed. It should be left to
   --  null if the editor is displayed in some other context like the project
   --  properties editor.

   function Get_Current_Value
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Pkg    : String;
      Name   : String;
      Index  : String := "")
      return GNAT.OS_Lib.String_List;
   --  Return the value of the attribute as currently edited.
   --  The returned value must be freed by the caller.
   --  ??? Relies on global variables, would be nice to eliminate through the
   --  use of the Project_Wizard_Page.

private
   type Root_Attribute_Editor_Record is abstract new Gtk.Box.Gtk_Box_Record
      with null record;
   --  An editor used to edit one specific attribute

   procedure Generate_Project
     (Editor             : access Root_Attribute_Editor_Record;
      Project            : Projects.Project_Type;
      Scenario_Variables : Projects.Scenario_Variable_Array;
      Project_Changed    : in out Boolean) is abstract;
   --  Generate the project entry for the attribute edited by the attribute.
   --  Project_Changed is set to True if the project is modified, unmodified
   --  otherwise.

   function Get_Value_As_String
     (Editor : access Root_Attribute_Editor_Record;
      Attribute_Index : String := "") return String is abstract;
   --  Return the current value in Editor for the given attribute_index. The
   --  latter is ignored if the attribute is in fact not indexed.

   function Get_Value_As_List
     (Editor          : access Root_Attribute_Editor_Record;
      Attribute_Index : String := "") return GNAT.OS_Lib.String_List
     is abstract;
   --  Return the current value in Editor for the given attribute_Index.
   --  The returned value must be freed by the user.

   function Is_Valid
     (Editor : access Root_Attribute_Editor_Record) return String;
   --  Whether the contents of the editor is valid for its project attribute.
   --  If invalid, an error message is returned, otherwise an empty string.

end Project_Properties;
