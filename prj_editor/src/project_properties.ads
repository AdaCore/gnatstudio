------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2002-2019, AdaCore                     --
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

--  <description>
--  This package provides a properties editor for projects. It basically gives
--  access to various aspects of project files, like their name, the list of
--  executables,...
--  </description>

with Gtk.Box;
with Gtk.GEntry;
with GNATCOLL.Projects;   use GNATCOLL.Projects;
with GPS.Kernel;
with GNAT.Strings;
with Commands.Interactive;
with Project_Viewers;     use Project_Viewers;

package Project_Properties is

   procedure Edit_Properties
     (Project : Project_Type;
      Kernel  : access GPS.Kernel.Kernel_Handle_Record'Class;
      Name    : String := "");
   --  Edit the properties for Project_View.
   --  If Name is set and a corresponding Page exists then select it.

   type Project_Properties_Editor_Command
     is new Commands.Interactive.Interactive_Command with null record;
   overriding function Execute
     (Command : access Project_Properties_Editor_Command;
      Context : Commands.Interactive.Interactive_Command_Context)
      return Commands.Command_Return_Type;
   --  Edit the properties of the project in Context. This is meant to be used
   --  as a callback for a contextual menu.
   --  Context.Context must be of type File_Selection_Context

   procedure Register_Module_Reader
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register the commands to retrieve the project properties

   procedure Register_Module_Writer
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register the project properties module

   -----------------------
   -- Attribute editors --
   -----------------------

   procedure For_Each_Project_Editor_Page
     (Kernel    : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      Project   : Project_Type;
      Path      : not null access Gtk.GEntry.Gtk_Entry_Record'Class;
      Context   : String := "properties";
      Read_Only : Boolean;
      Callback  : not null access procedure
        (Title : String;
         Page  : not null access Project_Editor_Page_Record'Class));
   --  For each project editor page.
   --  Path is the widget used to edit the project's path, which is used to
   --  resolve relative names.
   --  The Page has already been initialized via a call to Create.
   --  Context explains where the page is used, and impacts which attributes
   --  are displayed.

   function Get_Current_Value
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Pkg    : String;
      Name   : String;
      Index  : String := "")
      return GNAT.Strings.String_List_Access;
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
      Project            : Project_Type;
      Scenario_Variables : Scenario_Variable_Array;
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
      Attribute_Index : String := "") return GNAT.Strings.String_List
      is abstract;
   --  Return the current value in Editor for the given attribute_Index.
   --  The returned value must be freed by the user.

   function Is_Valid
     (Editor : access Root_Attribute_Editor_Record) return String;
   --  Whether the contents of the editor is valid for its project attribute.
   --  If invalid, an error message is returned, otherwise an empty string.

end Project_Properties;
