------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2002-2025, AdaCore                     --
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
with GNATCOLL.Projects;   use GNATCOLL.Projects;
with GPS.Kernel;
with GNAT.Strings;

package Project_Properties is

   function Paths_Are_Relative (Project : Project_Type) return Boolean;
   --  Return True if the paths in the project should be relative paths

   procedure Register_Module_Reader
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register the commands to retrieve the project properties

   procedure Register_Module_Writer
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register the project properties module

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
