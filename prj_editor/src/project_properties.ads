-----------------------------------------------------------------------
--                          G P S                                    --
--                                                                   --
--                        Copyright (C) 2002-2004                    --
--                            ACT-Europe                             --
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

with Glib.Object;
with Glide_Kernel;
with Gtk.Box;
with Projects;
with GNAT.OS_Lib;

package Project_Properties is

   procedure Edit_Properties
     (Project : Projects.Project_Type;
      Kernel  : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Edit the properties for Project_View.

   procedure Edit_Project_Properties
     (Widget  : access Glib.Object.GObject_Record'Class;
      Context : Glide_Kernel.Selection_Context_Access);
   --  Edit the properties of the project in Context. This is meant to be used
   --  as a callback for a contextual menu.
   --  Context.all must be of type File_Selection_Context

   procedure Register_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Register the project properties module

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
   --  latter is ignored if the attribute is in fact not indexed

   function Get_Value_As_List
     (Editor          : access Root_Attribute_Editor_Record;
      Attribute_Index : String := "") return GNAT.OS_Lib.String_List
     is abstract;
   --  Return the current value in Editor for the given attribute_Index.
   --  The returned value must be freed by the user

end Project_Properties;
