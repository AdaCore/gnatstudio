-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2002                            --
--                            ACT-Europe                             --
--                                                                   --
-- GPS is free  software; you  can redistribute it and/or modify  it --
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

with Gtk.Widget;
with Foreign_Naming_Scheme_Editor_Pkg; use Foreign_Naming_Scheme_Editor_Pkg;
with Types;
with Projects;
with Glide_Kernel;

package Foreign_Naming_Editors is

   type Foreign_Naming_Editor_Record is
     new Foreign_Naming_Scheme_Editor_Record with private;
   type Foreign_Naming_Editor is access all Foreign_Naming_Editor_Record'Class;

   procedure Gtk_New
     (Editor   : out Foreign_Naming_Editor;
      Language : Types.Name_Id);
   --  Create a new naming scheme editor for languages other than Ada.

   function Get_Window
     (Editor : access Foreign_Naming_Editor_Record)
      return Gtk.Widget.Gtk_Widget;
   --  Return the window to use to insert the editor in a parent container.
   --  You should not use Editor itself, which is a top-level window.
   --  Likewise, you shouldn't call Show_All on the editor itself, but rather
   --  on the window.

   function Create_Project_Entry
     (Editor  : access Foreign_Naming_Editor_Record;
      Project : Projects.Project_Type;
      Scenario_Variables : Projects.Scenario_Variable_Array) return Boolean;
   --  Create a new entry in the project file Project for the naming scheme
   --  defined in the editor.
   --  Project_View can be No_Project.
   --  Return if the project was changed.

   procedure Show_Project_Settings
     (Editor             : access Foreign_Naming_Editor_Record;
      Kernel             : access Glide_Kernel.Kernel_Handle_Record'Class;
      Project            : Projects.Project_Type;
      Display_Exceptions : Boolean := True);
   --  Show the settings used for Project_View, and the language Language.
   --  If Display_Exceptions is False, then the files in the exception list
   --  will not be displayed, only the suffixes will be. This is intended to be
   --  used when creating new projects based on an existing one.
   --  If Project_View is No_Project, then the default settings are displayed.

private
   type Foreign_Naming_Editor_Record is
     new Foreign_Naming_Scheme_Editor_Record with
   record
      Language : Types.Name_Id;
   end record;
end Foreign_Naming_Editors;
