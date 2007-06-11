-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2002-2006                       --
--                             AdaCore                               --
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

with Gtk.Notebook;
with GPS.Kernel;
with GNAT.Strings;
with Projects;
with Gtk.Widget;

package Naming_Editors is

   -------------------
   -- Naming editor --
   -------------------
   --  The following widget is a multi-language naming editor.

   type Naming_Editor_Record is new Gtk.Notebook.Gtk_Notebook_Record
     with private;
   type Naming_Editor is access all Naming_Editor_Record'Class;

   procedure Gtk_New
     (Editor    : out Naming_Editor;
      Kernel    : access GPS.Kernel.Kernel_Handle_Record'Class;
      Languages : GNAT.Strings.String_List);
   --  Create a new naming scheme editor, that supports the edition of all the
   --  languages in Languages.
   --  It is the responsability of the caller to free Languages.

   procedure Gtk_New
     (Editor       : out Naming_Editor;
      Kernel       : access GPS.Kernel.Kernel_Handle_Record'Class;
      Project      : Projects.Project_Type);
   --  Create a new naming scheme editor, that edits the languages supported by
   --  Project_View.

   function Create_Project_Entry
     (Editor          : access Naming_Editor_Record;
      Project         : Projects.Project_Type;
      Languages       : GNAT.Strings.String_List;
      Scenario_Variables : Projects.Scenario_Variable_Array) return Boolean;
   --  Create a new entry in the project file Project for the naming scheme
   --  defined in the editor.
   --  Return True if the project was changed.
   --  If Ignore_Scenario is True, then the entries will be created in the
   --  common section of the normalized project rather than in the case
   --  constructions for the scenario.
   --  Project_View represents the current view, and can be No_Project.

   procedure Show_Project_Settings
     (Editor             : access Naming_Editor_Record;
      Kernel             : access GPS.Kernel.Kernel_Handle_Record'Class;
      Project            : Projects.Project_Type;
      Display_Exceptions : Boolean := True);
   --  Show the settings used for Project_View.
   --  Note that only the languages that were given to Gtk_New will be
   --  editable.
   --  If Display_Exceptions is False, then the files in the exception list
   --  will not be displayed, only the suffixes will be. This is intended to be
   --  used when creating new projects based on an existing one.

   procedure Set_Visible_Pages
     (Editor       : access Naming_Editor_Record;
      Kernel       : access GPS.Kernel.Kernel_Handle_Record'Class;
      Languages    : GNAT.Strings.String_List;
      Project      : Projects.Project_Type);
   --  Change the visible pages in editor, based on languages

   ----------------------------
   -- Language naming editor --
   ----------------------------
   --  The following widget describes the naming scheme editor for a single
   --  language.

   type Language_Naming_Editor_Record is abstract tagged private;
   type Language_Naming_Editor is
     access all Language_Naming_Editor_Record'Class;

   procedure Destroy (Editor : access Language_Naming_Editor_Record)
      is abstract;
   --  Free the memory used by the editor (but not Editor itself).

   function Get_Window
     (Editor : access Language_Naming_Editor_Record)
      return Gtk.Widget.Gtk_Widget is abstract;
   --  Return the window to insert in the multi-language editor.

   function Create_Project_Entry
     (Editor             : access Language_Naming_Editor_Record;
      Project            : Projects.Project_Type;
      Languages          : GNAT.Strings.String_List;
      Scenario_Variables : Projects.Scenario_Variable_Array) return Boolean
      is abstract;
   --  Create a new entry in the project file Project for the naming scheme
   --  defined in Editor.
   --  Project can be No_Project.
   --  Return True if the project was changed.

   procedure Show_Project_Settings
     (Editor             : access Language_Naming_Editor_Record;
      Kernel             : access GPS.Kernel.Kernel_Handle_Record'Class;
      Project            : Projects.Project_Type;
      Display_Exceptions : Boolean := True) is abstract;
   --  Show the settings used for Project for the specific language.
   --  If Display_Exceptions is False, then the files in the exception list
   --  will not be displayed, only the suffixes will be. This is intended to be
   --  used when creating new projects based on an existing one.
   --  If Project is No_Project, then the default settings are displayed.

private
   type Language_Naming is record
      Language       : GNAT.Strings.String_Access;
      Naming         : Language_Naming_Editor;
      Is_Visible     : Boolean;
   end record;

   type Language_Naming_Editor_Record is abstract tagged null record;

   type Language_Naming_Array is array (Natural range <>) of Language_Naming;
   type Language_Naming_Array_Access is access Language_Naming_Array;

   type Naming_Editor_Record is new Gtk.Notebook.Gtk_Notebook_Record
     with record
        Pages : Language_Naming_Array_Access;
     end record;
end Naming_Editors;
