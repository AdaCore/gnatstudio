-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                        Copyright (C) 2001                         --
--                            ACT-Europe                             --
--                                                                   --
-- GLIDE is free software; you can redistribute it and/or modify  it --
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
with Gtk.Window;
with Gtk.GEntry;
with Naming_Scheme_Editor_Pkg; use Naming_Scheme_Editor_Pkg;
with Prj.Tree;

package Naming_Editors is

   type Naming_Editor_Record is new Naming_Scheme_Editor_Record with private;
   type Naming_Editor is access all Naming_Editor_Record'Class;

   procedure Gtk_New (Editor : out Naming_Editor);
   --  Create a new naming scheme editor.

   function Edit_Naming_Scheme
     (Parent       : access Gtk.Window.Gtk_Window_Record'Class;
      Project      : Prj.Tree.Project_Node_Id;
      Project_View : Prj.Project_Id) return Boolean;
   --  Open a dialog to edit the naming scheme for Project (given one of its
   --  views). This dialog is modal, and needs to be closed before the user can
   --  do anything else with Glide.
   --  Return True if the project was modified.

   function Get_Window
     (Editor : access Naming_Editor_Record) return Gtk.Widget.Gtk_Widget;
   --  Return the window to use to insert the editor in a parent container.
   --  You should not use Editor itself, which is a top-level window.
   --  Likewise, you shouldn't call Show_All on the editor itself, but rather
   --  on the window.

   procedure Set_Predefined_Scheme
     (Editor : access Naming_Editor_Record;
      Scheme_Num : Natural);
   --  Changes all the fields in the GUI to the specified predefined scheme.
   --  The definition for Scheme_Num depends on the order of the entries in
   --  the combo box.

   procedure Create_Project_Entry
     (Editor  : access Naming_Editor_Record;
      Project : Prj.Tree.Project_Node_Id);
   --  Create a new entry in the project file Project for the naming scheme
   --  defined in the editor.

   procedure Show_Project_Settings
     (Editor : access Naming_Editor_Record; Project_View : Prj.Project_Id);
   --  Show the settings used for Project_View

   ---------------
   -- Callbacks --
   ---------------
   --  The following subprograms are meant to be used only as callbacks for
   --  the GUI, and shouldn't be called directly.

   procedure Add_New_Exception (Editor : access Naming_Editor_Record);
   --  Define a new exception based on the contents of the fields.
   --  If the unit name is the name of a unit already in the exceptions list,
   --  it is replaced.

   procedure Reset_Exception_Fields
     (Editor : access Naming_Editor_Record'Class;
      Field  : Gtk.GEntry.Gtk_Entry := null);
   --  Put a default value in the entry fields for the exceptions. These
   --  values are used to provide help to the user.
   --  If Field is null, all fields are reset, otherwise only a specific one
   --  is.

   procedure Clear_Unit_Name (Editor : access Naming_Editor_Record);
   procedure Clear_Spec_Name (Editor : access Naming_Editor_Record);
   procedure Clear_Body_Name (Editor : access Naming_Editor_Record);
   --  Remove the text in one of the exception fields if it contains the
   --  default help value

private
   type Naming_Editor_Record is new Naming_Scheme_Editor_Record with record
      null;
   end record;
end Naming_Editors;
