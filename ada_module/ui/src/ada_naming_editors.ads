------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2012, AdaCore                     --
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

with Gtk.Widget;
with Gtk.GEntry;
with Naming_Scheme_Editor_Pkg; use Naming_Scheme_Editor_Pkg;
with Naming_Editors;
with GPS.Kernel;
with GNATCOLL.Projects;        use GNATCOLL.Projects;
with GNAT.Strings;

package Ada_Naming_Editors is

   type Ada_Naming_Editor_Record is new
     Naming_Editors.Language_Naming_Editor_Record with private;
   type Ada_Naming_Editor is access all Ada_Naming_Editor_Record'Class;

   procedure Gtk_New (Editor : out Ada_Naming_Editor);
   --  Create a new naming scheme editor.

   overriding procedure Destroy (Editor : access Ada_Naming_Editor_Record);
   overriding function Get_Window
     (Editor : access Ada_Naming_Editor_Record) return Gtk.Widget.Gtk_Widget;
   overriding function Create_Project_Entry
     (Editor             : access Ada_Naming_Editor_Record;
      Project            : Project_Type;
      Languages          : GNAT.Strings.String_List;
      Scenario_Variables : Scenario_Variable_Array) return Boolean;
   overriding procedure Show_Project_Settings
     (Editor             : access Ada_Naming_Editor_Record;
      Kernel             : access GPS.Kernel.Kernel_Handle_Record'Class;
      Project            : Project_Type;
      Display_Exceptions : Boolean := True);
   --  See doc for inherited subprogram

   ---------------
   -- Callbacks --
   ---------------
   --  The following subprograms are meant to be used only as callbacks for
   --  the GUI, and shouldn't be called directly.

   procedure Set_Predefined_Scheme
     (Editor : access Naming_Scheme_Editor_Record'Class;
      Scheme_Num : Natural);
   --  Changes all the fields in the GUI to the specified predefined scheme.
   --  The definition for Scheme_Num depends on the order of the entries in
   --  the combo box.

   procedure Add_New_Exception
     (Editor : access Naming_Scheme_Editor_Record'Class);
   --  Define a new exception based on the contents of the fields.
   --  If the unit name is the name of a unit already in the exceptions list,
   --  it is replaced.

   procedure Reset_Exception_Fields
     (Editor : access Naming_Scheme_Editor_Record'Class;
      Field  : Gtk.GEntry.Gtk_Entry := null);
   --  Put a default value in the entry fields for the exceptions. These
   --  values are used to provide help to the user.
   --  If Field is null, all fields are reset, otherwise only a specific one
   --  is.

   procedure Clear_Unit_Name
     (Editor : access Naming_Scheme_Editor_Record'Class);
   procedure Clear_Spec_Name
     (Editor : access Naming_Scheme_Editor_Record'Class);
   procedure Clear_Body_Name
     (Editor : access Naming_Scheme_Editor_Record'Class);
   --  Remove the text in one of the exception fields if it contains the
   --  default help value

private
   type Ada_Naming_Editor_Record is new
     Naming_Editors.Language_Naming_Editor_Record
   with record
      GUI : Naming_Scheme_Editor_Access;
   end record;
end Ada_Naming_Editors;
