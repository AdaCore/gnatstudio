------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2002-2015, AdaCore                     --
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
with GPS.Kernel;
with Naming_Editors;
with Naming_Exceptions;
with GNAT.Strings;
with GNATCOLL.Projects;   use GNATCOLL.Projects;
with Gtk.Box;
with Gtk.Combo_Box_Text;

package Foreign_Naming_Editors is

   type Foreign_Naming_Editor_Record is new
     Naming_Editors.Language_Naming_Editor_Record with private;
   type Foreign_Naming_Editor is access all Foreign_Naming_Editor_Record'Class;

   procedure Gtk_New
     (Editor   : out Foreign_Naming_Editor;
      Language : String);
   --  Create a new naming scheme editor for languages other than Ada.

   overriding procedure Destroy (Editor : access Foreign_Naming_Editor_Record);
   overriding function Get_Window
     (Editor : access Foreign_Naming_Editor_Record)
      return Gtk.Widget.Gtk_Widget;
   overriding function Create_Project_Entry
     (Editor             : access Foreign_Naming_Editor_Record;
      Project            : Project_Type;
      Languages          : GNAT.Strings.String_List;
      Scenario_Variables : Scenario_Variable_Array) return Boolean;
   overriding procedure Show_Project_Settings
     (Editor             : access Foreign_Naming_Editor_Record;
      Kernel             : access GPS.Kernel.Kernel_Handle_Record'Class;
      Project            : Project_Type;
      Display_Exceptions : Boolean := True);
   --  See doc for inherited subprogram

private
   type Foreign_Naming_Editor_Record is new
     Naming_Editors.Language_Naming_Editor_Record
   with record
      GUI        : Gtk.Box.Gtk_Box;
      Language   : GNAT.Strings.String_Access;
      Exceptions : Naming_Exceptions.Exceptions_Editor;
      Spec_Ext, Body_Ext : Gtk.Combo_Box_Text.Gtk_Combo_Box_Text;
   end record;
end Foreign_Naming_Editors;
