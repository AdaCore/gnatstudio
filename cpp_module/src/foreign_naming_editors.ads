-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2002-2003                       --
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
with Projects;
with Glide_Kernel;
with Naming_Editors;
with Naming_Exceptions;
with GNAT.OS_Lib;
with Gtk.Box;
with Gtk.Combo;

package Foreign_Naming_Editors is

   type Foreign_Naming_Editor_Record is new
     Naming_Editors.Language_Naming_Editor_Record with private;
   type Foreign_Naming_Editor is access all Foreign_Naming_Editor_Record'Class;

   procedure Gtk_New
     (Editor   : out Foreign_Naming_Editor;
      Language : String);
   --  Create a new naming scheme editor for languages other than Ada.

   procedure Destroy (Editor : access Foreign_Naming_Editor_Record);
   function Get_Window
     (Editor : access Foreign_Naming_Editor_Record)
      return Gtk.Widget.Gtk_Widget;
   function Create_Project_Entry
     (Editor  : access Foreign_Naming_Editor_Record;
      Project : Projects.Project_Type;
      Scenario_Variables : Projects.Scenario_Variable_Array) return Boolean;
   procedure Show_Project_Settings
     (Editor             : access Foreign_Naming_Editor_Record;
      Kernel             : access Glide_Kernel.Kernel_Handle_Record'Class;
      Project            : Projects.Project_Type;
      Display_Exceptions : Boolean := True);
   --  See doc for inherited subprogram

private
   type Foreign_Naming_Editor_Record is new
     Naming_Editors.Language_Naming_Editor_Record
   with record
      GUI        : Gtk.Box.Gtk_Box;
      Language   : GNAT.OS_Lib.String_Access;
      Exceptions : Naming_Exceptions.Exceptions_Editor;
      Spec_Ext, Body_Ext : Gtk.Combo.Gtk_Combo;
   end record;
end Foreign_Naming_Editors;
