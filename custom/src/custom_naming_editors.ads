-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2003                            --
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
with GNAT.OS_Lib;
with Gtk.Box;
with Gtk.GEntry;
with Naming_Exceptions;

package Custom_Naming_Editors is

   type Custom_Naming_Editor_Record is new
     Naming_Editors.Language_Naming_Editor_Record with private;
   type Custom_Naming_Editor is access all Custom_Naming_Editor_Record'Class;

   procedure Gtk_New
     (Editor   : out Custom_Naming_Editor;
      Kernel   : access Glide_Kernel.Kernel_Handle_Record'Class;
      Language : String);
   --  Create a new naming scheme editor for a custom language defined in XML.

   procedure Destroy (Editor : access Custom_Naming_Editor_Record);
   function Get_Window
     (Editor : access Custom_Naming_Editor_Record)
      return Gtk.Widget.Gtk_Widget;
   function Create_Project_Entry
     (Editor  : access Custom_Naming_Editor_Record;
      Project : Projects.Project_Type;
      Scenario_Variables : Projects.Scenario_Variable_Array) return Boolean;
   procedure Show_Project_Settings
     (Editor             : access Custom_Naming_Editor_Record;
      Kernel             : access Glide_Kernel.Kernel_Handle_Record'Class;
      Project            : Projects.Project_Type;
      Display_Exceptions : Boolean := True);
   --  See doc for inherited subprogram

private
   type Custom_Naming_Editor_Record is new
     Naming_Editors.Language_Naming_Editor_Record
   with record
      GUI        : Gtk.Box.Gtk_Box;
      Extension  : Gtk.GEntry.Gtk_Entry;
      Language   : GNAT.OS_Lib.String_Access;
      Exceptions : Naming_Exceptions.Exceptions_Editor;
   end record;
end Custom_Naming_Editors;
