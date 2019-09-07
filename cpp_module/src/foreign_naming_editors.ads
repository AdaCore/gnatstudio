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

with GPS.Kernel;          use GPS.Kernel;
with Naming_Exceptions;
with GNAT.Strings;
with GNATCOLL.Projects;   use GNATCOLL.Projects;
with Gtk.Combo_Box_Text;
with Project_Viewers;     use Project_Viewers;

package Foreign_Naming_Editors is

   type Foreign_Naming_Editor_Record is
     new Project_Editor_Page_Record
       (Flags => Multiple_Projects or Multiple_Scenarios) with private;
   type Foreign_Naming_Editor is access all Foreign_Naming_Editor_Record'Class;

   function Naming_Editor_Factory
     (Kernel : not null access Kernel_Handle_Record'Class;
      Lang   : String) return not null Project_Editor_Page;
   --  Create a new naming scheme editor for languages other than Ada.
   --  The profile is compatible with Register_Naming_Scheme_Editor

   overriding procedure Initialize
     (Self         : not null access Foreign_Naming_Editor_Record;
      Kernel       : not null access Kernel_Handle_Record'Class;
      Read_Only    : Boolean;
      Project      : Project_Type := No_Project);
   overriding function Edit_Project
     (Self               : not null access Foreign_Naming_Editor_Record;
      Project            : Project_Type;
      Kernel             : not null access Kernel_Handle_Record'Class;
      Languages          : GNAT.Strings.String_List;
      Scenario_Variables : Scenario_Variable_Array) return Boolean;
   overriding procedure Destroy (Self : in out Foreign_Naming_Editor_Record);
   overriding function Is_Visible
     (Self         : not null access Foreign_Naming_Editor_Record;
      Languages    : GNAT.Strings.String_List) return Boolean;

private
   type Foreign_Naming_Editor_Record is
     new Project_Editor_Page_Record
       (Flags => Multiple_Projects or Multiple_Scenarios) with
   record
      Language   : GNAT.Strings.String_Access;
      Exceptions : Naming_Exceptions.Exceptions_Editor;
      Spec_Ext, Body_Ext : Gtk.Combo_Box_Text.Gtk_Combo_Box_Text;
   end record;
end Foreign_Naming_Editors;
