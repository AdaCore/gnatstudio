------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2003-2019, AdaCore                     --
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

with GPS.Kernel;         use GPS.Kernel;
with GNAT.Strings;       use GNAT.Strings;
with GNATCOLL.Projects;  use GNATCOLL.Projects;
with Gtk.GEntry;         use Gtk.GEntry;
with Naming_Exceptions;  use Naming_Exceptions;
with Project_Viewers;    use Project_Viewers;

package Custom_Naming_Editors is

   type Custom_Naming_Editor_Record is
     new Project_Editor_Page_Record
       (Flags => Multiple_Projects or Multiple_Scenarios) with private;
   type Custom_Naming_Editor is access all Custom_Naming_Editor_Record'Class;

   function Custom_Naming_Editor_Factory
     (Kernel : not null access Kernel_Handle_Record'Class;
      Lang   : String) return not null Project_Editor_Page;
   --  Create a new naming scheme editor for a language defined in XML.
   --  The profile is compatible with Register_Naming_Scheme_Editor

   overriding procedure Initialize
     (Self         : not null access Custom_Naming_Editor_Record;
      Kernel       : not null access Kernel_Handle_Record'Class;
      Read_Only    : Boolean;
      Project      : Project_Type := No_Project);
   overriding function Edit_Project
     (Self               : not null access Custom_Naming_Editor_Record;
      Project            : Project_Type;
      Kernel             : not null access Kernel_Handle_Record'Class;
      Languages          : GNAT.Strings.String_List;
      Scenario_Variables : Scenario_Variable_Array) return Boolean;
   overriding procedure Destroy (Self : in out Custom_Naming_Editor_Record);
   overriding function Is_Visible
     (Self         : not null access Custom_Naming_Editor_Record;
      Languages    : GNAT.Strings.String_List) return Boolean;

private

   type Custom_Naming_Editor_Record is
     new Project_Editor_Page_Record
       (Flags => Multiple_Projects or Multiple_Scenarios) with
   record
      Spec_Extension : Gtk.GEntry.Gtk_Entry;
      Impl_Extension : Gtk.GEntry.Gtk_Entry;
      Language       : GNAT.Strings.String_Access;
      Exceptions     : Naming_Exceptions.Exceptions_Editor;
   end record;
end Custom_Naming_Editors;
