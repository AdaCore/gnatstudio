------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2001-2019, AdaCore                     --
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

with GNATCOLL.Projects;  use GNATCOLL.Projects;
with GNAT.Strings;

with Gtk;                use Gtk;
with Gtk.Combo_Box_Text; use Gtk.Combo_Box_Text;
with Gtk.Tree_Store;     use Gtk.Tree_Store;
with Gtk.Tree_View;      use Gtk.Tree_View;

with Dialog_Utils;       use Dialog_Utils;
with GPS.Kernel;         use GPS.Kernel;
with Project_Viewers;    use Project_Viewers;

package Ada_Naming_Editors is

   -----------------------
   -- Ada Naming Editor --
   -----------------------

   Default_Gnat_Dot_Replacement : constant String := "-";
   Default_Gnat_Spec_Suffix     : constant String := ".ads";
   Default_Gnat_Body_Suffix     : constant String := ".adb";
   Default_Gnat_Separate_Suffix : constant String := ".adb";
   --  Default settings for the GNAT naming scheme.

   type Ada_Naming_Editor_Record is
     new Project_Editor_Page_Record
       (Flags => Multiple_Projects or Multiple_Scenarios) with private;
   type Ada_Naming_Editor is access all Ada_Naming_Editor_Record'Class;
   --  Type used to create the 'Naming/Ada' page in the project properties
   --  editor.

   overriding procedure Initialize
     (Self         : not null access Ada_Naming_Editor_Record;
      Kernel       : not null access Kernel_Handle_Record'Class;
      Read_Only    : Boolean;
      Project      : Project_Type := GNATCOLL.Projects.No_Project);
   overriding function Edit_Project
     (Editor             : not null access Ada_Naming_Editor_Record;
      Project            : Project_Type;
      Kernel             : not null access Kernel_Handle_Record'Class;
      Languages          : GNAT.Strings.String_List;
      Scenario_Variables : Scenario_Variable_Array) return Boolean;
   overriding function Is_Visible
     (Self      : not null access Ada_Naming_Editor_Record;
      Languages : GNAT.Strings.String_List) return Boolean;

private

   type Ada_Naming_Editor_Record is
     new Project_Editor_Page_Record
       (Flags => Multiple_Projects or Multiple_Scenarios)
   with record
      Standard_Scheme           : Gtk_Combo_Box_Text;
      Spec_Extension            : Gtk_Combo_Box_Text;
      Body_Extension            : Gtk_Combo_Box_Text;
      Separate_Extension        : Gtk_Combo_Box_Text;
      Dot_Replacement           : Gtk_Combo_Box_Text;
      Casing                    : Gtk_Combo_Box_Text;
      Exception_List_View       : Dialog_View_With_Button_Box;
      Exception_List            : Gtk.Tree_View.Gtk_Tree_View;
      Exception_List_Model      : Gtk.Tree_Store.Gtk_Tree_Store;
   end record;
end Ada_Naming_Editors;
