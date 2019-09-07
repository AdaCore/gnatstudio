------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2004-2019, AdaCore                     --
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

with GNAT.Strings;
with GNATCOLL.Projects; use GNATCOLL.Projects;

with Gtk.Tree_View;     use Gtk.Tree_View;

with GPS.Kernel;        use GPS.Kernel;
with Project_Viewers;   use Project_Viewers;

package Project_Dependencies_Editors is

   type Project_Dependencies_Editor_Record is new Project_Editor_Page_Record
     with private;
   type Project_Dependencies_Editor is
     access all Project_Dependencies_Editor_Record'Class;

   overriding procedure Initialize
     (Self      : not null access Project_Dependencies_Editor_Record;
      Kernel    : not null access Kernel_Handle_Record'Class;
      Read_Only : Boolean;
      Project   : Project_Type := No_Project);
   overriding function Edit_Project
     (Self               : not null access Project_Dependencies_Editor_Record;
      Project            : Project_Type;
      Kernel             : not null access Kernel_Handle_Record'Class;
      Languages          : GNAT.Strings.String_List;
      Scenario_Variables : Scenario_Variable_Array) return Boolean;

private

   type Project_Dependencies_Editor_Record is new Project_Editor_Page_Record
   with record
      Kernel              : Kernel_Handle;
      Project             : Project_Type;
      Dependencies_Tree   : Gtk_Tree_View;
      Known_Projects_Tree : Gtk_Tree_View;
   end record;

end Project_Dependencies_Editors;
