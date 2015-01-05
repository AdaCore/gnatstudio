------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2010-2015, AdaCore                     --
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

with Gtk.Box;           use Gtk.Box;
with Gtk.Table;         use Gtk.Table;
with Gtk.Tree_Store;    use Gtk.Tree_Store;
with Gtk.Tree_View;     use Gtk.Tree_View;

with GNATCOLL.Projects; use GNATCOLL.Projects;

with GPS.Kernel;        use GPS.Kernel;

with Toolchains;

package Toolchains_Editor is

   type Toolchains_Edit_Record is new Gtk.Box.Gtk_Vbox_Record with private;
   type Toolchains_Edit is access all Toolchains_Edit_Record'Class;

   function Create_Language_Page
     (Project : Project_Type;
      Kernel  : access Kernel_Handle_Record'Class) return Toolchains_Edit;

   function Get_Languages (Editor : Toolchains_Edit)
     return GNAT.Strings.String_List_Access;

   function Generate_Project
     (Editor   : Toolchains_Edit;
      Project  : Project_Type;
      Scenarii : Scenario_Variable_Array) return Boolean;

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);

private

   type Toolchains_Edit_Record is new Gtk.Box.Gtk_Vbox_Record with record
      Kernel          : GPS.Kernel.Kernel_Handle;
      Languages       : Gtk_Tree_View;
      Lang_Model      : Gtk_Tree_Store;
      Toolchains_Tree : Gtk_Tree_View;
      Model           : Gtk_Tree_Store;
      Details_View    : Gtk_Table;
      Mgr             : Toolchains.Toolchain_Manager;
      Toolchain       : Toolchains.Toolchain := Toolchains.Null_Toolchain;
      Updating        : Boolean := False;
      Edited_Prj      : GNATCOLL.Projects.Project_Type;
   end record;

end Toolchains_Editor;
