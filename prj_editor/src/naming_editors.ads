-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2002                            --
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

with Gtk.Notebook;
with Gtk.Window;
with Glide_Kernel;
with Foreign_Naming_Editors;
with Ada_Naming_Editors;
with GNAT.OS_Lib;
with Prj.Tree;

package Naming_Editors is

   type Naming_Editor_Record is new Gtk.Notebook.Gtk_Notebook_Record
     with private;
   type Naming_Editor is access all Naming_Editor_Record'Class;

   procedure Gtk_New
     (Editor    : out Naming_Editor;
      Languages : GNAT.OS_Lib.Argument_List);
   --  Create a new naming scheme editor, that supports the edition of all the
   --  languages in Languages.
   --  It is the responsability of the caller to free Languages.

   procedure Gtk_New
     (Editor    : out Naming_Editor;
      Project_View : Prj.Project_Id);
   --  Create a new naming scheme editor, that edits the languages supported by
   --  Project_View.

   procedure Edit_Naming_Scheme
     (Parent       : access Gtk.Window.Gtk_Window_Record'Class;
      Kernel       : access Glide_Kernel.Kernel_Handle_Record'Class;
      Project_View : Prj.Project_Id);
   --  Open a dialog to edit the naming scheme for Project (given one of its
   --  views). This dialog is modal, and needs to be closed before the user can
   --  do anything else with Fps.

   procedure Create_Project_Entry
     (Editor          : access Naming_Editor_Record;
      Kernel          : access Glide_Kernel.Kernel_Handle_Record'Class;
      Project         : Prj.Tree.Project_Node_Id;
      Ignore_Scenario : Boolean := False);
   --  Create a new entry in the project file Project for the naming scheme
   --  defined in the editor.
   --  Return True if the project was changed.
   --  If Ignore_Scenario is True, then the entries will be created in the
   --  common section of the normalized project rather than in the case
   --  constructions for the scenario.

   procedure Show_Project_Settings
     (Editor             : access Naming_Editor_Record;
      Project_View       : Prj.Project_Id;
      Display_Exceptions : Boolean := True);
   --  Show the settings used for Project_View.
   --  Note that only the languages that were given to Gtk_New will be
   --  editable.
   --  If Display_Exceptions is False, then the files in the exception list
   --  will not be displayed, only the suffixes will be. This is intended to be
   --  used when creating new projects based on an existing one.

private
   type Language_Naming is record
      Language       : GNAT.OS_Lib.String_Access;
      Ada_Naming     : Ada_Naming_Editors.Ada_Naming_Editor;
      Foreign_Naming : Foreign_Naming_Editors.Foreign_Naming_Editor;
      --  ??? Should have a common ancestor for all naming editors, registered
      --  ??? in Language_Handlers.Glide. However, the latter must be
      --  ??? independent of GtkAda...
   end record;

   type Language_Naming_Array is array (Natural range <>) of Language_Naming;
   type Language_Naming_Array_Access is access Language_Naming_Array;

   type Naming_Editor_Record is new Gtk.Notebook.Gtk_Notebook_Record
     with record
        Pages : Language_Naming_Array_Access;
     end record;
end Naming_Editors;
