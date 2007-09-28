-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2001-2007, AdaCore             --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
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

--  <description>
--  This package implements a new widget to interactively edit the switches
--  for the GNAT tools (currently supported are gnatmake, gcc, gnatbind and
--  gnatlink).
--  A GUI is provided for the more common switches, but the user can always
--  edit them through an interactive command line.
--  </description>

with Gtk.Notebook;     use Gtk.Notebook;
with GNAT.Strings;
with GPS.Kernel;
with Projects;
with Switches_Chooser.Gtkada;
with VFS;
with Commands.Interactive;

package Switches_Editors is

   type Switches_Edit_Record is new Gtk_Notebook_Record with private;
   type Switches_Edit is access all Switches_Edit_Record'Class;

   -----------
   -- Pages --
   -----------

   type Switches_Editor_Page_Record is new
     Switches_Chooser.Gtkada.Switches_Editor_Record with private;
   type Switches_Editor_Page is access all Switches_Editor_Page_Record'Class;

   procedure Gtk_New
     (Page             : out Switches_Editor_Page;
      In_Editor        : Switches_Edit;
      Kernel           : access GPS.Kernel.Kernel_Handle_Record'Class;
      Tool             : GPS.Kernel.Tool_Properties_Record);
   --  Create a new page, that should be displayed.
   --  You can restrict the display of this page to specific languages by
   --  calling Add_Language below. However, by default it is displayed for
   --  all languages.
   --  The page is setup as a table of (Line + 1) x Cols, the last line
   --  being automatically created for the command line.
   --  Title is displayed in the notebook tab of the switches editor.
   --
   --  Project_Package is the name of the package, in the project files, where
   --  the switches are stored. Attribute_Index is the index of the attribute
   --  to be created in Project_Package.

   ---------------------
   -- Switches editor --
   ---------------------

   procedure Gtk_New
     (Editor : out Switches_Edit;
      Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Create a new switches editor.

   function Get_Page
     (Editor : access Switches_Edit_Record'Class;
      Title  : String) return Switches_Editor_Page;
   --  Return the page with the given title, or null if no such page has been
   --  added yet.

   procedure Set_Visible_Pages
     (Editor : access Switches_Edit_Record;
      Languages : GNAT.Strings.String_List);
   --  Set the visible pages based on the specific languages

   function Generate_Project
     (Switches           : access Switches_Edit_Record'Class;
      Project            : Projects.Project_Type;
      Languages          : GNAT.Strings.String_List;
      Scenario_Variables : Projects.Scenario_Variable_Array;
      Files              : VFS.File_Array)
      return Boolean;
   --  Generate the information in Project to represent the status of Switches.
   --  True is returned if at least one project was modified.
   --  Project can be No_Project, in which case the return value will
   --  always be non empty, after modification of the project.

   -----------------------------------------------------
   -- Editing switches for a specific file or project --
   -----------------------------------------------------
   --  The subprograms below are convenience subprogram to edit some specific
   --  switches. They provide a higher-level framework over the standard
   --  switches editor.

   type Edit_Switches_Command
     is new Commands.Interactive.Interactive_Command with null record;
   function Execute
     (Command : access Edit_Switches_Command;
      Context : Commands.Interactive.Interactive_Command_Context)
      return Commands.Command_Return_Type;
   --  Callback suitable for a contextual menu item.  If the context has no
   --  file information, then the default switches for the project are edited,
   --  otherwise the switches for the specific file are edited.

   function Edit_Switches_For_Files
     (Kernel       : access GPS.Kernel.Kernel_Handle_Record'Class;
      Project      : Projects.Project_Type;
      Files        : VFS.File_Array) return Boolean;
   --  Edit the switches for a list of files. All the files will be assigned
   --  the same switches.
   --  If there are no files in Files, the default switches are edited.
   --  Return true if the switches were modified.

   procedure Set_Switches
     (Editor : access Switches_Edit_Record; Project : Projects.Project_Type);
   --  Set the initial value for the switches, based on the contents
   --  of Project_View. If a page doesn't exist in Editor, it will not be
   --  automatically created.
   --  Project_View can be No_Project, in which case only Ada-related pages are
   --  displayed.

private
   type Switches_Editor_Page_Record is new
     Switches_Chooser.Gtkada.Switches_Editor_Record with
      record
         Tool_Name : GNAT.Strings.String_Access;
         Switches  : Switches_Edit;
      end record;

   overriding function Get_Tool_By_Name
     (Editor    : Switches_Editor_Page_Record;
      Tool_Name : String)
      return Switches_Chooser.Gtkada.Gtk_Switches_Editors.
         Root_Switches_Editor_Access;
   --  See inherited documentation

   type Pages_Array is array (Natural range <>) of Switches_Editor_Page;
   type Page_Array_Access is access Pages_Array;

   type Switches_Edit_Record is new Gtk_Notebook_Record with record
      Kernel   : GPS.Kernel.Kernel_Handle;
      Files    : VFS.File_Array_Access;
      Project  : Projects.Project_Type;
      Pages    : Page_Array_Access;
   end record;

end Switches_Editors;
