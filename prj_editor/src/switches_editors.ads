------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2018, AdaCore                     --
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

--  This package implements a new widget to interactively edit the switches
--  for the GNAT tools.
--  A GUI is provided for the more common switches, but the user can always
--  edit them through an interactive command line.

with GPS.Kernel;               use GPS.Kernel;
with GNATCOLL.Projects;        use GNATCOLL.Projects;
with GNATCOLL.VFS;             use GNATCOLL.VFS;
with Commands.Interactive;     use Commands.Interactive;
with Project_Viewers;          use Project_Viewers;
with Glib.Object;              use Glib.Object;

package Switches_Editors is

   type Page_Iterator is access procedure
     (Data     : access GObject_Record'Class;
      Callback : Page_Iterator_Callback);

   type Tool_From_Name_Getter is record
      Data     : access GObject_Record'Class;
      Iterator : Page_Iterator;
   end record;
   --  This is used to find the page that edit the switches for a specific
   --  tool, which is needed to resolve dependencies between the tools, for
   --  instance setting '-g' for the builder should set it for the compiler.
   --  The iterator is passed the data, and should return all project editor
   --  page (or at least all the switches editing pages).

   function Switches_Editor_For_Tool_Factory
     (Tool           : not null access GPS.Kernel.Tool_Properties_Record;
      Files          : File_Array := Empty_File_Array;
      Tool_From_Name : Tool_From_Name_Getter)
      return Project_Editor_Page;
   --  Create a new project editor page for the switches or naming scheme
   --  of a specific tool.
   --  These might return null if no special configuration is necessary for
   --  this tool.
   --  Files can be specified to only edit the switches for those files. It
   --  will be freed when the editor is destroyed.

   function Switches_Editor_For_All_Tools_Factory
     (Kernel         : not null access Kernel_Handle_Record'Class;
      Files          : File_Array := Empty_File_Array)
      return Project_Editor_Page;
   --  Create a page to edit the switches for all registered tools.
   --  Each tool is set on a separate page of a notebook

   -----------------------------------------------------
   -- Editing switches for a specific file or project --
   -----------------------------------------------------
   --  The subprograms below are convenience subprogram to edit some specific
   --  switches. They provide a higher-level framework over the standard
   --  switches editor.

   type Edit_Switches_Command
     is new Commands.Interactive.Interactive_Command with null record;
   overriding function Execute
     (Command : access Edit_Switches_Command;
      Context : Commands.Interactive.Interactive_Command_Context)
      return Commands.Command_Return_Type;
   --  Callback suitable for a contextual menu item.  If the context has no
   --  file information, then the default switches for the project are edited,
   --  otherwise the switches for the specific file are edited.

   function Edit_Switches_For_Files
     (Kernel       : access GPS.Kernel.Kernel_Handle_Record'Class;
      Project      : Project_Type;
      Files        : GNATCOLL.VFS.File_Array) return Boolean;
   --  Edit the switches for a list of files. All the files will be assigned
   --  the same switches.
   --  If there are no files in Files, the default switches are edited.
   --  Return true if the switches were modified.

end Switches_Editors;
