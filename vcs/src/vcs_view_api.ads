-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
--                            ACT-Europe                             --
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
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Gtk.Widget;
with Glib.Object;       use Glib.Object;
with Glide_Kernel;      use Glide_Kernel;
with Gtk.Menu;          use Gtk.Menu;
with VCS;               use VCS;
with Projects;          use Projects;

package VCS_View_API is

   procedure Open
     (Widget  : access GObject_Record'Class;
      Kernel  : Kernel_Handle);
   --  Open the selected files.

   procedure Get_Status
     (Widget  : access GObject_Record'Class;
      Kernel  : Kernel_Handle);
   --  Query status for the selected files.

   procedure Update
     (Widget  : access GObject_Record'Class;
      Kernel  : Kernel_Handle);
   --  Update the selected files.

   procedure View_Head_Diff
     (Widget  : access GObject_Record'Class;
      Kernel  : Kernel_Handle);
   --  Launch a visual comparison for the selected files and their
   --  head revisions.

   procedure View_Work_Diff
     (Widget  : access GObject_Record'Class;
      Kernel  : Kernel_Handle);
   --  Launch a visual comparison for the selected files and their
   --  work revisions.

   procedure View_Work_Head_Diff
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle);
   --  Launch a visual comparison for the selected files between their
   --  working and head revisions.

   procedure View_Log
     (Widget  : access GObject_Record'Class;
      Kernel  : Kernel_Handle);
   --  View the changelog for the selected files.

   procedure View_Annotate
     (Widget  : access GObject_Record'Class;
      Kernel  : Kernel_Handle);
   --  View annotations for the selected files.

   procedure Remove_Annotations
     (Widget  : access GObject_Record'Class;
      Kernel  : Kernel_Handle);
   --  Remove annotations for the selected files.

   procedure Edit_Log
     (Widget  : access GObject_Record'Class;
      Kernel  : Kernel_Handle);
   --  Launch a log editor for the selected files.

   procedure Commit
     (Widget  : access GObject_Record'Class;
      Kernel  : Kernel_Handle);
   --  Commit the selected files.

   procedure Add
     (Widget  : access GObject_Record'Class;
      Kernel  : Kernel_Handle);
   --  Add the selected files to the project repository.

   procedure Remove
     (Widget  : access GObject_Record'Class;
      Kernel  : Kernel_Handle);
   --  Remove the selected files from the project repository.

   procedure Revert
     (Widget  : access GObject_Record'Class;
      Kernel  : Kernel_Handle);
   --  Revert the selected files.

   procedure VCS_Contextual_Menu
     (Object  : access Glib.Object.GObject_Record'Class;
      Context : access Selection_Context'Class;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class);
   --  Complete Menu with the commands related to the VCS module,
   --  according to the information in Context.

   procedure Open_Explorer
     (Kernel  : Kernel_Handle;
      Context : Selection_Context_Access);
   --  If the VCS Explorer is not displayed, display it.

   function Get_Current_Ref
     (Context : Selection_Context_Access) return VCS_Access;
   --  Convenience function to get the current VCS system.
   --  If the creator of the current context is the VCS module, then
   --  the Ref will be obtained from the VCS Explorer, otherwise it
   --  will be obtained from the ???

   procedure Query_Status_For_Project
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle);
   --  List all open files in the project

   procedure Update_All
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle);
   --  Update all files in the project

   procedure Query_Status_For_Directory
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle);
   --  List all files in the current directory

   procedure Update_Directory
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle);
   --  Update all files in the current directory

   procedure Query_Status_For_Directory_Recursive
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle);
   --  List all files in the current directory and its subdirectories

   procedure Update_Directory_Recursive
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle);
   --  Update all files in the current directory and its subdirectories

   function Context_Factory
     (Kernel : access Kernel_Handle_Record'Class;
      Child  : Gtk.Widget.Gtk_Widget) return Selection_Context_Access;
   --  Return the current context relative to the VCS Explorer.

   function Get_Current_Ref (Project : Project_Type) return VCS_Access;
   --  Return the VCS reference registered in Project.

   function Get_Status_Name (Status : File_Status) return String;
   --  Return an explicit description of Status.

   procedure Display_Editor_Status
     (Kernel : access Kernel_Handle_Record'Class;
      Ref    : VCS_Access;
      Status : File_Status_Record);
   --  Display a label corresponding to the file. If the file is not open,
   --  do nothing.

end VCS_View_API;
