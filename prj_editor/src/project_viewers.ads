-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
--                            ACT-Europe                             --
--                                                                   --
-- GLIDE is free software; you can redistribute it and/or modify  it --
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

with Gtk.Box;
with Gtk.Clist;
with Gtk.Style;

with Prj;

with Glide_Kernel;
with Histories;

package Project_Viewers is

   Project_History_Key : constant Histories.History_Key := "project_files";
   --  Key to use in the kernel histories to store the most recently opened
   --  files.

   type Project_Viewer_Record is new Gtk.Box.Gtk_Hbox_Record with private;
   type Project_Viewer is access all Project_Viewer_Record'Class;

   procedure Register_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Register the module into the list

   procedure Gtk_New
     (Viewer  : out Project_Viewer;
      Kernel  : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Create a new project viewer.
   --  Every time the selection in Explorer changes, the info displayed in
   --  the viewer is changed.

   procedure Initialize
     (Viewer : access Project_Viewer_Record'Class;
      Kernel  : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Internal subprogram for creating widgets

   procedure Show_Project
     (Viewer              : access Project_Viewer_Record;
      Project_Filter      : Prj.Project_Id;
      Directory_Filter    : String := "");
   --  Shows all the direct source files of Project_Filter (ie not including
   --  imported projects, but including all source directories).
   --  This doesn't clear the list first!
   --  Directory_Filter should be used to limit the search path for the files.
   --  Only the files in Directory_Filter will be displayed.
   --
   --  Project_View mustn't be No_Project.

   procedure Clear (Viewer : access Project_Viewer_Record);
   --  Removes all files currently displayed in Viewer.

   procedure Add_To_Reopen
     (Kernel   : access Glide_Kernel.Kernel_Handle_Record'Class;
      Filename : String);
   --  Add Filename to the Project->Reopen menu

private
   type Project_Viewer_Record is new Gtk.Box.Gtk_Hbox_Record with record
      List : Gtk.Clist.Gtk_Clist;
      --  The actual contents of the viewer

      Default_Switches_Style : Gtk.Style.Gtk_Style;
      --  Style to use when displaying switches that are set at the project
      --  level, rather than file specific

      Kernel  : Glide_Kernel.Kernel_Handle;

      Current_Project : Prj.Project_Id;
      --  The project to which the files currently in the viewer belong. This
      --  indicates which project file should be normalized when a modification
      --  takes place.

      Project_Filter : Prj.Project_Id := Prj.No_Project;
   end record;
end Project_Viewers;
