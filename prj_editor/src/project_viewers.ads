-----------------------------------------------------------------------
--                                                                   --
--                     Copyright (C) 2001                            --
--                          ACT-Europe                               --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

with Gtk.Notebook; use Gtk.Notebook;
with Gtk.Clist;
with Gtk.Style;

with Prj;
with Types;

with Glide_Kernel;
with Project_Trees;

package Project_Viewers is

   type Project_Viewer_Record is new Gtk_Notebook_Record with private;
   type Project_Viewer is access all Project_Viewer_Record'Class;

   type View_Type is (System, Version_Control, Switches);
   subtype Filter is Types.String_Id;
   No_Filter : constant Filter := Types.No_String;

   procedure Gtk_New
     (Viewer  : out Project_Viewer;
      Kernel  : access Glide_Kernel.Kernel_Handle_Record'Class;
      Explorer : access Project_Trees.Project_Tree_Record'Class);
   --  Create a new project viewer.
   --  Every time the selection in Explorer changes, the info displayed in
   --  the viewer is changed.

   procedure Initialize
     (Viewer : access Project_Viewer_Record'Class;
      Kernel  : access Glide_Kernel.Kernel_Handle_Record'Class;
      Explorer : access Project_Trees.Project_Tree_Record'Class);
   --  Internal subprogram for creating widgets

   procedure Show_Project
     (Viewer              : access Project_Viewer_Record;
      Project_Filter      : Prj.Project_Id;
      Directory_Filter    : Filter := No_Filter);
   --  Shows all the direct source files of Project_Filter (ie not including
   --  imported projects, but including all source directories).
   --  This doesn't clear the list first!
   --  Directory_Filter should be used to limit the search path for the files.
   --  Only the files in one of the directories in Directory_Filter will be
   --  displayed. If Directory_Filter is an empty array, no filter is applied.
   --  Note that if Parent/ is not in the filter, but Parent/Child is, then
   --  all the files from Child will be shown.
   --
   --  Project_View mustn't be No_Project.

   procedure Clear (Viewer : access Project_Viewer_Record);
   --  Removes all files currently displayed in Viewer.

private
   type Boolean_View_Array is array (View_Type) of Boolean;
   type Clist_View_Array is array (View_Type) of Gtk.Clist.Gtk_Clist;

   type Project_Viewer_Record is new Gtk_Notebook_Record with record
      Page_Is_Up_To_Date : Boolean_View_Array := (others => False);
      --  If an entry is False, then the contents of the page needs to be
      --  refreshed upon switching to a new page.

      Pages : Clist_View_Array;
      --  The contents of all the pages in the viewer

      Default_Switches_Style : Gtk.Style.Gtk_Style;
      --  Style to use when displaying switches that are set at the project
      --  level, rather than file specific

      Kernel  : Glide_Kernel.Kernel_Handle;
      Explorer : Project_Trees.Project_Tree;

      Project_Filter : Prj.Project_Id := Prj.No_Project;
   end record;
end Project_Viewers;
