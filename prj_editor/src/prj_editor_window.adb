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

with Prj;          use Prj;
with Types;        use Types;

with Gtk.Window;      use Gtk.Window;
with Gtk.Enums;       use Gtk.Enums;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtkada.Handlers; use Gtkada.Handlers;
with Gtk.Widget;      use Gtk.Widget;

with Project_Trees;   use Project_Trees;
with Project_Viewers; use Project_Viewers;
with Scenario_Views;  use Scenario_Views;
with Glide_Kernel;    use Glide_Kernel;
with Glide_Kernel.Project; use Glide_Kernel.Project;

with Gtkada.MDI;      use Gtkada.MDI;

package body Prj_Editor_Window is

   procedure Selection_Changed (Tree : access Gtk_Widget_Record'Class);
   --  Called every time the selection has changed in the tree

   -----------------------
   -- Selection_Changed --
   -----------------------

   procedure Selection_Changed (Tree : access Gtk_Widget_Record'Class) is
      Prj          : constant Project_Editor :=
        Project_Editor (Get_Toplevel (Tree));
      T            : Project_Tree := Project_Tree (Tree);
      Project_View : Project_Id := Get_Selected_Project (T);
      Dir          : String_Id;

   begin
      Clear (Prj.Viewer);  --  ??? Should delete selectively

      if Project_View /= No_Project then
         Dir := Get_Selected_Directory (T, Project_View);
         if Dir = No_String then
            Show_Project (Prj.Viewer, Project_View);
         else
            Show_Project (Prj.Viewer, Project_View, Dir);
         end if;
      end if;
   end Selection_Changed;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Prj          : out Project_Editor;
      Kernel       : access Kernel_Handle_Record'Class) is
   begin
      Prj := new Project_Editor_Record;
      Initialize (Prj, Kernel);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Prj          : access Project_Editor_Record'Class;
      Kernel       : access Kernel_Handle_Record'Class)
   is
      Child   : MDI_Child;

   begin
      Gtk.Window.Initialize (Prj, Window_Toplevel);
      Set_Title (Prj, "Project Editor");
      Set_Default_Size (Prj, 500, 500);

      Gtk_New (Prj.MDI);
      Add (Prj, Prj.MDI);
      Set_Priorities (Prj.MDI, (Top => 1, Left => 2, others => 3));

      --  Explorer
      Gtk_New (Prj.Scrolled);
      Set_Policy (Prj.Scrolled, Policy_Automatic, Policy_Automatic);
      Gtk_New (Prj.Tree, Kernel);
      Add (Prj.Scrolled, Prj.Tree);
      Child := Put (Prj.MDI, Prj.Scrolled);
      Set_Title (Child, "Explorer");
      Set_Dock_Side (Child, Left);
      Dock_Child (Child);

      --  Widget_Callback.Connect
      --    (Prj.Tree, "tree_select_row",
      --     Widget_Callback.To_Marshaller (Selection_Changed'Access));
      --  Widget_Callback.Connect
      --    (Prj.Tree, "tree_unselect_row",
      --     Widget_Callback.To_Marshaller (Selection_Changed'Access));

      --  Scenario editor

      Gtk_New (Prj.Scenar, Kernel);
      Child := Put (Prj.MDI, Prj.Scenar);
      Set_Title (Child, "Current Scenario");
      Set_Dock_Side (Child, Top);
      Dock_Child (Child);

      --  Project Viewer

      Gtk_New (Prj.Viewer, Kernel, Prj.Tree);
      Child := Put (Prj.MDI, Prj.Viewer);
      Set_Title (Child, "Project Contents");

      --  Compute the current view, and initialize the explorer, scenario, ...
      Recompute_View (Kernel);

      Maximize_Children (Prj.MDI);
   end Initialize;

end Prj_Editor_Window;
