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

with Prj.Tree;     use Prj.Tree;
with Prj;          use Prj;
with Prj.Part;     use Prj.Part;

with Gtk.Main;        use Gtk.Main;
with Gtk.Window;      use Gtk.Window;
with Gtk.Enums;       use Gtk.Enums;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtkada.Handlers; use Gtkada.Handlers;
with Gtk.Widget;      use Gtk.Widget;

with Project_Trees;   use Project_Trees;
with Project_Viewers; use Project_Viewers;
with Scenario_Views;  use Scenario_Views;
with Prj_Manager;     use Prj_Manager;

with Gtkada.MDI;      use Gtkada.MDI;

procedure Prj_Editor is

   Project : Project_Node_Id;
   Win : Gtk_Window;
   Tree : Project_Tree;
   Viewer : Project_Viewer;
   Scenar : Scenario_View;
   MDI : MDI_Window;
   Child : MDI_Child;
   Scrolled : Gtk_Scrolled_Window;
   Manager : Project_Manager;

   procedure Selection_Changed (Tree : access Gtk_Widget_Record'Class);
   --  Called every time the selection has changed in the tree

   -----------------------
   -- Selection_Changed --
   -----------------------

   procedure Selection_Changed (Tree : access Gtk_Widget_Record'Class) is
      T : Project_Tree := Project_Tree (Tree);
      Project_View : Project_Id := Get_Selected_Project (T);
   begin
      Clear (Viewer);  --  ??? Should delete selectively
      if Project_View /= No_Project then
         declare
            Dirs : Name_Id_Array :=
              Get_Selected_Directories (T, Project_View);
         begin
            if Dirs'Length = 0 then
               Show_Project (Viewer, Project_View);
            else
               Show_Project (Viewer, Project_View, Dirs (Dirs'First));
            end if;
         end;
      end if;
   end Selection_Changed;

begin
   Gtk.Main.Init;

   Prj.Part.Parse (Project, "root.gpr", Always_Errout_Finalize => True);
   pragma Assert (Project /= Empty_Node);

   --  Project manager
   Gtk_New (Manager, Project);

   --  Main window
   Gtk_New (Win, Window_Toplevel);
   Set_Default_Size (Win, 500, 500);

   Gtk_New (MDI);
   Add (Win, MDI);
   Set_Priorities (MDI, (Top => 1, Left => 2, others => 3));

   --  Explorer
   Gtk_New (Scrolled);
   Set_Policy (Scrolled, Policy_Automatic, Policy_Automatic);
   Gtk_New (Tree, Manager, Columns => 1);
   Add (Scrolled, Tree);
   Child := Put (MDI, Scrolled);
   Set_Title (Child, "Explorer");
   Set_Dock_Side (Child, Left);
   Dock_Child (Child);

   Widget_Callback.Connect
     (Tree, "tree_select_row",
      Widget_Callback.To_Marshaller (Selection_Changed'Unrestricted_Access));
   Widget_Callback.Connect
     (Tree, "tree_unselect_row",
      Widget_Callback.To_Marshaller (Selection_Changed'Unrestricted_Access));

   --  Scenario editor

   Gtk_New (Scenar, Manager);
   Child := Put (MDI, Scenar);
   Set_Title (Child, "Current Scenario");
   Set_Dock_Side (Child, Top);
   Dock_Child (Child);

   --  Project Viewer

   Gtk_New (Viewer, Manager);
   Child := Put (MDI, Viewer);
   Set_Title (Child, "Project Contents");

   --  Compute the current view, and initialize the explorer, scenario, ...
   Recompute_View (Manager);

   Maximize_Children (MDI);

   Show_All (Win);
   Main;
end Prj_Editor;
