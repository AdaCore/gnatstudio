
with Gtk.Main;     use Gtk.Main;
with Prj.Tree;     use Prj.Tree;
with Prj;          use Prj;
with Prj.PP;       use Prj.PP;
with Prj.Part;     use Prj.Part;
with Prj.Proc;     use Prj.Proc;
with Prj.Com;      use Prj.Com;
with Errout;       use Errout;

with Gtk.Window;      use Gtk.Window;
with Gtk.Enums;       use Gtk.Enums;
with Gtkada.Handlers; use Gtkada.Handlers;
with Gtk.Widget;      use Gtk.Widget;
with Gtk.Box;         use Gtk.Box;
with Gtk.Button;      use Gtk.Button;

with Project_Trees;   use Project_Trees;
with Project_Viewers; use Project_Viewers;
with Scenario_Views;  use Scenario_Views;

procedure Prj_Editor is

   Project : Project_Node_Id;
   Project_View : Project_Id;

   Win : Gtk_Window;
   Box : Gtk_Box;
   Button : Gtk_Button;
   Tree : Project_Tree;
   Viewer : Project_Viewer;
   Scenar : Scenario_View;

   procedure Selection_Changed (Tree : access Gtk_Widget_Record'Class);
   --  Called every time the selection has changed in the tree

   procedure Refresh_Tree (Tree : access Gtk_Widget_Record'Class);
   --  Recompute the tree in the current scenario

   -----------------------
   -- Selection_Changed --
   -----------------------

   procedure Selection_Changed (Tree : access Gtk_Widget_Record'Class) is
      T : Project_Tree := Project_Tree (Tree);
      Project : Project_Id := Get_Selected_Project (T);
   begin
      Clear (Viewer);  --  ??? Should delete selectively
      if Project /= No_Project then
         declare
            Dirs : Name_Id_Array := Get_Selected_Directories (T, Project);
         begin
            if Dirs'Length = 0 then
               Show_Project (Viewer, Project);
            else
               Show_Project (Viewer, Project, Dirs (Dirs'First));
            end if;
         end;
      end if;
   end Selection_Changed;

   ------------------
   -- Refresh_Tree --
   ------------------

   procedure Refresh_Tree (Tree : access Gtk_Widget_Record'Class) is
      T : Project_Tree := Project_Tree (Tree);
   begin
      --  ??? Need to free as much memory as possible first.
      --  ??? Maybe a call to Prj.Reset is enough
      Errout.Initialize;
      pragma Assert (Project /= Empty_Node);
      Prj.Com.Units.Set_Last (No_Unit);
      Prj.Com.Units_Htable.Reset;

      Process (Project => Project_View, From_Project_Node => Project);
      Errout.Finalize;
      pragma Assert (Project_View /= No_Project);

      Load (T, Project, Project_View);
      Clear (Viewer);
      Refresh (Scenar, Project_View);
   end Refresh_Tree;

begin
   Gtk.Main.Init;
   Gtk_New (Win, Window_Toplevel);
   Set_Default_Size (Win, 200, 200);

   Gtk_New_Vbox (Box, Homogeneous => False);
   Add (Win, Box);

   Gtk_New (Tree, Columns => 1);
   Add (Box, Tree);

   Gtk_New (Button, "Refresh");
   Pack_Start (Box, Button, Fill => False, Expand => False);

   Widget_Callback.Connect
     (Tree, "tree_select_row",
      Widget_Callback.To_Marshaller (Selection_Changed'Unrestricted_Access));
   Widget_Callback.Connect
     (Tree, "tree_unselect_row",
      Widget_Callback.To_Marshaller (Selection_Changed'Unrestricted_Access));

   Widget_Callback.Object_Connect
     (Button, "clicked",
      Widget_Callback.To_Marshaller (Refresh_Tree'Unrestricted_Access), Tree);

   Prj.Part.Parse (Project, "root.gpr", Always_Errout_Finalize => False);
   pragma Assert (Project /= Empty_Node);

   Show_All (Win);

   Gtk_New (Win, Window_Toplevel);
   Set_Default_Size (Win, 200, 200);
   Gtk_New (Viewer);
   Add (Win, Viewer);
   Show_All (Win);

   Gtk_New (Win, Window_Toplevel);
   Gtk_New (Scenar, Project);
   Add (Win, Scenar);
   Show_All (Win);

   Widget_Callback.Object_Connect
     (Scenar, "changed",
      Widget_Callback.To_Marshaller (Refresh_Tree'Unrestricted_Access), Tree);

   Refresh_Tree (Tree);
   Main;
   Pretty_Print (Project);
end Prj_Editor;
