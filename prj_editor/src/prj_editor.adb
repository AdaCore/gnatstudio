
with Prj.Tree;     use Prj.Tree;
with Prj;          use Prj;
with Prj.PP;       use Prj.PP;
with Prj.Part;     use Prj.Part;
with Prj.Proc;     use Prj.Proc;
with Prj.Com;      use Prj.Com;
with Errout;       use Errout;

with Gtk.Main;     use Gtk.Main;
with Gtk.Window;      use Gtk.Window;
with Gtk.Enums;       use Gtk.Enums;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtkada.Handlers; use Gtkada.Handlers;
with Gtk.Widget;      use Gtk.Widget;

with Project_Trees;   use Project_Trees;
with Project_Viewers; use Project_Viewers;
with Scenario_Views;  use Scenario_Views;

with Gtkada.MDI;      use Gtkada.MDI;

procedure Prj_Editor is

   Project : Project_Node_Id;
   Project_View : Project_Id;

   Win : Gtk_Window;
   Tree : Project_Tree;
   Viewer : Project_Viewer;
   Scenar : Scenario_View;
   MDI : MDI_Window;
   Child : MDI_Child;
   Scrolled : Gtk_Scrolled_Window;

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

      Pretty_Print (Project);
      Process (Project => Project_View, From_Project_Node => Project);
      Errout.Finalize;
      pragma Assert (Project_View /= No_Project);

      Load (T, Project, Project_View);
      Refresh (Scenar, Project_View);
   end Refresh_Tree;

begin
   Prj.Part.Parse (Project, "root.gpr", Always_Errout_Finalize => True);
   pragma Assert (Project /= Empty_Node);

   Gtk.Main.Init;

   Gtk_New (Win, Window_Toplevel);
   Set_Default_Size (Win, 500, 500);

   Gtk_New (MDI);
   Add (Win, MDI);
   Set_Priorities (MDI, (Top => 1, Left => 2, others => 3));


   --  Explorer
   Gtk_New (Scrolled);
   Set_Policy (Scrolled, Policy_Automatic, Policy_Automatic);
   Gtk_New (Tree, Columns => 1);
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

   Gtk_New (Scenar, Project);
   Child := Put (MDI, Scenar);
   Set_Title (Child, "Current Scenario");
   Set_Dock_Side (Child, Top);
   Dock_Child (Child);

   Widget_Callback.Object_Connect
     (Scenar, "changed",
      Widget_Callback.To_Marshaller (Refresh_Tree'Unrestricted_Access), Tree);

   --  Project Viewer

   Gtk_New (Viewer);
   Child := Put (MDI, Viewer);
   Set_Title (Child, "Project Contents");


   Refresh_Tree (Tree);

   Maximize_Children (MDI);

   Show_All (Win);
   Main;
   Pretty_Print (Project);
end Prj_Editor;
