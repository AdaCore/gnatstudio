
with Gtk.Box;    use Gtk.Box;
with Gtk.Button; use Gtk.Button;
with Gtk.GEntry; use Gtk.GEntry;
with Gtkada.Handlers; use Gtkada.Handlers;
with Gtk.Widget; use Gtk.Widget;

with Prj_API;  use Prj_API;
with Variable_Editors; use Variable_Editors;

with Prj;      use Prj;
with Prj.Tree; use Prj.Tree;
with Prj.Util; use Prj.Util;
with Stringt;  use Stringt;
with Namet;    use Namet;

package body Scenario_Views is

   procedure On_Edit_Scenario (View : access Gtk_Widget_Record'Class);
   --  Callback for editing the current scenario

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (View : out Scenario_View; Project : Prj.Tree.Project_Node_Id) is
   begin
      View := new Scenario_View_Record;
      Initialize (View, Project);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (View : access Scenario_View_Record'Class;
      Project : Prj.Tree.Project_Node_Id)
   is
      Button : Gtk_Button;
   begin
      pragma Assert (Project /= Empty_Node);
      View.Project := Project;
      Initialize_Hbox (View, Homogeneous => False, Spacing => 10);

      Gtk_New (Button, "Edit Scenario");
      Pack_Start (View, Button, Expand => False, Fill => False);
      Widget_Callback.Object_Connect
        (Button, "clicked",
         Widget_Callback.To_Marshaller (On_Edit_Scenario'Access), View);

      Gtk_New (View.Field, 1024);
      Pack_Start (View, View.Field, Expand => True, Fill => True);
   end Initialize;

   ----------------------
   -- On_Edit_Scenario --
   ----------------------

   procedure On_Edit_Scenario (View : access Gtk_Widget_Record'Class) is
      V : Scenario_View := Scenario_View (View);
      Edit : Variable_Edit;
      Scenar_Var : Variable_Decl_Array := Find_Scenario_Variables (V.Project);
   begin
      Gtk_New (Edit, V.Project);

      for J in Scenar_Var'Range loop
         Refresh (Edit, Scenar_Var (J));
      end loop;

      Show_All (Edit);
   end On_Edit_Scenario;

   -------------
   -- Refresh --
   -------------

   procedure Refresh
     (View                 : access Scenario_View_Record'Class;
      Current_Project_View : Prj.Project_Id)
   is
      Vars : Variable_Decl_Array := Find_Scenario_Variables (View.Project);
      Value : Variable_Value;
      Pkg : Package_Id;
   begin
      Set_Text (View.Field, "");

      for V in Vars'Range loop
         Value := Prj.Util.Value_Of
           (Variable_Name => Name_Of (Vars (V)),
            In_Variables  =>
              Projects.Table (Current_Project_View).Decl.Variables);

         Pkg := Projects.Table (Current_Project_View).Decl.Packages;

         while Pkg /= No_Package
           and then Value = Nil_Variable_Value
         loop
            Value := Prj.Util.Value_Of
              (Variable_Name => Name_Of (Vars (V)),
               In_Variables  => Packages.Table (Pkg).Decl.Variables);
            Pkg := Packages.Table (Pkg).Next;
         end loop;

         pragma Assert (Value.Kind = Single);

         String_To_Name_Buffer (Value.Value);
         Append_Text
           (View.Field,
            Get_Name_String (Name_Of (Vars (V)))
            & "="""
            & Name_Buffer (1 .. Name_Len)
            & """");
      end loop;
   end Refresh;
end Scenario_Views;
