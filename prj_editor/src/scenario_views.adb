
with Glib;            use Glib;
with Gtk.Arguments;   use Gtk.Arguments;
with Gtk.Box;         use Gtk.Box;
with Gtk.Button;      use Gtk.Button;
with Gtk.GEntry;      use Gtk.GEntry;
with Gtkada.Handlers; use Gtkada.Handlers;
with Gtk.Widget;      use Gtk.Widget;
with Glib.Object;     use Glib.Object;

with Prj_API;          use Prj_API;
with Variable_Editors; use Variable_Editors;

with Prj;      use Prj;
with Prj.Ext;  use Prj.Ext;
with Prj.Tree; use Prj.Tree;
with Stringt;  use Stringt;
with Namet;    use Namet;
with Types;    use Types;

with Interfaces.C.Strings; use Interfaces.C.Strings;
with Text_IO; use Text_IO;

package body Scenario_Views is

   Scenario_Class : GObject_Class := Uninitialized_Class;
   --  The class structure for this widget

   Signals : constant chars_ptr_array :=
     (1 => New_String ("changed"));
   --  The list of signals defined for this widget

   procedure On_Edit_Scenario (View : access Gtk_Widget_Record'Class);
   --  Callback for editing the current scenario

   procedure Editor_Changed
     (View : access Gtk_Widget_Record'Class; Args : Gtk_Args);
   --  Callback when the variable editor reports that a variable has changed

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
      Signal_Parameters : constant Signal_Parameter_Types :=
        (1 => (1 => GType_None));

      Button : Gtk_Button;
   begin
      pragma Assert (Project /= Empty_Node);
      View.Project := Project;
      Initialize_Hbox (View, Homogeneous => False, Spacing => 10);

      Initialize_Class_Record
        (View, Signals, Scenario_Class, "ScenarioViews", Signal_Parameters);

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

      Widget_Callback.Object_Connect
        (Edit, "changed", Editor_Changed'Access, V);

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
      Str : String_Id;
   begin
      Set_Text (View.Field, "");

      for V in Vars'Range loop
         Str := External_Reference_Of (Vars (V));
         if Str /= No_String then
            String_To_Name_Buffer (Str);
            Str := Prj.Ext.Value_Of  (External_Name => Name_Find);
         end if;

         if V /= Vars'First then
            Append_Text (View.Field, ", ");
         end if;

         if Str = No_String then
            Append_Text
              (View.Field,
               Get_Name_String (Name_Of (Vars (V))) & "=""");
            Display_Expr (View.Field, Value_Of (Vars (V)));
            Append_Text (View.Field, """");

         else
            String_To_Name_Buffer (Str);
            Append_Text
              (View.Field,
               Get_Name_String (Name_Of (Vars (V)))
               & "=""" & Name_Buffer (1 .. Name_Len) & """");
         end if;
      end loop;
   end Refresh;

   -------------
   -- Changed --
   -------------

   procedure Changed (View : access Scenario_View_Record) is
   begin
      Widget_Callback.Emit_By_Name (View, "changed");
   end Changed;

   --------------------
   -- Editor_Changed --
   --------------------

   procedure Editor_Changed
     (View : access Gtk_Widget_Record'Class; Args : Gtk_Args)
   is
      --  Var : Project_Node_Id := Project_Node_Id (To_Gint (Args, 1));
   begin
      Changed (Scenario_View (View));
   end Editor_Changed;

end Scenario_Views;
