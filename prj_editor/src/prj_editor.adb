
with Gtk.Main;     use Gtk.Main;
with Variable_Editors; use Variable_Editors;
with Prj.Tree;     use Prj.Tree;
with Prj_API;      use Prj_API;
with Prj;          use Prj;
with Prj.PP;       use Prj.PP;

procedure Prj_Editor is
   Edit : Variable_Edit;
   Project, Var, Typ : Project_Node_Id;
begin
   Project := Create_Project (Name => "Project_Foo", Path => "/tmp");

   Gtk.Main.Init;

   Gtk_New (Edit, Project);

   Var := Get_Or_Create_Variable (Project, "foo", Single);
   Set_Value (Var, "foo_value");
   Refresh (Edit, Var);

   Var := Get_Or_Create_Variable (Project, "foo2", List);
   Append_To_List (Var, "value1");
   Append_To_List (Var, "value2");
   Append_To_List (Var, "val${foo}ue3");
   Append_To_List (Var, "value4");
   Append_To_List (Var, "value5");
   Refresh (Edit, Var);

   Typ := Get_Or_Create_Type (Project, "typ1");
   Add_Possible_Value (Typ, "value1");
   Add_Possible_Value (Typ, "value2");

   Var := Get_Or_Create_Typed_Variable (Project, "foo3", Typ);
   Set_Value (Var, "value1");
   Refresh (Edit, Var);

   Var := Get_Or_Create_Typed_Variable (Project, "foo4", Typ);
   Set_Value_As_External (Var, "OS", "value1");
   Refresh (Edit, Var);

--     Var := Get_Or_Create_Variable (Project, "foo5", Single);
--     Set_Value_As_External (Var, "Build", "Devel");
--     Refresh (Edit, Var);

   Pretty_Print (Project);

   Show_All (Edit);
   Main;
   Pretty_Print (Project);
end Prj_Editor;
