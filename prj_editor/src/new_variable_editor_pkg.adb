with Glib; use Glib;
with Gtk; use Gtk;
with Gdk.Types;       use Gdk.Types;
with Gtk.Widget;      use Gtk.Widget;
with Gtk.Enums;       use Gtk.Enums;
with Gtkada.Handlers; use Gtkada.Handlers;
with Callbacks_Switches_Glade; use Callbacks_Switches_Glade;
with Switches_Glade_Intl; use Switches_Glade_Intl;
with New_Variable_Editor_Pkg.Callbacks; use New_Variable_Editor_Pkg.Callbacks;

package body New_Variable_Editor_Pkg is

procedure Gtk_New (New_Variable_Editor : out New_Variable_Editor_Access) is
begin
   New_Variable_Editor := new New_Variable_Editor_Record;
   New_Variable_Editor_Pkg.Initialize (New_Variable_Editor);
end Gtk_New;

procedure Initialize (New_Variable_Editor : access New_Variable_Editor_Record'Class) is
   pragma Suppress (All_Checks);
   List_Env_Variables_Items : String_List.Glist;
   Vbox39_Group : Widget_SList.GSList;

begin
   Gtk.Window.Initialize (New_Variable_Editor, Window_Toplevel);
   Set_Title (New_Variable_Editor, -"Creating a variable");
   Set_Policy (New_Variable_Editor, True, True, True);
   Set_Position (New_Variable_Editor, Win_Pos_Center);
   Set_Modal (New_Variable_Editor, True);

   Gtk_New_Vbox (New_Variable_Editor.Vbox37, False, 0);
   Add (New_Variable_Editor, New_Variable_Editor.Vbox37);

   Gtk_New (New_Variable_Editor.Frame36, -"Name");
   Set_Border_Width (New_Variable_Editor.Frame36, 10);
   Set_Shadow_Type (New_Variable_Editor.Frame36, Shadow_Etched_In);
   Pack_Start (New_Variable_Editor.Vbox37, New_Variable_Editor.Frame36, False, False, 0);

   Gtk_New (New_Variable_Editor.Variable_Name);
   Set_Editable (New_Variable_Editor.Variable_Name, True);
   Set_Max_Length (New_Variable_Editor.Variable_Name, 0);
   Set_Text (New_Variable_Editor.Variable_Name, -"");
   Set_Visibility (New_Variable_Editor.Variable_Name, True);
   Add (New_Variable_Editor.Frame36, New_Variable_Editor.Variable_Name);

   Gtk_New (New_Variable_Editor.Frame33, -"Importing");
   Set_Border_Width (New_Variable_Editor.Frame33, 10);
   Set_Shadow_Type (New_Variable_Editor.Frame33, Shadow_Etched_In);
   Pack_Start (New_Variable_Editor.Vbox37, New_Variable_Editor.Frame33, False, False, 0);

   Gtk_New_Vbox (New_Variable_Editor.Vbox38, False, 0);
   Add (New_Variable_Editor.Frame33, New_Variable_Editor.Vbox38);

   Gtk_New (New_Variable_Editor.Get_Environment, -"Get value from environment");
   Set_Active (New_Variable_Editor.Get_Environment, False);
   Pack_Start (New_Variable_Editor.Vbox38, New_Variable_Editor.Get_Environment, False, False, 0);
   Widget_Callback.Object_Connect
     (New_Variable_Editor.Get_Environment, "toggled",
      Widget_Callback.To_Marshaller (On_Get_Environment_Toggled'Access), New_Variable_Editor);

   Gtk_New
     (New_Variable_Editor.Alignment3, 1.0, 0.5, 0.91,
      0.91);
   Pack_Start (New_Variable_Editor.Vbox38, New_Variable_Editor.Alignment3, True, True, 4);

   Gtk_New (New_Variable_Editor.List_Env_Variables);
   Set_Case_Sensitive (New_Variable_Editor.List_Env_Variables, False);
   Set_Use_Arrows (New_Variable_Editor.List_Env_Variables, True);
   Set_Use_Arrows_Always (New_Variable_Editor.List_Env_Variables, False);
   String_List.Append (List_Env_Variables_Items, -"HOME");
   String_List.Append (List_Env_Variables_Items, -"MACHINE");
   String_List.Append (List_Env_Variables_Items, -"FOO");
   Combo.Set_Popdown_Strings (New_Variable_Editor.List_Env_Variables, List_Env_Variables_Items);
   Free_String_List (List_Env_Variables_Items);
   Set_Sensitive (New_Variable_Editor.List_Env_Variables, False);
   Add (New_Variable_Editor.Alignment3, New_Variable_Editor.List_Env_Variables);

   New_Variable_Editor.Combo_Entry7 := Get_Entry (New_Variable_Editor.List_Env_Variables);
   Set_Editable (New_Variable_Editor.Combo_Entry7, True);
   Set_Max_Length (New_Variable_Editor.Combo_Entry7, 0);
   Set_Text (New_Variable_Editor.Combo_Entry7, -"HOME");
   Set_Visibility (New_Variable_Editor.Combo_Entry7, True);

   Gtk_New (New_Variable_Editor.Frame34, -"Type");
   Set_Border_Width (New_Variable_Editor.Frame34, 10);
   Set_Shadow_Type (New_Variable_Editor.Frame34, Shadow_Etched_In);
   Pack_Start (New_Variable_Editor.Vbox37, New_Variable_Editor.Frame34, False, False, 0);

   Gtk_New_Vbox (New_Variable_Editor.Vbox39, False, 0);
   Add (New_Variable_Editor.Frame34, New_Variable_Editor.Vbox39);

   Gtk_New (New_Variable_Editor.Typed_Variable, Vbox39_Group, -"Fixed list of possible values");
   Vbox39_Group := Group (New_Variable_Editor.Typed_Variable);
   Set_Active (New_Variable_Editor.Typed_Variable, False);
   Pack_Start (New_Variable_Editor.Vbox39, New_Variable_Editor.Typed_Variable, False, False, 0);
   Widget_Callback.Object_Connect
     (New_Variable_Editor.Typed_Variable, "toggled",
      Widget_Callback.To_Marshaller (On_Typed_Variable_Toggled'Access), New_Variable_Editor);

   Gtk_New (New_Variable_Editor.Untyped_Variable, Vbox39_Group, -"Any value authorized");
   Vbox39_Group := Group (New_Variable_Editor.Untyped_Variable);
   Set_Active (New_Variable_Editor.Untyped_Variable, False);
   Pack_Start (New_Variable_Editor.Vbox39, New_Variable_Editor.Untyped_Variable, False, False, 0);

   Gtk_New (New_Variable_Editor.Value_Frame, -"List of values / Current value");
   Set_Border_Width (New_Variable_Editor.Value_Frame, 10);
   Set_Shadow_Type (New_Variable_Editor.Value_Frame, Shadow_Etched_In);
   Pack_Start (New_Variable_Editor.Vbox37, New_Variable_Editor.Value_Frame, True, True, 0);

   Gtk_New_Vbox (New_Variable_Editor.Vbox40, False, 0);
   Add (New_Variable_Editor.Value_Frame, New_Variable_Editor.Vbox40);

   Gtk_New (New_Variable_Editor.Scrolledwindow4);
   Set_Policy (New_Variable_Editor.Scrolledwindow4, Policy_Never, Policy_Always);
   Pack_Start (New_Variable_Editor.Vbox40, New_Variable_Editor.Scrolledwindow4, True, True, 0);

   Gtk_New (New_Variable_Editor.List_Values);
   Set_Editable (New_Variable_Editor.List_Values, False);
   Add (New_Variable_Editor.Scrolledwindow4, New_Variable_Editor.List_Values);

   Gtk_New
     (New_Variable_Editor.Alignment2, 1.0, 0.0, 0.0,
      0.0);
   Pack_Start (New_Variable_Editor.Vbox40, New_Variable_Editor.Alignment2, False, False, 0);

   Gtk_New (New_Variable_Editor.Concatenate_Button, -"Concatenate...");
   Widget_Callback.Object_Connect
     (New_Variable_Editor.Concatenate_Button, "clicked",
      Widget_Callback.To_Marshaller (On_Concatenate_Clicked'Access), New_Variable_Editor);
   Add (New_Variable_Editor.Alignment2, New_Variable_Editor.Concatenate_Button);

   Gtk_New_Hseparator (New_Variable_Editor.Hseparator4);
   Pack_Start (New_Variable_Editor.Vbox37, New_Variable_Editor.Hseparator4, True, True, 0);

   Gtk_New (New_Variable_Editor.Hbuttonbox3);
   Set_Spacing (New_Variable_Editor.Hbuttonbox3, 30);
   Set_Layout (New_Variable_Editor.Hbuttonbox3, Buttonbox_Spread);
   Set_Child_Size (New_Variable_Editor.Hbuttonbox3, 85, 27);
   Set_Child_Ipadding (New_Variable_Editor.Hbuttonbox3, 7, 0);
   Pack_Start (New_Variable_Editor.Vbox37, New_Variable_Editor.Hbuttonbox3, False, False, 0);

   Gtk_New (New_Variable_Editor.Add_Button, -"Add");
   Set_Flags (New_Variable_Editor.Add_Button, Can_Default);
   Widget_Callback.Object_Connect
     (New_Variable_Editor.Add_Button, "clicked",
      Widget_Callback.To_Marshaller (On_Add_Clicked'Access), New_Variable_Editor);
   Add (New_Variable_Editor.Hbuttonbox3, New_Variable_Editor.Add_Button);

   Gtk_New (New_Variable_Editor.Cancel_Button, -"Cancel");
   Set_Flags (New_Variable_Editor.Cancel_Button, Can_Default);
   Widget_Callback.Object_Connect
     (New_Variable_Editor.Cancel_Button, "clicked",
      Widget_Callback.To_Marshaller (On_Cancel_Clicked'Access), New_Variable_Editor);
   Add (New_Variable_Editor.Hbuttonbox3, New_Variable_Editor.Cancel_Button);

end Initialize;

end New_Variable_Editor_Pkg;
