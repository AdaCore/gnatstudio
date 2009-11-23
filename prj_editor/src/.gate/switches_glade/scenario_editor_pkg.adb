with Glib; use Glib;
with Gtk; use Gtk;
with Gdk.Types;       use Gdk.Types;
with Gtk.Widget;      use Gtk.Widget;
with Gtk.Enums;       use Gtk.Enums;
with Gtkada.Handlers; use Gtkada.Handlers;
with Callbacks_Switches_Glade; use Callbacks_Switches_Glade;
with Switches_Glade_Intl; use Switches_Glade_Intl;
with Scenario_Editor_Pkg.Callbacks; use Scenario_Editor_Pkg.Callbacks;

package body Scenario_Editor_Pkg is

procedure Gtk_New (Scenario_Editor : out Scenario_Editor_Access) is
begin
   Scenario_Editor := new Scenario_Editor_Record;
   Scenario_Editor_Pkg.Initialize (Scenario_Editor);
end Gtk_New;

procedure Initialize (Scenario_Editor : access Scenario_Editor_Record'Class) is
   pragma Suppress (All_Checks);
   Combo6_Items : String_List.Glist;
   Combo7_Items : String_List.Glist;

begin
   Gtk.Window.Initialize (Scenario_Editor, Window_Toplevel);
   Set_Title (Scenario_Editor, -"Variables");
   Set_Policy (Scenario_Editor, True, True, True);
   Set_Position (Scenario_Editor, Win_Pos_None);
   Set_Modal (Scenario_Editor, False);
   Set_Default_Size (Scenario_Editor, 640, 400);

   Gtk_New_Vbox (Scenario_Editor.Vbox31, False, 0);
   Add (Scenario_Editor, Scenario_Editor.Vbox31);

   Gtk_New (Scenario_Editor.Scrolledwindow3);
   Set_Policy (Scenario_Editor.Scrolledwindow3, Policy_Automatic, Policy_Always);
   Pack_Start (Scenario_Editor.Vbox31, Scenario_Editor.Scrolledwindow3, True, True, 0);

   Gtk_New (Scenario_Editor.Viewport2);
   Set_Shadow_Type (Scenario_Editor.Viewport2, Shadow_In);
   Add (Scenario_Editor.Scrolledwindow3, Scenario_Editor.Viewport2);

   Gtk_New (Scenario_Editor.List_Variables, 6, 5, False);
   Set_Row_Spacings (Scenario_Editor.List_Variables, 0);
   Set_Col_Spacings (Scenario_Editor.List_Variables, 6);
   Add (Scenario_Editor.Viewport2, Scenario_Editor.List_Variables);

   Gtk_New (Scenario_Editor.Label43, -("Env_Var_Name"));
   Set_Alignment (Scenario_Editor.Label43, 1.0, 0.5);
   Set_Padding (Scenario_Editor.Label43, 0, 0);
   Set_Justify (Scenario_Editor.Label43, Justify_Left);
   Set_Line_Wrap (Scenario_Editor.Label43, False);
   Attach (Scenario_Editor.List_Variables, Scenario_Editor.Label43, 2, 3, 2, 3,
     Expand or Shrink or Fill, 0,
     0, 0);

   Gtk_New (Scenario_Editor.Label45, -("Typed_Var_Name"));
   Set_Alignment (Scenario_Editor.Label45, 7.45058e-09, 0.5);
   Set_Padding (Scenario_Editor.Label45, 0, 0);
   Set_Justify (Scenario_Editor.Label45, Justify_Center);
   Set_Line_Wrap (Scenario_Editor.Label45, False);
   Attach (Scenario_Editor.List_Variables, Scenario_Editor.Label45, 2, 3, 3, 4,
     Expand or Shrink or Fill, 0,
     0, 0);

   Gtk_New (Scenario_Editor.Label47, -("String_Var_Name"));
   Set_Alignment (Scenario_Editor.Label47, 7.45058e-09, 0.5);
   Set_Padding (Scenario_Editor.Label47, 0, 0);
   Set_Justify (Scenario_Editor.Label47, Justify_Center);
   Set_Line_Wrap (Scenario_Editor.Label47, False);
   Attach (Scenario_Editor.List_Variables, Scenario_Editor.Label47, 2, 3, 4, 5,
     Expand or Shrink or Fill, 0,
     0, 0);

   Gtk_New (Scenario_Editor.Label48, -("List_Var_Name"));
   Set_Alignment (Scenario_Editor.Label48, 7.45058e-09, 0.5);
   Set_Padding (Scenario_Editor.Label48, 0, 0);
   Set_Justify (Scenario_Editor.Label48, Justify_Center);
   Set_Line_Wrap (Scenario_Editor.Label48, False);
   Attach (Scenario_Editor.List_Variables, Scenario_Editor.Label48, 2, 3, 5, 6,
     Expand or Shrink or Fill, 0,
     0, 0);

   Gtk_New (Scenario_Editor.Combo6);
   Set_Case_Sensitive (Scenario_Editor.Combo6, False);
   Set_Use_Arrows (Scenario_Editor.Combo6, True);
   Set_Use_Arrows_Always (Scenario_Editor.Combo6, False);
   String_List.Append (Combo6_Items, -"Possible_Value1");
   String_List.Append (Combo6_Items, -"Possible_Value2");
   Combo.Set_Popdown_Strings (Scenario_Editor.Combo6, Combo6_Items);
   Free_String_List (Combo6_Items);
   Attach (Scenario_Editor.List_Variables, Scenario_Editor.Combo6, 3, 4, 2, 3,
     Expand or Fill, 0,
     0, 0);

   Scenario_Editor.Entry7 := Get_Entry (Scenario_Editor.Combo6);
   Set_Editable (Scenario_Editor.Entry7, False);
   Set_Max_Length (Scenario_Editor.Entry7, 0);
   Set_Text (Scenario_Editor.Entry7, -"Possible_Value1");
   Set_Visibility (Scenario_Editor.Entry7, True);

   Gtk_New (Scenario_Editor.Combo7);
   Set_Case_Sensitive (Scenario_Editor.Combo7, False);
   Set_Use_Arrows (Scenario_Editor.Combo7, True);
   Set_Use_Arrows_Always (Scenario_Editor.Combo7, False);
   String_List.Append (Combo7_Items, -"Possible_Value1");
   String_List.Append (Combo7_Items, -"Possible_Value2");
   Combo.Set_Popdown_Strings (Scenario_Editor.Combo7, Combo7_Items);
   Free_String_List (Combo7_Items);
   Attach (Scenario_Editor.List_Variables, Scenario_Editor.Combo7, 3, 4, 3, 4,
     Expand or Fill, 0,
     0, 0);

   Scenario_Editor.Entry8 := Get_Entry (Scenario_Editor.Combo7);
   Set_Editable (Scenario_Editor.Entry8, False);
   Set_Max_Length (Scenario_Editor.Entry8, 0);
   Set_Text (Scenario_Editor.Entry8, -"Possible_Value1");
   Set_Visibility (Scenario_Editor.Entry8, True);

   Gtk_New (Scenario_Editor.Entry10);
   Set_Editable (Scenario_Editor.Entry10, True);
   Set_Max_Length (Scenario_Editor.Entry10, 0);
   Set_Text (Scenario_Editor.Entry10, -"Simple_Value");
   Set_Visibility (Scenario_Editor.Entry10, True);
   Attach (Scenario_Editor.List_Variables, Scenario_Editor.Entry10, 3, 4, 4, 5,
     Expand or Fill, 0,
     0, 0);

   Gtk_New (Scenario_Editor.Entry9);
   Set_Editable (Scenario_Editor.Entry9, True);
   Set_Max_Length (Scenario_Editor.Entry9, 0);
   Set_Text (Scenario_Editor.Entry9, -"Current_Value, Value2, Value3");
   Set_Visibility (Scenario_Editor.Entry9, True);
   Attach (Scenario_Editor.List_Variables, Scenario_Editor.Entry9, 3, 4, 5, 6,
     Expand or Fill, 0,
     0, 0);

   Gtk_New (Scenario_Editor.Label44, -("Machine"));
   Set_Alignment (Scenario_Editor.Label44, 7.45058e-09, 0.5);
   Set_Padding (Scenario_Editor.Label44, 0, 0);
   Set_Justify (Scenario_Editor.Label44, Justify_Center);
   Set_Line_Wrap (Scenario_Editor.Label44, False);
   Attach (Scenario_Editor.List_Variables, Scenario_Editor.Label44, 4, 5, 2, 3,
     Expand or Shrink or Fill, 0,
     0, 0);

   Gtk_New (Scenario_Editor.Label49);
   Set_Alignment (Scenario_Editor.Label49, 7.45058e-09, 0.5);
   Set_Padding (Scenario_Editor.Label49, 0, 0);
   Set_Justify (Scenario_Editor.Label49, Justify_Center);
   Set_Line_Wrap (Scenario_Editor.Label49, False);
   Attach (Scenario_Editor.List_Variables, Scenario_Editor.Label49, 4, 5, 3, 4,
     Expand or Shrink or Fill, 0,
     0, 0);

   Gtk_New (Scenario_Editor.Label50);
   Set_Alignment (Scenario_Editor.Label50, 7.45058e-09, 0.5);
   Set_Padding (Scenario_Editor.Label50, 0, 0);
   Set_Justify (Scenario_Editor.Label50, Justify_Center);
   Set_Line_Wrap (Scenario_Editor.Label50, False);
   Attach (Scenario_Editor.List_Variables, Scenario_Editor.Label50, 4, 5, 4, 5,
     Expand or Shrink or Fill, 0,
     0, 0);

   Gtk_New (Scenario_Editor.Label51);
   Set_Alignment (Scenario_Editor.Label51, 7.45058e-09, 0.5);
   Set_Padding (Scenario_Editor.Label51, 0, 0);
   Set_Justify (Scenario_Editor.Label51, Justify_Center);
   Set_Line_Wrap (Scenario_Editor.Label51, False);
   Attach (Scenario_Editor.List_Variables, Scenario_Editor.Label51, 4, 5, 5, 6,
     Expand or Shrink or Fill, 0,
     0, 0);

   Gtk_New (Scenario_Editor.Label52, -("Name"));
   Set_Alignment (Scenario_Editor.Label52, 0.5, 0.5);
   Set_Padding (Scenario_Editor.Label52, 0, 0);
   Set_Justify (Scenario_Editor.Label52, Justify_Center);
   Set_Line_Wrap (Scenario_Editor.Label52, False);
   Attach (Scenario_Editor.List_Variables, Scenario_Editor.Label52, 2, 3, 0, 1,
     0, 0,
     0, 0);

   Gtk_New (Scenario_Editor.Label53, -("Default/Current Value"));
   Set_Alignment (Scenario_Editor.Label53, 0.5, 0.5);
   Set_Padding (Scenario_Editor.Label53, 0, 0);
   Set_Justify (Scenario_Editor.Label53, Justify_Center);
   Set_Line_Wrap (Scenario_Editor.Label53, False);
   Attach (Scenario_Editor.List_Variables, Scenario_Editor.Label53, 3, 4, 0, 1,
     0, 0,
     0, 0);

   Gtk_New (Scenario_Editor.Label54, -("Environment Var."));
   Set_Alignment (Scenario_Editor.Label54, 0.5, 0.5);
   Set_Padding (Scenario_Editor.Label54, 0, 0);
   Set_Justify (Scenario_Editor.Label54, Justify_Center);
   Set_Line_Wrap (Scenario_Editor.Label54, False);
   Attach (Scenario_Editor.List_Variables, Scenario_Editor.Label54, 4, 5, 0, 1,
     0, 0,
     0, 0);

   Gtk_New_Hseparator (Scenario_Editor.Hseparator3);
   Attach (Scenario_Editor.List_Variables, Scenario_Editor.Hseparator3, 0, 5, 1, 2,
     Expand or Fill, Fill,
     0, 5);

   Gtk_New (Scenario_Editor.Button8, -"D");
   Attach (Scenario_Editor.List_Variables, Scenario_Editor.Button8, 1, 2, 2, 3,
     Shrink, 0,
     0, 0);

   Gtk_New (Scenario_Editor.Button12, -"D");
   Attach (Scenario_Editor.List_Variables, Scenario_Editor.Button12, 1, 2, 3, 4,
     Shrink, 0,
     0, 0);

   Gtk_New (Scenario_Editor.Button13, -"D");
   Attach (Scenario_Editor.List_Variables, Scenario_Editor.Button13, 1, 2, 4, 5,
     Shrink, 0,
     0, 0);

   Gtk_New (Scenario_Editor.Button7, -"E");
   Attach (Scenario_Editor.List_Variables, Scenario_Editor.Button7, 0, 1, 2, 3,
     Shrink, 0,
     0, 0);

   Gtk_New (Scenario_Editor.Button9, -"E");
   Attach (Scenario_Editor.List_Variables, Scenario_Editor.Button9, 0, 1, 3, 4,
     Shrink, 0,
     0, 0);

   Gtk_New (Scenario_Editor.Button10, -"E");
   Attach (Scenario_Editor.List_Variables, Scenario_Editor.Button10, 0, 1, 4, 5,
     Shrink, 0,
     0, 0);

   Gtk_New (Scenario_Editor.Button11, -"E");
   Attach (Scenario_Editor.List_Variables, Scenario_Editor.Button11, 0, 1, 5, 6,
     Shrink, 0,
     0, 0);

   Gtk_New (Scenario_Editor.Button16, -"D");
   Attach (Scenario_Editor.List_Variables, Scenario_Editor.Button16, 1, 2, 5, 6,
     Shrink, 0,
     0, 0);

   Gtk_New (Scenario_Editor.Hbuttonbox2);
   Set_Spacing (Scenario_Editor.Hbuttonbox2, 30);
   Set_Layout (Scenario_Editor.Hbuttonbox2, Buttonbox_Spread);
   Set_Child_Size (Scenario_Editor.Hbuttonbox2, 85, 27);
   Set_Child_Ipadding (Scenario_Editor.Hbuttonbox2, 7, 0);
   Pack_Start (Scenario_Editor.Vbox31, Scenario_Editor.Hbuttonbox2, False, False, 0);

   Gtk_New (Scenario_Editor.Add_Button, -"Add...");
   Set_Flags (Scenario_Editor.Add_Button, Can_Default);
   Widget_Callback.Object_Connect
     (Scenario_Editor.Add_Button, "clicked",
      Widget_Callback.To_Marshaller (On_Add_Clicked'Access), Scenario_Editor);
   Add (Scenario_Editor.Hbuttonbox2, Scenario_Editor.Add_Button);

   Gtk_New (Scenario_Editor.Close_Button, -"Close");
   Set_Flags (Scenario_Editor.Close_Button, Can_Default);
   Widget_Callback.Object_Connect
     (Scenario_Editor.Close_Button, "clicked",
      Widget_Callback.To_Marshaller (On_Close_Clicked'Access), Scenario_Editor);
   Add (Scenario_Editor.Hbuttonbox2, Scenario_Editor.Close_Button);

end Initialize;

end Scenario_Editor_Pkg;
