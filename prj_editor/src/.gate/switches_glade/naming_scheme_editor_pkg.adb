with Glib; use Glib;
with Gtk; use Gtk;
with Gdk.Types;       use Gdk.Types;
with Gtk.Widget;      use Gtk.Widget;
with Gtk.Enums;       use Gtk.Enums;
with Gtkada.Handlers; use Gtkada.Handlers;
with Callbacks_Switches_Glade; use Callbacks_Switches_Glade;
with Switches_Glade_Intl; use Switches_Glade_Intl;
with Naming_Scheme_Editor_Pkg.Callbacks; use Naming_Scheme_Editor_Pkg.Callbacks;

package body Naming_Scheme_Editor_Pkg is

procedure Gtk_New (Naming_Scheme_Editor : out Naming_Scheme_Editor_Access) is
begin
   Naming_Scheme_Editor := new Naming_Scheme_Editor_Record;
   Naming_Scheme_Editor_Pkg.Initialize (Naming_Scheme_Editor);
end Gtk_New;

procedure Initialize (Naming_Scheme_Editor : access Naming_Scheme_Editor_Record'Class) is
   pragma Suppress (All_Checks);
   Standard_Scheme_Items : String_List.Glist;
   Casing_Items : String_List.Glist;
   Spec_Extension_Items : String_List.Glist;
   Body_Extension_Items : String_List.Glist;
   Separate_Extension_Items : String_List.Glist;

begin
   Gtk.Window.Initialize (Naming_Scheme_Editor, Window_Toplevel);
   Set_Title (Naming_Scheme_Editor, -"Naming scheme");
   Set_Policy (Naming_Scheme_Editor, False, True, False);
   Set_Position (Naming_Scheme_Editor, Win_Pos_None);
   Set_Modal (Naming_Scheme_Editor, False);

   Gtk_New
     (Naming_Scheme_Editor.Alignment1, 0.5, 0.5, 1.0, 
      1.0);
   Add (Naming_Scheme_Editor, Naming_Scheme_Editor.Alignment1);

   Gtk_New_Vbox (Naming_Scheme_Editor.Main_Box, False, 9);
   Set_Border_Width (Naming_Scheme_Editor.Main_Box, 4);
   Add (Naming_Scheme_Editor.Alignment1, Naming_Scheme_Editor.Main_Box);

   Gtk_New (Naming_Scheme_Editor.Frame29, -"Standard naming scheme");
   Set_Shadow_Type (Naming_Scheme_Editor.Frame29, Shadow_Etched_In);
   Pack_Start (Naming_Scheme_Editor.Main_Box, Naming_Scheme_Editor.Frame29, False, False, 0);

   Gtk_New (Naming_Scheme_Editor.Standard_Scheme);
   Set_Case_Sensitive (Naming_Scheme_Editor.Standard_Scheme, False);
   Set_Use_Arrows (Naming_Scheme_Editor.Standard_Scheme, True);
   Set_Use_Arrows_Always (Naming_Scheme_Editor.Standard_Scheme, False);
   String_List.Append (Standard_Scheme_Items, -"GNAT default");
   String_List.Append (Standard_Scheme_Items, -"unit.separate.1.ada");
   String_List.Append (Standard_Scheme_Items, -"unit__separate_.ada");
   String_List.Append (Standard_Scheme_Items, -"<custom>");
   Combo.Set_Popdown_Strings (Naming_Scheme_Editor.Standard_Scheme, Standard_Scheme_Items);
   Free_String_List (Standard_Scheme_Items);
   Add (Naming_Scheme_Editor.Frame29, Naming_Scheme_Editor.Standard_Scheme);

   Naming_Scheme_Editor.Combo_Entry3 := Get_Entry (Naming_Scheme_Editor.Standard_Scheme);
   Set_Editable (Naming_Scheme_Editor.Combo_Entry3, False);
   Set_Max_Length (Naming_Scheme_Editor.Combo_Entry3, 0);
   Set_Text (Naming_Scheme_Editor.Combo_Entry3, -"GNAT default");
   Set_Visibility (Naming_Scheme_Editor.Combo_Entry3, True);
   Widget_Callback.Object_Connect
     (Naming_Scheme_Editor.Combo_Entry3, "changed",
      Widget_Callback.To_Marshaller (On_Standard_Scheme_Changed'Access), Naming_Scheme_Editor);

   Gtk_New (Naming_Scheme_Editor.Frame28, -"Details");
   Set_Shadow_Type (Naming_Scheme_Editor.Frame28, Shadow_Etched_In);
   Pack_Start (Naming_Scheme_Editor.Main_Box, Naming_Scheme_Editor.Frame28, False, False, 0);

   Gtk_New (Naming_Scheme_Editor.Table1, 5, 2, False);
   Set_Row_Spacings (Naming_Scheme_Editor.Table1, 0);
   Set_Col_Spacings (Naming_Scheme_Editor.Table1, 10);
   Add (Naming_Scheme_Editor.Frame28, Naming_Scheme_Editor.Table1);

   Gtk_New (Naming_Scheme_Editor.Casing);
   Set_Case_Sensitive (Naming_Scheme_Editor.Casing, False);
   Set_Use_Arrows (Naming_Scheme_Editor.Casing, True);
   Set_Use_Arrows_Always (Naming_Scheme_Editor.Casing, False);
   String_List.Append (Casing_Items, -"lowercase");
   String_List.Append (Casing_Items, -"uppercase");
   String_List.Append (Casing_Items, -"mixedcase");
   Combo.Set_Popdown_Strings (Naming_Scheme_Editor.Casing, Casing_Items);
   Free_String_List (Casing_Items);
   Attach (Naming_Scheme_Editor.Table1, Naming_Scheme_Editor.Casing, 1, 2, 0, 1,
     Expand or Fill, 0,
     0, 0);

   Naming_Scheme_Editor.Combo_Entry2 := Get_Entry (Naming_Scheme_Editor.Casing);
   Set_Editable (Naming_Scheme_Editor.Combo_Entry2, False);
   Set_Max_Length (Naming_Scheme_Editor.Combo_Entry2, 0);
   Set_Text (Naming_Scheme_Editor.Combo_Entry2, -"lowercase");
   Set_Visibility (Naming_Scheme_Editor.Combo_Entry2, True);
   Widget_Callback.Object_Connect
     (Naming_Scheme_Editor.Combo_Entry2, "changed",
      Widget_Callback.To_Marshaller (Customized'Access), Naming_Scheme_Editor);

   Gtk_New (Naming_Scheme_Editor.Label23, -("Filename casing:"));
   Set_Alignment (Naming_Scheme_Editor.Label23, 0.5, 0.5);
   Set_Padding (Naming_Scheme_Editor.Label23, 0, 0);
   Set_Justify (Naming_Scheme_Editor.Label23, Justify_Center);
   Set_Line_Wrap (Naming_Scheme_Editor.Label23, False);
   Attach (Naming_Scheme_Editor.Table1, Naming_Scheme_Editor.Label23, 0, 1, 0, 1,
     0, 0,
     0, 0);

   Gtk_New (Naming_Scheme_Editor.Label24, -("Dot replacement:"));
   Set_Alignment (Naming_Scheme_Editor.Label24, 0.5, 0.5);
   Set_Padding (Naming_Scheme_Editor.Label24, 0, 0);
   Set_Justify (Naming_Scheme_Editor.Label24, Justify_Center);
   Set_Line_Wrap (Naming_Scheme_Editor.Label24, False);
   Attach (Naming_Scheme_Editor.Table1, Naming_Scheme_Editor.Label24, 0, 1, 1, 2,
     0, 0,
     0, 0);

   Gtk_New (Naming_Scheme_Editor.Dot_Replacement);
   Set_Editable (Naming_Scheme_Editor.Dot_Replacement, True);
   Set_Max_Length (Naming_Scheme_Editor.Dot_Replacement, 0);
   Set_Text (Naming_Scheme_Editor.Dot_Replacement, -"-");
   Set_Visibility (Naming_Scheme_Editor.Dot_Replacement, True);
   Attach (Naming_Scheme_Editor.Table1, Naming_Scheme_Editor.Dot_Replacement, 1, 2, 1, 2,
     Expand or Fill, 0,
     0, 0);
   Widget_Callback.Object_Connect
     (Naming_Scheme_Editor.Dot_Replacement, "changed",
      Widget_Callback.To_Marshaller (Customized'Access), Naming_Scheme_Editor);

   Gtk_New (Naming_Scheme_Editor.Label25, -("Spec extensions:"));
   Set_Alignment (Naming_Scheme_Editor.Label25, 0.5, 0.5);
   Set_Padding (Naming_Scheme_Editor.Label25, 0, 0);
   Set_Justify (Naming_Scheme_Editor.Label25, Justify_Center);
   Set_Line_Wrap (Naming_Scheme_Editor.Label25, False);
   Attach (Naming_Scheme_Editor.Table1, Naming_Scheme_Editor.Label25, 0, 1, 2, 3,
     0, 0,
     0, 0);

   Gtk_New (Naming_Scheme_Editor.Label26, -("Body extensions:"));
   Set_Alignment (Naming_Scheme_Editor.Label26, 0.5, 0.5);
   Set_Padding (Naming_Scheme_Editor.Label26, 0, 0);
   Set_Justify (Naming_Scheme_Editor.Label26, Justify_Center);
   Set_Line_Wrap (Naming_Scheme_Editor.Label26, False);
   Attach (Naming_Scheme_Editor.Table1, Naming_Scheme_Editor.Label26, 0, 1, 3, 4,
     0, 0,
     0, 0);

   Gtk_New (Naming_Scheme_Editor.Label27, -("Separate extensions:"));
   Set_Alignment (Naming_Scheme_Editor.Label27, 0.5, 0.5);
   Set_Padding (Naming_Scheme_Editor.Label27, 0, 0);
   Set_Justify (Naming_Scheme_Editor.Label27, Justify_Center);
   Set_Line_Wrap (Naming_Scheme_Editor.Label27, False);
   Attach (Naming_Scheme_Editor.Table1, Naming_Scheme_Editor.Label27, 0, 1, 4, 5,
     0, 0,
     0, 0);

   Gtk_New (Naming_Scheme_Editor.Spec_Extension);
   Set_Case_Sensitive (Naming_Scheme_Editor.Spec_Extension, False);
   Set_Use_Arrows (Naming_Scheme_Editor.Spec_Extension, True);
   Set_Use_Arrows_Always (Naming_Scheme_Editor.Spec_Extension, False);
   String_List.Append (Spec_Extension_Items, -".ads");
   String_List.Append (Spec_Extension_Items, -".1.ada");
   String_List.Append (Spec_Extension_Items, -"_.ada");
   Combo.Set_Popdown_Strings (Naming_Scheme_Editor.Spec_Extension, Spec_Extension_Items);
   Free_String_List (Spec_Extension_Items);
   Attach (Naming_Scheme_Editor.Table1, Naming_Scheme_Editor.Spec_Extension, 1, 2, 2, 3,
     Expand or Fill, 0,
     0, 0);

   Naming_Scheme_Editor.Combo_Entry4 := Get_Entry (Naming_Scheme_Editor.Spec_Extension);
   Set_Editable (Naming_Scheme_Editor.Combo_Entry4, True);
   Set_Max_Length (Naming_Scheme_Editor.Combo_Entry4, 0);
   Set_Text (Naming_Scheme_Editor.Combo_Entry4, -".ads");
   Set_Visibility (Naming_Scheme_Editor.Combo_Entry4, True);
   Widget_Callback.Object_Connect
     (Naming_Scheme_Editor.Combo_Entry4, "changed",
      Widget_Callback.To_Marshaller (Customized'Access), Naming_Scheme_Editor);

   Gtk_New (Naming_Scheme_Editor.Body_Extension);
   Set_Case_Sensitive (Naming_Scheme_Editor.Body_Extension, False);
   Set_Use_Arrows (Naming_Scheme_Editor.Body_Extension, True);
   Set_Use_Arrows_Always (Naming_Scheme_Editor.Body_Extension, False);
   String_List.Append (Body_Extension_Items, -".adb");
   String_List.Append (Body_Extension_Items, -".2.ada");
   String_List.Append (Body_Extension_Items, -".ada");
   Combo.Set_Popdown_Strings (Naming_Scheme_Editor.Body_Extension, Body_Extension_Items);
   Free_String_List (Body_Extension_Items);
   Attach (Naming_Scheme_Editor.Table1, Naming_Scheme_Editor.Body_Extension, 1, 2, 3, 4,
     Expand or Fill, 0,
     0, 0);

   Naming_Scheme_Editor.Combo_Entry5 := Get_Entry (Naming_Scheme_Editor.Body_Extension);
   Set_Editable (Naming_Scheme_Editor.Combo_Entry5, True);
   Set_Max_Length (Naming_Scheme_Editor.Combo_Entry5, 0);
   Set_Text (Naming_Scheme_Editor.Combo_Entry5, -".adb");
   Set_Visibility (Naming_Scheme_Editor.Combo_Entry5, True);
   Widget_Callback.Object_Connect
     (Naming_Scheme_Editor.Combo_Entry5, "changed",
      Widget_Callback.To_Marshaller (Customized'Access), Naming_Scheme_Editor);

   Gtk_New (Naming_Scheme_Editor.Separate_Extension);
   Set_Case_Sensitive (Naming_Scheme_Editor.Separate_Extension, False);
   Set_Use_Arrows (Naming_Scheme_Editor.Separate_Extension, True);
   Set_Use_Arrows_Always (Naming_Scheme_Editor.Separate_Extension, False);
   String_List.Append (Separate_Extension_Items, -".adb");
   String_List.Append (Separate_Extension_Items, -".2.ada");
   String_List.Append (Separate_Extension_Items, -".ada");
   Combo.Set_Popdown_Strings (Naming_Scheme_Editor.Separate_Extension, Separate_Extension_Items);
   Free_String_List (Separate_Extension_Items);
   Attach (Naming_Scheme_Editor.Table1, Naming_Scheme_Editor.Separate_Extension, 1, 2, 4, 5,
     Expand or Fill, 0,
     0, 0);

   Naming_Scheme_Editor.Combo_Entry6 := Get_Entry (Naming_Scheme_Editor.Separate_Extension);
   Set_Editable (Naming_Scheme_Editor.Combo_Entry6, True);
   Set_Max_Length (Naming_Scheme_Editor.Combo_Entry6, 0);
   Set_Text (Naming_Scheme_Editor.Combo_Entry6, -".adb");
   Set_Visibility (Naming_Scheme_Editor.Combo_Entry6, True);
   Widget_Callback.Object_Connect
     (Naming_Scheme_Editor.Combo_Entry6, "changed",
      Widget_Callback.To_Marshaller (Customized'Access), Naming_Scheme_Editor);

   Gtk_New (Naming_Scheme_Editor.Frame30, -"Exceptions");
   Set_Shadow_Type (Naming_Scheme_Editor.Frame30, Shadow_Etched_In);
   Pack_Start (Naming_Scheme_Editor.Main_Box, Naming_Scheme_Editor.Frame30, True, True, 0);

   Gtk_New_Vbox (Naming_Scheme_Editor.Vbox29, False, 0);
   Set_Border_Width (Naming_Scheme_Editor.Vbox29, 3);
   Add (Naming_Scheme_Editor.Frame30, Naming_Scheme_Editor.Vbox29);

   Gtk_New (Naming_Scheme_Editor.Scrolledwindow1);
   Set_Policy (Naming_Scheme_Editor.Scrolledwindow1, Policy_Automatic, Policy_Automatic);
   Pack_Start (Naming_Scheme_Editor.Vbox29, Naming_Scheme_Editor.Scrolledwindow1, True, True, 0);

   Gtk_New (Naming_Scheme_Editor.Exception_List, 3);
   Set_Selection_Mode (Naming_Scheme_Editor.Exception_List, Selection_Single);
   Set_Shadow_Type (Naming_Scheme_Editor.Exception_List, Shadow_In);
   Set_Show_Titles (Naming_Scheme_Editor.Exception_List, True);
   Set_Column_Width (Naming_Scheme_Editor.Exception_List, 0, 80);
   Set_Column_Width (Naming_Scheme_Editor.Exception_List, 1, 124);
   Set_Column_Width (Naming_Scheme_Editor.Exception_List, 2, 80);
   Widget_Callback.Object_Connect
     (Naming_Scheme_Editor.Exception_List, "select_row", On_Exceptions_List_Select_Row'Access, Naming_Scheme_Editor);
   Return_Callback.Object_Connect
     (Naming_Scheme_Editor.Exception_List, "key_press_event", On_Exception_List_Key_Press_Event'Access, Naming_Scheme_Editor);
   Add (Naming_Scheme_Editor.Scrolledwindow1, Naming_Scheme_Editor.Exception_List);

   Gtk_New (Naming_Scheme_Editor.Label28, -("Unit name"));
   Set_Alignment (Naming_Scheme_Editor.Label28, 0.5, 0.5);
   Set_Padding (Naming_Scheme_Editor.Label28, 0, 0);
   Set_Justify (Naming_Scheme_Editor.Label28, Justify_Center);
   Set_Line_Wrap (Naming_Scheme_Editor.Label28, False);
   Set_Column_Widget (Naming_Scheme_Editor.Exception_List, 0, Naming_Scheme_Editor.Label28);

   Gtk_New (Naming_Scheme_Editor.Label29, -("Spec filename"));
   Set_Alignment (Naming_Scheme_Editor.Label29, 0.5, 0.5);
   Set_Padding (Naming_Scheme_Editor.Label29, 0, 0);
   Set_Justify (Naming_Scheme_Editor.Label29, Justify_Center);
   Set_Line_Wrap (Naming_Scheme_Editor.Label29, False);
   Set_Column_Widget (Naming_Scheme_Editor.Exception_List, 1, Naming_Scheme_Editor.Label29);

   Gtk_New (Naming_Scheme_Editor.Label30, -("Body filename"));
   Set_Alignment (Naming_Scheme_Editor.Label30, 0.5, 0.5);
   Set_Padding (Naming_Scheme_Editor.Label30, 0, 0);
   Set_Justify (Naming_Scheme_Editor.Label30, Justify_Center);
   Set_Line_Wrap (Naming_Scheme_Editor.Label30, False);
   Set_Column_Widget (Naming_Scheme_Editor.Exception_List, 2, Naming_Scheme_Editor.Label30);

   Gtk_New_Hbox (Naming_Scheme_Editor.Hbox3, False, 3);
   Pack_Start (Naming_Scheme_Editor.Vbox29, Naming_Scheme_Editor.Hbox3, False, False, 0);

   Gtk_New (Naming_Scheme_Editor.Unit_Name_Entry);
   Set_Editable (Naming_Scheme_Editor.Unit_Name_Entry, True);
   Set_Max_Length (Naming_Scheme_Editor.Unit_Name_Entry, 0);
   Set_Text (Naming_Scheme_Editor.Unit_Name_Entry, -"");
   Set_Visibility (Naming_Scheme_Editor.Unit_Name_Entry, True);
   Pack_Start (Naming_Scheme_Editor.Hbox3, Naming_Scheme_Editor.Unit_Name_Entry, True, True, 0);
   Widget_Callback.Object_Connect
     (Naming_Scheme_Editor.Unit_Name_Entry, "activate",
      Widget_Callback.To_Marshaller (On_Update_Clicked'Access), Naming_Scheme_Editor);
   Widget_Callback.Object_Connect
     (Naming_Scheme_Editor.Unit_Name_Entry, "insert_text", On_Unit_Name_Entry_Insert_Text'Access, Naming_Scheme_Editor);
   Return_Callback.Object_Connect
     (Naming_Scheme_Editor.Unit_Name_Entry, "key_press_event", On_Unit_Name_Entry_Key_Press_Event'Access, Naming_Scheme_Editor);

   Gtk_New (Naming_Scheme_Editor.Spec_Filename_Entry);
   Set_Editable (Naming_Scheme_Editor.Spec_Filename_Entry, True);
   Set_Max_Length (Naming_Scheme_Editor.Spec_Filename_Entry, 0);
   Set_Text (Naming_Scheme_Editor.Spec_Filename_Entry, -"");
   Set_Visibility (Naming_Scheme_Editor.Spec_Filename_Entry, True);
   Pack_Start (Naming_Scheme_Editor.Hbox3, Naming_Scheme_Editor.Spec_Filename_Entry, True, True, 0);
   Widget_Callback.Object_Connect
     (Naming_Scheme_Editor.Spec_Filename_Entry, "activate",
      Widget_Callback.To_Marshaller (On_Update_Clicked'Access), Naming_Scheme_Editor);
   Widget_Callback.Object_Connect
     (Naming_Scheme_Editor.Spec_Filename_Entry, "insert_text", On_Spec_Filename_Entry_Insert_Text'Access, Naming_Scheme_Editor);
   Return_Callback.Object_Connect
     (Naming_Scheme_Editor.Spec_Filename_Entry, "key_press_event", On_Spec_Filename_Entry_Key_Press_Event'Access, Naming_Scheme_Editor);

   Gtk_New (Naming_Scheme_Editor.Body_Filename_Entry);
   Set_Editable (Naming_Scheme_Editor.Body_Filename_Entry, True);
   Set_Max_Length (Naming_Scheme_Editor.Body_Filename_Entry, 0);
   Set_Text (Naming_Scheme_Editor.Body_Filename_Entry, -"");
   Set_Visibility (Naming_Scheme_Editor.Body_Filename_Entry, True);
   Pack_Start (Naming_Scheme_Editor.Hbox3, Naming_Scheme_Editor.Body_Filename_Entry, True, True, 0);
   Widget_Callback.Object_Connect
     (Naming_Scheme_Editor.Body_Filename_Entry, "activate",
      Widget_Callback.To_Marshaller (On_Update_Clicked'Access), Naming_Scheme_Editor);
   Widget_Callback.Object_Connect
     (Naming_Scheme_Editor.Body_Filename_Entry, "insert_text", On_Body_Filename_Entry_Insert_Text'Access, Naming_Scheme_Editor);
   Return_Callback.Object_Connect
     (Naming_Scheme_Editor.Body_Filename_Entry, "key_press_event", On_Body_Filename_Entry_Key_Press_Event'Access, Naming_Scheme_Editor);

   Gtk_New (Naming_Scheme_Editor.Update, -"Update");
   Pack_Start (Naming_Scheme_Editor.Hbox3, Naming_Scheme_Editor.Update, False, False, 0);
   Widget_Callback.Object_Connect
     (Naming_Scheme_Editor.Update, "clicked",
      Widget_Callback.To_Marshaller (On_Update_Clicked'Access), Naming_Scheme_Editor);

end Initialize;

end Naming_Scheme_Editor_Pkg;
