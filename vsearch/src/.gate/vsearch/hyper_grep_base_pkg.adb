with Glib; use Glib;
with Gtk; use Gtk;
with Gdk.Types;       use Gdk.Types;
with Gtk.Widget;      use Gtk.Widget;
with Gtk.Enums;       use Gtk.Enums;
with Gtkada.Handlers; use Gtkada.Handlers;
with Callbacks_Hypergrep; use Callbacks_Hypergrep;
with Hyper_Grep_Base_Pkg.Callbacks; use Hyper_Grep_Base_Pkg.Callbacks;

package body Hyper_Grep_Base_Pkg is

procedure Gtk_New (Hyper_Grep_Base : out Hyper_Grep_Base_Access) is
begin
   Hyper_Grep_Base := new Hyper_Grep_Base_Record;
   Hyper_Grep_Base_Pkg.Initialize (Hyper_Grep_Base);
end Gtk_New;

procedure Initialize (Hyper_Grep_Base : access Hyper_Grep_Base_Record'Class) is
   pragma Suppress (All_Checks);
   Pattern_Combo_Items : String_List.Glist;
   Scan_In_Vbox_Group : Widget_SList.GSList;
   Directory_Combo_Items : String_List.Glist;
   Files_Combo_Items : String_List.Glist;

begin
   Gtk.Window.Initialize (Hyper_Grep_Base, Window_Toplevel);
   Set_Title (Hyper_Grep_Base, "Hyper grep");
   Set_Policy (Hyper_Grep_Base, False, True, False);
   Set_Position (Hyper_Grep_Base, Win_Pos_None);
   Set_Modal (Hyper_Grep_Base, False);
   Return_Callback.Connect
     (Hyper_Grep_Base, "delete_event", On_Hyper_Grep_Delete_Event'Access);

   Gtk_New_Vbox (Hyper_Grep_Base.Main_Table, False, 0);
   Add (Hyper_Grep_Base, Hyper_Grep_Base.Main_Table);

   Gtk_New (Hyper_Grep_Base.Search_Frame, "Search");
   Set_Border_Width (Hyper_Grep_Base.Search_Frame, 5);
   Set_Shadow_Type (Hyper_Grep_Base.Search_Frame, Shadow_Etched_In);
   Pack_Start (Hyper_Grep_Base.Main_Table, Hyper_Grep_Base.Search_Frame, False, False, 0);

   Gtk_New (Hyper_Grep_Base.Search_Table, 2, 4, False);
   Set_Border_Width (Hyper_Grep_Base.Search_Table, 5);
   Set_Row_Spacings (Hyper_Grep_Base.Search_Table, 0);
   Set_Col_Spacings (Hyper_Grep_Base.Search_Table, 0);
   Add (Hyper_Grep_Base.Search_Frame, Hyper_Grep_Base.Search_Table);

   Gtk_New (Hyper_Grep_Base.Pattern_Combo);
   Set_Case_Sensitive (Hyper_Grep_Base.Pattern_Combo, False);
   Set_Use_Arrows (Hyper_Grep_Base.Pattern_Combo, True);
   Set_Use_Arrows_Always (Hyper_Grep_Base.Pattern_Combo, False);
   String_List.Append (Pattern_Combo_Items, "");
   Combo.Set_Popdown_Strings (Hyper_Grep_Base.Pattern_Combo, Pattern_Combo_Items);
   Free_String_List (Pattern_Combo_Items);
   Attach (Hyper_Grep_Base.Search_Table, Hyper_Grep_Base.Pattern_Combo, 1, 4, 0, 1,
     Expand or Fill, 0,
     0, 0);

   Hyper_Grep_Base.Pattern_Entry := Get_Entry (Hyper_Grep_Base.Pattern_Combo);
   Set_Editable (Hyper_Grep_Base.Pattern_Entry, True);
   Set_Max_Length (Hyper_Grep_Base.Pattern_Entry, 0);
   Set_Text (Hyper_Grep_Base.Pattern_Entry, "");
   Set_Visibility (Hyper_Grep_Base.Pattern_Entry, True);

   Gtk_New (Hyper_Grep_Base.Search_For_Label, "Search for:");
   Set_Alignment (Hyper_Grep_Base.Search_For_Label, 0.0, 0.5);
   Set_Padding (Hyper_Grep_Base.Search_For_Label, 0, 0);
   Set_Justify (Hyper_Grep_Base.Search_For_Label, Justify_Center);
   Set_Line_Wrap (Hyper_Grep_Base.Search_For_Label, False);
   Attach (Hyper_Grep_Base.Search_Table, Hyper_Grep_Base.Search_For_Label, 0, 1, 0, 1,
     Fill, 0,
     0, 0);

   Gtk_New (Hyper_Grep_Base.Scope_Frame, "Scope");
   Set_Border_Width (Hyper_Grep_Base.Scope_Frame, 5);
   Set_Shadow_Type (Hyper_Grep_Base.Scope_Frame, Shadow_Etched_In);
   Attach (Hyper_Grep_Base.Search_Table, Hyper_Grep_Base.Scope_Frame, 2, 4, 1, 2,
     Fill, Fill,
     0, 0);

   Gtk_New_Vbox (Hyper_Grep_Base.Scan_In_Vbox, False, 0);
   Add (Hyper_Grep_Base.Scope_Frame, Hyper_Grep_Base.Scan_In_Vbox);

   Gtk_New (Hyper_Grep_Base.Whole_Rbutton, Scan_In_Vbox_Group, "Whole text");
   Scan_In_Vbox_Group := Group (Hyper_Grep_Base.Whole_Rbutton);
   Set_Active (Hyper_Grep_Base.Whole_Rbutton, True);
   Pack_Start (Hyper_Grep_Base.Scan_In_Vbox, Hyper_Grep_Base.Whole_Rbutton, False, False, 0);

   Gtk_New (Hyper_Grep_Base.Comm_Only_Rbutton, Scan_In_Vbox_Group, "Comments only");
   Scan_In_Vbox_Group := Group (Hyper_Grep_Base.Comm_Only_Rbutton);
   Set_Active (Hyper_Grep_Base.Comm_Only_Rbutton, False);
   Pack_Start (Hyper_Grep_Base.Scan_In_Vbox, Hyper_Grep_Base.Comm_Only_Rbutton, False, False, 0);

   Gtk_New (Hyper_Grep_Base.Strings_Rbutton, Scan_In_Vbox_Group, "Strings only");
   Scan_In_Vbox_Group := Group (Hyper_Grep_Base.Strings_Rbutton);
   Set_Active (Hyper_Grep_Base.Strings_Rbutton, False);
   Pack_Start (Hyper_Grep_Base.Scan_In_Vbox, Hyper_Grep_Base.Strings_Rbutton, False, False, 0);

   Gtk_New (Hyper_Grep_Base.Comm_Str_Rbutton, Scan_In_Vbox_Group, "Comments and strings");
   Scan_In_Vbox_Group := Group (Hyper_Grep_Base.Comm_Str_Rbutton);
   Set_Active (Hyper_Grep_Base.Comm_Str_Rbutton, False);
   Pack_Start (Hyper_Grep_Base.Scan_In_Vbox, Hyper_Grep_Base.Comm_Str_Rbutton, False, False, 0);

   Gtk_New (Hyper_Grep_Base.All_But_Comm_Rbutton, Scan_In_Vbox_Group, "All but comments");
   Scan_In_Vbox_Group := Group (Hyper_Grep_Base.All_But_Comm_Rbutton);
   Set_Active (Hyper_Grep_Base.All_But_Comm_Rbutton, False);
   Pack_Start (Hyper_Grep_Base.Scan_In_Vbox, Hyper_Grep_Base.All_But_Comm_Rbutton, False, False, 0);

   Gtk_New (Hyper_Grep_Base.Options_Frame, "Options");
   Set_Border_Width (Hyper_Grep_Base.Options_Frame, 5);
   Set_Shadow_Type (Hyper_Grep_Base.Options_Frame, Shadow_Etched_In);
   Attach (Hyper_Grep_Base.Search_Table, Hyper_Grep_Base.Options_Frame, 0, 2, 1, 2,
     Fill, Fill,
     0, 0);

   Gtk_New_Vbox (Hyper_Grep_Base.Options_Vbox, False, 0);
   Add (Hyper_Grep_Base.Options_Frame, Hyper_Grep_Base.Options_Vbox);

   Gtk_New (Hyper_Grep_Base.Case_Check, "Match case");
   Set_Active (Hyper_Grep_Base.Case_Check, False);
   Pack_Start (Hyper_Grep_Base.Options_Vbox, Hyper_Grep_Base.Case_Check, True, True, 0);

   Gtk_New (Hyper_Grep_Base.Whole_Word_Check, "Match whole word");
   Set_Active (Hyper_Grep_Base.Whole_Word_Check, False);
   Pack_Start (Hyper_Grep_Base.Options_Vbox, Hyper_Grep_Base.Whole_Word_Check, True, True, 0);

   Gtk_New (Hyper_Grep_Base.Regexp_Check, "Regular expression");
   Set_Active (Hyper_Grep_Base.Regexp_Check, False);
   Pack_Start (Hyper_Grep_Base.Options_Vbox, Hyper_Grep_Base.Regexp_Check, True, True, 0);

   Gtk_New (Hyper_Grep_Base.Files_Frame, "Files");
   Set_Border_Width (Hyper_Grep_Base.Files_Frame, 5);
   Set_Shadow_Type (Hyper_Grep_Base.Files_Frame, Shadow_Etched_In);
   Pack_Start (Hyper_Grep_Base.Main_Table, Hyper_Grep_Base.Files_Frame, False, False, 0);

   Gtk_New (Hyper_Grep_Base.Files_Table, 4, 3, False);
   Set_Border_Width (Hyper_Grep_Base.Files_Table, 5);
   Set_Row_Spacings (Hyper_Grep_Base.Files_Table, 5);
   Set_Col_Spacings (Hyper_Grep_Base.Files_Table, 5);
   Add (Hyper_Grep_Base.Files_Frame, Hyper_Grep_Base.Files_Table);

   Gtk_New (Hyper_Grep_Base.Browse_Button, "Browse...");
   Set_Sensitive (Hyper_Grep_Base.Browse_Button, False);
   Attach (Hyper_Grep_Base.Files_Table, Hyper_Grep_Base.Browse_Button, 2, 3, 2, 3,
     0, 0,
     0, 0);
   Button_Callback.Connect
     (Hyper_Grep_Base.Browse_Button, "clicked",
      Button_Callback.To_Marshaller (On_Browse_Button_Clicked'Access));

   Gtk_New (Hyper_Grep_Base.Files_Label, "Files:");
   Set_Alignment (Hyper_Grep_Base.Files_Label, 0.0, 0.5);
   Set_Padding (Hyper_Grep_Base.Files_Label, 0, 0);
   Set_Justify (Hyper_Grep_Base.Files_Label, Justify_Center);
   Set_Line_Wrap (Hyper_Grep_Base.Files_Label, False);
   Set_Sensitive (Hyper_Grep_Base.Files_Label, False);
   Attach (Hyper_Grep_Base.Files_Table, Hyper_Grep_Base.Files_Label, 0, 1, 1, 2,
     Fill, 0,
     0, 0);

   Gtk_New (Hyper_Grep_Base.Directory_Label, "Directory:");
   Set_Alignment (Hyper_Grep_Base.Directory_Label, 0.0, 0.5);
   Set_Padding (Hyper_Grep_Base.Directory_Label, 0, 0);
   Set_Justify (Hyper_Grep_Base.Directory_Label, Justify_Center);
   Set_Line_Wrap (Hyper_Grep_Base.Directory_Label, False);
   Set_Sensitive (Hyper_Grep_Base.Directory_Label, False);
   Attach (Hyper_Grep_Base.Files_Table, Hyper_Grep_Base.Directory_Label, 0, 1, 2, 3,
     Fill, 0,
     0, 0);

   Gtk_New (Hyper_Grep_Base.Directory_Combo);
   Set_Case_Sensitive (Hyper_Grep_Base.Directory_Combo, False);
   Set_Use_Arrows (Hyper_Grep_Base.Directory_Combo, True);
   Set_Use_Arrows_Always (Hyper_Grep_Base.Directory_Combo, False);
   String_List.Append (Directory_Combo_Items, "");
   Combo.Set_Popdown_Strings (Hyper_Grep_Base.Directory_Combo, Directory_Combo_Items);
   Free_String_List (Directory_Combo_Items);
   Set_Sensitive (Hyper_Grep_Base.Directory_Combo, False);
   Attach (Hyper_Grep_Base.Files_Table, Hyper_Grep_Base.Directory_Combo, 1, 2, 2, 3,
     Expand or Fill, 0,
     0, 0);

   Hyper_Grep_Base.Directory_Entry := Get_Entry (Hyper_Grep_Base.Directory_Combo);
   Set_Editable (Hyper_Grep_Base.Directory_Entry, True);
   Set_Max_Length (Hyper_Grep_Base.Directory_Entry, 0);
   Set_Text (Hyper_Grep_Base.Directory_Entry, "");
   Set_Visibility (Hyper_Grep_Base.Directory_Entry, True);

   Gtk_New (Hyper_Grep_Base.Only_Project_Check, "Scan only project files");
   Set_Active (Hyper_Grep_Base.Only_Project_Check, True);
   Attach (Hyper_Grep_Base.Files_Table, Hyper_Grep_Base.Only_Project_Check, 0, 3, 0, 1,
     Fill, 0,
     0, 0);
   Check_Button_Callback.Connect
     (Hyper_Grep_Base.Only_Project_Check, "toggled",
      Check_Button_Callback.To_Marshaller (On_Only_Project_Check_Toggled'Access));

   Gtk_New (Hyper_Grep_Base.Subdirs_Check, "Include sub-directories");
   Set_Active (Hyper_Grep_Base.Subdirs_Check, False);
   Set_Sensitive (Hyper_Grep_Base.Subdirs_Check, False);
   Attach (Hyper_Grep_Base.Files_Table, Hyper_Grep_Base.Subdirs_Check, 1, 3, 3, 4,
     Fill, 0,
     0, 0);

   Gtk_New (Hyper_Grep_Base.Files_Combo);
   Set_Case_Sensitive (Hyper_Grep_Base.Files_Combo, False);
   Set_Use_Arrows (Hyper_Grep_Base.Files_Combo, True);
   Set_Use_Arrows_Always (Hyper_Grep_Base.Files_Combo, False);
   String_List.Append (Files_Combo_Items, "");
   Combo.Set_Popdown_Strings (Hyper_Grep_Base.Files_Combo, Files_Combo_Items);
   Free_String_List (Files_Combo_Items);
   Set_Sensitive (Hyper_Grep_Base.Files_Combo, False);
   Attach (Hyper_Grep_Base.Files_Table, Hyper_Grep_Base.Files_Combo, 1, 2, 1, 2,
     Expand or Fill, 0,
     0, 0);

   Hyper_Grep_Base.Files_Entry := Get_Entry (Hyper_Grep_Base.Files_Combo);
   Set_Editable (Hyper_Grep_Base.Files_Entry, True);
   Set_Max_Length (Hyper_Grep_Base.Files_Entry, 0);
   Set_Text (Hyper_Grep_Base.Files_Entry, "");
   Set_Visibility (Hyper_Grep_Base.Files_Entry, True);

   Gtk_New (Hyper_Grep_Base.Hbuttonbox);
   Set_Spacing (Hyper_Grep_Base.Hbuttonbox, 30);
   Set_Layout (Hyper_Grep_Base.Hbuttonbox, Buttonbox_Spread);
   Set_Child_Size (Hyper_Grep_Base.Hbuttonbox, 85, 27);
   Set_Child_Ipadding (Hyper_Grep_Base.Hbuttonbox, 0, 0);
   Add (Hyper_Grep_Base.Main_Table, Hyper_Grep_Base.Hbuttonbox);

   Gtk_New (Hyper_Grep_Base.Start_Button, "Start");
   Set_Flags (Hyper_Grep_Base.Start_Button, Can_Default);
   Grab_Default (Hyper_Grep_Base.Start_Button);
   Button_Callback.Connect
     (Hyper_Grep_Base.Start_Button, "clicked",
      Button_Callback.To_Marshaller (On_Start_Button_Clicked'Access));
   Add (Hyper_Grep_Base.Hbuttonbox, Hyper_Grep_Base.Start_Button);

   Gtk_New (Hyper_Grep_Base.Stop_Button, "Stop");
   Set_Flags (Hyper_Grep_Base.Stop_Button, Can_Default);
   Button_Callback.Connect
     (Hyper_Grep_Base.Stop_Button, "clicked",
      Button_Callback.To_Marshaller (On_Stop_Button_Clicked'Access));
   Add (Hyper_Grep_Base.Hbuttonbox, Hyper_Grep_Base.Stop_Button);

   Gtk_New (Hyper_Grep_Base.Close_Button, "Close");
   Set_Flags (Hyper_Grep_Base.Close_Button, Can_Default);
   Button_Callback.Connect
     (Hyper_Grep_Base.Close_Button, "clicked",
      Button_Callback.To_Marshaller (On_Close_Button_Clicked'Access));
   Add (Hyper_Grep_Base.Hbuttonbox, Hyper_Grep_Base.Close_Button);

end Initialize;

end Hyper_Grep_Base_Pkg;
