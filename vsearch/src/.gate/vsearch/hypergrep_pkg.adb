with Glib; use Glib;
with Gtk; use Gtk;
with Gdk.Types;       use Gdk.Types;
with Gtk.Widget;      use Gtk.Widget;
with Gtk.Enums;       use Gtk.Enums;
with Gtkada.Handlers; use Gtkada.Handlers;
with Callbacks_Hypergrep; use Callbacks_Hypergrep;
with Hypergrep_Pkg.Callbacks; use Hypergrep_Pkg.Callbacks;

package body Hypergrep_Pkg is

procedure Gtk_New (Hypergrep : out Hypergrep_Access) is
begin
   Hypergrep := new Hypergrep_Record;
   Hypergrep_Pkg.Initialize (Hypergrep);
end Gtk_New;

procedure Initialize (Hypergrep : access Hypergrep_Record'Class) is
   pragma Suppress (All_Checks);
   Pattern_Combo_Items : String_List.Glist;
   Directory_Combo_Items : String_List.Glist;
   Files_Combo_Items : String_List.Glist;

begin
   Gtk.Window.Initialize (Hypergrep, Window_Toplevel);
   Set_Title (Hypergrep, "Hyper grep");
   Set_Policy (Hypergrep, False, True, False);
   Set_Position (Hypergrep, Win_Pos_None);
   Set_Modal (Hypergrep, False);
   Return_Callback.Connect
     (Hypergrep, "delete_event", On_Hyper_Grep_Delete_Event'Access);

   Gtk_New_Vbox (Hypergrep.Main_Box, False, 0);
   Add (Hypergrep, Hypergrep.Main_Box);

   Gtk_New (Hypergrep.Search_Frame, "Search");
   Set_Border_Width (Hypergrep.Search_Frame, 5);
   Set_Shadow_Type (Hypergrep.Search_Frame, Shadow_Etched_In);
   Pack_Start (Hypergrep.Main_Box, Hypergrep.Search_Frame, False, False, 0);

   Gtk_New (Hypergrep.Tbl_Find, 5, 3, False);
   Set_Border_Width (Hypergrep.Tbl_Find, 5);
   Set_Row_Spacings (Hypergrep.Tbl_Find, 0);
   Set_Col_Spacings (Hypergrep.Tbl_Find, 0);
   Add (Hypergrep.Search_Frame, Hypergrep.Tbl_Find);

   Gtk_New (Hypergrep.Statements_Check, "Statements");
   Set_Active (Hypergrep.Statements_Check, True);
   Attach (Hypergrep.Tbl_Find, Hypergrep.Statements_Check, 2, 3, 2, 3,
     Fill, 0,
     0, 0);

   Gtk_New (Hypergrep.Strings_Check, "Strings");
   Set_Active (Hypergrep.Strings_Check, True);
   Attach (Hypergrep.Tbl_Find, Hypergrep.Strings_Check, 2, 3, 3, 4,
     Fill, 0,
     0, 0);

   Gtk_New (Hypergrep.Comments_Check, "Comments");
   Set_Active (Hypergrep.Comments_Check, True);
   Attach (Hypergrep.Tbl_Find, Hypergrep.Comments_Check, 2, 3, 4, 5,
     Fill, 0,
     0, 0);

   Gtk_New (Hypergrep.Case_Check, "Match case");
   Set_Active (Hypergrep.Case_Check, False);
   Attach (Hypergrep.Tbl_Find, Hypergrep.Case_Check, 0, 2, 2, 3,
     Fill, 0,
     0, 0);

   Gtk_New (Hypergrep.Whole_Word_Check, "Match whole word");
   Set_Active (Hypergrep.Whole_Word_Check, False);
   Attach (Hypergrep.Tbl_Find, Hypergrep.Whole_Word_Check, 0, 2, 3, 4,
     Fill, 0,
     0, 0);

   Gtk_New (Hypergrep.Regexp_Check, "Regular expression");
   Set_Active (Hypergrep.Regexp_Check, False);
   Attach (Hypergrep.Tbl_Find, Hypergrep.Regexp_Check, 0, 2, 4, 5,
     Fill, 0,
     0, 0);

   Gtk_New (Hypergrep.Label8, "Search for:");
   Set_Alignment (Hypergrep.Label8, 0.0, 0.5);
   Set_Padding (Hypergrep.Label8, 0, 0);
   Set_Justify (Hypergrep.Label8, Justify_Center);
   Set_Line_Wrap (Hypergrep.Label8, False);
   Attach (Hypergrep.Tbl_Find, Hypergrep.Label8, 0, 1, 0, 1,
     Fill, 0,
     0, 0);

   Gtk_New (Hypergrep.Pattern_Combo);
   Set_Case_Sensitive (Hypergrep.Pattern_Combo, False);
   Set_Use_Arrows (Hypergrep.Pattern_Combo, True);
   Set_Use_Arrows_Always (Hypergrep.Pattern_Combo, False);
   String_List.Append (Pattern_Combo_Items, "");
   Combo.Set_Popdown_Strings (Hypergrep.Pattern_Combo, Pattern_Combo_Items);
   Free_String_List (Pattern_Combo_Items);
   Attach (Hypergrep.Tbl_Find, Hypergrep.Pattern_Combo, 1, 3, 0, 1,
     Expand or Fill, 0,
     0, 0);

   Hypergrep.Pattern_Entry := Get_Entry (Hypergrep.Pattern_Combo);
   Set_Editable (Hypergrep.Pattern_Entry, True);
   Set_Max_Length (Hypergrep.Pattern_Entry, 0);
   Set_Text (Hypergrep.Pattern_Entry, "");
   Set_Visibility (Hypergrep.Pattern_Entry, True);

   Gtk_New (Hypergrep.Label9, "Options:");
   Set_Alignment (Hypergrep.Label9, 0.0, 1.0);
   Set_Padding (Hypergrep.Label9, 0, 0);
   Set_Justify (Hypergrep.Label9, Justify_Center);
   Set_Line_Wrap (Hypergrep.Label9, False);
   Attach (Hypergrep.Tbl_Find, Hypergrep.Label9, 0, 2, 1, 2,
     Fill, 0,
     0, 3);

   Gtk_New (Hypergrep.Label10, "Search through:");
   Set_Alignment (Hypergrep.Label10, 0.0, 1.0);
   Set_Padding (Hypergrep.Label10, 0, 0);
   Set_Justify (Hypergrep.Label10, Justify_Center);
   Set_Line_Wrap (Hypergrep.Label10, False);
   Attach (Hypergrep.Tbl_Find, Hypergrep.Label10, 2, 3, 1, 2,
     Fill, 0,
     0, 3);

   Gtk_New (Hypergrep.Frame4, "Files");
   Set_Border_Width (Hypergrep.Frame4, 5);
   Set_Shadow_Type (Hypergrep.Frame4, Shadow_Etched_In);
   Pack_Start (Hypergrep.Main_Box, Hypergrep.Frame4, False, False, 0);

   Gtk_New (Hypergrep.Table3, 4, 3, False);
   Set_Border_Width (Hypergrep.Table3, 5);
   Set_Row_Spacings (Hypergrep.Table3, 5);
   Set_Col_Spacings (Hypergrep.Table3, 5);
   Add (Hypergrep.Frame4, Hypergrep.Table3);

   Gtk_New (Hypergrep.Browse_Button, "Browse...");
   Attach (Hypergrep.Table3, Hypergrep.Browse_Button, 2, 3, 2, 3,
     0, 0,
     0, 0);
   Button_Callback.Connect
     (Hypergrep.Browse_Button, "clicked",
      Button_Callback.To_Marshaller (On_Browse_Button_Clicked'Access));

   Gtk_New (Hypergrep.Files_Label, "Files:");
   Set_Alignment (Hypergrep.Files_Label, 0.0, 0.5);
   Set_Padding (Hypergrep.Files_Label, 0, 0);
   Set_Justify (Hypergrep.Files_Label, Justify_Center);
   Set_Line_Wrap (Hypergrep.Files_Label, False);
   Attach (Hypergrep.Table3, Hypergrep.Files_Label, 0, 1, 1, 2,
     Fill, 0,
     0, 0);

   Gtk_New (Hypergrep.Directory_Label, "Directory:");
   Set_Alignment (Hypergrep.Directory_Label, 0.0, 0.5);
   Set_Padding (Hypergrep.Directory_Label, 0, 0);
   Set_Justify (Hypergrep.Directory_Label, Justify_Center);
   Set_Line_Wrap (Hypergrep.Directory_Label, False);
   Attach (Hypergrep.Table3, Hypergrep.Directory_Label, 0, 1, 2, 3,
     Fill, 0,
     0, 0);

   Gtk_New (Hypergrep.Directory_Combo);
   Set_Case_Sensitive (Hypergrep.Directory_Combo, False);
   Set_Use_Arrows (Hypergrep.Directory_Combo, True);
   Set_Use_Arrows_Always (Hypergrep.Directory_Combo, False);
   String_List.Append (Directory_Combo_Items, "");
   Combo.Set_Popdown_Strings (Hypergrep.Directory_Combo, Directory_Combo_Items);
   Free_String_List (Directory_Combo_Items);
   Attach (Hypergrep.Table3, Hypergrep.Directory_Combo, 1, 2, 2, 3,
     Expand or Fill, 0,
     0, 0);

   Hypergrep.Directory_Entry := Get_Entry (Hypergrep.Directory_Combo);
   Set_Editable (Hypergrep.Directory_Entry, True);
   Set_Max_Length (Hypergrep.Directory_Entry, 0);
   Set_Text (Hypergrep.Directory_Entry, "");
   Set_Visibility (Hypergrep.Directory_Entry, True);

   Gtk_New (Hypergrep.Only_Project_Check, "Scan only project files");
   Set_Active (Hypergrep.Only_Project_Check, True);
   Attach (Hypergrep.Table3, Hypergrep.Only_Project_Check, 0, 3, 0, 1,
     Fill, 0,
     0, 0);
   Check_Button_Callback.Connect
     (Hypergrep.Only_Project_Check, "toggled",
      Check_Button_Callback.To_Marshaller (On_Only_Project_Check_Toggled'Access));

   Gtk_New (Hypergrep.Subdirs_Check, "Include sub-directories");
   Set_Active (Hypergrep.Subdirs_Check, False);
   Attach (Hypergrep.Table3, Hypergrep.Subdirs_Check, 1, 3, 3, 4,
     Fill, 0,
     0, 0);

   Gtk_New (Hypergrep.Files_Combo);
   Set_Case_Sensitive (Hypergrep.Files_Combo, False);
   Set_Use_Arrows (Hypergrep.Files_Combo, True);
   Set_Use_Arrows_Always (Hypergrep.Files_Combo, False);
   String_List.Append (Files_Combo_Items, "");
   Combo.Set_Popdown_Strings (Hypergrep.Files_Combo, Files_Combo_Items);
   Free_String_List (Files_Combo_Items);
   Attach (Hypergrep.Table3, Hypergrep.Files_Combo, 1, 2, 1, 2,
     Expand or Fill, 0,
     0, 0);

   Hypergrep.Files_Entry := Get_Entry (Hypergrep.Files_Combo);
   Set_Editable (Hypergrep.Files_Entry, True);
   Set_Max_Length (Hypergrep.Files_Entry, 0);
   Set_Text (Hypergrep.Files_Entry, "");
   Set_Visibility (Hypergrep.Files_Entry, True);

   Gtk_New (Hypergrep.Hbuttonbox2);
   Set_Spacing (Hypergrep.Hbuttonbox2, 30);
   Set_Layout (Hypergrep.Hbuttonbox2, Buttonbox_Spread);
   Set_Child_Size (Hypergrep.Hbuttonbox2, 85, 27);
   Set_Child_Ipadding (Hypergrep.Hbuttonbox2, 0, 0);
   Add (Hypergrep.Main_Box, Hypergrep.Hbuttonbox2);

   Gtk_New (Hypergrep.Start_Button, "Start");
   Set_Flags (Hypergrep.Start_Button, Can_Default);
   Grab_Default (Hypergrep.Start_Button);
   Button_Callback.Connect
     (Hypergrep.Start_Button, "clicked",
      Button_Callback.To_Marshaller (On_Start_Button_Clicked'Access));
   Add (Hypergrep.Hbuttonbox2, Hypergrep.Start_Button);

   Gtk_New (Hypergrep.Stop_Button, "Stop");
   Set_Flags (Hypergrep.Stop_Button, Can_Default);
   Button_Callback.Connect
     (Hypergrep.Stop_Button, "clicked",
      Button_Callback.To_Marshaller (On_Stop_Button_Clicked'Access));
   Add (Hypergrep.Hbuttonbox2, Hypergrep.Stop_Button);

   Gtk_New (Hypergrep.Close_Button, "Close");
   Set_Flags (Hypergrep.Close_Button, Can_Default);
   Button_Callback.Connect
     (Hypergrep.Close_Button, "clicked",
      Button_Callback.To_Marshaller (On_Close_Button_Clicked'Access));
   Add (Hypergrep.Hbuttonbox2, Hypergrep.Close_Button);

end Initialize;

end Hypergrep_Pkg;
