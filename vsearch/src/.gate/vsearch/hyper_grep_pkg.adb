with Glib; use Glib;
with Gtk; use Gtk;
with Gdk.Types;       use Gdk.Types;
with Gtk.Widget;      use Gtk.Widget;
with Gtk.Enums;       use Gtk.Enums;
with Gtkada.Handlers; use Gtkada.Handlers;
with Callbacks_Hypergrep; use Callbacks_Hypergrep;
with Hyper_Grep_Pkg.Callbacks; use Hyper_Grep_Pkg.Callbacks;

package body Hyper_Grep_Pkg is

procedure Gtk_New (Hyper_Grep : out Hyper_Grep_Access) is
begin
   Hyper_Grep := new Hyper_Grep_Record;
   Hyper_Grep_Pkg.Initialize (Hyper_Grep);
end Gtk_New;

procedure Initialize (Hyper_Grep : access Hyper_Grep_Record'Class) is
   pragma Suppress (All_Checks);
   Pattern_Combo_Items : String_List.Glist;
   Directory_Combo_Items : String_List.Glist;
   Files_Combo_Items : String_List.Glist;

begin
   Gtk.Window.Initialize (Hyper_Grep, Window_Toplevel);
   Set_Title (Hyper_Grep, "Hyper grep");
   Set_Policy (Hyper_Grep, False, True, False);
   Set_Position (Hyper_Grep, Win_Pos_None);
   Set_Modal (Hyper_Grep, False);
   Return_Callback.Connect
     (Hyper_Grep, "delete_event", On_Hyper_Grep_Delete_Event'Access);

   Gtk_New_Vbox (Hyper_Grep.Main_Box, False, 0);
   Add (Hyper_Grep, Hyper_Grep.Main_Box);

   Gtk_New (Hyper_Grep.Search_Frame, "Search");
   Set_Border_Width (Hyper_Grep.Search_Frame, 5);
   Set_Shadow_Type (Hyper_Grep.Search_Frame, Shadow_Etched_In);
   Pack_Start (Hyper_Grep.Main_Box, Hyper_Grep.Search_Frame, False, False, 0);

   Gtk_New (Hyper_Grep.Tbl_Find, 5, 3, False);
   Set_Border_Width (Hyper_Grep.Tbl_Find, 5);
   Set_Row_Spacings (Hyper_Grep.Tbl_Find, 0);
   Set_Col_Spacings (Hyper_Grep.Tbl_Find, 0);
   Add (Hyper_Grep.Search_Frame, Hyper_Grep.Tbl_Find);

   Gtk_New (Hyper_Grep.Statements_Check, "Statements");
   Set_Active (Hyper_Grep.Statements_Check, True);
   Attach (Hyper_Grep.Tbl_Find, Hyper_Grep.Statements_Check, 2, 3, 2, 3,
     Fill, 0,
     0, 0);

   Gtk_New (Hyper_Grep.Strings_Check, "Strings");
   Set_Active (Hyper_Grep.Strings_Check, True);
   Attach (Hyper_Grep.Tbl_Find, Hyper_Grep.Strings_Check, 2, 3, 3, 4,
     Fill, 0,
     0, 0);

   Gtk_New (Hyper_Grep.Comments_Check, "Comments");
   Set_Active (Hyper_Grep.Comments_Check, True);
   Attach (Hyper_Grep.Tbl_Find, Hyper_Grep.Comments_Check, 2, 3, 4, 5,
     Fill, 0,
     0, 0);

   Gtk_New (Hyper_Grep.Case_Check, "Match case");
   Set_Active (Hyper_Grep.Case_Check, False);
   Attach (Hyper_Grep.Tbl_Find, Hyper_Grep.Case_Check, 0, 2, 2, 3,
     Fill, 0,
     0, 0);

   Gtk_New (Hyper_Grep.Whole_Word_Check, "Match whole word");
   Set_Active (Hyper_Grep.Whole_Word_Check, False);
   Attach (Hyper_Grep.Tbl_Find, Hyper_Grep.Whole_Word_Check, 0, 2, 3, 4,
     Fill, 0,
     0, 0);

   Gtk_New (Hyper_Grep.Regexp_Check, "Regular expression");
   Set_Active (Hyper_Grep.Regexp_Check, False);
   Attach (Hyper_Grep.Tbl_Find, Hyper_Grep.Regexp_Check, 0, 2, 4, 5,
     Fill, 0,
     0, 0);

   Gtk_New (Hyper_Grep.Label8, "Search for:");
   Set_Alignment (Hyper_Grep.Label8, 0.0, 0.5);
   Set_Padding (Hyper_Grep.Label8, 0, 0);
   Set_Justify (Hyper_Grep.Label8, Justify_Center);
   Set_Line_Wrap (Hyper_Grep.Label8, False);
   Attach (Hyper_Grep.Tbl_Find, Hyper_Grep.Label8, 0, 1, 0, 1,
     Fill, 0,
     0, 0);

   Gtk_New (Hyper_Grep.Pattern_Combo);
   Set_Case_Sensitive (Hyper_Grep.Pattern_Combo, False);
   Set_Use_Arrows (Hyper_Grep.Pattern_Combo, True);
   Set_Use_Arrows_Always (Hyper_Grep.Pattern_Combo, False);
   String_List.Append (Pattern_Combo_Items, "");
   Combo.Set_Popdown_Strings (Hyper_Grep.Pattern_Combo, Pattern_Combo_Items);
   Free_String_List (Pattern_Combo_Items);
   Attach (Hyper_Grep.Tbl_Find, Hyper_Grep.Pattern_Combo, 1, 3, 0, 1,
     Expand or Fill, 0,
     0, 0);

   Hyper_Grep.Pattern_Entry := Get_Entry (Hyper_Grep.Pattern_Combo);
   Set_Editable (Hyper_Grep.Pattern_Entry, True);
   Set_Max_Length (Hyper_Grep.Pattern_Entry, 0);
   Set_Text (Hyper_Grep.Pattern_Entry, "");
   Set_Visibility (Hyper_Grep.Pattern_Entry, True);

   Gtk_New (Hyper_Grep.Label9, "Options:");
   Set_Alignment (Hyper_Grep.Label9, 0.0, 1.0);
   Set_Padding (Hyper_Grep.Label9, 0, 0);
   Set_Justify (Hyper_Grep.Label9, Justify_Center);
   Set_Line_Wrap (Hyper_Grep.Label9, False);
   Attach (Hyper_Grep.Tbl_Find, Hyper_Grep.Label9, 0, 2, 1, 2,
     Fill, 0,
     0, 3);

   Gtk_New (Hyper_Grep.Label10, "Search through:");
   Set_Alignment (Hyper_Grep.Label10, 0.0, 1.0);
   Set_Padding (Hyper_Grep.Label10, 0, 0);
   Set_Justify (Hyper_Grep.Label10, Justify_Center);
   Set_Line_Wrap (Hyper_Grep.Label10, False);
   Attach (Hyper_Grep.Tbl_Find, Hyper_Grep.Label10, 2, 3, 1, 2,
     Fill, 0,
     0, 3);

   Gtk_New (Hyper_Grep.Frame4, "Files");
   Set_Border_Width (Hyper_Grep.Frame4, 5);
   Set_Shadow_Type (Hyper_Grep.Frame4, Shadow_Etched_In);
   Pack_Start (Hyper_Grep.Main_Box, Hyper_Grep.Frame4, False, False, 0);

   Gtk_New (Hyper_Grep.Table3, 4, 3, False);
   Set_Border_Width (Hyper_Grep.Table3, 5);
   Set_Row_Spacings (Hyper_Grep.Table3, 5);
   Set_Col_Spacings (Hyper_Grep.Table3, 5);
   Add (Hyper_Grep.Frame4, Hyper_Grep.Table3);

   Gtk_New (Hyper_Grep.Browse_Button, "Browse...");
   Attach (Hyper_Grep.Table3, Hyper_Grep.Browse_Button, 2, 3, 2, 3,
     0, 0,
     0, 0);
   Button_Callback.Connect
     (Hyper_Grep.Browse_Button, "clicked",
      Button_Callback.To_Marshaller (On_Browse_Button_Clicked'Access));

   Gtk_New (Hyper_Grep.Files_Label, "Files:");
   Set_Alignment (Hyper_Grep.Files_Label, 0.0, 0.5);
   Set_Padding (Hyper_Grep.Files_Label, 0, 0);
   Set_Justify (Hyper_Grep.Files_Label, Justify_Center);
   Set_Line_Wrap (Hyper_Grep.Files_Label, False);
   Attach (Hyper_Grep.Table3, Hyper_Grep.Files_Label, 0, 1, 1, 2,
     Fill, 0,
     0, 0);

   Gtk_New (Hyper_Grep.Directory_Label, "Directory:");
   Set_Alignment (Hyper_Grep.Directory_Label, 0.0, 0.5);
   Set_Padding (Hyper_Grep.Directory_Label, 0, 0);
   Set_Justify (Hyper_Grep.Directory_Label, Justify_Center);
   Set_Line_Wrap (Hyper_Grep.Directory_Label, False);
   Attach (Hyper_Grep.Table3, Hyper_Grep.Directory_Label, 0, 1, 2, 3,
     Fill, 0,
     0, 0);

   Gtk_New (Hyper_Grep.Directory_Combo);
   Set_Case_Sensitive (Hyper_Grep.Directory_Combo, False);
   Set_Use_Arrows (Hyper_Grep.Directory_Combo, True);
   Set_Use_Arrows_Always (Hyper_Grep.Directory_Combo, False);
   String_List.Append (Directory_Combo_Items, "");
   Combo.Set_Popdown_Strings (Hyper_Grep.Directory_Combo, Directory_Combo_Items);
   Free_String_List (Directory_Combo_Items);
   Attach (Hyper_Grep.Table3, Hyper_Grep.Directory_Combo, 1, 2, 2, 3,
     Expand or Fill, 0,
     0, 0);

   Hyper_Grep.Directory_Entry := Get_Entry (Hyper_Grep.Directory_Combo);
   Set_Editable (Hyper_Grep.Directory_Entry, True);
   Set_Max_Length (Hyper_Grep.Directory_Entry, 0);
   Set_Text (Hyper_Grep.Directory_Entry, "");
   Set_Visibility (Hyper_Grep.Directory_Entry, True);

   Gtk_New (Hyper_Grep.Only_Project_Check, "Scan only project files");
   Set_Active (Hyper_Grep.Only_Project_Check, True);
   Attach (Hyper_Grep.Table3, Hyper_Grep.Only_Project_Check, 0, 3, 0, 1,
     Fill, 0,
     0, 0);
   Check_Button_Callback.Connect
     (Hyper_Grep.Only_Project_Check, "toggled",
      Check_Button_Callback.To_Marshaller (On_Only_Project_Check_Toggled'Access));

   Gtk_New (Hyper_Grep.Subdirs_Check, "Include sub-directories");
   Set_Active (Hyper_Grep.Subdirs_Check, False);
   Attach (Hyper_Grep.Table3, Hyper_Grep.Subdirs_Check, 1, 3, 3, 4,
     Fill, 0,
     0, 0);

   Gtk_New (Hyper_Grep.Files_Combo);
   Set_Case_Sensitive (Hyper_Grep.Files_Combo, False);
   Set_Use_Arrows (Hyper_Grep.Files_Combo, True);
   Set_Use_Arrows_Always (Hyper_Grep.Files_Combo, False);
   String_List.Append (Files_Combo_Items, "");
   Combo.Set_Popdown_Strings (Hyper_Grep.Files_Combo, Files_Combo_Items);
   Free_String_List (Files_Combo_Items);
   Attach (Hyper_Grep.Table3, Hyper_Grep.Files_Combo, 1, 2, 1, 2,
     Expand or Fill, 0,
     0, 0);

   Hyper_Grep.Files_Entry := Get_Entry (Hyper_Grep.Files_Combo);
   Set_Editable (Hyper_Grep.Files_Entry, True);
   Set_Max_Length (Hyper_Grep.Files_Entry, 0);
   Set_Text (Hyper_Grep.Files_Entry, "");
   Set_Visibility (Hyper_Grep.Files_Entry, True);

   Gtk_New (Hyper_Grep.Hbuttonbox2);
   Set_Spacing (Hyper_Grep.Hbuttonbox2, 30);
   Set_Layout (Hyper_Grep.Hbuttonbox2, Buttonbox_Spread);
   Set_Child_Size (Hyper_Grep.Hbuttonbox2, 85, 27);
   Set_Child_Ipadding (Hyper_Grep.Hbuttonbox2, 0, 0);
   Add (Hyper_Grep.Main_Box, Hyper_Grep.Hbuttonbox2);

   Gtk_New (Hyper_Grep.Start_Button, "Start");
   Set_Flags (Hyper_Grep.Start_Button, Can_Default);
   Grab_Default (Hyper_Grep.Start_Button);
   Button_Callback.Connect
     (Hyper_Grep.Start_Button, "clicked",
      Button_Callback.To_Marshaller (On_Start_Button_Clicked'Access));
   Add (Hyper_Grep.Hbuttonbox2, Hyper_Grep.Start_Button);

   Gtk_New (Hyper_Grep.Stop_Button, "Stop");
   Set_Flags (Hyper_Grep.Stop_Button, Can_Default);
   Button_Callback.Connect
     (Hyper_Grep.Stop_Button, "clicked",
      Button_Callback.To_Marshaller (On_Stop_Button_Clicked'Access));
   Add (Hyper_Grep.Hbuttonbox2, Hyper_Grep.Stop_Button);

   Gtk_New (Hyper_Grep.Close_Button, "Close");
   Set_Flags (Hyper_Grep.Close_Button, Can_Default);
   Button_Callback.Connect
     (Hyper_Grep.Close_Button, "clicked",
      Button_Callback.To_Marshaller (On_Close_Button_Clicked'Access));
   Add (Hyper_Grep.Hbuttonbox2, Hyper_Grep.Close_Button);

end Initialize;

end Hyper_Grep_Pkg;
