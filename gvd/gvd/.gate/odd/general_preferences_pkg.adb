with Glib; use Glib;
with Gtk; use Gtk;
with Gdk.Types;       use Gdk.Types;
with Gtk.Widget;      use Gtk.Widget;
with Gtk.Enums;       use Gtk.Enums;
with Gtkada.Handlers; use Gtkada.Handlers;
with Callbacks_Odd; use Callbacks_Odd;
with Odd_Intl; use Odd_Intl;
with Odd_Preferences_Pkg.Callbacks; use Odd_Preferences_Pkg.Callbacks;

package body Odd_Preferences_Pkg is

procedure Gtk_New (Odd_Preferences : out Odd_Preferences_Access) is
begin
   Odd_Preferences := new Odd_Preferences_Record;
   Odd_Preferences_Pkg.Initialize (Odd_Preferences);
end Gtk_New;

procedure Initialize (Odd_Preferences : access Odd_Preferences_Record'Class) is
   File_Name_Bg_Combo_Items : String_List.Glist;
   Comment_Color_Combo_Items : String_List.Glist;
   String_Color_Combo_Items : String_List.Glist;
   Keyword_Color_Combo_Items : String_List.Glist;
   Asm_Highlight_Combo_Items : String_List.Glist;
   Big_Item_Spin_Adj : Gtk_Adjustment;
   Xref_Color_Combo_Items : String_List.Glist;
   Change_Color_Combo_Items : String_List.Glist;
   Thaw_Bg_Color_Combo_Items : String_List.Glist;
   Title_Color_Combo_Items : String_List.Glist;
   Freeze_Bg_Color_Combo_Items : String_List.Glist;
   Debug_Higlight_Combo_Items : String_List.Glist;

begin
   Gtk.Window.Initialize (Odd_Preferences, Window_Dialog);
   Return_Callback.Connect
     (Odd_Preferences, "delete_event", On_Odd_Preferences_Delete_Event'Access);
   Set_Title (Odd_Preferences, -"Preferences");
   Set_Policy (Odd_Preferences, False, True, False);
   Set_Position (Odd_Preferences, Win_Pos_Center);
   Set_Modal (Odd_Preferences, True);

   Gtk_New_Vbox (Odd_Preferences.Vbox2, False, 0);
   Add (Odd_Preferences, Odd_Preferences.Vbox2);

   Gtk_New (Odd_Preferences.Notebook1);
   Pack_Start (Odd_Preferences.Vbox2, Odd_Preferences.Notebook1, True, True, 0);
   Set_Scrollable (Odd_Preferences.Notebook1, False);
   Set_Show_Border (Odd_Preferences.Notebook1, True);
   Set_Show_Tabs (Odd_Preferences.Notebook1, True);
   Set_Tab_Hborder (Odd_Preferences.Notebook1, 2);
   Set_Tab_Vborder (Odd_Preferences.Notebook1, 2);
   Set_Tab_Pos (Odd_Preferences.Notebook1, Pos_Top);

   Gtk_New (Odd_Preferences.Frame1);
   Add (Odd_Preferences.Notebook1, Odd_Preferences.Frame1);
   Set_Shadow_Type (Odd_Preferences.Frame1, Shadow_Etched_In);

   Gtk_New (Odd_Preferences.Table1, 5, 4, False);
   Add (Odd_Preferences.Frame1, Odd_Preferences.Table1);
   Set_Row_Spacings (Odd_Preferences.Table1, 2);
   Set_Col_Spacings (Odd_Preferences.Table1, 0);

   Gtk_New (Odd_Preferences.Label13, -("Automatic Display of Button Hints"));
   Attach (Odd_Preferences.Table1, Odd_Preferences.Label13, 0, 1, 0, 1,
     Expand or Fill, 0,
     0, 0);
   Set_Alignment (Odd_Preferences.Label13, 7.45058e-09, 0.5);
   Set_Padding (Odd_Preferences.Label13, 2, 0);
   Set_Justify (Odd_Preferences.Label13, Justify_Left);
   Set_Line_Wrap (Odd_Preferences.Label13, False);

   Gtk_New (Odd_Preferences.Label14, -("Automatic Display of Variable Values"));
   Attach (Odd_Preferences.Table1, Odd_Preferences.Label14, 0, 1, 1, 2,
     Expand or Fill, 0,
     0, 0);
   Set_Alignment (Odd_Preferences.Label14, 7.45058e-09, 0.5);
   Set_Padding (Odd_Preferences.Label14, 2, 0);
   Set_Justify (Odd_Preferences.Label14, Justify_Left);
   Set_Line_Wrap (Odd_Preferences.Label14, False);

   Gtk_New (Odd_Preferences.Button_Hint_Popup_Check, -"as Popup Tips");
   Attach (Odd_Preferences.Table1, Odd_Preferences.Button_Hint_Popup_Check, 1, 3, 0, 1,
     Expand, 0,
     0, 0);
   Set_Active (Odd_Preferences.Button_Hint_Popup_Check, False);

   Gtk_New (Odd_Preferences.Button_Hint_Status_Check, -"in the Status Line");
   Attach (Odd_Preferences.Table1, Odd_Preferences.Button_Hint_Status_Check, 3, 4, 0, 1,
     Expand, 0,
     0, 0);
   Set_Active (Odd_Preferences.Button_Hint_Status_Check, False);

   Gtk_New (Odd_Preferences.Variable_Popup_Check, -"as Popup Tips");
   Attach (Odd_Preferences.Table1, Odd_Preferences.Variable_Popup_Check, 1, 3, 1, 2,
     Expand, 0,
     0, 0);
   Set_Active (Odd_Preferences.Variable_Popup_Check, False);

   Gtk_New (Odd_Preferences.Variable_Status_Check, -"in the Status Line");
   Attach (Odd_Preferences.Table1, Odd_Preferences.Variable_Status_Check, 3, 4, 1, 2,
     Expand, 0,
     0, 0);
   Set_Active (Odd_Preferences.Variable_Status_Check, False);

   Gtk_New (Odd_Preferences.Warn_Multiple_Check, -"Warn if Multiple GVD Instances are Running");
   Attach (Odd_Preferences.Table1, Odd_Preferences.Warn_Multiple_Check, 0, 4, 2, 3,
     Expand or Fill, 0,
     0, 0);
   Set_Active (Odd_Preferences.Warn_Multiple_Check, False);

   Gtk_New (Odd_Preferences.Label16, -("Status Bar Time Out"));
   Attach (Odd_Preferences.Table1, Odd_Preferences.Label16, 0, 1, 4, 5,
     Expand or Fill, 0,
     0, 0);
   Set_Alignment (Odd_Preferences.Label16, 7.45058e-09, 0.5);
   Set_Padding (Odd_Preferences.Label16, 2, 0);
   Set_Justify (Odd_Preferences.Label16, Justify_Left);
   Set_Line_Wrap (Odd_Preferences.Label16, False);

   Gtk_New (Odd_Preferences.Statusbar_Timeout_Entry);
   Attach (Odd_Preferences.Table1, Odd_Preferences.Statusbar_Timeout_Entry, 1, 2, 4, 5,
     Expand or Fill, 0,
     0, 0);
   Set_Editable (Odd_Preferences.Statusbar_Timeout_Entry, True);
   Set_Max_Length (Odd_Preferences.Statusbar_Timeout_Entry, 0);
   Set_Text (Odd_Preferences.Statusbar_Timeout_Entry, -"");
   Set_Visibility (Odd_Preferences.Statusbar_Timeout_Entry, True);

   Gtk_New (Odd_Preferences.Label17, -("ms"));
   Attach (Odd_Preferences.Table1, Odd_Preferences.Label17, 2, 4, 4, 5,
     Expand or Fill, 0,
     0, 0);
   Set_Alignment (Odd_Preferences.Label17, 7.45058e-09, 0.5);
   Set_Padding (Odd_Preferences.Label17, 2, 0);
   Set_Justify (Odd_Preferences.Label17, Justify_Center);
   Set_Line_Wrap (Odd_Preferences.Label17, False);

   Gtk_New (Odd_Preferences.Break_Exception_Check, -"Break On Exceptions");
   Attach (Odd_Preferences.Table1, Odd_Preferences.Break_Exception_Check, 0, 4, 3, 4,
     Expand or Fill, 0,
     0, 0);
   Set_Active (Odd_Preferences.Break_Exception_Check, False);

   Gtk_New (Odd_Preferences.Label7, -("General"));
   Set_Alignment (Odd_Preferences.Label7, 0.5, 0.5);
   Set_Padding (Odd_Preferences.Label7, 0, 0);
   Set_Justify (Odd_Preferences.Label7, Justify_Center);
   Set_Line_Wrap (Odd_Preferences.Label7, False);
   Set_Tab (Odd_Preferences.Notebook1, 0, Odd_Preferences.Label7);

   Gtk_New_Vbox (Odd_Preferences.Vbox18, False, 2);
   Add (Odd_Preferences.Notebook1, Odd_Preferences.Vbox18);
   Set_Border_Width (Odd_Preferences.Vbox18, 2);

   Gtk_New (Odd_Preferences.Frame16, -"Explorer");
   Pack_Start (Odd_Preferences.Vbox18, Odd_Preferences.Frame16, False, False, 0);
   Set_Shadow_Type (Odd_Preferences.Frame16, Shadow_Etched_In);

   Gtk_New (Odd_Preferences.Table9, 2, 2, False);
   Add (Odd_Preferences.Frame16, Odd_Preferences.Table9);
   Set_Row_Spacings (Odd_Preferences.Table9, 2);
   Set_Col_Spacings (Odd_Preferences.Table9, 5);

   Gtk_New (Odd_Preferences.Display_Explorer_Check, -"Display Explorer Tree");
   Attach (Odd_Preferences.Table9, Odd_Preferences.Display_Explorer_Check, 0, 2, 0, 1,
     Expand or Fill, 0,
     0, 0);
   Set_Active (Odd_Preferences.Display_Explorer_Check, True);

   Gtk_New (Odd_Preferences.Label75, -("File Name Background"));
   Attach (Odd_Preferences.Table9, Odd_Preferences.Label75, 0, 1, 1, 2,
     Fill, 0,
     0, 0);
   Set_Alignment (Odd_Preferences.Label75, 7.45058e-09, 0.5);
   Set_Padding (Odd_Preferences.Label75, 0, 0);
   Set_Justify (Odd_Preferences.Label75, Justify_Center);
   Set_Line_Wrap (Odd_Preferences.Label75, False);

   Gtk_New (Odd_Preferences.File_Name_Bg_Combo);
   Attach (Odd_Preferences.Table9, Odd_Preferences.File_Name_Bg_Combo, 1, 2, 1, 2,
     Fill, 0,
     0, 0);
   Set_Case_Sensitive (Odd_Preferences.File_Name_Bg_Combo, False);
   Set_Use_Arrows (Odd_Preferences.File_Name_Bg_Combo, True);
   Set_Use_Arrows_Always (Odd_Preferences.File_Name_Bg_Combo, False);
   String_List.Append (File_Name_Bg_Combo_Items, -"");
   Combo.Set_Popdown_Strings (Odd_Preferences.File_Name_Bg_Combo, File_Name_Bg_Combo_Items);
   Free_String_List (File_Name_Bg_Combo_Items);

   Odd_Preferences.Combo_Entry6 := Get_Entry (Odd_Preferences.File_Name_Bg_Combo);
   Set_Editable (Odd_Preferences.Combo_Entry6, True);
   Set_Max_Length (Odd_Preferences.Combo_Entry6, 0);
   Set_Text (Odd_Preferences.Combo_Entry6, -"");
   Set_Visibility (Odd_Preferences.Combo_Entry6, True);

   Gtk_New (Odd_Preferences.Frame17, -"Source");
   Pack_Start (Odd_Preferences.Vbox18, Odd_Preferences.Frame17, False, False, 0);
   Set_Shadow_Type (Odd_Preferences.Frame17, Shadow_Etched_In);

   Gtk_New (Odd_Preferences.Table10, 6, 2, False);
   Add (Odd_Preferences.Frame17, Odd_Preferences.Table10);
   Set_Row_Spacings (Odd_Preferences.Table10, 2);
   Set_Col_Spacings (Odd_Preferences.Table10, 5);

   Gtk_New (Odd_Preferences.Label76, -("Font"));
   Attach (Odd_Preferences.Table10, Odd_Preferences.Label76, 0, 1, 0, 1,
     Fill, 0,
     0, 0);
   Set_Alignment (Odd_Preferences.Label76, 7.45058e-09, 0.5);
   Set_Padding (Odd_Preferences.Label76, 0, 0);
   Set_Justify (Odd_Preferences.Label76, Justify_Center);
   Set_Line_Wrap (Odd_Preferences.Label76, False);

   Gtk_New (Odd_Preferences.Show_Lines_Code_Check, -"Show Lines with Code");
   Attach (Odd_Preferences.Table10, Odd_Preferences.Show_Lines_Code_Check, 1, 2, 1, 2,
     Fill, 0,
     0, 0);
   Set_Active (Odd_Preferences.Show_Lines_Code_Check, True);

   Gtk_New (Odd_Preferences.Show_Line_Numbers_Check, -"Show Line Numbers");
   Attach (Odd_Preferences.Table10, Odd_Preferences.Show_Line_Numbers_Check, 0, 1, 1, 2,
     Fill, 0,
     0, 0);
   Set_Active (Odd_Preferences.Show_Line_Numbers_Check, True);

   Gtk_New (Odd_Preferences.Syntax_Hilight_Check, -"Syntax Highlighting");
   Attach (Odd_Preferences.Table10, Odd_Preferences.Syntax_Hilight_Check, 0, 2, 2, 3,
     Fill, 0,
     0, 0);
   Set_Active (Odd_Preferences.Syntax_Hilight_Check, True);

   Gtk_New (Odd_Preferences.Label79, -("Comments"));
   Attach (Odd_Preferences.Table10, Odd_Preferences.Label79, 0, 1, 3, 4,
     Fill, 0,
     0, 0);
   Set_Alignment (Odd_Preferences.Label79, 7.45058e-09, 0.5);
   Set_Padding (Odd_Preferences.Label79, 0, 0);
   Set_Justify (Odd_Preferences.Label79, Justify_Center);
   Set_Line_Wrap (Odd_Preferences.Label79, False);

   Gtk_New (Odd_Preferences.Label80, -("Strings"));
   Attach (Odd_Preferences.Table10, Odd_Preferences.Label80, 0, 1, 4, 5,
     Fill, 0,
     0, 0);
   Set_Alignment (Odd_Preferences.Label80, 7.45058e-09, 0.5);
   Set_Padding (Odd_Preferences.Label80, 0, 0);
   Set_Justify (Odd_Preferences.Label80, Justify_Center);
   Set_Line_Wrap (Odd_Preferences.Label80, False);

   Gtk_New (Odd_Preferences.Label81, -("Keywords"));
   Attach (Odd_Preferences.Table10, Odd_Preferences.Label81, 0, 1, 5, 6,
     Fill, 0,
     0, 0);
   Set_Alignment (Odd_Preferences.Label81, 7.45058e-09, 0.5);
   Set_Padding (Odd_Preferences.Label81, 0, 0);
   Set_Justify (Odd_Preferences.Label81, Justify_Center);
   Set_Line_Wrap (Odd_Preferences.Label81, False);

   Gtk_New (Odd_Preferences.Editor_Font_Combo, Orientation_Horizontal, Toolbar_Both);
   Attach (Odd_Preferences.Table10, Odd_Preferences.Editor_Font_Combo, 1, 2, 0, 1,
     Fill, 0,
     0, 0);
   Set_Space_Size (Odd_Preferences.Editor_Font_Combo, 5);
   Set_Space_Style (Odd_Preferences.Editor_Font_Combo, Toolbar_Space_Empty);
   Set_Tooltips (Odd_Preferences.Editor_Font_Combo, True);
   Set_Button_Relief (Odd_Preferences.Editor_Font_Combo, Relief_Normal);

   Gtk_New (Odd_Preferences.Comment_Color_Combo);
   Attach (Odd_Preferences.Table10, Odd_Preferences.Comment_Color_Combo, 1, 2, 3, 4,
     Fill, 0,
     0, 0);
   Set_Case_Sensitive (Odd_Preferences.Comment_Color_Combo, False);
   Set_Use_Arrows (Odd_Preferences.Comment_Color_Combo, True);
   Set_Use_Arrows_Always (Odd_Preferences.Comment_Color_Combo, False);
   String_List.Append (Comment_Color_Combo_Items, -"");
   Combo.Set_Popdown_Strings (Odd_Preferences.Comment_Color_Combo, Comment_Color_Combo_Items);
   Free_String_List (Comment_Color_Combo_Items);

   Odd_Preferences.Combo_Entry10 := Get_Entry (Odd_Preferences.Comment_Color_Combo);
   Set_Editable (Odd_Preferences.Combo_Entry10, True);
   Set_Max_Length (Odd_Preferences.Combo_Entry10, 0);
   Set_Text (Odd_Preferences.Combo_Entry10, -"");
   Set_Visibility (Odd_Preferences.Combo_Entry10, True);

   Gtk_New (Odd_Preferences.String_Color_Combo);
   Attach (Odd_Preferences.Table10, Odd_Preferences.String_Color_Combo, 1, 2, 4, 5,
     Fill, 0,
     0, 0);
   Set_Case_Sensitive (Odd_Preferences.String_Color_Combo, False);
   Set_Use_Arrows (Odd_Preferences.String_Color_Combo, True);
   Set_Use_Arrows_Always (Odd_Preferences.String_Color_Combo, False);
   String_List.Append (String_Color_Combo_Items, -"");
   Combo.Set_Popdown_Strings (Odd_Preferences.String_Color_Combo, String_Color_Combo_Items);
   Free_String_List (String_Color_Combo_Items);

   Odd_Preferences.Combo_Entry9 := Get_Entry (Odd_Preferences.String_Color_Combo);
   Set_Editable (Odd_Preferences.Combo_Entry9, True);
   Set_Max_Length (Odd_Preferences.Combo_Entry9, 0);
   Set_Text (Odd_Preferences.Combo_Entry9, -"");
   Set_Visibility (Odd_Preferences.Combo_Entry9, True);

   Gtk_New (Odd_Preferences.Keyword_Color_Combo);
   Attach (Odd_Preferences.Table10, Odd_Preferences.Keyword_Color_Combo, 1, 2, 5, 6,
     Fill, 0,
     0, 0);
   Set_Case_Sensitive (Odd_Preferences.Keyword_Color_Combo, False);
   Set_Use_Arrows (Odd_Preferences.Keyword_Color_Combo, True);
   Set_Use_Arrows_Always (Odd_Preferences.Keyword_Color_Combo, False);
   String_List.Append (Keyword_Color_Combo_Items, -"");
   Combo.Set_Popdown_Strings (Odd_Preferences.Keyword_Color_Combo, Keyword_Color_Combo_Items);
   Free_String_List (Keyword_Color_Combo_Items);

   Odd_Preferences.Combo_Entry11 := Get_Entry (Odd_Preferences.Keyword_Color_Combo);
   Set_Editable (Odd_Preferences.Combo_Entry11, True);
   Set_Max_Length (Odd_Preferences.Combo_Entry11, 0);
   Set_Text (Odd_Preferences.Combo_Entry11, -"");
   Set_Visibility (Odd_Preferences.Combo_Entry11, True);

   Gtk_New (Odd_Preferences.Frame18, -"Assembly");
   Pack_Start (Odd_Preferences.Vbox18, Odd_Preferences.Frame18, False, False, 0);
   Set_Shadow_Type (Odd_Preferences.Frame18, Shadow_Etched_In);

   Gtk_New_Hbox (Odd_Preferences.Hbox7, False, 5);
   Add (Odd_Preferences.Frame18, Odd_Preferences.Hbox7);

   Gtk_New (Odd_Preferences.Label82, -("Current Line"));
   Pack_Start (Odd_Preferences.Hbox7, Odd_Preferences.Label82, False, True, 0);
   Set_Alignment (Odd_Preferences.Label82, 7.45058e-09, 0.5);
   Set_Padding (Odd_Preferences.Label82, 0, 0);
   Set_Justify (Odd_Preferences.Label82, Justify_Center);
   Set_Line_Wrap (Odd_Preferences.Label82, False);

   Gtk_New (Odd_Preferences.Asm_Highlight_Combo);
   Pack_Start (Odd_Preferences.Hbox7, Odd_Preferences.Asm_Highlight_Combo, False, True, 0);
   Set_Case_Sensitive (Odd_Preferences.Asm_Highlight_Combo, False);
   Set_Use_Arrows (Odd_Preferences.Asm_Highlight_Combo, True);
   Set_Use_Arrows_Always (Odd_Preferences.Asm_Highlight_Combo, False);
   String_List.Append (Asm_Highlight_Combo_Items, -"");
   Combo.Set_Popdown_Strings (Odd_Preferences.Asm_Highlight_Combo, Asm_Highlight_Combo_Items);
   Free_String_List (Asm_Highlight_Combo_Items);

   Odd_Preferences.Combo_Entry12 := Get_Entry (Odd_Preferences.Asm_Highlight_Combo);
   Set_Editable (Odd_Preferences.Combo_Entry12, True);
   Set_Max_Length (Odd_Preferences.Combo_Entry12, 0);
   Set_Text (Odd_Preferences.Combo_Entry12, -"");
   Set_Visibility (Odd_Preferences.Combo_Entry12, True);

   Gtk_New (Odd_Preferences.Label8, -("Source"));
   Set_Alignment (Odd_Preferences.Label8, 0.5, 0.5);
   Set_Padding (Odd_Preferences.Label8, 0, 0);
   Set_Justify (Odd_Preferences.Label8, Justify_Center);
   Set_Line_Wrap (Odd_Preferences.Label8, False);
   Set_Tab (Odd_Preferences.Notebook1, 1, Odd_Preferences.Label8);

   Gtk_New (Odd_Preferences.Frame3);
   Add (Odd_Preferences.Notebook1, Odd_Preferences.Frame3);
   Set_Border_Width (Odd_Preferences.Frame3, 2);
   Set_Shadow_Type (Odd_Preferences.Frame3, Shadow_Etched_In);

   Gtk_New (Odd_Preferences.Table3, 11, 4, False);
   Add (Odd_Preferences.Frame3, Odd_Preferences.Table3);
   Set_Row_Spacings (Odd_Preferences.Table3, 2);
   Set_Col_Spacings (Odd_Preferences.Table3, 2);

   Gtk_New (Odd_Preferences.Label83, -("Clickable Item"));
   Attach (Odd_Preferences.Table3, Odd_Preferences.Label83, 0, 1, 0, 1,
     Fill, 0,
     0, 0);
   Set_Alignment (Odd_Preferences.Label83, 7.45058e-09, 0.5);
   Set_Padding (Odd_Preferences.Label83, 0, 0);
   Set_Justify (Odd_Preferences.Label83, Justify_Center);
   Set_Line_Wrap (Odd_Preferences.Label83, False);

   Gtk_New (Odd_Preferences.Detect_Aliases_Check, -"Detect Aliases (shared data structures)");
   Attach (Odd_Preferences.Table3, Odd_Preferences.Detect_Aliases_Check, 0, 4, 8, 9,
     Expand or Fill, 0,
     0, 0);
   Set_Active (Odd_Preferences.Detect_Aliases_Check, True);

   Gtk_New (Odd_Preferences.Align_Grid_Check, -"Auto-Align Displays on Nearest Grid Point");
   Attach (Odd_Preferences.Table3, Odd_Preferences.Align_Grid_Check, 0, 4, 10, 11,
     Expand or Fill, 0,
     0, 0);
   Set_Active (Odd_Preferences.Align_Grid_Check, True);

   Gtk_New (Odd_Preferences.Label84, -("Title Background"));
   Attach (Odd_Preferences.Table3, Odd_Preferences.Label84, 2, 3, 0, 1,
     Fill, 0,
     0, 0);
   Set_Alignment (Odd_Preferences.Label84, 7.45058e-09, 0.5);
   Set_Padding (Odd_Preferences.Label84, 0, 0);
   Set_Justify (Odd_Preferences.Label84, Justify_Center);
   Set_Line_Wrap (Odd_Preferences.Label84, False);

   Gtk_New (Odd_Preferences.Label85, -("Changed Data"));
   Attach (Odd_Preferences.Table3, Odd_Preferences.Label85, 0, 1, 1, 2,
     Fill, 0,
     0, 0);
   Set_Alignment (Odd_Preferences.Label85, 7.45058e-09, 0.5);
   Set_Padding (Odd_Preferences.Label85, 0, 0);
   Set_Justify (Odd_Preferences.Label85, Justify_Center);
   Set_Line_Wrap (Odd_Preferences.Label85, False);

   Gtk_New (Odd_Preferences.Label86, -("Auto-Refreshed"));
   Attach (Odd_Preferences.Table3, Odd_Preferences.Label86, 0, 1, 2, 3,
     Fill, 0,
     0, 0);
   Set_Alignment (Odd_Preferences.Label86, 7.45058e-09, 0.5);
   Set_Padding (Odd_Preferences.Label86, 0, 0);
   Set_Justify (Odd_Preferences.Label86, Justify_Center);
   Set_Line_Wrap (Odd_Preferences.Label86, False);

   Gtk_New (Odd_Preferences.Label87, -("Frozen"));
   Attach (Odd_Preferences.Table3, Odd_Preferences.Label87, 2, 3, 2, 3,
     Fill, 0,
     0, 0);
   Set_Alignment (Odd_Preferences.Label87, 7.45058e-09, 0.5);
   Set_Padding (Odd_Preferences.Label87, 0, 0);
   Set_Justify (Odd_Preferences.Label87, Justify_Center);
   Set_Line_Wrap (Odd_Preferences.Label87, False);

   Gtk_New (Odd_Preferences.Look_3d_Check, -"3D Look");
   Attach (Odd_Preferences.Table3, Odd_Preferences.Look_3d_Check, 0, 4, 3, 4,
     Expand or Fill, 0,
     0, 0);
   Set_Active (Odd_Preferences.Look_3d_Check, True);

   Gtk_New (Odd_Preferences.Label88, -("Item Name"));
   Attach (Odd_Preferences.Table3, Odd_Preferences.Label88, 0, 1, 4, 5,
     Fill, 0,
     0, 0);
   Set_Alignment (Odd_Preferences.Label88, 7.45058e-09, 0.5);
   Set_Padding (Odd_Preferences.Label88, 0, 0);
   Set_Justify (Odd_Preferences.Label88, Justify_Center);
   Set_Line_Wrap (Odd_Preferences.Label88, False);

   Gtk_New (Odd_Preferences.Label89, -("Item Value"));
   Attach (Odd_Preferences.Table3, Odd_Preferences.Label89, 0, 1, 5, 6,
     Fill, 0,
     0, 0);
   Set_Alignment (Odd_Preferences.Label89, 7.45058e-09, 0.5);
   Set_Padding (Odd_Preferences.Label89, 0, 0);
   Set_Justify (Odd_Preferences.Label89, Justify_Center);
   Set_Line_Wrap (Odd_Preferences.Label89, False);

   Gtk_New (Odd_Preferences.Label90, -("Item Type"));
   Attach (Odd_Preferences.Table3, Odd_Preferences.Label90, 0, 1, 6, 7,
     Fill, 0,
     0, 0);
   Set_Alignment (Odd_Preferences.Label90, 0.0, 0.5);
   Set_Padding (Odd_Preferences.Label90, 0, 0);
   Set_Justify (Odd_Preferences.Label90, Justify_Center);
   Set_Line_Wrap (Odd_Preferences.Label90, False);

   Gtk_New (Odd_Preferences.Hide_Big_Items_Check, -"Hide Big Items");
   Attach (Odd_Preferences.Table3, Odd_Preferences.Hide_Big_Items_Check, 0, 2, 7, 8,
     Expand or Fill, 0,
     0, 0);
   Set_Active (Odd_Preferences.Hide_Big_Items_Check, True);

   Gtk_New (Odd_Preferences.Label91, -("Big Item Height"));
   Attach (Odd_Preferences.Table3, Odd_Preferences.Label91, 2, 3, 7, 8,
     Fill, 0,
     0, 0);
   Set_Alignment (Odd_Preferences.Label91, 7.45058e-09, 0.5);
   Set_Padding (Odd_Preferences.Label91, 0, 0);
   Set_Justify (Odd_Preferences.Label91, Justify_Center);
   Set_Line_Wrap (Odd_Preferences.Label91, False);

   Gtk_New (Big_Item_Spin_Adj, 150.0, 0.0, 1000.0, 1.0, 10.0, 10.0);
   Gtk_New (Odd_Preferences.Big_Item_Spin, Big_Item_Spin_Adj, 1.0, 0);
   Attach (Odd_Preferences.Table3, Odd_Preferences.Big_Item_Spin, 3, 4, 7, 8,
     Expand or Fill, 0,
     0, 0);
   Set_Numeric (Odd_Preferences.Big_Item_Spin, False);
   Set_Snap_To_Ticks (Odd_Preferences.Big_Item_Spin, False);
   Set_Update_Policy (Odd_Preferences.Big_Item_Spin, Update_Always);
   Set_Value (Odd_Preferences.Big_Item_Spin, 150.0);
   Set_Wrap (Odd_Preferences.Big_Item_Spin, False);

   Gtk_New (Odd_Preferences.Display_Grid_Check, -"Display Grid Points");
   Attach (Odd_Preferences.Table3, Odd_Preferences.Display_Grid_Check, 0, 4, 9, 10,
     Expand or Fill, 0,
     0, 0);
   Set_Active (Odd_Preferences.Display_Grid_Check, True);

   Gtk_New (Odd_Preferences.Title_Font_Combo, Orientation_Horizontal, Toolbar_Both);
   Attach (Odd_Preferences.Table3, Odd_Preferences.Title_Font_Combo, 1, 4, 4, 5,
     Fill, 0,
     0, 0);
   Set_Space_Size (Odd_Preferences.Title_Font_Combo, 5);
   Set_Space_Style (Odd_Preferences.Title_Font_Combo, Toolbar_Space_Empty);
   Set_Tooltips (Odd_Preferences.Title_Font_Combo, True);
   Set_Button_Relief (Odd_Preferences.Title_Font_Combo, Relief_Normal);

   Gtk_New (Odd_Preferences.Value_Font_Combo, Orientation_Horizontal, Toolbar_Both);
   Attach (Odd_Preferences.Table3, Odd_Preferences.Value_Font_Combo, 1, 4, 5, 6,
     Fill, 0,
     0, 0);
   Set_Space_Size (Odd_Preferences.Value_Font_Combo, 5);
   Set_Space_Style (Odd_Preferences.Value_Font_Combo, Toolbar_Space_Empty);
   Set_Tooltips (Odd_Preferences.Value_Font_Combo, True);
   Set_Button_Relief (Odd_Preferences.Value_Font_Combo, Relief_Normal);

   Gtk_New (Odd_Preferences.Type_Font_Combo, Orientation_Horizontal, Toolbar_Both);
   Attach (Odd_Preferences.Table3, Odd_Preferences.Type_Font_Combo, 1, 4, 6, 7,
     Fill, 0,
     0, 0);
   Set_Space_Size (Odd_Preferences.Type_Font_Combo, 5);
   Set_Space_Style (Odd_Preferences.Type_Font_Combo, Toolbar_Space_Empty);
   Set_Tooltips (Odd_Preferences.Type_Font_Combo, True);
   Set_Button_Relief (Odd_Preferences.Type_Font_Combo, Relief_Normal);

   Gtk_New (Odd_Preferences.Xref_Color_Combo);
   Attach (Odd_Preferences.Table3, Odd_Preferences.Xref_Color_Combo, 1, 2, 0, 1,
     Fill, 0,
     0, 0);
   Set_Case_Sensitive (Odd_Preferences.Xref_Color_Combo, False);
   Set_Use_Arrows (Odd_Preferences.Xref_Color_Combo, True);
   Set_Use_Arrows_Always (Odd_Preferences.Xref_Color_Combo, False);
   String_List.Append (Xref_Color_Combo_Items, -"");
   Combo.Set_Popdown_Strings (Odd_Preferences.Xref_Color_Combo, Xref_Color_Combo_Items);
   Free_String_List (Xref_Color_Combo_Items);

   Odd_Preferences.Combo_Entry13 := Get_Entry (Odd_Preferences.Xref_Color_Combo);
   Set_Editable (Odd_Preferences.Combo_Entry13, True);
   Set_Max_Length (Odd_Preferences.Combo_Entry13, 0);
   Set_Text (Odd_Preferences.Combo_Entry13, -"");
   Set_Visibility (Odd_Preferences.Combo_Entry13, True);

   Gtk_New (Odd_Preferences.Change_Color_Combo);
   Attach (Odd_Preferences.Table3, Odd_Preferences.Change_Color_Combo, 1, 2, 1, 2,
     Fill, 0,
     0, 0);
   Set_Case_Sensitive (Odd_Preferences.Change_Color_Combo, False);
   Set_Use_Arrows (Odd_Preferences.Change_Color_Combo, True);
   Set_Use_Arrows_Always (Odd_Preferences.Change_Color_Combo, False);
   String_List.Append (Change_Color_Combo_Items, -"");
   Combo.Set_Popdown_Strings (Odd_Preferences.Change_Color_Combo, Change_Color_Combo_Items);
   Free_String_List (Change_Color_Combo_Items);

   Odd_Preferences.Combo_Entry15 := Get_Entry (Odd_Preferences.Change_Color_Combo);
   Set_Editable (Odd_Preferences.Combo_Entry15, True);
   Set_Max_Length (Odd_Preferences.Combo_Entry15, 0);
   Set_Text (Odd_Preferences.Combo_Entry15, -"");
   Set_Visibility (Odd_Preferences.Combo_Entry15, True);

   Gtk_New (Odd_Preferences.Thaw_Bg_Color_Combo);
   Attach (Odd_Preferences.Table3, Odd_Preferences.Thaw_Bg_Color_Combo, 1, 2, 2, 3,
     Fill, 0,
     0, 0);
   Set_Case_Sensitive (Odd_Preferences.Thaw_Bg_Color_Combo, False);
   Set_Use_Arrows (Odd_Preferences.Thaw_Bg_Color_Combo, True);
   Set_Use_Arrows_Always (Odd_Preferences.Thaw_Bg_Color_Combo, False);
   String_List.Append (Thaw_Bg_Color_Combo_Items, -"");
   Combo.Set_Popdown_Strings (Odd_Preferences.Thaw_Bg_Color_Combo, Thaw_Bg_Color_Combo_Items);
   Free_String_List (Thaw_Bg_Color_Combo_Items);

   Odd_Preferences.Combo_Entry16 := Get_Entry (Odd_Preferences.Thaw_Bg_Color_Combo);
   Set_Editable (Odd_Preferences.Combo_Entry16, True);
   Set_Max_Length (Odd_Preferences.Combo_Entry16, 0);
   Set_Text (Odd_Preferences.Combo_Entry16, -"");
   Set_Visibility (Odd_Preferences.Combo_Entry16, True);

   Gtk_New (Odd_Preferences.Title_Color_Combo);
   Attach (Odd_Preferences.Table3, Odd_Preferences.Title_Color_Combo, 3, 4, 0, 1,
     Fill, 0,
     0, 0);
   Set_Case_Sensitive (Odd_Preferences.Title_Color_Combo, False);
   Set_Use_Arrows (Odd_Preferences.Title_Color_Combo, True);
   Set_Use_Arrows_Always (Odd_Preferences.Title_Color_Combo, False);
   String_List.Append (Title_Color_Combo_Items, -"");
   Combo.Set_Popdown_Strings (Odd_Preferences.Title_Color_Combo, Title_Color_Combo_Items);
   Free_String_List (Title_Color_Combo_Items);

   Odd_Preferences.Combo_Entry14 := Get_Entry (Odd_Preferences.Title_Color_Combo);
   Set_Editable (Odd_Preferences.Combo_Entry14, True);
   Set_Max_Length (Odd_Preferences.Combo_Entry14, 0);
   Set_Text (Odd_Preferences.Combo_Entry14, -"");
   Set_Visibility (Odd_Preferences.Combo_Entry14, True);

   Gtk_New (Odd_Preferences.Freeze_Bg_Color_Combo);
   Attach (Odd_Preferences.Table3, Odd_Preferences.Freeze_Bg_Color_Combo, 3, 4, 2, 3,
     Fill, 0,
     0, 0);
   Set_Case_Sensitive (Odd_Preferences.Freeze_Bg_Color_Combo, False);
   Set_Use_Arrows (Odd_Preferences.Freeze_Bg_Color_Combo, True);
   Set_Use_Arrows_Always (Odd_Preferences.Freeze_Bg_Color_Combo, False);
   String_List.Append (Freeze_Bg_Color_Combo_Items, -"");
   Combo.Set_Popdown_Strings (Odd_Preferences.Freeze_Bg_Color_Combo, Freeze_Bg_Color_Combo_Items);
   Free_String_List (Freeze_Bg_Color_Combo_Items);

   Odd_Preferences.Combo_Entry17 := Get_Entry (Odd_Preferences.Freeze_Bg_Color_Combo);
   Set_Editable (Odd_Preferences.Combo_Entry17, True);
   Set_Max_Length (Odd_Preferences.Combo_Entry17, 0);
   Set_Text (Odd_Preferences.Combo_Entry17, -"");
   Set_Visibility (Odd_Preferences.Combo_Entry17, True);

   Gtk_New (Odd_Preferences.Label9, -("Data"));
   Set_Alignment (Odd_Preferences.Label9, 0.5, 0.5);
   Set_Padding (Odd_Preferences.Label9, 0, 0);
   Set_Justify (Odd_Preferences.Label9, Justify_Center);
   Set_Line_Wrap (Odd_Preferences.Label9, False);
   Set_Tab (Odd_Preferences.Notebook1, 2, Odd_Preferences.Label9);

   Gtk_New (Odd_Preferences.Frame13);
   Add (Odd_Preferences.Notebook1, Odd_Preferences.Frame13);
   Set_Border_Width (Odd_Preferences.Frame13, 2);
   Set_Shadow_Type (Odd_Preferences.Frame13, Shadow_Etched_In);

   Gtk_New (Odd_Preferences.Table8, 2, 2, False);
   Add (Odd_Preferences.Frame13, Odd_Preferences.Table8);
   Set_Row_Spacings (Odd_Preferences.Table8, 2);
   Set_Col_Spacings (Odd_Preferences.Table8, 2);

   Gtk_New (Odd_Preferences.Label92, -("Color Hilighting"));
   Attach (Odd_Preferences.Table8, Odd_Preferences.Label92, 0, 1, 0, 1,
     Fill, 0,
     0, 0);
   Set_Alignment (Odd_Preferences.Label92, 7.45058e-09, 0.5);
   Set_Padding (Odd_Preferences.Label92, 0, 0);
   Set_Justify (Odd_Preferences.Label92, Justify_Center);
   Set_Line_Wrap (Odd_Preferences.Label92, False);

   Gtk_New (Odd_Preferences.Label93, -("Font"));
   Attach (Odd_Preferences.Table8, Odd_Preferences.Label93, 0, 1, 1, 2,
     Fill, 0,
     0, 0);
   Set_Alignment (Odd_Preferences.Label93, 7.45058e-09, 0.5);
   Set_Padding (Odd_Preferences.Label93, 0, 0);
   Set_Justify (Odd_Preferences.Label93, Justify_Center);
   Set_Line_Wrap (Odd_Preferences.Label93, False);

   Gtk_New (Odd_Preferences.Debug_Font_Combo, Orientation_Horizontal, Toolbar_Both);
   Attach (Odd_Preferences.Table8, Odd_Preferences.Debug_Font_Combo, 1, 2, 1, 2,
     Fill, 0,
     0, 0);
   Set_Space_Size (Odd_Preferences.Debug_Font_Combo, 5);
   Set_Space_Style (Odd_Preferences.Debug_Font_Combo, Toolbar_Space_Empty);
   Set_Tooltips (Odd_Preferences.Debug_Font_Combo, True);
   Set_Button_Relief (Odd_Preferences.Debug_Font_Combo, Relief_Normal);

   Gtk_New (Odd_Preferences.Debug_Higlight_Combo);
   Attach (Odd_Preferences.Table8, Odd_Preferences.Debug_Higlight_Combo, 1, 2, 0, 1,
     Fill, 0,
     0, 0);
   Set_Case_Sensitive (Odd_Preferences.Debug_Higlight_Combo, False);
   Set_Use_Arrows (Odd_Preferences.Debug_Higlight_Combo, True);
   Set_Use_Arrows_Always (Odd_Preferences.Debug_Higlight_Combo, False);
   String_List.Append (Debug_Higlight_Combo_Items, -"");
   Combo.Set_Popdown_Strings (Odd_Preferences.Debug_Higlight_Combo, Debug_Higlight_Combo_Items);
   Free_String_List (Debug_Higlight_Combo_Items);

   Odd_Preferences.Combo_Entry24 := Get_Entry (Odd_Preferences.Debug_Higlight_Combo);
   Set_Editable (Odd_Preferences.Combo_Entry24, True);
   Set_Max_Length (Odd_Preferences.Combo_Entry24, 0);
   Set_Text (Odd_Preferences.Combo_Entry24, -"");
   Set_Visibility (Odd_Preferences.Combo_Entry24, True);

   Gtk_New (Odd_Preferences.Label10, -("Command"));
   Set_Alignment (Odd_Preferences.Label10, 0.5, 0.5);
   Set_Padding (Odd_Preferences.Label10, 0, 0);
   Set_Justify (Odd_Preferences.Label10, Justify_Center);
   Set_Line_Wrap (Odd_Preferences.Label10, False);
   Set_Tab (Odd_Preferences.Notebook1, 3, Odd_Preferences.Label10);

   Gtk_New (Odd_Preferences.Frame6);
   Add (Odd_Preferences.Notebook1, Odd_Preferences.Frame6);
   Set_Border_Width (Odd_Preferences.Frame6, 2);
   Set_Shadow_Type (Odd_Preferences.Frame6, Shadow_Etched_In);

   Gtk_New (Odd_Preferences.Table6, 4, 2, False);
   Add (Odd_Preferences.Frame6, Odd_Preferences.Table6);
   Set_Row_Spacings (Odd_Preferences.Table6, 2);
   Set_Col_Spacings (Odd_Preferences.Table6, 0);

   Gtk_New (Odd_Preferences.Edit_Source_Entry);
   Attach (Odd_Preferences.Table6, Odd_Preferences.Edit_Source_Entry, 1, 2, 0, 1,
     Expand or Fill, 0,
     3, 0);
   Set_Editable (Odd_Preferences.Edit_Source_Entry, True);
   Set_Max_Length (Odd_Preferences.Edit_Source_Entry, 0);
   Set_Text (Odd_Preferences.Edit_Source_Entry, -"");
   Set_Visibility (Odd_Preferences.Edit_Source_Entry, True);

   Gtk_New (Odd_Preferences.Get_Core_File_Entry);
   Attach (Odd_Preferences.Table6, Odd_Preferences.Get_Core_File_Entry, 1, 2, 1, 2,
     Expand or Fill, 0,
     3, 0);
   Set_Editable (Odd_Preferences.Get_Core_File_Entry, True);
   Set_Max_Length (Odd_Preferences.Get_Core_File_Entry, 0);
   Set_Text (Odd_Preferences.Get_Core_File_Entry, -"");
   Set_Visibility (Odd_Preferences.Get_Core_File_Entry, True);

   Gtk_New (Odd_Preferences.List_Processes_Entry);
   Attach (Odd_Preferences.Table6, Odd_Preferences.List_Processes_Entry, 1, 2, 2, 3,
     Expand or Fill, 0,
     3, 0);
   Set_Editable (Odd_Preferences.List_Processes_Entry, True);
   Set_Max_Length (Odd_Preferences.List_Processes_Entry, 0);
   Set_Text (Odd_Preferences.List_Processes_Entry, -"");
   Set_Visibility (Odd_Preferences.List_Processes_Entry, True);

   Gtk_New (Odd_Preferences.Web_Browser_Entry);
   Attach (Odd_Preferences.Table6, Odd_Preferences.Web_Browser_Entry, 1, 2, 3, 4,
     Expand or Fill, 0,
     3, 0);
   Set_Editable (Odd_Preferences.Web_Browser_Entry, True);
   Set_Max_Length (Odd_Preferences.Web_Browser_Entry, 0);
   Set_Text (Odd_Preferences.Web_Browser_Entry, -"");
   Set_Visibility (Odd_Preferences.Web_Browser_Entry, True);

   Gtk_New (Odd_Preferences.Label43, -("Edit Sources"));
   Attach (Odd_Preferences.Table6, Odd_Preferences.Label43, 0, 1, 0, 1,
     Fill, 0,
     0, 0);
   Set_Alignment (Odd_Preferences.Label43, 7.45058e-09, 0.5);
   Set_Padding (Odd_Preferences.Label43, 3, 0);
   Set_Justify (Odd_Preferences.Label43, Justify_Center);
   Set_Line_Wrap (Odd_Preferences.Label43, False);

   Gtk_New (Odd_Preferences.Label44, -("Get Core File"));
   Attach (Odd_Preferences.Table6, Odd_Preferences.Label44, 0, 1, 1, 2,
     Fill, 0,
     0, 0);
   Set_Alignment (Odd_Preferences.Label44, 7.45058e-09, 0.5);
   Set_Padding (Odd_Preferences.Label44, 3, 0);
   Set_Justify (Odd_Preferences.Label44, Justify_Center);
   Set_Line_Wrap (Odd_Preferences.Label44, False);

   Gtk_New (Odd_Preferences.Label45, -("List Processes"));
   Attach (Odd_Preferences.Table6, Odd_Preferences.Label45, 0, 1, 2, 3,
     Fill, 0,
     0, 0);
   Set_Alignment (Odd_Preferences.Label45, 7.45058e-09, 0.5);
   Set_Padding (Odd_Preferences.Label45, 3, 0);
   Set_Justify (Odd_Preferences.Label45, Justify_Center);
   Set_Line_Wrap (Odd_Preferences.Label45, False);

   Gtk_New (Odd_Preferences.Label48, -("Web Browser"));
   Attach (Odd_Preferences.Table6, Odd_Preferences.Label48, 0, 1, 3, 4,
     Fill, 0,
     0, 0);
   Set_Alignment (Odd_Preferences.Label48, 7.45058e-09, 0.5);
   Set_Padding (Odd_Preferences.Label48, 3, 0);
   Set_Justify (Odd_Preferences.Label48, Justify_Center);
   Set_Line_Wrap (Odd_Preferences.Label48, False);

   Gtk_New (Odd_Preferences.Label12, -("Helpers"));
   Set_Alignment (Odd_Preferences.Label12, 0.5, 0.5);
   Set_Padding (Odd_Preferences.Label12, 0, 0);
   Set_Justify (Odd_Preferences.Label12, Justify_Center);
   Set_Line_Wrap (Odd_Preferences.Label12, False);
   Set_Tab (Odd_Preferences.Notebook1, 4, Odd_Preferences.Label12);

   Gtk_New (Odd_Preferences.Hbuttonbox6);
   Pack_Start (Odd_Preferences.Vbox2, Odd_Preferences.Hbuttonbox6, False, False, 3);
   Set_Spacing (Odd_Preferences.Hbuttonbox6, 30);
   Set_Layout (Odd_Preferences.Hbuttonbox6, Buttonbox_Spread);
   Set_Child_Size (Odd_Preferences.Hbuttonbox6, 85, 27);
   Set_Child_Ipadding (Odd_Preferences.Hbuttonbox6, 7, 0);

   Gtk_New (Odd_Preferences.Ok_Button, -"OK");
   Set_Flags (Odd_Preferences.Ok_Button, Can_Default);
   Button_Callback.Connect
     (Odd_Preferences.Ok_Button, "clicked",
      Button_Callback.To_Marshaller (On_Ok_Button_Clicked'Access));
   Add (Odd_Preferences.Hbuttonbox6, Odd_Preferences.Ok_Button);

   Gtk_New (Odd_Preferences.Reset_Button, -"Reset");
   Set_Flags (Odd_Preferences.Reset_Button, Can_Default);
   Button_Callback.Connect
     (Odd_Preferences.Reset_Button, "clicked",
      Button_Callback.To_Marshaller (On_Reset_Button_Clicked'Access));
   Add (Odd_Preferences.Hbuttonbox6, Odd_Preferences.Reset_Button);

   Gtk_New (Odd_Preferences.Help_Button, -"Help");
   Set_Flags (Odd_Preferences.Help_Button, Can_Default);
   Button_Callback.Connect
     (Odd_Preferences.Help_Button, "clicked",
      Button_Callback.To_Marshaller (On_Help_Button_Clicked'Access));
   Add (Odd_Preferences.Hbuttonbox6, Odd_Preferences.Help_Button);

end Initialize;

end Odd_Preferences_Pkg;
