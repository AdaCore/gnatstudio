with Glib; use Glib;
with Gtk; use Gtk;
with Gdk.Types;       use Gdk.Types;
with Gtk.Widget;      use Gtk.Widget;
with Gtk.Enums;       use Gtk.Enums;
with Gtkada.Handlers; use Gtkada.Handlers;
with Callbacks_Odd; use Callbacks_Odd;
with Odd_Intl; use Odd_Intl;
with General_Preferences_Pkg.Callbacks; use General_Preferences_Pkg.Callbacks;

package body General_Preferences_Pkg is

procedure Gtk_New (General_Preferences : out General_Preferences_Access) is
begin
   General_Preferences := new General_Preferences_Record;
   General_Preferences_Pkg.Initialize (General_Preferences);
end Gtk_New;

procedure Initialize (General_Preferences : access General_Preferences_Record'Class) is
   pragma Suppress (All_Checks);
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
   Gtk.Window.Initialize (General_Preferences, Window_Dialog);
   Set_Title (General_Preferences, -"Preferences");
   Set_Policy (General_Preferences, False, True, False);
   Set_Position (General_Preferences, Win_Pos_Center);
   Set_Modal (General_Preferences, True);
   Return_Callback.Connect
     (General_Preferences, "delete_event", On_Odd_Preferences_Delete_Event'Access);

   Gtk_New_Vbox (General_Preferences.Vbox2, False, 0);
   Add (General_Preferences, General_Preferences.Vbox2);

   Gtk_New (General_Preferences.Notebook1);
   Set_Scrollable (General_Preferences.Notebook1, False);
   Set_Show_Border (General_Preferences.Notebook1, True);
   Set_Show_Tabs (General_Preferences.Notebook1, True);
   Set_Tab_Hborder (General_Preferences.Notebook1, 2);
   Set_Tab_Vborder (General_Preferences.Notebook1, 2);
   Set_Tab_Pos (General_Preferences.Notebook1, Pos_Top);
   Pack_Start (General_Preferences.Vbox2, General_Preferences.Notebook1, True, True, 0);

   Gtk_New (General_Preferences.Frame1);
   Set_Shadow_Type (General_Preferences.Frame1, Shadow_Etched_In);
   Add (General_Preferences.Notebook1, General_Preferences.Frame1);

   Gtk_New (General_Preferences.Table1, 5, 4, False);
   Set_Row_Spacings (General_Preferences.Table1, 2);
   Set_Col_Spacings (General_Preferences.Table1, 0);
   Add (General_Preferences.Frame1, General_Preferences.Table1);

   Gtk_New (General_Preferences.Label13, -("Automatic Display of Button Hints"));
   Set_Alignment (General_Preferences.Label13, 7.45058e-09, 0.5);
   Set_Padding (General_Preferences.Label13, 2, 0);
   Set_Justify (General_Preferences.Label13, Justify_Left);
   Set_Line_Wrap (General_Preferences.Label13, False);
   Attach (General_Preferences.Table1, General_Preferences.Label13, 0, 1, 0, 1,
     Expand or Fill, 0,
     0, 0);

   Gtk_New (General_Preferences.Label14, -("Automatic Display of Variable Values"));
   Set_Alignment (General_Preferences.Label14, 7.45058e-09, 0.5);
   Set_Padding (General_Preferences.Label14, 2, 0);
   Set_Justify (General_Preferences.Label14, Justify_Left);
   Set_Line_Wrap (General_Preferences.Label14, False);
   Attach (General_Preferences.Table1, General_Preferences.Label14, 0, 1, 1, 2,
     Expand or Fill, 0,
     0, 0);

   Gtk_New (General_Preferences.Button_Hint_Popup_Check, -"as Popup Tips");
   Set_Active (General_Preferences.Button_Hint_Popup_Check, False);
   Attach (General_Preferences.Table1, General_Preferences.Button_Hint_Popup_Check, 1, 3, 0, 1,
     Expand, 0,
     0, 0);

   Gtk_New (General_Preferences.Button_Hint_Status_Check, -"in the Status Line");
   Set_Active (General_Preferences.Button_Hint_Status_Check, False);
   Attach (General_Preferences.Table1, General_Preferences.Button_Hint_Status_Check, 3, 4, 0, 1,
     Expand, 0,
     0, 0);

   Gtk_New (General_Preferences.Variable_Popup_Check, -"as Popup Tips");
   Set_Active (General_Preferences.Variable_Popup_Check, False);
   Attach (General_Preferences.Table1, General_Preferences.Variable_Popup_Check, 1, 3, 1, 2,
     Expand, 0,
     0, 0);

   Gtk_New (General_Preferences.Variable_Status_Check, -"in the Status Line");
   Set_Active (General_Preferences.Variable_Status_Check, False);
   Attach (General_Preferences.Table1, General_Preferences.Variable_Status_Check, 3, 4, 1, 2,
     Expand, 0,
     0, 0);

   Gtk_New (General_Preferences.Warn_Multiple_Check, -"Warn if Multiple GVD Instances are Running");
   Set_Active (General_Preferences.Warn_Multiple_Check, False);
   Attach (General_Preferences.Table1, General_Preferences.Warn_Multiple_Check, 0, 4, 2, 3,
     Expand or Fill, 0,
     0, 0);

   Gtk_New (General_Preferences.Label16, -("Status Bar Time Out"));
   Set_Alignment (General_Preferences.Label16, 7.45058e-09, 0.5);
   Set_Padding (General_Preferences.Label16, 2, 0);
   Set_Justify (General_Preferences.Label16, Justify_Left);
   Set_Line_Wrap (General_Preferences.Label16, False);
   Attach (General_Preferences.Table1, General_Preferences.Label16, 0, 1, 4, 5,
     Expand or Fill, 0,
     0, 0);

   Gtk_New (General_Preferences.Statusbar_Timeout_Entry);
   Set_Editable (General_Preferences.Statusbar_Timeout_Entry, True);
   Set_Max_Length (General_Preferences.Statusbar_Timeout_Entry, 0);
   Set_Text (General_Preferences.Statusbar_Timeout_Entry, -"");
   Set_Visibility (General_Preferences.Statusbar_Timeout_Entry, True);
   Attach (General_Preferences.Table1, General_Preferences.Statusbar_Timeout_Entry, 1, 2, 4, 5,
     Expand or Fill, 0,
     0, 0);

   Gtk_New (General_Preferences.Label17, -("ms"));
   Set_Alignment (General_Preferences.Label17, 7.45058e-09, 0.5);
   Set_Padding (General_Preferences.Label17, 2, 0);
   Set_Justify (General_Preferences.Label17, Justify_Center);
   Set_Line_Wrap (General_Preferences.Label17, False);
   Attach (General_Preferences.Table1, General_Preferences.Label17, 2, 4, 4, 5,
     Expand or Fill, 0,
     0, 0);

   Gtk_New (General_Preferences.Break_Exception_Check, -"Break On Exceptions");
   Set_Active (General_Preferences.Break_Exception_Check, False);
   Attach (General_Preferences.Table1, General_Preferences.Break_Exception_Check, 0, 4, 3, 4,
     Expand or Fill, 0,
     0, 0);

   Gtk_New (General_Preferences.Label7, -("General"));
   Set_Alignment (General_Preferences.Label7, 0.5, 0.5);
   Set_Padding (General_Preferences.Label7, 0, 0);
   Set_Justify (General_Preferences.Label7, Justify_Center);
   Set_Line_Wrap (General_Preferences.Label7, False);
   Set_Tab (General_Preferences.Notebook1, 0, General_Preferences.Label7);

   Gtk_New_Vbox (General_Preferences.Vbox18, False, 2);
   Set_Border_Width (General_Preferences.Vbox18, 2);
   Add (General_Preferences.Notebook1, General_Preferences.Vbox18);

   Gtk_New (General_Preferences.Frame16, -"Explorer");
   Set_Shadow_Type (General_Preferences.Frame16, Shadow_Etched_In);
   Pack_Start (General_Preferences.Vbox18, General_Preferences.Frame16, False, False, 0);

   Gtk_New (General_Preferences.Table9, 2, 2, False);
   Set_Row_Spacings (General_Preferences.Table9, 2);
   Set_Col_Spacings (General_Preferences.Table9, 5);
   Add (General_Preferences.Frame16, General_Preferences.Table9);

   Gtk_New (General_Preferences.Display_Explorer_Check, -"Display Explorer Tree");
   Set_Active (General_Preferences.Display_Explorer_Check, True);
   Attach (General_Preferences.Table9, General_Preferences.Display_Explorer_Check, 0, 2, 0, 1,
     Expand or Fill, 0,
     0, 0);

   Gtk_New (General_Preferences.Label75, -("File Name Background"));
   Set_Alignment (General_Preferences.Label75, 7.45058e-09, 0.5);
   Set_Padding (General_Preferences.Label75, 0, 0);
   Set_Justify (General_Preferences.Label75, Justify_Center);
   Set_Line_Wrap (General_Preferences.Label75, False);
   Attach (General_Preferences.Table9, General_Preferences.Label75, 0, 1, 1, 2,
     Fill, 0,
     0, 0);

   Gtk_New (General_Preferences.File_Name_Bg_Combo);
   Set_Case_Sensitive (General_Preferences.File_Name_Bg_Combo, False);
   Set_Use_Arrows (General_Preferences.File_Name_Bg_Combo, True);
   Set_Use_Arrows_Always (General_Preferences.File_Name_Bg_Combo, False);
   String_List.Append (File_Name_Bg_Combo_Items, -"");
   Combo.Set_Popdown_Strings (General_Preferences.File_Name_Bg_Combo, File_Name_Bg_Combo_Items);
   Free_String_List (File_Name_Bg_Combo_Items);
   Attach (General_Preferences.Table9, General_Preferences.File_Name_Bg_Combo, 1, 2, 1, 2,
     0, 0,
     0, 0);

   General_Preferences.Combo_Entry6 := Get_Entry (General_Preferences.File_Name_Bg_Combo);
   Set_Editable (General_Preferences.Combo_Entry6, True);
   Set_Max_Length (General_Preferences.Combo_Entry6, 0);
   Set_Text (General_Preferences.Combo_Entry6, -"");
   Set_Visibility (General_Preferences.Combo_Entry6, True);

   Gtk_New (General_Preferences.Frame17, -"Source");
   Set_Shadow_Type (General_Preferences.Frame17, Shadow_Etched_In);
   Pack_Start (General_Preferences.Vbox18, General_Preferences.Frame17, False, False, 0);

   Gtk_New (General_Preferences.Table10, 6, 2, False);
   Set_Row_Spacings (General_Preferences.Table10, 2);
   Set_Col_Spacings (General_Preferences.Table10, 5);
   Add (General_Preferences.Frame17, General_Preferences.Table10);

   Gtk_New (General_Preferences.Label76, -("Font"));
   Set_Alignment (General_Preferences.Label76, 7.45058e-09, 0.5);
   Set_Padding (General_Preferences.Label76, 0, 0);
   Set_Justify (General_Preferences.Label76, Justify_Center);
   Set_Line_Wrap (General_Preferences.Label76, False);
   Attach (General_Preferences.Table10, General_Preferences.Label76, 0, 1, 0, 1,
     Fill, 0,
     0, 0);

   Gtk_New (General_Preferences.Show_Lines_Code_Check, -"Show Lines with Code");
   Set_Active (General_Preferences.Show_Lines_Code_Check, True);
   Attach (General_Preferences.Table10, General_Preferences.Show_Lines_Code_Check, 1, 2, 1, 2,
     Fill, 0,
     0, 0);

   Gtk_New (General_Preferences.Show_Line_Numbers_Check, -"Show Line Numbers");
   Set_Active (General_Preferences.Show_Line_Numbers_Check, True);
   Attach (General_Preferences.Table10, General_Preferences.Show_Line_Numbers_Check, 0, 1, 1, 2,
     Fill, 0,
     0, 0);

   Gtk_New (General_Preferences.Syntax_Hilight_Check, -"Syntax Highlighting");
   Set_Active (General_Preferences.Syntax_Hilight_Check, True);
   Attach (General_Preferences.Table10, General_Preferences.Syntax_Hilight_Check, 0, 2, 2, 3,
     Fill, 0,
     0, 0);

   Gtk_New (General_Preferences.Label79, -("Comments"));
   Set_Alignment (General_Preferences.Label79, 7.45058e-09, 0.5);
   Set_Padding (General_Preferences.Label79, 0, 0);
   Set_Justify (General_Preferences.Label79, Justify_Center);
   Set_Line_Wrap (General_Preferences.Label79, False);
   Attach (General_Preferences.Table10, General_Preferences.Label79, 0, 1, 3, 4,
     Fill, 0,
     0, 0);

   Gtk_New (General_Preferences.Label80, -("Strings"));
   Set_Alignment (General_Preferences.Label80, 7.45058e-09, 0.5);
   Set_Padding (General_Preferences.Label80, 0, 0);
   Set_Justify (General_Preferences.Label80, Justify_Center);
   Set_Line_Wrap (General_Preferences.Label80, False);
   Attach (General_Preferences.Table10, General_Preferences.Label80, 0, 1, 4, 5,
     Fill, 0,
     0, 0);

   Gtk_New (General_Preferences.Label81, -("Keywords"));
   Set_Alignment (General_Preferences.Label81, 7.45058e-09, 0.5);
   Set_Padding (General_Preferences.Label81, 0, 0);
   Set_Justify (General_Preferences.Label81, Justify_Center);
   Set_Line_Wrap (General_Preferences.Label81, False);
   Attach (General_Preferences.Table10, General_Preferences.Label81, 0, 1, 5, 6,
     Fill, 0,
     0, 0);

   Gtk_New (General_Preferences.Editor_Font_Combo, Orientation_Horizontal, Toolbar_Both);
   Set_Space_Size (General_Preferences.Editor_Font_Combo, 5);
   Set_Space_Style (General_Preferences.Editor_Font_Combo, Toolbar_Space_Empty);
   Set_Tooltips (General_Preferences.Editor_Font_Combo, True);
   Set_Button_Relief (General_Preferences.Editor_Font_Combo, Relief_Normal);
   Attach (General_Preferences.Table10, General_Preferences.Editor_Font_Combo, 1, 2, 0, 1,
     Fill, 0,
     0, 0);

   Gtk_New (General_Preferences.Comment_Color_Combo);
   Set_Case_Sensitive (General_Preferences.Comment_Color_Combo, False);
   Set_Use_Arrows (General_Preferences.Comment_Color_Combo, True);
   Set_Use_Arrows_Always (General_Preferences.Comment_Color_Combo, False);
   String_List.Append (Comment_Color_Combo_Items, -"");
   Combo.Set_Popdown_Strings (General_Preferences.Comment_Color_Combo, Comment_Color_Combo_Items);
   Free_String_List (Comment_Color_Combo_Items);
   Attach (General_Preferences.Table10, General_Preferences.Comment_Color_Combo, 1, 2, 3, 4,
     0, 0,
     0, 0);

   General_Preferences.Combo_Entry10 := Get_Entry (General_Preferences.Comment_Color_Combo);
   Set_Editable (General_Preferences.Combo_Entry10, True);
   Set_Max_Length (General_Preferences.Combo_Entry10, 0);
   Set_Text (General_Preferences.Combo_Entry10, -"");
   Set_Visibility (General_Preferences.Combo_Entry10, True);

   Gtk_New (General_Preferences.String_Color_Combo);
   Set_Case_Sensitive (General_Preferences.String_Color_Combo, False);
   Set_Use_Arrows (General_Preferences.String_Color_Combo, True);
   Set_Use_Arrows_Always (General_Preferences.String_Color_Combo, False);
   String_List.Append (String_Color_Combo_Items, -"");
   Combo.Set_Popdown_Strings (General_Preferences.String_Color_Combo, String_Color_Combo_Items);
   Free_String_List (String_Color_Combo_Items);
   Attach (General_Preferences.Table10, General_Preferences.String_Color_Combo, 1, 2, 4, 5,
     0, 0,
     0, 0);

   General_Preferences.Combo_Entry9 := Get_Entry (General_Preferences.String_Color_Combo);
   Set_Editable (General_Preferences.Combo_Entry9, True);
   Set_Max_Length (General_Preferences.Combo_Entry9, 0);
   Set_Text (General_Preferences.Combo_Entry9, -"");
   Set_Visibility (General_Preferences.Combo_Entry9, True);

   Gtk_New (General_Preferences.Keyword_Color_Combo);
   Set_Case_Sensitive (General_Preferences.Keyword_Color_Combo, False);
   Set_Use_Arrows (General_Preferences.Keyword_Color_Combo, True);
   Set_Use_Arrows_Always (General_Preferences.Keyword_Color_Combo, False);
   String_List.Append (Keyword_Color_Combo_Items, -"");
   Combo.Set_Popdown_Strings (General_Preferences.Keyword_Color_Combo, Keyword_Color_Combo_Items);
   Free_String_List (Keyword_Color_Combo_Items);
   Attach (General_Preferences.Table10, General_Preferences.Keyword_Color_Combo, 1, 2, 5, 6,
     0, 0,
     0, 0);

   General_Preferences.Combo_Entry11 := Get_Entry (General_Preferences.Keyword_Color_Combo);
   Set_Editable (General_Preferences.Combo_Entry11, True);
   Set_Max_Length (General_Preferences.Combo_Entry11, 0);
   Set_Text (General_Preferences.Combo_Entry11, -"");
   Set_Visibility (General_Preferences.Combo_Entry11, True);

   Gtk_New (General_Preferences.Frame18, -"Assembly");
   Set_Shadow_Type (General_Preferences.Frame18, Shadow_Etched_In);
   Pack_Start (General_Preferences.Vbox18, General_Preferences.Frame18, False, False, 0);

   Gtk_New_Hbox (General_Preferences.Hbox7, False, 5);
   Add (General_Preferences.Frame18, General_Preferences.Hbox7);

   Gtk_New (General_Preferences.Label82, -("Current Line"));
   Set_Alignment (General_Preferences.Label82, 7.45058e-09, 0.5);
   Set_Padding (General_Preferences.Label82, 0, 0);
   Set_Justify (General_Preferences.Label82, Justify_Center);
   Set_Line_Wrap (General_Preferences.Label82, False);
   Pack_Start (General_Preferences.Hbox7, General_Preferences.Label82, False, True, 0);

   Gtk_New (General_Preferences.Asm_Highlight_Combo);
   Set_Case_Sensitive (General_Preferences.Asm_Highlight_Combo, False);
   Set_Use_Arrows (General_Preferences.Asm_Highlight_Combo, True);
   Set_Use_Arrows_Always (General_Preferences.Asm_Highlight_Combo, False);
   String_List.Append (Asm_Highlight_Combo_Items, -"");
   Combo.Set_Popdown_Strings (General_Preferences.Asm_Highlight_Combo, Asm_Highlight_Combo_Items);
   Free_String_List (Asm_Highlight_Combo_Items);
   Pack_Start (General_Preferences.Hbox7, General_Preferences.Asm_Highlight_Combo, False, True, 0);

   General_Preferences.Combo_Entry12 := Get_Entry (General_Preferences.Asm_Highlight_Combo);
   Set_Editable (General_Preferences.Combo_Entry12, True);
   Set_Max_Length (General_Preferences.Combo_Entry12, 0);
   Set_Text (General_Preferences.Combo_Entry12, -"");
   Set_Visibility (General_Preferences.Combo_Entry12, True);

   Gtk_New (General_Preferences.Label8, -("Source"));
   Set_Alignment (General_Preferences.Label8, 0.5, 0.5);
   Set_Padding (General_Preferences.Label8, 0, 0);
   Set_Justify (General_Preferences.Label8, Justify_Center);
   Set_Line_Wrap (General_Preferences.Label8, False);
   Set_Tab (General_Preferences.Notebook1, 1, General_Preferences.Label8);

   Gtk_New (General_Preferences.Frame3);
   Set_Border_Width (General_Preferences.Frame3, 2);
   Set_Shadow_Type (General_Preferences.Frame3, Shadow_Etched_In);
   Add (General_Preferences.Notebook1, General_Preferences.Frame3);

   Gtk_New (General_Preferences.Table3, 11, 4, False);
   Set_Row_Spacings (General_Preferences.Table3, 2);
   Set_Col_Spacings (General_Preferences.Table3, 2);
   Add (General_Preferences.Frame3, General_Preferences.Table3);

   Gtk_New (General_Preferences.Label83, -("Clickable Item"));
   Set_Alignment (General_Preferences.Label83, 7.45058e-09, 0.5);
   Set_Padding (General_Preferences.Label83, 0, 0);
   Set_Justify (General_Preferences.Label83, Justify_Center);
   Set_Line_Wrap (General_Preferences.Label83, False);
   Attach (General_Preferences.Table3, General_Preferences.Label83, 0, 1, 0, 1,
     Fill, 0,
     0, 0);

   Gtk_New (General_Preferences.Detect_Aliases_Check, -"Detect Aliases (shared data structures)");
   Set_Active (General_Preferences.Detect_Aliases_Check, True);
   Attach (General_Preferences.Table3, General_Preferences.Detect_Aliases_Check, 0, 4, 8, 9,
     Expand or Fill, 0,
     0, 0);

   Gtk_New (General_Preferences.Align_Grid_Check, -"Auto-Align Displays on Nearest Grid Point");
   Set_Active (General_Preferences.Align_Grid_Check, True);
   Attach (General_Preferences.Table3, General_Preferences.Align_Grid_Check, 0, 4, 10, 11,
     Expand or Fill, 0,
     0, 0);

   Gtk_New (General_Preferences.Label84, -("Title Background"));
   Set_Alignment (General_Preferences.Label84, 7.45058e-09, 0.5);
   Set_Padding (General_Preferences.Label84, 0, 0);
   Set_Justify (General_Preferences.Label84, Justify_Center);
   Set_Line_Wrap (General_Preferences.Label84, False);
   Attach (General_Preferences.Table3, General_Preferences.Label84, 2, 3, 0, 1,
     Fill, 0,
     0, 0);

   Gtk_New (General_Preferences.Label85, -("Changed Data"));
   Set_Alignment (General_Preferences.Label85, 7.45058e-09, 0.5);
   Set_Padding (General_Preferences.Label85, 0, 0);
   Set_Justify (General_Preferences.Label85, Justify_Center);
   Set_Line_Wrap (General_Preferences.Label85, False);
   Attach (General_Preferences.Table3, General_Preferences.Label85, 0, 1, 1, 2,
     Fill, 0,
     0, 0);

   Gtk_New (General_Preferences.Label86, -("Auto-Refreshed"));
   Set_Alignment (General_Preferences.Label86, 7.45058e-09, 0.5);
   Set_Padding (General_Preferences.Label86, 0, 0);
   Set_Justify (General_Preferences.Label86, Justify_Center);
   Set_Line_Wrap (General_Preferences.Label86, False);
   Attach (General_Preferences.Table3, General_Preferences.Label86, 0, 1, 2, 3,
     Fill, 0,
     0, 0);

   Gtk_New (General_Preferences.Label87, -("Frozen"));
   Set_Alignment (General_Preferences.Label87, 7.45058e-09, 0.5);
   Set_Padding (General_Preferences.Label87, 0, 0);
   Set_Justify (General_Preferences.Label87, Justify_Center);
   Set_Line_Wrap (General_Preferences.Label87, False);
   Attach (General_Preferences.Table3, General_Preferences.Label87, 2, 3, 2, 3,
     Fill, 0,
     0, 0);

   Gtk_New (General_Preferences.Look_3d_Check, -"3D Look");
   Set_Active (General_Preferences.Look_3d_Check, True);
   Attach (General_Preferences.Table3, General_Preferences.Look_3d_Check, 0, 4, 3, 4,
     Expand or Fill, 0,
     0, 0);

   Gtk_New (General_Preferences.Label88, -("Item Name"));
   Set_Alignment (General_Preferences.Label88, 7.45058e-09, 0.5);
   Set_Padding (General_Preferences.Label88, 0, 0);
   Set_Justify (General_Preferences.Label88, Justify_Center);
   Set_Line_Wrap (General_Preferences.Label88, False);
   Attach (General_Preferences.Table3, General_Preferences.Label88, 0, 1, 4, 5,
     Fill, 0,
     0, 0);

   Gtk_New (General_Preferences.Label89, -("Item Value"));
   Set_Alignment (General_Preferences.Label89, 7.45058e-09, 0.5);
   Set_Padding (General_Preferences.Label89, 0, 0);
   Set_Justify (General_Preferences.Label89, Justify_Center);
   Set_Line_Wrap (General_Preferences.Label89, False);
   Attach (General_Preferences.Table3, General_Preferences.Label89, 0, 1, 5, 6,
     Fill, 0,
     0, 0);

   Gtk_New (General_Preferences.Label90, -("Item Type"));
   Set_Alignment (General_Preferences.Label90, 0.0, 0.5);
   Set_Padding (General_Preferences.Label90, 0, 0);
   Set_Justify (General_Preferences.Label90, Justify_Center);
   Set_Line_Wrap (General_Preferences.Label90, False);
   Attach (General_Preferences.Table3, General_Preferences.Label90, 0, 1, 6, 7,
     Fill, 0,
     0, 0);

   Gtk_New (General_Preferences.Hide_Big_Items_Check, -"Hide Big Items");
   Set_Active (General_Preferences.Hide_Big_Items_Check, True);
   Attach (General_Preferences.Table3, General_Preferences.Hide_Big_Items_Check, 0, 2, 7, 8,
     Expand or Fill, 0,
     0, 0);

   Gtk_New (General_Preferences.Label91, -("Big Item Height"));
   Set_Alignment (General_Preferences.Label91, 7.45058e-09, 0.5);
   Set_Padding (General_Preferences.Label91, 0, 0);
   Set_Justify (General_Preferences.Label91, Justify_Center);
   Set_Line_Wrap (General_Preferences.Label91, False);
   Attach (General_Preferences.Table3, General_Preferences.Label91, 2, 3, 7, 8,
     Fill, 0,
     0, 0);

   Gtk_New (Big_Item_Spin_Adj, 150.0, 0.0, 1000.0, 1.0, 10.0, 10.0);
   Gtk_New (General_Preferences.Big_Item_Spin, Big_Item_Spin_Adj, 1.0, 0);
   Set_Numeric (General_Preferences.Big_Item_Spin, False);
   Set_Snap_To_Ticks (General_Preferences.Big_Item_Spin, False);
   Set_Update_Policy (General_Preferences.Big_Item_Spin, Update_Always);
   Set_Value (General_Preferences.Big_Item_Spin, 150.0);
   Set_Wrap (General_Preferences.Big_Item_Spin, False);
   Attach (General_Preferences.Table3, General_Preferences.Big_Item_Spin, 3, 4, 7, 8,
     Expand or Fill, 0,
     0, 0);

   Gtk_New (General_Preferences.Display_Grid_Check, -"Display Grid Points");
   Set_Active (General_Preferences.Display_Grid_Check, True);
   Attach (General_Preferences.Table3, General_Preferences.Display_Grid_Check, 0, 4, 9, 10,
     Expand or Fill, 0,
     0, 0);

   Gtk_New (General_Preferences.Title_Font_Combo, Orientation_Horizontal, Toolbar_Both);
   Set_Space_Size (General_Preferences.Title_Font_Combo, 5);
   Set_Space_Style (General_Preferences.Title_Font_Combo, Toolbar_Space_Empty);
   Set_Tooltips (General_Preferences.Title_Font_Combo, True);
   Set_Button_Relief (General_Preferences.Title_Font_Combo, Relief_Normal);
   Attach (General_Preferences.Table3, General_Preferences.Title_Font_Combo, 1, 4, 4, 5,
     Fill, 0,
     0, 0);

   Gtk_New (General_Preferences.Value_Font_Combo, Orientation_Horizontal, Toolbar_Both);
   Set_Space_Size (General_Preferences.Value_Font_Combo, 5);
   Set_Space_Style (General_Preferences.Value_Font_Combo, Toolbar_Space_Empty);
   Set_Tooltips (General_Preferences.Value_Font_Combo, True);
   Set_Button_Relief (General_Preferences.Value_Font_Combo, Relief_Normal);
   Attach (General_Preferences.Table3, General_Preferences.Value_Font_Combo, 1, 4, 5, 6,
     Fill, 0,
     0, 0);

   Gtk_New (General_Preferences.Type_Font_Combo, Orientation_Horizontal, Toolbar_Both);
   Set_Space_Size (General_Preferences.Type_Font_Combo, 5);
   Set_Space_Style (General_Preferences.Type_Font_Combo, Toolbar_Space_Empty);
   Set_Tooltips (General_Preferences.Type_Font_Combo, True);
   Set_Button_Relief (General_Preferences.Type_Font_Combo, Relief_Normal);
   Attach (General_Preferences.Table3, General_Preferences.Type_Font_Combo, 1, 4, 6, 7,
     Fill, 0,
     0, 0);

   Gtk_New (General_Preferences.Xref_Color_Combo);
   Set_Case_Sensitive (General_Preferences.Xref_Color_Combo, False);
   Set_Use_Arrows (General_Preferences.Xref_Color_Combo, True);
   Set_Use_Arrows_Always (General_Preferences.Xref_Color_Combo, False);
   String_List.Append (Xref_Color_Combo_Items, -"");
   Combo.Set_Popdown_Strings (General_Preferences.Xref_Color_Combo, Xref_Color_Combo_Items);
   Free_String_List (Xref_Color_Combo_Items);
   Attach (General_Preferences.Table3, General_Preferences.Xref_Color_Combo, 1, 2, 0, 1,
     0, 0,
     0, 0);

   General_Preferences.Combo_Entry13 := Get_Entry (General_Preferences.Xref_Color_Combo);
   Set_Editable (General_Preferences.Combo_Entry13, True);
   Set_Max_Length (General_Preferences.Combo_Entry13, 0);
   Set_Text (General_Preferences.Combo_Entry13, -"");
   Set_Visibility (General_Preferences.Combo_Entry13, True);

   Gtk_New (General_Preferences.Change_Color_Combo);
   Set_Case_Sensitive (General_Preferences.Change_Color_Combo, False);
   Set_Use_Arrows (General_Preferences.Change_Color_Combo, True);
   Set_Use_Arrows_Always (General_Preferences.Change_Color_Combo, False);
   String_List.Append (Change_Color_Combo_Items, -"");
   Combo.Set_Popdown_Strings (General_Preferences.Change_Color_Combo, Change_Color_Combo_Items);
   Free_String_List (Change_Color_Combo_Items);
   Attach (General_Preferences.Table3, General_Preferences.Change_Color_Combo, 1, 2, 1, 2,
     0, 0,
     0, 0);

   General_Preferences.Combo_Entry15 := Get_Entry (General_Preferences.Change_Color_Combo);
   Set_Editable (General_Preferences.Combo_Entry15, True);
   Set_Max_Length (General_Preferences.Combo_Entry15, 0);
   Set_Text (General_Preferences.Combo_Entry15, -"");
   Set_Visibility (General_Preferences.Combo_Entry15, True);

   Gtk_New (General_Preferences.Thaw_Bg_Color_Combo);
   Set_Case_Sensitive (General_Preferences.Thaw_Bg_Color_Combo, False);
   Set_Use_Arrows (General_Preferences.Thaw_Bg_Color_Combo, True);
   Set_Use_Arrows_Always (General_Preferences.Thaw_Bg_Color_Combo, False);
   String_List.Append (Thaw_Bg_Color_Combo_Items, -"");
   Combo.Set_Popdown_Strings (General_Preferences.Thaw_Bg_Color_Combo, Thaw_Bg_Color_Combo_Items);
   Free_String_List (Thaw_Bg_Color_Combo_Items);
   Attach (General_Preferences.Table3, General_Preferences.Thaw_Bg_Color_Combo, 1, 2, 2, 3,
     0, 0,
     0, 0);

   General_Preferences.Combo_Entry16 := Get_Entry (General_Preferences.Thaw_Bg_Color_Combo);
   Set_Editable (General_Preferences.Combo_Entry16, True);
   Set_Max_Length (General_Preferences.Combo_Entry16, 0);
   Set_Text (General_Preferences.Combo_Entry16, -"");
   Set_Visibility (General_Preferences.Combo_Entry16, True);

   Gtk_New (General_Preferences.Title_Color_Combo);
   Set_Case_Sensitive (General_Preferences.Title_Color_Combo, False);
   Set_Use_Arrows (General_Preferences.Title_Color_Combo, True);
   Set_Use_Arrows_Always (General_Preferences.Title_Color_Combo, False);
   String_List.Append (Title_Color_Combo_Items, -"");
   Combo.Set_Popdown_Strings (General_Preferences.Title_Color_Combo, Title_Color_Combo_Items);
   Free_String_List (Title_Color_Combo_Items);
   Attach (General_Preferences.Table3, General_Preferences.Title_Color_Combo, 3, 4, 0, 1,
     0, 0,
     0, 0);

   General_Preferences.Combo_Entry14 := Get_Entry (General_Preferences.Title_Color_Combo);
   Set_Editable (General_Preferences.Combo_Entry14, True);
   Set_Max_Length (General_Preferences.Combo_Entry14, 0);
   Set_Text (General_Preferences.Combo_Entry14, -"");
   Set_Visibility (General_Preferences.Combo_Entry14, True);

   Gtk_New (General_Preferences.Freeze_Bg_Color_Combo);
   Set_Case_Sensitive (General_Preferences.Freeze_Bg_Color_Combo, False);
   Set_Use_Arrows (General_Preferences.Freeze_Bg_Color_Combo, True);
   Set_Use_Arrows_Always (General_Preferences.Freeze_Bg_Color_Combo, False);
   String_List.Append (Freeze_Bg_Color_Combo_Items, -"");
   Combo.Set_Popdown_Strings (General_Preferences.Freeze_Bg_Color_Combo, Freeze_Bg_Color_Combo_Items);
   Free_String_List (Freeze_Bg_Color_Combo_Items);
   Attach (General_Preferences.Table3, General_Preferences.Freeze_Bg_Color_Combo, 3, 4, 2, 3,
     0, 0,
     0, 0);

   General_Preferences.Combo_Entry17 := Get_Entry (General_Preferences.Freeze_Bg_Color_Combo);
   Set_Editable (General_Preferences.Combo_Entry17, True);
   Set_Max_Length (General_Preferences.Combo_Entry17, 0);
   Set_Text (General_Preferences.Combo_Entry17, -"");
   Set_Visibility (General_Preferences.Combo_Entry17, True);

   Gtk_New (General_Preferences.Label9, -("Data"));
   Set_Alignment (General_Preferences.Label9, 0.5, 0.5);
   Set_Padding (General_Preferences.Label9, 0, 0);
   Set_Justify (General_Preferences.Label9, Justify_Center);
   Set_Line_Wrap (General_Preferences.Label9, False);
   Set_Tab (General_Preferences.Notebook1, 2, General_Preferences.Label9);

   Gtk_New (General_Preferences.Frame13);
   Set_Border_Width (General_Preferences.Frame13, 2);
   Set_Shadow_Type (General_Preferences.Frame13, Shadow_Etched_In);
   Add (General_Preferences.Notebook1, General_Preferences.Frame13);

   Gtk_New (General_Preferences.Table8, 2, 2, False);
   Set_Row_Spacings (General_Preferences.Table8, 2);
   Set_Col_Spacings (General_Preferences.Table8, 2);
   Add (General_Preferences.Frame13, General_Preferences.Table8);

   Gtk_New (General_Preferences.Label92, -("Color Highlighting"));
   Set_Alignment (General_Preferences.Label92, 7.45058e-09, 0.5);
   Set_Padding (General_Preferences.Label92, 0, 0);
   Set_Justify (General_Preferences.Label92, Justify_Center);
   Set_Line_Wrap (General_Preferences.Label92, False);
   Attach (General_Preferences.Table8, General_Preferences.Label92, 0, 1, 0, 1,
     Fill, 0,
     0, 0);

   Gtk_New (General_Preferences.Label93, -("Font"));
   Set_Alignment (General_Preferences.Label93, 7.45058e-09, 0.5);
   Set_Padding (General_Preferences.Label93, 0, 0);
   Set_Justify (General_Preferences.Label93, Justify_Center);
   Set_Line_Wrap (General_Preferences.Label93, False);
   Attach (General_Preferences.Table8, General_Preferences.Label93, 0, 1, 1, 2,
     Fill, 0,
     0, 0);

   Gtk_New (General_Preferences.Debug_Font_Combo, Orientation_Horizontal, Toolbar_Both);
   Set_Space_Size (General_Preferences.Debug_Font_Combo, 5);
   Set_Space_Style (General_Preferences.Debug_Font_Combo, Toolbar_Space_Empty);
   Set_Tooltips (General_Preferences.Debug_Font_Combo, True);
   Set_Button_Relief (General_Preferences.Debug_Font_Combo, Relief_Normal);
   Attach (General_Preferences.Table8, General_Preferences.Debug_Font_Combo, 1, 2, 1, 2,
     Fill, 0,
     0, 0);

   Gtk_New (General_Preferences.Debug_Higlight_Combo);
   Set_Case_Sensitive (General_Preferences.Debug_Higlight_Combo, False);
   Set_Use_Arrows (General_Preferences.Debug_Higlight_Combo, True);
   Set_Use_Arrows_Always (General_Preferences.Debug_Higlight_Combo, False);
   String_List.Append (Debug_Higlight_Combo_Items, -"");
   Combo.Set_Popdown_Strings (General_Preferences.Debug_Higlight_Combo, Debug_Higlight_Combo_Items);
   Free_String_List (Debug_Higlight_Combo_Items);
   Attach (General_Preferences.Table8, General_Preferences.Debug_Higlight_Combo, 1, 2, 0, 1,
     0, 0,
     0, 0);

   General_Preferences.Combo_Entry24 := Get_Entry (General_Preferences.Debug_Higlight_Combo);
   Set_Editable (General_Preferences.Combo_Entry24, True);
   Set_Max_Length (General_Preferences.Combo_Entry24, 0);
   Set_Text (General_Preferences.Combo_Entry24, -"");
   Set_Visibility (General_Preferences.Combo_Entry24, True);

   Gtk_New (General_Preferences.Label10, -("Command"));
   Set_Alignment (General_Preferences.Label10, 0.5, 0.5);
   Set_Padding (General_Preferences.Label10, 0, 0);
   Set_Justify (General_Preferences.Label10, Justify_Center);
   Set_Line_Wrap (General_Preferences.Label10, False);
   Set_Tab (General_Preferences.Notebook1, 3, General_Preferences.Label10);

   Gtk_New (General_Preferences.Frame6);
   Set_Border_Width (General_Preferences.Frame6, 2);
   Set_Shadow_Type (General_Preferences.Frame6, Shadow_Etched_In);
   Add (General_Preferences.Notebook1, General_Preferences.Frame6);

   Gtk_New (General_Preferences.Table6, 4, 2, False);
   Set_Row_Spacings (General_Preferences.Table6, 2);
   Set_Col_Spacings (General_Preferences.Table6, 0);
   Add (General_Preferences.Frame6, General_Preferences.Table6);

   Gtk_New (General_Preferences.Edit_Source_Entry);
   Set_Editable (General_Preferences.Edit_Source_Entry, True);
   Set_Max_Length (General_Preferences.Edit_Source_Entry, 0);
   Set_Text (General_Preferences.Edit_Source_Entry, -"");
   Set_Visibility (General_Preferences.Edit_Source_Entry, True);
   Attach (General_Preferences.Table6, General_Preferences.Edit_Source_Entry, 1, 2, 0, 1,
     Expand or Fill, 0,
     3, 0);

   Gtk_New (General_Preferences.Get_Core_File_Entry);
   Set_Editable (General_Preferences.Get_Core_File_Entry, True);
   Set_Max_Length (General_Preferences.Get_Core_File_Entry, 0);
   Set_Text (General_Preferences.Get_Core_File_Entry, -"");
   Set_Visibility (General_Preferences.Get_Core_File_Entry, True);
   Attach (General_Preferences.Table6, General_Preferences.Get_Core_File_Entry, 1, 2, 1, 2,
     Expand or Fill, 0,
     3, 0);

   Gtk_New (General_Preferences.List_Processes_Entry);
   Set_Editable (General_Preferences.List_Processes_Entry, True);
   Set_Max_Length (General_Preferences.List_Processes_Entry, 0);
   Set_Text (General_Preferences.List_Processes_Entry, -"");
   Set_Visibility (General_Preferences.List_Processes_Entry, True);
   Attach (General_Preferences.Table6, General_Preferences.List_Processes_Entry, 1, 2, 2, 3,
     Expand or Fill, 0,
     3, 0);

   Gtk_New (General_Preferences.Web_Browser_Entry);
   Set_Editable (General_Preferences.Web_Browser_Entry, True);
   Set_Max_Length (General_Preferences.Web_Browser_Entry, 0);
   Set_Text (General_Preferences.Web_Browser_Entry, -"");
   Set_Visibility (General_Preferences.Web_Browser_Entry, True);
   Attach (General_Preferences.Table6, General_Preferences.Web_Browser_Entry, 1, 2, 3, 4,
     Expand or Fill, 0,
     3, 0);

   Gtk_New (General_Preferences.Label43, -("Edit Sources"));
   Set_Alignment (General_Preferences.Label43, 7.45058e-09, 0.5);
   Set_Padding (General_Preferences.Label43, 3, 0);
   Set_Justify (General_Preferences.Label43, Justify_Center);
   Set_Line_Wrap (General_Preferences.Label43, False);
   Attach (General_Preferences.Table6, General_Preferences.Label43, 0, 1, 0, 1,
     Fill, 0,
     0, 0);

   Gtk_New (General_Preferences.Label44, -("Get Core File"));
   Set_Alignment (General_Preferences.Label44, 7.45058e-09, 0.5);
   Set_Padding (General_Preferences.Label44, 3, 0);
   Set_Justify (General_Preferences.Label44, Justify_Center);
   Set_Line_Wrap (General_Preferences.Label44, False);
   Attach (General_Preferences.Table6, General_Preferences.Label44, 0, 1, 1, 2,
     Fill, 0,
     0, 0);

   Gtk_New (General_Preferences.Label45, -("List Processes"));
   Set_Alignment (General_Preferences.Label45, 7.45058e-09, 0.5);
   Set_Padding (General_Preferences.Label45, 3, 0);
   Set_Justify (General_Preferences.Label45, Justify_Center);
   Set_Line_Wrap (General_Preferences.Label45, False);
   Attach (General_Preferences.Table6, General_Preferences.Label45, 0, 1, 2, 3,
     Fill, 0,
     0, 0);

   Gtk_New (General_Preferences.Label48, -("Web Browser"));
   Set_Alignment (General_Preferences.Label48, 7.45058e-09, 0.5);
   Set_Padding (General_Preferences.Label48, 3, 0);
   Set_Justify (General_Preferences.Label48, Justify_Center);
   Set_Line_Wrap (General_Preferences.Label48, False);
   Attach (General_Preferences.Table6, General_Preferences.Label48, 0, 1, 3, 4,
     Fill, 0,
     0, 0);

   Gtk_New (General_Preferences.Label12, -("Helpers"));
   Set_Alignment (General_Preferences.Label12, 0.5, 0.5);
   Set_Padding (General_Preferences.Label12, 0, 0);
   Set_Justify (General_Preferences.Label12, Justify_Center);
   Set_Line_Wrap (General_Preferences.Label12, False);
   Set_Tab (General_Preferences.Notebook1, 4, General_Preferences.Label12);

   Gtk_New (General_Preferences.Hbuttonbox6);
   Set_Spacing (General_Preferences.Hbuttonbox6, 30);
   Set_Layout (General_Preferences.Hbuttonbox6, Buttonbox_Spread);
   Set_Child_Size (General_Preferences.Hbuttonbox6, 85, 27);
   Set_Child_Ipadding (General_Preferences.Hbuttonbox6, 7, 0);
   Pack_Start (General_Preferences.Vbox2, General_Preferences.Hbuttonbox6, False, False, 3);

   Gtk_New (General_Preferences.Ok_Button, -"OK");
   Set_Flags (General_Preferences.Ok_Button, Can_Default);
   Button_Callback.Connect
     (General_Preferences.Ok_Button, "clicked",
      Button_Callback.To_Marshaller (On_Ok_Button_Clicked'Access));
   Add (General_Preferences.Hbuttonbox6, General_Preferences.Ok_Button);

   Gtk_New (General_Preferences.Reset_Button, -"Reset");
   Set_Flags (General_Preferences.Reset_Button, Can_Default);
   Button_Callback.Connect
     (General_Preferences.Reset_Button, "clicked",
      Button_Callback.To_Marshaller (On_Reset_Button_Clicked'Access));
   Add (General_Preferences.Hbuttonbox6, General_Preferences.Reset_Button);

   Gtk_New (General_Preferences.Help_Button, -"Help");
   Set_Flags (General_Preferences.Help_Button, Can_Default);
   Button_Callback.Connect
     (General_Preferences.Help_Button, "clicked",
      Button_Callback.To_Marshaller (On_Help_Button_Clicked'Access));
   Add (General_Preferences.Hbuttonbox6, General_Preferences.Help_Button);

end Initialize;

end General_Preferences_Pkg;
