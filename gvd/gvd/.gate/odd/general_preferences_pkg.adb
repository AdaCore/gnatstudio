with Glib; use Glib;
with Gtk; use Gtk;
with Gdk.Types; use Gdk.Types;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Enums;  use Gtk.Enums;
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
   Table1_Group : Widget_SList.GSList;
   Hscale1_Adj : Gtk_Adjustment;
   Hscale2_Adj : Gtk_Adjustment;
   Hscale3_Adj : Gtk_Adjustment;
   Table2_Group : Widget_SList.GSList;
   Hscale4_Adj : Gtk_Adjustment;
   Table4_Group : Widget_SList.GSList;

begin
   Gtk.Window.Initialize (Odd_Preferences, Window_Dialog);
   Window_Callback.Connect
     (Odd_Preferences, "delete_event", On_Odd_Preferences_Delete_Event'Access);
   Set_Title (Odd_Preferences, -"Preferences");
   Set_Policy (Odd_Preferences, False, True, False);
   Set_Position (Odd_Preferences, Win_Pos_Center);
   Set_Modal (Odd_Preferences, False);

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

   Gtk_New (Odd_Preferences.Table1, 9, 4, False);
   Add (Odd_Preferences.Frame1, Odd_Preferences.Table1);
   Set_Row_Spacings (Odd_Preferences.Table1, 0);
   Set_Col_Spacings (Odd_Preferences.Table1, 0);

   Gtk_New (Odd_Preferences.Label14, -("Automatic Display of Variable Values"));
   Attach (Odd_Preferences.Table1, Odd_Preferences.Label14, 0, 1, 1, 2,
     Expand or Fill, Expand,
     0, 0);
   Set_Alignment (Odd_Preferences.Label14, 7.45058e-09, 0.5);
   Set_Padding (Odd_Preferences.Label14, 2, 0);
   Set_Justify (Odd_Preferences.Label14, Justify_Left);
   Set_Line_Wrap (Odd_Preferences.Label14, False);

   Gtk_New (Odd_Preferences.Checkbutton2, -"as Popup Tips");
   Attach (Odd_Preferences.Table1, Odd_Preferences.Checkbutton2, 1, 3, 0, 1,
     Expand, Expand,
     0, 0);
   Set_Active (Odd_Preferences.Checkbutton2, False);

   Gtk_New (Odd_Preferences.Checkbutton3, -"in the Status Line");
   Attach (Odd_Preferences.Table1, Odd_Preferences.Checkbutton3, 3, 4, 0, 1,
     Expand, Expand,
     0, 0);
   Set_Active (Odd_Preferences.Checkbutton3, False);

   Gtk_New (Odd_Preferences.Checkbutton4, -"as Popup Tips");
   Attach (Odd_Preferences.Table1, Odd_Preferences.Checkbutton4, 1, 3, 1, 2,
     Expand, Expand,
     0, 0);
   Set_Active (Odd_Preferences.Checkbutton4, False);

   Gtk_New (Odd_Preferences.Checkbutton5, -"in the Status Line");
   Attach (Odd_Preferences.Table1, Odd_Preferences.Checkbutton5, 3, 4, 1, 2,
     Expand, Expand,
     0, 0);
   Set_Active (Odd_Preferences.Checkbutton5, False);

   Gtk_New (Odd_Preferences.Label15, -("TAB Key Completes"));
   Attach (Odd_Preferences.Table1, Odd_Preferences.Label15, 0, 1, 2, 3,
     Expand or Fill, Expand,
     0, 0);
   Set_Alignment (Odd_Preferences.Label15, 7.45058e-09, 0.5);
   Set_Padding (Odd_Preferences.Label15, 2, 0);
   Set_Justify (Odd_Preferences.Label15, Justify_Left);
   Set_Line_Wrap (Odd_Preferences.Label15, False);

   Gtk_New (Odd_Preferences.Radiobutton1, Table1_Group, -"in All Windows");
   Table1_Group := Group (Odd_Preferences.Radiobutton1);
   Attach (Odd_Preferences.Table1, Odd_Preferences.Radiobutton1, 1, 3, 2, 3,
     Expand, Expand,
     0, 0);
   Set_Active (Odd_Preferences.Radiobutton1, False);

   Gtk_New (Odd_Preferences.Radiobutton2, Table1_Group, -"in Console Only");
   Table1_Group := Group (Odd_Preferences.Radiobutton2);
   Attach (Odd_Preferences.Table1, Odd_Preferences.Radiobutton2, 3, 4, 2, 3,
     Expand, Expand,
     0, 0);
   Set_Active (Odd_Preferences.Radiobutton2, False);

   Gtk_New (Odd_Preferences.Checkbutton6, -"Iconify all Windows at Once");
   Attach (Odd_Preferences.Table1, Odd_Preferences.Checkbutton6, 0, 4, 3, 4,
     Expand or Fill, Expand,
     0, 0);
   Set_Active (Odd_Preferences.Checkbutton6, False);

   Gtk_New (Odd_Preferences.Checkbutton7, -"Uniconify When Ready");
   Attach (Odd_Preferences.Table1, Odd_Preferences.Checkbutton7, 0, 4, 4, 5,
     Expand or Fill, Expand,
     0, 0);
   Set_Active (Odd_Preferences.Checkbutton7, False);

   Gtk_New (Odd_Preferences.Checkbutton8, -"Suppress X Warnings");
   Attach (Odd_Preferences.Table1, Odd_Preferences.Checkbutton8, 0, 4, 5, 6,
     Expand or Fill, Expand,
     0, 0);
   Set_Active (Odd_Preferences.Checkbutton8, False);

   Gtk_New (Odd_Preferences.Checkbutton9, -"Warn if Multiple ODD Instances are Running");
   Attach (Odd_Preferences.Table1, Odd_Preferences.Checkbutton9, 0, 4, 6, 7,
     Expand or Fill, Expand,
     0, 0);
   Set_Active (Odd_Preferences.Checkbutton9, False);

   Gtk_New (Odd_Preferences.Checkbutton10, -"Continue Automatically when Mouse Pointer is Frozen");
   Attach (Odd_Preferences.Table1, Odd_Preferences.Checkbutton10, 0, 4, 7, 8,
     Expand or Fill, Expand,
     0, 0);
   Set_Active (Odd_Preferences.Checkbutton10, False);

   Gtk_New (Odd_Preferences.Button39, -"Clear Undo Buffer");
   Attach (Odd_Preferences.Table1, Odd_Preferences.Button39, 3, 4, 8, 9,
     Expand, Expand,
     0, 0);

   Gtk_New (Odd_Preferences.Label13, -("Automatic Display of Button Hints"));
   Attach (Odd_Preferences.Table1, Odd_Preferences.Label13, 0, 1, 0, 1,
     Expand or Fill, Expand,
     0, 0);
   Set_Alignment (Odd_Preferences.Label13, 7.45058e-09, 0.5);
   Set_Padding (Odd_Preferences.Label13, 2, 0);
   Set_Justify (Odd_Preferences.Label13, Justify_Left);
   Set_Line_Wrap (Odd_Preferences.Label13, False);

   Gtk_New (Odd_Preferences.Label16, -("Undo Buffer Size"));
   Attach (Odd_Preferences.Table1, Odd_Preferences.Label16, 0, 1, 8, 9,
     Expand or Fill, Expand,
     0, 0);
   Set_Alignment (Odd_Preferences.Label16, 7.45058e-09, 0.5);
   Set_Padding (Odd_Preferences.Label16, 2, 0);
   Set_Justify (Odd_Preferences.Label16, Justify_Left);
   Set_Line_Wrap (Odd_Preferences.Label16, False);

   Gtk_New (Odd_Preferences.Label17, -("kBytes"));
   Attach (Odd_Preferences.Table1, Odd_Preferences.Label17, 2, 3, 8, 9,
     Expand or Fill, Expand,
     0, 0);
   Set_Alignment (Odd_Preferences.Label17, 7.45058e-09, 0.5);
   Set_Padding (Odd_Preferences.Label17, 2, 0);
   Set_Justify (Odd_Preferences.Label17, Justify_Center);
   Set_Line_Wrap (Odd_Preferences.Label17, False);

   Gtk_New (Odd_Preferences.Entry2);
   Attach (Odd_Preferences.Table1, Odd_Preferences.Entry2, 1, 2, 8, 9,
     Expand or Fill, Expand,
     0, 0);
   Set_Editable (Odd_Preferences.Entry2, True);
   Set_Max_Length (Odd_Preferences.Entry2, 0);
   Set_Text (Odd_Preferences.Entry2, -"");
   Set_Visibility (Odd_Preferences.Entry2, True);

   Gtk_New (Odd_Preferences.Label7, -("General"));
   Set_Alignment (Odd_Preferences.Label7, 0.5, 0.5);
   Set_Padding (Odd_Preferences.Label7, 0, 0);
   Set_Justify (Odd_Preferences.Label7, Justify_Center);
   Set_Line_Wrap (Odd_Preferences.Label7, False);
   Set_Tab (Odd_Preferences.Notebook1, 0, Odd_Preferences.Label7);

   Gtk_New (Odd_Preferences.Frame2);
   Add (Odd_Preferences.Notebook1, Odd_Preferences.Frame2);
   Set_Shadow_Type (Odd_Preferences.Frame2, Shadow_Etched_In);

   Gtk_New (Odd_Preferences.Table2, 8, 3, True);
   Add (Odd_Preferences.Frame2, Odd_Preferences.Table2);
   Set_Row_Spacings (Odd_Preferences.Table2, 0);
   Set_Col_Spacings (Odd_Preferences.Table2, 0);

   Adjustment.Gtk_New (Hscale1_Adj, 8.0, 0.0, 32.0, 1.0, 0.0, 0.0);
   Gtk_New_Hscale (Odd_Preferences.Hscale1, Hscale1_Adj);
   Attach (Odd_Preferences.Table2, Odd_Preferences.Hscale1, 0, 1, 6, 7,
     Expand or Fill, Expand or Fill,
     0, 0);
   Set_Update_Policy (Odd_Preferences.Hscale1, Update_Continuous);
   Set_Digits (Odd_Preferences.Hscale1, 0);
   Set_Draw_Value (Odd_Preferences.Hscale1, True);
   Set_Value_Pos (Odd_Preferences.Hscale1, Pos_Top);

   Adjustment.Gtk_New (Hscale2_Adj, 0.0, 0.0, 16.0, 1.0, 0.0, 0.0);
   Gtk_New_Hscale (Odd_Preferences.Hscale2, Hscale2_Adj);
   Attach (Odd_Preferences.Table2, Odd_Preferences.Hscale2, 1, 2, 6, 7,
     Expand or Fill, Fill,
     0, 0);
   Set_Update_Policy (Odd_Preferences.Hscale2, Update_Continuous);
   Set_Digits (Odd_Preferences.Hscale2, 0);
   Set_Draw_Value (Odd_Preferences.Hscale2, True);
   Set_Value_Pos (Odd_Preferences.Hscale2, Pos_Top);

   Adjustment.Gtk_New (Hscale3_Adj, 4.0, 0.0, 16.0, 1.0, 0.0, 0.0);
   Gtk_New_Hscale (Odd_Preferences.Hscale3, Hscale3_Adj);
   Attach (Odd_Preferences.Table2, Odd_Preferences.Hscale3, 2, 3, 6, 7,
     Expand or Fill, Fill,
     0, 0);
   Set_Update_Policy (Odd_Preferences.Hscale3, Update_Continuous);
   Set_Digits (Odd_Preferences.Hscale3, 0);
   Set_Draw_Value (Odd_Preferences.Hscale3, True);
   Set_Value_Pos (Odd_Preferences.Hscale3, Pos_Top);

   Gtk_New (Odd_Preferences.Label22, -("Tool buttons location"));
   Attach (Odd_Preferences.Table2, Odd_Preferences.Label22, 0, 1, 1, 2,
     Fill, 0,
     0, 0);
   Set_Alignment (Odd_Preferences.Label22, 7.45058e-09, 0.5);
   Set_Padding (Odd_Preferences.Label22, 2, 0);
   Set_Justify (Odd_Preferences.Label22, Justify_Left);
   Set_Line_Wrap (Odd_Preferences.Label22, False);

   Gtk_New (Odd_Preferences.Label21, -("Show Position and Breakpoints"));
   Attach (Odd_Preferences.Table2, Odd_Preferences.Label21, 0, 1, 0, 1,
     Fill, 0,
     0, 0);
   Set_Alignment (Odd_Preferences.Label21, 7.45058e-09, 0.5);
   Set_Padding (Odd_Preferences.Label21, 2, 0);
   Set_Justify (Odd_Preferences.Label21, Justify_Left);
   Set_Line_Wrap (Odd_Preferences.Label21, False);

   Gtk_New (Odd_Preferences.Radiobutton4, Table2_Group, -"as Text Characters");
   Table2_Group := Group (Odd_Preferences.Radiobutton4);
   Attach (Odd_Preferences.Table2, Odd_Preferences.Radiobutton4, 2, 3, 0, 1,
     Fill, 0,
     0, 0);
   Set_Active (Odd_Preferences.Radiobutton4, False);

   Gtk_New (Odd_Preferences.Radiobutton6, Table2_Group, -"Source Window");
   Table2_Group := Group (Odd_Preferences.Radiobutton6);
   Attach (Odd_Preferences.Table2, Odd_Preferences.Radiobutton6, 2, 3, 1, 2,
     Fill, 0,
     0, 0);
   Set_Active (Odd_Preferences.Radiobutton6, False);

   Gtk_New (Odd_Preferences.Radiobutton3, Table2_Group, -"as Glyphs");
   Table2_Group := Group (Odd_Preferences.Radiobutton3);
   Attach (Odd_Preferences.Table2, Odd_Preferences.Radiobutton3, 1, 2, 0, 1,
     Fill, 0,
     2, 0);
   Set_Active (Odd_Preferences.Radiobutton3, False);

   Gtk_New (Odd_Preferences.Radiobutton5, Table2_Group, -"Command Tool");
   Table2_Group := Group (Odd_Preferences.Radiobutton5);
   Attach (Odd_Preferences.Table2, Odd_Preferences.Radiobutton5, 1, 2, 1, 2,
     Fill, 0,
     2, 0);
   Set_Active (Odd_Preferences.Radiobutton5, False);

   Gtk_New (Odd_Preferences.Label18, -("Tab Width"));
   Attach (Odd_Preferences.Table2, Odd_Preferences.Label18, 0, 1, 7, 8,
     Fill, 0,
     0, 0);
   Set_Alignment (Odd_Preferences.Label18, 0.5, 0.5);
   Set_Padding (Odd_Preferences.Label18, 0, 0);
   Set_Justify (Odd_Preferences.Label18, Justify_Left);
   Set_Line_Wrap (Odd_Preferences.Label18, False);

   Gtk_New (Odd_Preferences.Label19, -("Source Indentation"));
   Attach (Odd_Preferences.Table2, Odd_Preferences.Label19, 1, 2, 7, 8,
     Fill, 0,
     0, 0);
   Set_Alignment (Odd_Preferences.Label19, 0.5, 0.5);
   Set_Padding (Odd_Preferences.Label19, 0, 0);
   Set_Justify (Odd_Preferences.Label19, Justify_Left);
   Set_Line_Wrap (Odd_Preferences.Label19, False);

   Gtk_New (Odd_Preferences.Label20, -("Machine Code Indentation"));
   Attach (Odd_Preferences.Table2, Odd_Preferences.Label20, 2, 3, 7, 8,
     Fill, 0,
     0, 0);
   Set_Alignment (Odd_Preferences.Label20, 0.5, 0.5);
   Set_Padding (Odd_Preferences.Label20, 0, 0);
   Set_Justify (Odd_Preferences.Label20, Justify_Left);
   Set_Line_Wrap (Odd_Preferences.Label20, False);

   Gtk_New (Odd_Preferences.Radiobutton8, Table2_Group, -"by Base Name");
   Table2_Group := Group (Odd_Preferences.Radiobutton8);
   Set_Sensitive (Odd_Preferences.Radiobutton8, False);
   Attach (Odd_Preferences.Table2, Odd_Preferences.Radiobutton8, 2, 3, 2, 3,
     Fill, 0,
     0, 0);
   Set_Active (Odd_Preferences.Radiobutton8, False);

   Gtk_New (Odd_Preferences.Radiobutton7, Table2_Group, -"by Path Name");
   Table2_Group := Group (Odd_Preferences.Radiobutton7);
   Set_Sensitive (Odd_Preferences.Radiobutton7, False);
   Attach (Odd_Preferences.Table2, Odd_Preferences.Radiobutton7, 1, 2, 2, 3,
     Fill, 0,
     2, 0);
   Set_Active (Odd_Preferences.Radiobutton7, False);

   Gtk_New (Odd_Preferences.Label23, -("Refer to Program Sources"));
   Set_Sensitive (Odd_Preferences.Label23, False);
   Attach (Odd_Preferences.Table2, Odd_Preferences.Label23, 0, 1, 2, 3,
     Fill, 0,
     0, 0);
   Set_Alignment (Odd_Preferences.Label23, 7.45058e-09, 0.5);
   Set_Padding (Odd_Preferences.Label23, 2, 0);
   Set_Justify (Odd_Preferences.Label23, Justify_Left);
   Set_Line_Wrap (Odd_Preferences.Label23, False);

   Gtk_New (Odd_Preferences.Label24, -("Find"));
   Attach (Odd_Preferences.Table2, Odd_Preferences.Label24, 0, 1, 3, 4,
     Fill, 0,
     0, 0);
   Set_Alignment (Odd_Preferences.Label24, 7.45058e-09, 0.5);
   Set_Padding (Odd_Preferences.Label24, 2, 0);
   Set_Justify (Odd_Preferences.Label24, Justify_Left);
   Set_Line_Wrap (Odd_Preferences.Label24, False);

   Gtk_New (Odd_Preferences.Label25, -("Cache"));
   Attach (Odd_Preferences.Table2, Odd_Preferences.Label25, 0, 1, 4, 5,
     Fill, 0,
     0, 0);
   Set_Alignment (Odd_Preferences.Label25, 7.45058e-09, 0.5);
   Set_Padding (Odd_Preferences.Label25, 2, 0);
   Set_Justify (Odd_Preferences.Label25, Justify_Left);
   Set_Line_Wrap (Odd_Preferences.Label25, False);

   Gtk_New (Odd_Preferences.Checkbutton11, -"Display Source Line Numbers");
   Attach (Odd_Preferences.Table2, Odd_Preferences.Checkbutton11, 0, 3, 5, 6,
     Fill, 0,
     0, 0);
   Set_Active (Odd_Preferences.Checkbutton11, False);

   Gtk_New (Odd_Preferences.Checkbutton13, -"Case Sensitive");
   Attach (Odd_Preferences.Table2, Odd_Preferences.Checkbutton13, 2, 3, 3, 4,
     Fill, 0,
     0, 0);
   Set_Active (Odd_Preferences.Checkbutton13, False);

   Gtk_New (Odd_Preferences.Checkbutton12, -"Words Only");
   Attach (Odd_Preferences.Table2, Odd_Preferences.Checkbutton12, 1, 2, 3, 4,
     Fill, 0,
     2, 0);
   Set_Active (Odd_Preferences.Checkbutton12, False);

   Gtk_New (Odd_Preferences.Checkbutton14, -"Source Files");
   Attach (Odd_Preferences.Table2, Odd_Preferences.Checkbutton14, 1, 2, 4, 5,
     Fill, 0,
     2, 0);
   Set_Active (Odd_Preferences.Checkbutton14, False);

   Gtk_New (Odd_Preferences.Checkbutton15, -"Machine Code");
   Attach (Odd_Preferences.Table2, Odd_Preferences.Checkbutton15, 2, 3, 4, 5,
     Fill, 0,
     0, 0);
   Set_Active (Odd_Preferences.Checkbutton15, False);

   Gtk_New (Odd_Preferences.Label8, -("Source"));
   Set_Alignment (Odd_Preferences.Label8, 0.5, 0.5);
   Set_Padding (Odd_Preferences.Label8, 0, 0);
   Set_Justify (Odd_Preferences.Label8, Justify_Center);
   Set_Line_Wrap (Odd_Preferences.Label8, False);
   Set_Tab (Odd_Preferences.Notebook1, 1, Odd_Preferences.Label8);

   Gtk_New (Odd_Preferences.Frame3);
   Add (Odd_Preferences.Notebook1, Odd_Preferences.Frame3);
   Set_Shadow_Type (Odd_Preferences.Frame3, Shadow_Etched_In);

   Gtk_New (Odd_Preferences.Table3, 9, 4, False);
   Add (Odd_Preferences.Frame3, Odd_Preferences.Table3);
   Set_Row_Spacings (Odd_Preferences.Table3, 0);
   Set_Col_Spacings (Odd_Preferences.Table3, 0);

   Gtk_New (Odd_Preferences.Checkbutton20, -"Automatic");
   Attach (Odd_Preferences.Table3, Odd_Preferences.Checkbutton20, 2, 4, 1, 2,
     Expand or Fill, Expand,
     0, 0);
   Set_Active (Odd_Preferences.Checkbutton20, False);

   Gtk_New (Odd_Preferences.Checkbutton19, -"Compact");
   Attach (Odd_Preferences.Table3, Odd_Preferences.Checkbutton19, 1, 2, 1, 2,
     Expand or Fill, Expand,
     0, 0);
   Set_Active (Odd_Preferences.Checkbutton19, False);

   Gtk_New (Odd_Preferences.Checkbutton16, -"Edge Hints");
   Attach (Odd_Preferences.Table3, Odd_Preferences.Checkbutton16, 1, 2, 0, 1,
     Expand or Fill, Expand,
     0, 0);
   Set_Active (Odd_Preferences.Checkbutton16, False);

   Gtk_New (Odd_Preferences.Checkbutton17, -"Edge Annotations");
   Attach (Odd_Preferences.Table3, Odd_Preferences.Checkbutton17, 2, 3, 0, 1,
     Expand or Fill, Expand,
     0, 0);
   Set_Active (Odd_Preferences.Checkbutton17, False);

   Gtk_New (Odd_Preferences.Checkbutton18, -"Titles of Dependent Displays");
   Attach (Odd_Preferences.Table3, Odd_Preferences.Checkbutton18, 3, 4, 0, 1,
     Expand or Fill, Expand,
     0, 0);
   Set_Active (Odd_Preferences.Checkbutton18, False);

   Gtk_New (Odd_Preferences.Checkbutton21, -"Detect Aliases (shared data structures)");
   Attach (Odd_Preferences.Table3, Odd_Preferences.Checkbutton21, 0, 4, 2, 3,
     Expand or Fill, Expand,
     0, 0);
   Set_Active (Odd_Preferences.Checkbutton21, False);

   Gtk_New (Odd_Preferences.Checkbutton22, -"Cluster Data Displays");
   Attach (Odd_Preferences.Table3, Odd_Preferences.Checkbutton22, 0, 4, 3, 4,
     Expand or Fill, Expand,
     0, 0);
   Set_Active (Odd_Preferences.Checkbutton22, False);

   Gtk_New (Odd_Preferences.Label26, -("Show"));
   Attach (Odd_Preferences.Table3, Odd_Preferences.Label26, 0, 1, 0, 1,
     Expand or Fill, Expand,
     0, 0);
   Set_Alignment (Odd_Preferences.Label26, 7.45058e-09, 0.5);
   Set_Padding (Odd_Preferences.Label26, 2, 0);
   Set_Justify (Odd_Preferences.Label26, Justify_Left);
   Set_Line_Wrap (Odd_Preferences.Label26, False);

   Gtk_New (Odd_Preferences.Label27, -("Layout"));
   Attach (Odd_Preferences.Table3, Odd_Preferences.Label27, 0, 1, 1, 2,
     Expand or Fill, Expand,
     0, 0);
   Set_Alignment (Odd_Preferences.Label27, 7.45058e-09, 0.5);
   Set_Padding (Odd_Preferences.Label27, 2, 0);
   Set_Justify (Odd_Preferences.Label27, Justify_Left);
   Set_Line_Wrap (Odd_Preferences.Label27, False);

   Gtk_New (Odd_Preferences.Checkbutton23, -"Display Two-Dimensional Arrays as Tables");
   Attach (Odd_Preferences.Table3, Odd_Preferences.Checkbutton23, 0, 4, 4, 5,
     Expand or Fill, Expand,
     0, 0);
   Set_Active (Odd_Preferences.Checkbutton23, False);

   Gtk_New (Odd_Preferences.Checkbutton24, -"Close Data Window when Deleting last Display");
   Attach (Odd_Preferences.Table3, Odd_Preferences.Checkbutton24, 0, 4, 5, 6,
     Expand or Fill, Expand,
     0, 0);
   Set_Active (Odd_Preferences.Checkbutton24, False);

   Gtk_New (Odd_Preferences.Checkbutton25, -"Auto-Align Displays on Nearest Grid Point");
   Attach (Odd_Preferences.Table3, Odd_Preferences.Checkbutton25, 0, 4, 6, 7,
     Expand or Fill, Expand,
     0, 0);
   Set_Active (Odd_Preferences.Checkbutton25, False);

   Adjustment.Gtk_New (Hscale4_Adj, 16.0, 0.0, 64.0, 1.0, 0.0, 0.0);
   Gtk_New_Hscale (Odd_Preferences.Hscale4, Hscale4_Adj);
   Attach (Odd_Preferences.Table3, Odd_Preferences.Hscale4, 0, 4, 7, 8,
     Fill, Expand or Fill,
     0, 0);
   Set_Update_Policy (Odd_Preferences.Hscale4, Update_Continuous);
   Set_Digits (Odd_Preferences.Hscale4, 0);
   Set_Draw_Value (Odd_Preferences.Hscale4, True);
   Set_Value_Pos (Odd_Preferences.Hscale4, Pos_Top);

   Gtk_New (Odd_Preferences.Label28, -("Grid Size"));
   Attach (Odd_Preferences.Table3, Odd_Preferences.Label28, 0, 4, 8, 9,
     Expand or Fill, Expand,
     0, 0);
   Set_Alignment (Odd_Preferences.Label28, 0.0, 0.5);
   Set_Padding (Odd_Preferences.Label28, 2, 0);
   Set_Justify (Odd_Preferences.Label28, Justify_Left);
   Set_Line_Wrap (Odd_Preferences.Label28, False);

   Gtk_New (Odd_Preferences.Label9, -("Data"));
   Set_Alignment (Odd_Preferences.Label9, 0.5, 0.5);
   Set_Padding (Odd_Preferences.Label9, 0, 0);
   Set_Justify (Odd_Preferences.Label9, Justify_Center);
   Set_Line_Wrap (Odd_Preferences.Label9, False);
   Set_Tab (Odd_Preferences.Notebook1, 2, Odd_Preferences.Label9);

   Gtk_New (Odd_Preferences.Frame4);
   Add (Odd_Preferences.Notebook1, Odd_Preferences.Frame4);
   Set_Shadow_Type (Odd_Preferences.Frame4, Shadow_Etched_In);

   Gtk_New (Odd_Preferences.Table4, 9, 7, False);
   Add (Odd_Preferences.Frame4, Odd_Preferences.Table4);
   Set_Row_Spacings (Odd_Preferences.Table4, 0);
   Set_Col_Spacings (Odd_Preferences.Table4, 0);

   Gtk_New (Odd_Preferences.Label36, -("Startup Windows"));
   Attach (Odd_Preferences.Table4, Odd_Preferences.Label36, 0, 1, 8, 9,
     Expand or Fill, Expand,
     0, 0);
   Set_Alignment (Odd_Preferences.Label36, 7.45058e-09, 0.5);
   Set_Padding (Odd_Preferences.Label36, 0, 0);
   Set_Justify (Odd_Preferences.Label36, Justify_Center);
   Set_Line_Wrap (Odd_Preferences.Label36, False);

   Gtk_New (Odd_Preferences.Label34, -("Data Scrolling"));
   Attach (Odd_Preferences.Table4, Odd_Preferences.Label34, 0, 1, 5, 6,
     Expand or Fill, Expand,
     0, 0);
   Set_Alignment (Odd_Preferences.Label34, 7.45058e-09, 0.5);
   Set_Padding (Odd_Preferences.Label34, 0, 0);
   Set_Justify (Odd_Preferences.Label34, Justify_Center);
   Set_Line_Wrap (Odd_Preferences.Label34, False);

   Gtk_New (Odd_Preferences.Label33, -("Keyboard Focus"));
   Attach (Odd_Preferences.Table4, Odd_Preferences.Label33, 0, 1, 4, 5,
     Expand or Fill, Expand,
     0, 0);
   Set_Alignment (Odd_Preferences.Label33, 7.45058e-09, 0.5);
   Set_Padding (Odd_Preferences.Label33, 0, 0);
   Set_Justify (Odd_Preferences.Label33, Justify_Center);
   Set_Line_Wrap (Odd_Preferences.Label33, False);

   Gtk_New (Odd_Preferences.Label32, -("Tool Bar Appearance"));
   Attach (Odd_Preferences.Table4, Odd_Preferences.Label32, 0, 1, 3, 4,
     Expand or Fill, Expand,
     0, 0);
   Set_Alignment (Odd_Preferences.Label32, 7.45058e-09, 0.5);
   Set_Padding (Odd_Preferences.Label32, 0, 0);
   Set_Justify (Odd_Preferences.Label32, Justify_Center);
   Set_Line_Wrap (Odd_Preferences.Label32, False);

   Gtk_New (Odd_Preferences.Label31, -("Ctrl+A is"));
   Attach (Odd_Preferences.Table4, Odd_Preferences.Label31, 0, 1, 2, 3,
     Expand or Fill, Expand,
     0, 0);
   Set_Alignment (Odd_Preferences.Label31, 7.45058e-09, 0.5);
   Set_Padding (Odd_Preferences.Label31, 0, 0);
   Set_Justify (Odd_Preferences.Label31, Justify_Center);
   Set_Line_Wrap (Odd_Preferences.Label31, False);

   Gtk_New (Odd_Preferences.Label30, -("Ctrl+C is"));
   Attach (Odd_Preferences.Table4, Odd_Preferences.Label30, 0, 1, 1, 2,
     Expand or Fill, Expand,
     0, 0);
   Set_Alignment (Odd_Preferences.Label30, 7.45058e-09, 0.5);
   Set_Padding (Odd_Preferences.Label30, 0, 0);
   Set_Justify (Odd_Preferences.Label30, Justify_Center);
   Set_Line_Wrap (Odd_Preferences.Label30, False);

   Gtk_New (Odd_Preferences.Label29, -("Window Layout"));
   Attach (Odd_Preferences.Table4, Odd_Preferences.Label29, 0, 1, 0, 1,
     Expand or Fill, Expand,
     0, 0);
   Set_Alignment (Odd_Preferences.Label29, 0.0, 0.5);
   Set_Padding (Odd_Preferences.Label29, 0, 0);
   Set_Justify (Odd_Preferences.Label29, Justify_Center);
   Set_Line_Wrap (Odd_Preferences.Label29, False);

   Gtk_New (Odd_Preferences.Label35, -("Debugger Type"));
   Attach (Odd_Preferences.Table4, Odd_Preferences.Label35, 0, 1, 6, 8,
     Expand or Fill, Expand,
     0, 0);
   Set_Alignment (Odd_Preferences.Label35, 7.45058e-09, 0.5);
   Set_Padding (Odd_Preferences.Label35, 0, 0);
   Set_Justify (Odd_Preferences.Label35, Justify_Center);
   Set_Line_Wrap (Odd_Preferences.Label35, False);

   Gtk_New (Odd_Preferences.Checkbutton26, -"Determine Automatically from Arguments");
   Attach (Odd_Preferences.Table4, Odd_Preferences.Checkbutton26, 1, 7, 6, 7,
     Expand or Fill, Expand,
     0, 0);
   Set_Active (Odd_Preferences.Checkbutton26, False);

   Gtk_New (Odd_Preferences.Radiobutton9, Table4_Group, -"GDB");
   Table4_Group := Group (Odd_Preferences.Radiobutton9);
   Attach (Odd_Preferences.Table4, Odd_Preferences.Radiobutton9, 1, 2, 7, 8,
     Expand or Fill, Expand,
     0, 0);
   Set_Active (Odd_Preferences.Radiobutton9, False);

   Gtk_New (Odd_Preferences.Radiobutton10, Table4_Group, -"DBX");
   Table4_Group := Group (Odd_Preferences.Radiobutton10);
   Set_Sensitive (Odd_Preferences.Radiobutton10, False);
   Attach (Odd_Preferences.Table4, Odd_Preferences.Radiobutton10, 2, 3, 7, 8,
     Expand or Fill, Expand,
     0, 0);
   Set_Active (Odd_Preferences.Radiobutton10, False);

   Gtk_New (Odd_Preferences.Radiobutton11, Table4_Group, -"XDB");
   Table4_Group := Group (Odd_Preferences.Radiobutton11);
   Set_Sensitive (Odd_Preferences.Radiobutton11, False);
   Attach (Odd_Preferences.Table4, Odd_Preferences.Radiobutton11, 3, 4, 7, 8,
     Expand or Fill, Expand,
     0, 0);
   Set_Active (Odd_Preferences.Radiobutton11, False);

   Gtk_New (Odd_Preferences.Radiobutton12, Table4_Group, -"JDB");
   Table4_Group := Group (Odd_Preferences.Radiobutton12);
   Set_Sensitive (Odd_Preferences.Radiobutton12, False);
   Attach (Odd_Preferences.Table4, Odd_Preferences.Radiobutton12, 4, 5, 7, 8,
     Expand or Fill, Expand,
     0, 0);
   Set_Active (Odd_Preferences.Radiobutton12, False);

   Gtk_New (Odd_Preferences.Radiobutton13, Table4_Group, -"PYDB");
   Table4_Group := Group (Odd_Preferences.Radiobutton13);
   Set_Sensitive (Odd_Preferences.Radiobutton13, False);
   Attach (Odd_Preferences.Table4, Odd_Preferences.Radiobutton13, 5, 6, 7, 8,
     Expand or Fill, Expand,
     0, 0);
   Set_Active (Odd_Preferences.Radiobutton13, False);

   Gtk_New (Odd_Preferences.Radiobutton14, Table4_Group, -"Perl");
   Table4_Group := Group (Odd_Preferences.Radiobutton14);
   Set_Sensitive (Odd_Preferences.Radiobutton14, False);
   Attach (Odd_Preferences.Table4, Odd_Preferences.Radiobutton14, 6, 7, 7, 8,
     Expand or Fill, Expand,
     0, 0);
   Set_Active (Odd_Preferences.Radiobutton14, False);

   Gtk_New (Odd_Preferences.Checkbutton27, -"Images");
   Attach (Odd_Preferences.Table4, Odd_Preferences.Checkbutton27, 1, 2, 3, 4,
     Expand or Fill, Expand,
     0, 0);
   Set_Active (Odd_Preferences.Checkbutton27, False);

   Gtk_New (Odd_Preferences.Checkbutton28, -"Captions");
   Attach (Odd_Preferences.Table4, Odd_Preferences.Checkbutton28, 2, 3, 3, 4,
     Expand or Fill, Expand,
     0, 0);
   Set_Active (Odd_Preferences.Checkbutton28, False);

   Gtk_New (Odd_Preferences.Checkbutton29, -"Flat");
   Attach (Odd_Preferences.Table4, Odd_Preferences.Checkbutton29, 3, 4, 3, 4,
     Expand or Fill, Expand,
     0, 0);
   Set_Active (Odd_Preferences.Checkbutton29, False);

   Gtk_New (Odd_Preferences.Checkbutton30, -"Color");
   Attach (Odd_Preferences.Table4, Odd_Preferences.Checkbutton30, 4, 5, 3, 4,
     Expand or Fill, Expand,
     0, 0);
   Set_Active (Odd_Preferences.Checkbutton30, False);

   Gtk_New (Odd_Preferences.Checkbutton31, -"Bottom");
   Set_Sensitive (Odd_Preferences.Checkbutton31, False);
   Attach (Odd_Preferences.Table4, Odd_Preferences.Checkbutton31, 5, 7, 3, 4,
     Expand or Fill, Expand,
     0, 0);
   Set_Active (Odd_Preferences.Checkbutton31, False);

   Gtk_New (Odd_Preferences.Checkbutton32, -"ODD Splash Screen");
   Attach (Odd_Preferences.Table4, Odd_Preferences.Checkbutton32, 1, 3, 8, 9,
     Expand or Fill, Expand,
     0, 0);
   Set_Active (Odd_Preferences.Checkbutton32, False);

   Gtk_New (Odd_Preferences.Checkbutton33, -"Tip of the Day");
   Attach (Odd_Preferences.Table4, Odd_Preferences.Checkbutton33, 3, 7, 8, 9,
     Expand or Fill, Expand,
     0, 0);
   Set_Active (Odd_Preferences.Checkbutton33, False);

   Gtk_New (Odd_Preferences.Radiobutton15, Table4_Group, -"Panner");
   Table4_Group := Group (Odd_Preferences.Radiobutton15);
   Attach (Odd_Preferences.Table4, Odd_Preferences.Radiobutton15, 1, 3, 5, 6,
     Expand or Fill, Expand,
     0, 0);
   Set_Active (Odd_Preferences.Radiobutton15, False);

   Gtk_New (Odd_Preferences.Radiobutton17, Table4_Group, -"Point to Type");
   Table4_Group := Group (Odd_Preferences.Radiobutton17);
   Attach (Odd_Preferences.Table4, Odd_Preferences.Radiobutton17, 1, 3, 4, 5,
     Expand or Fill, Expand,
     0, 0);
   Set_Active (Odd_Preferences.Radiobutton17, False);

   Gtk_New (Odd_Preferences.Radiobutton18, Table4_Group, -"Click to Type");
   Table4_Group := Group (Odd_Preferences.Radiobutton18);
   Attach (Odd_Preferences.Table4, Odd_Preferences.Radiobutton18, 3, 7, 4, 5,
     Expand or Fill, Expand,
     0, 0);
   Set_Active (Odd_Preferences.Radiobutton18, False);

   Gtk_New (Odd_Preferences.Radiobutton16, Table4_Group, -"Scrollbars");
   Table4_Group := Group (Odd_Preferences.Radiobutton16);
   Attach (Odd_Preferences.Table4, Odd_Preferences.Radiobutton16, 3, 7, 5, 6,
     Expand or Fill, Expand,
     0, 0);
   Set_Active (Odd_Preferences.Radiobutton16, False);

   Gtk_New (Odd_Preferences.Radiobutton19, Table4_Group, -"Select All");
   Table4_Group := Group (Odd_Preferences.Radiobutton19);
   Attach (Odd_Preferences.Table4, Odd_Preferences.Radiobutton19, 1, 3, 2, 3,
     Expand or Fill, Expand,
     0, 0);
   Set_Active (Odd_Preferences.Radiobutton19, False);

   Gtk_New (Odd_Preferences.Radiobutton20, Table4_Group, -"Beginning of Line");
   Table4_Group := Group (Odd_Preferences.Radiobutton20);
   Attach (Odd_Preferences.Table4, Odd_Preferences.Radiobutton20, 3, 7, 2, 3,
     Expand or Fill, Expand,
     0, 0);
   Set_Active (Odd_Preferences.Radiobutton20, False);

   Gtk_New (Odd_Preferences.Radiobutton21, Table4_Group, -"Copy");
   Table4_Group := Group (Odd_Preferences.Radiobutton21);
   Attach (Odd_Preferences.Table4, Odd_Preferences.Radiobutton21, 1, 3, 1, 2,
     Expand or Fill, Expand,
     0, 0);
   Set_Active (Odd_Preferences.Radiobutton21, False);

   Gtk_New (Odd_Preferences.Radiobutton22, Table4_Group, -"Interrupt");
   Table4_Group := Group (Odd_Preferences.Radiobutton22);
   Attach (Odd_Preferences.Table4, Odd_Preferences.Radiobutton22, 3, 7, 1, 2,
     Expand or Fill, Expand,
     0, 0);
   Set_Active (Odd_Preferences.Radiobutton22, False);

   Gtk_New (Odd_Preferences.Radiobutton23, Table4_Group, -"Stacked Windows");
   Table4_Group := Group (Odd_Preferences.Radiobutton23);
   Attach (Odd_Preferences.Table4, Odd_Preferences.Radiobutton23, 1, 3, 0, 1,
     Expand or Fill, Expand,
     0, 0);
   Set_Active (Odd_Preferences.Radiobutton23, False);

   Gtk_New (Odd_Preferences.Radiobutton24, Table4_Group, -"Separate Windows");
   Table4_Group := Group (Odd_Preferences.Radiobutton24);
   Attach (Odd_Preferences.Table4, Odd_Preferences.Radiobutton24, 3, 7, 0, 1,
     Expand or Fill, Expand,
     0, 0);
   Set_Active (Odd_Preferences.Radiobutton24, False);

   Gtk_New (Odd_Preferences.Label10, -("Startup"));
   Set_Alignment (Odd_Preferences.Label10, 0.5, 0.5);
   Set_Padding (Odd_Preferences.Label10, 0, 0);
   Set_Justify (Odd_Preferences.Label10, Justify_Center);
   Set_Line_Wrap (Odd_Preferences.Label10, False);
   Set_Tab (Odd_Preferences.Notebook1, 3, Odd_Preferences.Label10);

   Gtk_New (Odd_Preferences.Frame5);
   Add (Odd_Preferences.Notebook1, Odd_Preferences.Frame5);
   Set_Shadow_Type (Odd_Preferences.Frame5, Shadow_Etched_In);

   Gtk_New (Odd_Preferences.Table5, 3, 5, False);
   Add (Odd_Preferences.Frame5, Odd_Preferences.Table5);
   Set_Row_Spacings (Odd_Preferences.Table5, 0);
   Set_Col_Spacings (Odd_Preferences.Table5, 0);

   Gtk_New (Odd_Preferences.Entry3);
   Attach (Odd_Preferences.Table5, Odd_Preferences.Entry3, 1, 2, 0, 1,
     Expand or Fill, 0,
     0, 0);
   Set_Editable (Odd_Preferences.Entry3, True);
   Set_Max_Length (Odd_Preferences.Entry3, 0);
   Set_Text (Odd_Preferences.Entry3, -"");
   Set_Visibility (Odd_Preferences.Entry3, True);

   Gtk_New (Odd_Preferences.Label38, -("Size"));
   Attach (Odd_Preferences.Table5, Odd_Preferences.Label38, 2, 3, 0, 1,
     0, 0,
     0, 0);
   Set_Alignment (Odd_Preferences.Label38, 0.5, 0.5);
   Set_Padding (Odd_Preferences.Label38, 3, 0);
   Set_Justify (Odd_Preferences.Label38, Justify_Center);
   Set_Line_Wrap (Odd_Preferences.Label38, False);

   Gtk_New (Odd_Preferences.Entry5);
   Attach (Odd_Preferences.Table5, Odd_Preferences.Entry5, 1, 2, 1, 2,
     Expand or Fill, 0,
     0, 0);
   Set_Editable (Odd_Preferences.Entry5, True);
   Set_Max_Length (Odd_Preferences.Entry5, 0);
   Set_Text (Odd_Preferences.Entry5, -"");
   Set_Visibility (Odd_Preferences.Entry5, True);

   Gtk_New (Odd_Preferences.Label40, -("Size"));
   Attach (Odd_Preferences.Table5, Odd_Preferences.Label40, 2, 3, 1, 2,
     0, 0,
     0, 0);
   Set_Alignment (Odd_Preferences.Label40, 0.5, 0.5);
   Set_Padding (Odd_Preferences.Label40, 3, 0);
   Set_Justify (Odd_Preferences.Label40, Justify_Center);
   Set_Line_Wrap (Odd_Preferences.Label40, False);

   Gtk_New (Odd_Preferences.Entry6);
   Attach (Odd_Preferences.Table5, Odd_Preferences.Entry6, 3, 4, 1, 2,
     Expand or Fill, 0,
     0, 0);
   Set_Editable (Odd_Preferences.Entry6, True);
   Set_Max_Length (Odd_Preferences.Entry6, 0);
   Set_Text (Odd_Preferences.Entry6, -"");
   Set_Visibility (Odd_Preferences.Entry6, True);

   Gtk_New (Odd_Preferences.Entry7);
   Attach (Odd_Preferences.Table5, Odd_Preferences.Entry7, 1, 2, 2, 3,
     Expand or Fill, 0,
     0, 0);
   Set_Editable (Odd_Preferences.Entry7, True);
   Set_Max_Length (Odd_Preferences.Entry7, 0);
   Set_Text (Odd_Preferences.Entry7, -"");
   Set_Visibility (Odd_Preferences.Entry7, True);

   Gtk_New (Odd_Preferences.Label42, -("Size"));
   Attach (Odd_Preferences.Table5, Odd_Preferences.Label42, 2, 3, 2, 3,
     0, 0,
     0, 0);
   Set_Alignment (Odd_Preferences.Label42, 0.5, 0.5);
   Set_Padding (Odd_Preferences.Label42, 3, 0);
   Set_Justify (Odd_Preferences.Label42, Justify_Center);
   Set_Line_Wrap (Odd_Preferences.Label42, False);

   Gtk_New (Odd_Preferences.Entry8);
   Attach (Odd_Preferences.Table5, Odd_Preferences.Entry8, 3, 4, 2, 3,
     Expand or Fill, 0,
     0, 0);
   Set_Editable (Odd_Preferences.Entry8, True);
   Set_Max_Length (Odd_Preferences.Entry8, 0);
   Set_Text (Odd_Preferences.Entry8, -"");
   Set_Visibility (Odd_Preferences.Entry8, True);

   Gtk_New (Odd_Preferences.Label37, -("Default Font"));
   Attach (Odd_Preferences.Table5, Odd_Preferences.Label37, 0, 1, 0, 1,
     Fill, 0,
     0, 0);
   Set_Alignment (Odd_Preferences.Label37, 7.45058e-09, 0.5);
   Set_Padding (Odd_Preferences.Label37, 3, 0);
   Set_Justify (Odd_Preferences.Label37, Justify_Center);
   Set_Line_Wrap (Odd_Preferences.Label37, False);

   Gtk_New (Odd_Preferences.Label39, -("Variable Width"));
   Attach (Odd_Preferences.Table5, Odd_Preferences.Label39, 0, 1, 1, 2,
     Fill, 0,
     0, 0);
   Set_Alignment (Odd_Preferences.Label39, 7.45058e-09, 0.5);
   Set_Padding (Odd_Preferences.Label39, 3, 0);
   Set_Justify (Odd_Preferences.Label39, Justify_Center);
   Set_Line_Wrap (Odd_Preferences.Label39, False);

   Gtk_New (Odd_Preferences.Label41, -("Fixed Width"));
   Attach (Odd_Preferences.Table5, Odd_Preferences.Label41, 0, 1, 2, 3,
     Fill, 0,
     0, 0);
   Set_Alignment (Odd_Preferences.Label41, 7.45058e-09, 0.5);
   Set_Padding (Odd_Preferences.Label41, 3, 0);
   Set_Justify (Odd_Preferences.Label41, Justify_Center);
   Set_Line_Wrap (Odd_Preferences.Label41, False);

   Gtk_New (Odd_Preferences.Button46, -"Browse...");
   Attach (Odd_Preferences.Table5, Odd_Preferences.Button46, 4, 5, 0, 1,
     0, 0,
     3, 0);

   Gtk_New (Odd_Preferences.Button47, -"Browse...");
   Attach (Odd_Preferences.Table5, Odd_Preferences.Button47, 4, 5, 1, 2,
     0, 0,
     3, 0);

   Gtk_New (Odd_Preferences.Button48, -"Browse...");
   Attach (Odd_Preferences.Table5, Odd_Preferences.Button48, 4, 5, 2, 3,
     0, 0,
     3, 0);

   Gtk_New (Odd_Preferences.Entry4);
   Attach (Odd_Preferences.Table5, Odd_Preferences.Entry4, 3, 4, 0, 1,
     Expand or Fill, 0,
     0, 0);
   Set_Editable (Odd_Preferences.Entry4, True);
   Set_Max_Length (Odd_Preferences.Entry4, 0);
   Set_Text (Odd_Preferences.Entry4, -"");
   Set_Visibility (Odd_Preferences.Entry4, True);

   Gtk_New (Odd_Preferences.Label11, -("Fonts"));
   Set_Alignment (Odd_Preferences.Label11, 0.5, 0.5);
   Set_Padding (Odd_Preferences.Label11, 0, 0);
   Set_Justify (Odd_Preferences.Label11, Justify_Center);
   Set_Line_Wrap (Odd_Preferences.Label11, False);
   Set_Tab (Odd_Preferences.Notebook1, 4, Odd_Preferences.Label11);

   Gtk_New (Odd_Preferences.Frame6);
   Add (Odd_Preferences.Notebook1, Odd_Preferences.Frame6);
   Set_Shadow_Type (Odd_Preferences.Frame6, Shadow_Etched_In);

   Gtk_New (Odd_Preferences.Table6, 6, 2, False);
   Add (Odd_Preferences.Frame6, Odd_Preferences.Table6);
   Set_Row_Spacings (Odd_Preferences.Table6, 0);
   Set_Col_Spacings (Odd_Preferences.Table6, 0);

   Gtk_New (Odd_Preferences.Entry9);
   Attach (Odd_Preferences.Table6, Odd_Preferences.Entry9, 1, 2, 0, 1,
     Expand or Fill, 0,
     3, 0);
   Set_Editable (Odd_Preferences.Entry9, True);
   Set_Max_Length (Odd_Preferences.Entry9, 0);
   Set_Text (Odd_Preferences.Entry9, -"");
   Set_Visibility (Odd_Preferences.Entry9, True);

   Gtk_New (Odd_Preferences.Entry10);
   Attach (Odd_Preferences.Table6, Odd_Preferences.Entry10, 1, 2, 1, 2,
     Expand or Fill, 0,
     3, 0);
   Set_Editable (Odd_Preferences.Entry10, True);
   Set_Max_Length (Odd_Preferences.Entry10, 0);
   Set_Text (Odd_Preferences.Entry10, -"");
   Set_Visibility (Odd_Preferences.Entry10, True);

   Gtk_New (Odd_Preferences.Entry11);
   Attach (Odd_Preferences.Table6, Odd_Preferences.Entry11, 1, 2, 2, 3,
     Expand or Fill, 0,
     3, 0);
   Set_Editable (Odd_Preferences.Entry11, True);
   Set_Max_Length (Odd_Preferences.Entry11, 0);
   Set_Text (Odd_Preferences.Entry11, -"");
   Set_Visibility (Odd_Preferences.Entry11, True);

   Gtk_New (Odd_Preferences.Entry12);
   Attach (Odd_Preferences.Table6, Odd_Preferences.Entry12, 1, 2, 3, 4,
     Expand or Fill, 0,
     3, 0);
   Set_Editable (Odd_Preferences.Entry12, True);
   Set_Max_Length (Odd_Preferences.Entry12, 0);
   Set_Text (Odd_Preferences.Entry12, -"");
   Set_Visibility (Odd_Preferences.Entry12, True);

   Gtk_New (Odd_Preferences.Entry13);
   Attach (Odd_Preferences.Table6, Odd_Preferences.Entry13, 1, 2, 4, 5,
     Expand or Fill, 0,
     3, 0);
   Set_Editable (Odd_Preferences.Entry13, True);
   Set_Max_Length (Odd_Preferences.Entry13, 0);
   Set_Text (Odd_Preferences.Entry13, -"");
   Set_Visibility (Odd_Preferences.Entry13, True);

   Gtk_New (Odd_Preferences.Entry14);
   Attach (Odd_Preferences.Table6, Odd_Preferences.Entry14, 1, 2, 5, 6,
     Expand or Fill, 0,
     3, 0);
   Set_Editable (Odd_Preferences.Entry14, True);
   Set_Max_Length (Odd_Preferences.Entry14, 0);
   Set_Text (Odd_Preferences.Entry14, -"");
   Set_Visibility (Odd_Preferences.Entry14, True);

   Gtk_New (Odd_Preferences.Label43, -("Edit Sources"));
   Attach (Odd_Preferences.Table6, Odd_Preferences.Label43, 0, 1, 0, 1,
     Fill, Expand,
     0, 0);
   Set_Alignment (Odd_Preferences.Label43, 7.45058e-09, 0.5);
   Set_Padding (Odd_Preferences.Label43, 3, 0);
   Set_Justify (Odd_Preferences.Label43, Justify_Center);
   Set_Line_Wrap (Odd_Preferences.Label43, False);

   Gtk_New (Odd_Preferences.Label44, -("Get Core File"));
   Attach (Odd_Preferences.Table6, Odd_Preferences.Label44, 0, 1, 1, 2,
     Fill, Expand,
     0, 0);
   Set_Alignment (Odd_Preferences.Label44, 7.45058e-09, 0.5);
   Set_Padding (Odd_Preferences.Label44, 3, 0);
   Set_Justify (Odd_Preferences.Label44, Justify_Center);
   Set_Line_Wrap (Odd_Preferences.Label44, False);

   Gtk_New (Odd_Preferences.Label45, -("List Processes"));
   Attach (Odd_Preferences.Table6, Odd_Preferences.Label45, 0, 1, 2, 3,
     Fill, Expand,
     0, 0);
   Set_Alignment (Odd_Preferences.Label45, 7.45058e-09, 0.5);
   Set_Padding (Odd_Preferences.Label45, 3, 0);
   Set_Justify (Odd_Preferences.Label45, Justify_Center);
   Set_Line_Wrap (Odd_Preferences.Label45, False);

   Gtk_New (Odd_Preferences.Label46, -("Execution Window"));
   Attach (Odd_Preferences.Table6, Odd_Preferences.Label46, 0, 1, 3, 4,
     Fill, Expand,
     0, 0);
   Set_Alignment (Odd_Preferences.Label46, 7.45058e-09, 0.5);
   Set_Padding (Odd_Preferences.Label46, 3, 0);
   Set_Justify (Odd_Preferences.Label46, Justify_Center);
   Set_Line_Wrap (Odd_Preferences.Label46, False);

   Gtk_New (Odd_Preferences.Label47, -("Uncompress"));
   Attach (Odd_Preferences.Table6, Odd_Preferences.Label47, 0, 1, 4, 5,
     Fill, Expand,
     0, 0);
   Set_Alignment (Odd_Preferences.Label47, 7.45058e-09, 0.5);
   Set_Padding (Odd_Preferences.Label47, 3, 0);
   Set_Justify (Odd_Preferences.Label47, Justify_Center);
   Set_Line_Wrap (Odd_Preferences.Label47, False);

   Gtk_New (Odd_Preferences.Label48, -("Web Browser"));
   Attach (Odd_Preferences.Table6, Odd_Preferences.Label48, 0, 1, 5, 6,
     Fill, Expand,
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
   Set_Tab (Odd_Preferences.Notebook1, 5, Odd_Preferences.Label12);

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
