-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                         Copyright (C) 2000                        --
--                             ACT-Europe                            --
--                                                                   --
-- GVD is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Gtk; use Gtk;
with Gtk.Adjustment;  use Gtk.Adjustment;
with Gtk.Widget;      use Gtk.Widget;
with Gtk.Enums;       use Gtk.Enums;
with Gtkada.Handlers; use Gtkada.Handlers;
with Callbacks_Odd; use Callbacks_Odd;
with Odd_Intl; use Odd_Intl;
with General_Preferences_Pkg.Callbacks; use General_Preferences_Pkg.Callbacks;
with GVD.Pixmaps; use GVD.Pixmaps;
with Gdk.Pixmap; use Gdk.Pixmap;
with Gdk.Bitmap; use Gdk.Bitmap;
with Gtk.Pixmap; use Gtk.Pixmap;

package body General_Preferences_Pkg is

procedure Gtk_New (General_Preferences : out General_Preferences_Access) is
begin
   General_Preferences := new General_Preferences_Record;
   General_Preferences_Pkg.Initialize (General_Preferences);
end Gtk_New;

procedure Initialize (General_Preferences : access General_Preferences_Record'Class) is
   pragma Suppress (All_Checks);
   Big_Item_Spin_Adj : Gtk_Adjustment;
   Color_Pixmap : Gtk_Pixmap;
   Val  : Gdk_Pixmap;
   Mask : Gdk_Bitmap;

begin
   Gtk.Window.Initialize (General_Preferences, Window_Dialog);
   Return_Callback.Connect
     (General_Preferences, "delete_event", On_Odd_Preferences_Delete_Event'Access);
   Set_Title (General_Preferences, -"Preferences");
   Set_Policy (General_Preferences, False, True, False);
   Set_Position (General_Preferences, Win_Pos_Center);
   Set_Modal (General_Preferences, True);

   Gtk_New_Vbox (General_Preferences.Vbox2, False, 0);
   Add (General_Preferences, General_Preferences.Vbox2);

   Gtk_New (General_Preferences.Notebook1);
   Pack_Start (General_Preferences.Vbox2, General_Preferences.Notebook1, True, True, 0);
   Set_Scrollable (General_Preferences.Notebook1, False);
   Set_Show_Border (General_Preferences.Notebook1, True);
   Set_Show_Tabs (General_Preferences.Notebook1, True);
   Set_Tab_Hborder (General_Preferences.Notebook1, 2);
   Set_Tab_Vborder (General_Preferences.Notebook1, 2);
   Set_Tab_Pos (General_Preferences.Notebook1, Pos_Top);

   Gtk_New (General_Preferences.Frame1);
   Add (General_Preferences.Notebook1, General_Preferences.Frame1);
   Set_Shadow_Type (General_Preferences.Frame1, Shadow_Etched_In);

   Gtk_New (General_Preferences.Table1, 5, 4, False);
   Add (General_Preferences.Frame1, General_Preferences.Table1);
   Set_Row_Spacings (General_Preferences.Table1, 2);
   Set_Col_Spacings (General_Preferences.Table1, 0);

   Gtk_New (General_Preferences.Label13, -("Automatic Display of Button Hints"));
   Attach (General_Preferences.Table1, General_Preferences.Label13, 0, 1, 0, 1,
     Expand or Fill, 0,
     0, 0);
   Set_Alignment (General_Preferences.Label13, 7.45058e-09, 0.5);
   Set_Padding (General_Preferences.Label13, 2, 0);
   Set_Justify (General_Preferences.Label13, Justify_Left);
   Set_Line_Wrap (General_Preferences.Label13, False);

   Gtk_New (General_Preferences.Label14, -("Automatic Display of Variable Values"));
   Attach (General_Preferences.Table1, General_Preferences.Label14, 0, 1, 1, 2,
     Expand or Fill, 0,
     0, 0);
   Set_Alignment (General_Preferences.Label14, 7.45058e-09, 0.5);
   Set_Padding (General_Preferences.Label14, 2, 0);
   Set_Justify (General_Preferences.Label14, Justify_Left);
   Set_Line_Wrap (General_Preferences.Label14, False);

   Gtk_New (General_Preferences.Button_Hint_Popup_Check, -"as Popup Tips");
   Attach (General_Preferences.Table1, General_Preferences.Button_Hint_Popup_Check, 1, 3, 0, 1,
     Expand, 0,
     0, 0);
   Set_Active (General_Preferences.Button_Hint_Popup_Check, False);

   Gtk_New (General_Preferences.Button_Hint_Status_Check, -"in the Status Line");
   Attach (General_Preferences.Table1, General_Preferences.Button_Hint_Status_Check, 3, 4, 0, 1,
     Expand, 0,
     0, 0);
   Set_Active (General_Preferences.Button_Hint_Status_Check, False);

   Gtk_New (General_Preferences.Variable_Popup_Check, -"as Popup Tips");
   Attach (General_Preferences.Table1, General_Preferences.Variable_Popup_Check, 1, 3, 1, 2,
     Expand, 0,
     0, 0);
   Set_Active (General_Preferences.Variable_Popup_Check, False);

   Gtk_New (General_Preferences.Variable_Status_Check, -"in the Status Line");
   Attach (General_Preferences.Table1, General_Preferences.Variable_Status_Check, 3, 4, 1, 2,
     Expand, 0,
     0, 0);
   Set_Active (General_Preferences.Variable_Status_Check, False);

   Gtk_New (General_Preferences.Warn_Multiple_Check, -"Warn if Multiple GVD Instances are Running");
   Attach (General_Preferences.Table1, General_Preferences.Warn_Multiple_Check, 0, 4, 2, 3,
     Expand or Fill, 0,
     0, 0);
   Set_Active (General_Preferences.Warn_Multiple_Check, False);

   Gtk_New (General_Preferences.Label16, -("Status Bar Time Out"));
   Attach (General_Preferences.Table1, General_Preferences.Label16, 0, 1, 4, 5,
     Expand or Fill, 0,
     0, 0);
   Set_Alignment (General_Preferences.Label16, 7.45058e-09, 0.5);
   Set_Padding (General_Preferences.Label16, 2, 0);
   Set_Justify (General_Preferences.Label16, Justify_Left);
   Set_Line_Wrap (General_Preferences.Label16, False);

   Gtk_New (General_Preferences.Statusbar_Timeout_Entry);
   Attach (General_Preferences.Table1, General_Preferences.Statusbar_Timeout_Entry, 1, 2, 4, 5,
     Expand or Fill, 0,
     0, 0);
   Set_Editable (General_Preferences.Statusbar_Timeout_Entry, True);
   Set_Max_Length (General_Preferences.Statusbar_Timeout_Entry, 0);
   Set_Text (General_Preferences.Statusbar_Timeout_Entry, -"");
   Set_Visibility (General_Preferences.Statusbar_Timeout_Entry, True);

   Gtk_New (General_Preferences.Label17, -("ms"));
   Attach (General_Preferences.Table1, General_Preferences.Label17, 2, 4, 4, 5,
     Expand or Fill, 0,
     0, 0);
   Set_Alignment (General_Preferences.Label17, 7.45058e-09, 0.5);
   Set_Padding (General_Preferences.Label17, 2, 0);
   Set_Justify (General_Preferences.Label17, Justify_Center);
   Set_Line_Wrap (General_Preferences.Label17, False);

   Gtk_New (General_Preferences.Break_Exception_Check, -"Break On Exceptions");
   Attach (General_Preferences.Table1, General_Preferences.Break_Exception_Check, 0, 4, 3, 4,
     Expand or Fill, 0,
     0, 0);
   Set_Active (General_Preferences.Break_Exception_Check, False);

   Gtk_New (General_Preferences.Label7, -("General"));
   Set_Alignment (General_Preferences.Label7, 0.5, 0.5);
   Set_Padding (General_Preferences.Label7, 0, 0);
   Set_Justify (General_Preferences.Label7, Justify_Center);
   Set_Line_Wrap (General_Preferences.Label7, False);
   Set_Tab (General_Preferences.Notebook1, 0, General_Preferences.Label7);

   Gtk_New_Vbox (General_Preferences.Vbox18, False, 2);
   Add (General_Preferences.Notebook1, General_Preferences.Vbox18);
   Set_Border_Width (General_Preferences.Vbox18, 2);

   Gtk_New (General_Preferences.Frame16, -"Explorer");
   Pack_Start (General_Preferences.Vbox18, General_Preferences.Frame16, False, False, 0);
   Set_Shadow_Type (General_Preferences.Frame16, Shadow_Etched_In);

   Gtk_New (General_Preferences.Table9, 2, 2, False);
   Add (General_Preferences.Frame16, General_Preferences.Table9);
   Set_Row_Spacings (General_Preferences.Table9, 2);
   Set_Col_Spacings (General_Preferences.Table9, 5);

   Gtk_New (General_Preferences.Display_Explorer_Check, -"Display Explorer Tree");
   Attach (General_Preferences.Table9, General_Preferences.Display_Explorer_Check, 0, 2, 0, 1,
     Expand or Fill, 0,
     0, 0);
   Set_Active (General_Preferences.Display_Explorer_Check, True);

   Gtk_New (General_Preferences.Label75, -("File Name Background"));
   Attach (General_Preferences.Table9, General_Preferences.Label75, 0, 1, 1, 2,
     Fill, 0,
     0, 0);
   Set_Alignment (General_Preferences.Label75, 7.45058e-09, 0.5);
   Set_Padding (General_Preferences.Label75, 0, 0);
   Set_Justify (General_Preferences.Label75, Justify_Center);
   Set_Line_Wrap (General_Preferences.Label75, False);

   Gtk_New (General_Preferences.File_Name_Bg_Combo);
   Attach (General_Preferences.Table9, General_Preferences.File_Name_Bg_Combo, 1, 2, 1, 2,
     0, 0,
     0, 0);

   Gtk_New (General_Preferences.Frame17, -"Source");
   Pack_Start (General_Preferences.Vbox18, General_Preferences.Frame17, False, False, 0);
   Set_Shadow_Type (General_Preferences.Frame17, Shadow_Etched_In);

   Gtk_New (General_Preferences.Table10, 6, 2, False);
   Add (General_Preferences.Frame17, General_Preferences.Table10);
   Set_Row_Spacings (General_Preferences.Table10, 2);
   Set_Col_Spacings (General_Preferences.Table10, 5);

   Gtk_New (General_Preferences.Label76, -("Font"));
   Attach (General_Preferences.Table10, General_Preferences.Label76, 0, 1, 0, 1,
     Fill, 0,
     0, 0);
   Set_Alignment (General_Preferences.Label76, 7.45058e-09, 0.5);
   Set_Padding (General_Preferences.Label76, 0, 0);
   Set_Justify (General_Preferences.Label76, Justify_Center);
   Set_Line_Wrap (General_Preferences.Label76, False);

   Gtk_New (General_Preferences.Show_Lines_Code_Check, -"Show Lines with Code");
   Attach (General_Preferences.Table10, General_Preferences.Show_Lines_Code_Check, 1, 2, 1, 2,
     Fill, 0,
     0, 0);
   Set_Active (General_Preferences.Show_Lines_Code_Check, True);

   Gtk_New (General_Preferences.Show_Line_Numbers_Check, -"Show Line Numbers");
   Attach (General_Preferences.Table10, General_Preferences.Show_Line_Numbers_Check, 0, 1, 1, 2,
     Fill, 0,
     0, 0);
   Set_Active (General_Preferences.Show_Line_Numbers_Check, True);

   Gtk_New (General_Preferences.Syntax_Hilight_Check, -"Syntax Highlighting");
   Attach (General_Preferences.Table10, General_Preferences.Syntax_Hilight_Check, 0, 2, 2, 3,
     Fill, 0,
     0, 0);
   Set_Active (General_Preferences.Syntax_Hilight_Check, True);

   Gtk_New (General_Preferences.Label79, -("Comments"));
   Attach (General_Preferences.Table10, General_Preferences.Label79, 0, 1, 3, 4,
     Fill, 0,
     0, 0);
   Set_Alignment (General_Preferences.Label79, 7.45058e-09, 0.5);
   Set_Padding (General_Preferences.Label79, 0, 0);
   Set_Justify (General_Preferences.Label79, Justify_Center);
   Set_Line_Wrap (General_Preferences.Label79, False);

   Gtk_New (General_Preferences.Label80, -("Strings"));
   Attach (General_Preferences.Table10, General_Preferences.Label80, 0, 1, 4, 5,
     Fill, 0,
     0, 0);
   Set_Alignment (General_Preferences.Label80, 7.45058e-09, 0.5);
   Set_Padding (General_Preferences.Label80, 0, 0);
   Set_Justify (General_Preferences.Label80, Justify_Center);
   Set_Line_Wrap (General_Preferences.Label80, False);

   Gtk_New (General_Preferences.Label81, -("Keywords"));
   Attach (General_Preferences.Table10, General_Preferences.Label81, 0, 1, 5, 6,
     Fill, 0,
     0, 0);
   Set_Alignment (General_Preferences.Label81, 7.45058e-09, 0.5);
   Set_Padding (General_Preferences.Label81, 0, 0);
   Set_Justify (General_Preferences.Label81, Justify_Center);
   Set_Line_Wrap (General_Preferences.Label81, False);

   Gtk_New (General_Preferences.Editor_Font_Combo);
   Attach (General_Preferences.Table10, General_Preferences.Editor_Font_Combo, 1, 2, 0, 1,
     Fill, 0,
     0, 0);

   Gtk_New (General_Preferences.Comment_Color_Combo);
   Attach (General_Preferences.Table10, General_Preferences.Comment_Color_Combo, 1, 2, 3, 4,
     0, 0,
     0, 0);

   Gtk_New (General_Preferences.String_Color_Combo);
   Attach (General_Preferences.Table10, General_Preferences.String_Color_Combo, 1, 2, 4, 5,
     0, 0,
     0, 0);

   Gtk_New (General_Preferences.Keyword_Color_Combo);
   Attach (General_Preferences.Table10, General_Preferences.Keyword_Color_Combo, 1, 2, 5, 6,
     0, 0,
     0, 0);

   Gtk_New (General_Preferences.Frame18, -"Assembly");
   Pack_Start (General_Preferences.Vbox18, General_Preferences.Frame18, False, False, 0);
   Set_Shadow_Type (General_Preferences.Frame18, Shadow_Etched_In);

   Gtk_New_Hbox (General_Preferences.Hbox7, False, 5);
   Add (General_Preferences.Frame18, General_Preferences.Hbox7);

   Gtk_New (General_Preferences.Label82, -("Current Line"));
   Pack_Start (General_Preferences.Hbox7, General_Preferences.Label82, False, True, 0);
   Set_Alignment (General_Preferences.Label82, 7.45058e-09, 0.5);
   Set_Padding (General_Preferences.Label82, 0, 0);
   Set_Justify (General_Preferences.Label82, Justify_Center);
   Set_Line_Wrap (General_Preferences.Label82, False);

   Gtk_New (General_Preferences.Asm_Highlight_Combo);
   Pack_Start (General_Preferences.Hbox7, General_Preferences.Asm_Highlight_Combo, False, True, 0);

   Gtk_New (General_Preferences.Label8, -("Source"));
   Set_Alignment (General_Preferences.Label8, 0.5, 0.5);
   Set_Padding (General_Preferences.Label8, 0, 0);
   Set_Justify (General_Preferences.Label8, Justify_Center);
   Set_Line_Wrap (General_Preferences.Label8, False);
   Set_Tab (General_Preferences.Notebook1, 1, General_Preferences.Label8);

   Gtk_New (General_Preferences.Frame3);
   Add (General_Preferences.Notebook1, General_Preferences.Frame3);
   Set_Border_Width (General_Preferences.Frame3, 2);
   Set_Shadow_Type (General_Preferences.Frame3, Shadow_Etched_In);

   Gtk_New (General_Preferences.Table3, 11, 4, False);
   Add (General_Preferences.Frame3, General_Preferences.Table3);
   Set_Row_Spacings (General_Preferences.Table3, 2);
   Set_Col_Spacings (General_Preferences.Table3, 2);

   Gtk_New (General_Preferences.Label83, -("Clickable Item"));
   Attach (General_Preferences.Table3, General_Preferences.Label83, 0, 1, 0, 1,
     Fill, 0,
     0, 0);
   Set_Alignment (General_Preferences.Label83, 7.45058e-09, 0.5);
   Set_Padding (General_Preferences.Label83, 0, 0);
   Set_Justify (General_Preferences.Label83, Justify_Center);
   Set_Line_Wrap (General_Preferences.Label83, False);

   Gtk_New (General_Preferences.Detect_Aliases_Check, -"Detect Aliases (shared data structures)");
   Attach (General_Preferences.Table3, General_Preferences.Detect_Aliases_Check, 0, 4, 8, 9,
     Expand or Fill, 0,
     0, 0);
   Set_Active (General_Preferences.Detect_Aliases_Check, True);

   Gtk_New (General_Preferences.Align_Grid_Check, -"Auto-Align Displays on Nearest Grid Point");
   Attach (General_Preferences.Table3, General_Preferences.Align_Grid_Check, 0, 4, 10, 11,
     Expand or Fill, 0,
     0, 0);
   Set_Active (General_Preferences.Align_Grid_Check, True);

   Gtk_New (General_Preferences.Label84, -("Title Background"));
   Attach (General_Preferences.Table3, General_Preferences.Label84, 2, 3, 0, 1,
     Fill, 0,
     0, 0);
   Set_Alignment (General_Preferences.Label84, 7.45058e-09, 0.5);
   Set_Padding (General_Preferences.Label84, 0, 0);
   Set_Justify (General_Preferences.Label84, Justify_Center);
   Set_Line_Wrap (General_Preferences.Label84, False);

   Gtk_New (General_Preferences.Label85, -("Changed Data"));
   Attach (General_Preferences.Table3, General_Preferences.Label85, 0, 1, 1, 2,
     Fill, 0,
     0, 0);
   Set_Alignment (General_Preferences.Label85, 7.45058e-09, 0.5);
   Set_Padding (General_Preferences.Label85, 0, 0);
   Set_Justify (General_Preferences.Label85, Justify_Center);
   Set_Line_Wrap (General_Preferences.Label85, False);

   Gtk_New (General_Preferences.Label86, -("Auto-Refreshed"));
   Attach (General_Preferences.Table3, General_Preferences.Label86, 0, 1, 2, 3,
     Fill, 0,
     0, 0);
   Set_Alignment (General_Preferences.Label86, 7.45058e-09, 0.5);
   Set_Padding (General_Preferences.Label86, 0, 0);
   Set_Justify (General_Preferences.Label86, Justify_Center);
   Set_Line_Wrap (General_Preferences.Label86, False);

   Gtk_New (General_Preferences.Label87, -("Frozen"));
   Attach (General_Preferences.Table3, General_Preferences.Label87, 2, 3, 2, 3,
     Fill, 0,
     0, 0);
   Set_Alignment (General_Preferences.Label87, 7.45058e-09, 0.5);
   Set_Padding (General_Preferences.Label87, 0, 0);
   Set_Justify (General_Preferences.Label87, Justify_Center);
   Set_Line_Wrap (General_Preferences.Label87, False);

   Gtk_New (General_Preferences.Look_3d_Check, -"3D Look");
   Attach (General_Preferences.Table3, General_Preferences.Look_3d_Check, 0, 4, 3, 4,
     Expand or Fill, 0,
     0, 0);
   Set_Active (General_Preferences.Look_3d_Check, True);

   Gtk_New (General_Preferences.Label88, -("Item Name"));
   Attach (General_Preferences.Table3, General_Preferences.Label88, 0, 1, 4, 5,
     Fill, 0,
     0, 0);
   Set_Alignment (General_Preferences.Label88, 7.45058e-09, 0.5);
   Set_Padding (General_Preferences.Label88, 0, 0);
   Set_Justify (General_Preferences.Label88, Justify_Center);
   Set_Line_Wrap (General_Preferences.Label88, False);

   Gtk_New (General_Preferences.Label89, -("Item Value"));
   Attach (General_Preferences.Table3, General_Preferences.Label89, 0, 1, 5, 6,
     Fill, 0,
     0, 0);
   Set_Alignment (General_Preferences.Label89, 7.45058e-09, 0.5);
   Set_Padding (General_Preferences.Label89, 0, 0);
   Set_Justify (General_Preferences.Label89, Justify_Center);
   Set_Line_Wrap (General_Preferences.Label89, False);

   Gtk_New (General_Preferences.Label90, -("Item Type"));
   Attach (General_Preferences.Table3, General_Preferences.Label90, 0, 1, 6, 7,
     Fill, 0,
     0, 0);
   Set_Alignment (General_Preferences.Label90, 0.0, 0.5);
   Set_Padding (General_Preferences.Label90, 0, 0);
   Set_Justify (General_Preferences.Label90, Justify_Center);
   Set_Line_Wrap (General_Preferences.Label90, False);

   Gtk_New (General_Preferences.Hide_Big_Items_Check, -"Hide Big Items");
   Attach (General_Preferences.Table3, General_Preferences.Hide_Big_Items_Check, 0, 2, 7, 8,
     Expand or Fill, 0,
     0, 0);
   Set_Active (General_Preferences.Hide_Big_Items_Check, True);

   Gtk_New (General_Preferences.Label91, -("Big Item Height"));
   Attach (General_Preferences.Table3, General_Preferences.Label91, 2, 3, 7, 8,
     Fill, 0,
     0, 0);
   Set_Alignment (General_Preferences.Label91, 7.45058e-09, 0.5);
   Set_Padding (General_Preferences.Label91, 0, 0);
   Set_Justify (General_Preferences.Label91, Justify_Center);
   Set_Line_Wrap (General_Preferences.Label91, False);

   Gtk_New (Big_Item_Spin_Adj, 150.0, 0.0, 1000.0, 1.0, 10.0, 10.0);
   Gtk_New (General_Preferences.Big_Item_Spin, Big_Item_Spin_Adj, 1.0, 0);
   Attach (General_Preferences.Table3, General_Preferences.Big_Item_Spin, 3, 4, 7, 8,
     Expand or Fill, 0,
     0, 0);
   Set_Numeric (General_Preferences.Big_Item_Spin, False);
   Set_Snap_To_Ticks (General_Preferences.Big_Item_Spin, False);
   Set_Update_Policy (General_Preferences.Big_Item_Spin, Update_Always);
   Set_Value (General_Preferences.Big_Item_Spin, 150.0);
   Set_Wrap (General_Preferences.Big_Item_Spin, False);

   Gtk_New (General_Preferences.Display_Grid_Check, -"Display Grid Points");
   Attach (General_Preferences.Table3, General_Preferences.Display_Grid_Check, 0, 4, 9, 10,
     Expand or Fill, 0,
     0, 0);
   Set_Active (General_Preferences.Display_Grid_Check, True);

   Gtk_New (General_Preferences.Title_Font_Combo);
   Attach (General_Preferences.Table3, General_Preferences.Title_Font_Combo, 1, 4, 4, 5,
     Fill, 0,
     0, 0);

   Gtk_New (General_Preferences.Value_Font_Combo);
   Attach (General_Preferences.Table3, General_Preferences.Value_Font_Combo, 1, 4, 5, 6,
     Fill, 0,
     0, 0);

   Gtk_New (General_Preferences.Type_Font_Combo);
   Attach (General_Preferences.Table3, General_Preferences.Type_Font_Combo, 1, 4, 6, 7,
     Fill, 0,
     0, 0);

   Gtk_New (General_Preferences.Xref_Color_Combo);
   Attach (General_Preferences.Table3, General_Preferences.Xref_Color_Combo, 1, 2, 0, 1,
     0, 0,
     0, 0);

   Gtk_New (General_Preferences.Change_Color_Combo);
   Attach (General_Preferences.Table3, General_Preferences.Change_Color_Combo, 1, 2, 1, 2,
     0, 0,
     0, 0);

   Gtk_New (General_Preferences.Thaw_Bg_Color_Combo);
   Attach (General_Preferences.Table3, General_Preferences.Thaw_Bg_Color_Combo, 1, 2, 2, 3,
     0, 0,
     0, 0);

   Gtk_New (General_Preferences.Title_Color_Combo);
   Attach (General_Preferences.Table3, General_Preferences.Title_Color_Combo, 3, 4, 0, 1,
     0, 0,
     0, 0);

   Gtk_New (General_Preferences.Freeze_Bg_Color_Combo);
   Attach (General_Preferences.Table3, General_Preferences.Freeze_Bg_Color_Combo, 3, 4, 2, 3,
     0, 0,
     0, 0);

   Gtk_New (General_Preferences.Label9, -("Data"));
   Set_Alignment (General_Preferences.Label9, 0.5, 0.5);
   Set_Padding (General_Preferences.Label9, 0, 0);
   Set_Justify (General_Preferences.Label9, Justify_Center);
   Set_Line_Wrap (General_Preferences.Label9, False);
   Set_Tab (General_Preferences.Notebook1, 2, General_Preferences.Label9);

   Gtk_New (General_Preferences.Frame13);
   Add (General_Preferences.Notebook1, General_Preferences.Frame13);
   Set_Border_Width (General_Preferences.Frame13, 2);
   Set_Shadow_Type (General_Preferences.Frame13, Shadow_Etched_In);

   Gtk_New (General_Preferences.Table8, 2, 2, False);
   Add (General_Preferences.Frame13, General_Preferences.Table8);
   Set_Row_Spacings (General_Preferences.Table8, 2);
   Set_Col_Spacings (General_Preferences.Table8, 2);

   Gtk_New (General_Preferences.Label92, -("Color Highlighting"));
   Attach (General_Preferences.Table8, General_Preferences.Label92, 0, 1, 0, 1,
     Fill, 0,
     0, 0);
   Set_Alignment (General_Preferences.Label92, 7.45058e-09, 0.5);
   Set_Padding (General_Preferences.Label92, 0, 0);
   Set_Justify (General_Preferences.Label92, Justify_Center);
   Set_Line_Wrap (General_Preferences.Label92, False);

   Gtk_New (General_Preferences.Label93, -("Font"));
   Attach (General_Preferences.Table8, General_Preferences.Label93, 0, 1, 1, 2,
     Fill, 0,
     0, 0);
   Set_Alignment (General_Preferences.Label93, 7.45058e-09, 0.5);
   Set_Padding (General_Preferences.Label93, 0, 0);
   Set_Justify (General_Preferences.Label93, Justify_Center);
   Set_Line_Wrap (General_Preferences.Label93, False);

   Gtk_New (General_Preferences.Debug_Font_Combo);
   Attach (General_Preferences.Table8, General_Preferences.Debug_Font_Combo, 1, 2, 1, 2,
     Fill, 0,
     0, 0);

   Gtk_New (General_Preferences.Debug_Higlight_Combo);
   Attach (General_Preferences.Table8, General_Preferences.Debug_Higlight_Combo, 1, 2, 0, 1,
     0, 0,
     0, 0);

   Gtk_New (General_Preferences.Label10, -("Command"));
   Set_Alignment (General_Preferences.Label10, 0.5, 0.5);
   Set_Padding (General_Preferences.Label10, 0, 0);
   Set_Justify (General_Preferences.Label10, Justify_Center);
   Set_Line_Wrap (General_Preferences.Label10, False);
   Set_Tab (General_Preferences.Notebook1, 3, General_Preferences.Label10);

   Gtk_New (General_Preferences.Frame6);
   Add (General_Preferences.Notebook1, General_Preferences.Frame6);
   Set_Border_Width (General_Preferences.Frame6, 2);
   Set_Shadow_Type (General_Preferences.Frame6, Shadow_Etched_In);

   Gtk_New (General_Preferences.Table6, 4, 2, False);
   Add (General_Preferences.Frame6, General_Preferences.Table6);
   Set_Row_Spacings (General_Preferences.Table6, 2);
   Set_Col_Spacings (General_Preferences.Table6, 0);

   Gtk_New (General_Preferences.Edit_Source_Entry);
   Attach (General_Preferences.Table6, General_Preferences.Edit_Source_Entry, 1, 2, 0, 1,
     Expand or Fill, 0,
     3, 0);
   Set_Editable (General_Preferences.Edit_Source_Entry, True);
   Set_Max_Length (General_Preferences.Edit_Source_Entry, 0);
   Set_Text (General_Preferences.Edit_Source_Entry, -"");
   Set_Visibility (General_Preferences.Edit_Source_Entry, True);

   Gtk_New (General_Preferences.Get_Core_File_Entry);
   Attach (General_Preferences.Table6, General_Preferences.Get_Core_File_Entry, 1, 2, 1, 2,
     Expand or Fill, 0,
     3, 0);
   Set_Editable (General_Preferences.Get_Core_File_Entry, True);
   Set_Max_Length (General_Preferences.Get_Core_File_Entry, 0);
   Set_Text (General_Preferences.Get_Core_File_Entry, -"");
   Set_Visibility (General_Preferences.Get_Core_File_Entry, True);

   Gtk_New (General_Preferences.List_Processes_Entry);
   Attach (General_Preferences.Table6, General_Preferences.List_Processes_Entry, 1, 2, 2, 3,
     Expand or Fill, 0,
     3, 0);
   Set_Editable (General_Preferences.List_Processes_Entry, True);
   Set_Max_Length (General_Preferences.List_Processes_Entry, 0);
   Set_Text (General_Preferences.List_Processes_Entry, -"");
   Set_Visibility (General_Preferences.List_Processes_Entry, True);

   Gtk_New (General_Preferences.Web_Browser_Entry);
   Attach (General_Preferences.Table6, General_Preferences.Web_Browser_Entry, 1, 2, 3, 4,
     Expand or Fill, 0,
     3, 0);
   Set_Editable (General_Preferences.Web_Browser_Entry, True);
   Set_Max_Length (General_Preferences.Web_Browser_Entry, 0);
   Set_Text (General_Preferences.Web_Browser_Entry, -"");
   Set_Visibility (General_Preferences.Web_Browser_Entry, True);

   Gtk_New (General_Preferences.Label43, -("Edit Sources"));
   Attach (General_Preferences.Table6, General_Preferences.Label43, 0, 1, 0, 1,
     Fill, 0,
     0, 0);
   Set_Alignment (General_Preferences.Label43, 7.45058e-09, 0.5);
   Set_Padding (General_Preferences.Label43, 3, 0);
   Set_Justify (General_Preferences.Label43, Justify_Center);
   Set_Line_Wrap (General_Preferences.Label43, False);

   Gtk_New (General_Preferences.Label44, -("Get Core File"));
   Attach (General_Preferences.Table6, General_Preferences.Label44, 0, 1, 1, 2,
     Fill, 0,
     0, 0);
   Set_Alignment (General_Preferences.Label44, 7.45058e-09, 0.5);
   Set_Padding (General_Preferences.Label44, 3, 0);
   Set_Justify (General_Preferences.Label44, Justify_Center);
   Set_Line_Wrap (General_Preferences.Label44, False);

   Gtk_New (General_Preferences.Label45, -("List Processes"));
   Attach (General_Preferences.Table6, General_Preferences.Label45, 0, 1, 2, 3,
     Fill, 0,
     0, 0);
   Set_Alignment (General_Preferences.Label45, 7.45058e-09, 0.5);
   Set_Padding (General_Preferences.Label45, 3, 0);
   Set_Justify (General_Preferences.Label45, Justify_Center);
   Set_Line_Wrap (General_Preferences.Label45, False);

   Gtk_New (General_Preferences.Label48, -("Web Browser"));
   Attach (General_Preferences.Table6, General_Preferences.Label48, 0, 1, 3, 4,
     Fill, 0,
     0, 0);
   Set_Alignment (General_Preferences.Label48, 7.45058e-09, 0.5);
   Set_Padding (General_Preferences.Label48, 3, 0);
   Set_Justify (General_Preferences.Label48, Justify_Center);
   Set_Line_Wrap (General_Preferences.Label48, False);

   Gtk_New (General_Preferences.Label12, -("Helpers"));
   Set_Alignment (General_Preferences.Label12, 0.5, 0.5);
   Set_Padding (General_Preferences.Label12, 0, 0);
   Set_Justify (General_Preferences.Label12, Justify_Center);
   Set_Line_Wrap (General_Preferences.Label12, False);
   Set_Tab (General_Preferences.Notebook1, 4, General_Preferences.Label12);

   Gtk_New (General_Preferences.Hbuttonbox6);
   Pack_Start (General_Preferences.Vbox2, General_Preferences.Hbuttonbox6, False, False, 3);
   Set_Spacing (General_Preferences.Hbuttonbox6, 30);
   Set_Layout (General_Preferences.Hbuttonbox6, Buttonbox_Spread);
   Set_Child_Size (General_Preferences.Hbuttonbox6, 85, 27);
   Set_Child_Ipadding (General_Preferences.Hbuttonbox6, 7, 0);

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

   Color_Pixmap := Create_Pixmap (paint_xpm, General_Preferences);
   Get (Color_Pixmap, Val, Mask);
   Add (Get_Button (General_Preferences.File_Name_Bg_Combo), Color_Pixmap);
   Gtk_New (Color_Pixmap, Val, Mask);
   Add (Get_Button (General_Preferences.Comment_Color_Combo), Color_Pixmap);
   Gtk_New (Color_Pixmap, Val, Mask);
   Add (Get_Button (General_Preferences.String_Color_Combo), Color_Pixmap);
   Gtk_New (Color_Pixmap, Val, Mask);
   Add (Get_Button (General_Preferences.Keyword_Color_Combo), Color_Pixmap);
   Gtk_New (Color_Pixmap, Val, Mask);
   Add (Get_Button (General_Preferences.Asm_Highlight_Combo), Color_Pixmap);
   Gtk_New (Color_Pixmap, Val, Mask);
   Add (Get_Button (General_Preferences.Xref_Color_Combo), Color_Pixmap);
   Gtk_New (Color_Pixmap, Val, Mask);
   Add (Get_Button (General_Preferences.Change_Color_Combo), Color_Pixmap);
   Gtk_New (Color_Pixmap, Val, Mask);
   Add (Get_Button (General_Preferences.Thaw_Bg_Color_Combo), Color_Pixmap);
   Gtk_New (Color_Pixmap, Val, Mask);
   Add (Get_Button (General_Preferences.Title_Color_Combo), Color_Pixmap);
   Gtk_New (Color_Pixmap, Val, Mask);
   Add (Get_Button (General_Preferences.Freeze_Bg_Color_Combo), Color_Pixmap);
   Gtk_New (Color_Pixmap, Val, Mask);
   Add (Get_Button (General_Preferences.Debug_Higlight_Combo), Color_Pixmap);
end Initialize;

end General_Preferences_Pkg;
