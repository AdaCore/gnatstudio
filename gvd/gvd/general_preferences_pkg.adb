-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                      Copyright (C) 2000-2001                      --
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
with Odd_Intl; use Odd_Intl;
with General_Preferences_Pkg.Callbacks; use General_Preferences_Pkg.Callbacks;
with Gtk.Pixmap; use Gtk.Pixmap;
with Gtk.Handlers;
with Gtk.Arguments; use Gtk.Arguments;
with Gdk.GC;        use Gdk.GC;
with Gdk.Color;     use Gdk.Color;
with Gdk.Drawable;  use Gdk.Drawable;
with Gdk.Rectangle; use Gdk.Rectangle;
with GVD.Color_Combo; use GVD.Color_Combo;

package body General_Preferences_Pkg is

   package Color_Cb is new Gtk.Handlers.Callback
     (Gvd_Color_Combo_Record);

   procedure Color_Changed
     (Combo : access Gvd_Color_Combo_Record'Class;
      Args  : Gtk.Arguments.Gtk_Args);

procedure Gtk_New
  (General_Preferences : out General_Preferences_Access;
   Main_Window         : access Gtk.Window.Gtk_Window_Record'Class) is
begin
   General_Preferences := new General_Preferences_Record;
   General_Preferences_Pkg.Initialize (General_Preferences, Main_Window);
end Gtk_New;

procedure Initialize
  (General_Preferences : access General_Preferences_Record'Class;
   Main_Window         : access Gtk.Window.Gtk_Window_Record'Class) is
   pragma Suppress (All_Checks);
   Big_Item_Spin_Adj : Gtk_Adjustment;

begin
   Gtk.Window.Initialize (General_Preferences, Window_Dialog);
   Set_Title (General_Preferences, -"Preferences");
   Set_Policy (General_Preferences, False, True, False);
   Set_Position (General_Preferences, Win_Pos_Center);
   Set_Modal (General_Preferences, False);
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

   Gtk_New (General_Preferences.Table1, 4, 4, False);
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

   Gtk_New (General_Preferences.Warn_Multiple_Check, -"Warn if Multiple GVD Instances are Running");
   Set_Active (General_Preferences.Warn_Multiple_Check, False);
   Attach (General_Preferences.Table1, General_Preferences.Warn_Multiple_Check, 0, 4, 1, 2,
     Expand or Fill, 0,
     0, 0);

   Gtk_New (General_Preferences.Label16, -("Status Bar Time Out"));
   Set_Alignment (General_Preferences.Label16, 7.45058e-09, 0.5);
   Set_Padding (General_Preferences.Label16, 2, 0);
   Set_Justify (General_Preferences.Label16, Justify_Left);
   Set_Line_Wrap (General_Preferences.Label16, False);
   Attach (General_Preferences.Table1, General_Preferences.Label16, 0, 1, 3, 4,
     Expand or Fill, 0,
     0, 0);

   Gtk_New (General_Preferences.Statusbar_Timeout_Entry);
   Set_Editable (General_Preferences.Statusbar_Timeout_Entry, True);
   Set_Max_Length (General_Preferences.Statusbar_Timeout_Entry, 0);
   Set_Text (General_Preferences.Statusbar_Timeout_Entry, -"");
   Set_Visibility (General_Preferences.Statusbar_Timeout_Entry, True);
   Attach (General_Preferences.Table1, General_Preferences.Statusbar_Timeout_Entry, 1, 2, 3, 4,
     Expand or Fill, 0,
     0, 0);

   Gtk_New (General_Preferences.Label17, -("ms"));
   Set_Alignment (General_Preferences.Label17, 7.45058e-09, 0.5);
   Set_Padding (General_Preferences.Label17, 2, 0);
   Set_Justify (General_Preferences.Label17, Justify_Center);
   Set_Line_Wrap (General_Preferences.Label17, False);
   Attach (General_Preferences.Table1, General_Preferences.Label17, 2, 4, 3, 4,
     Expand or Fill, 0,
     0, 0);

   Gtk_New (General_Preferences.Break_Exception_Check, -"Break On Exceptions");
   Set_Active (General_Preferences.Break_Exception_Check, False);
   Attach (General_Preferences.Table1, General_Preferences.Break_Exception_Check, 0, 4, 2, 3,
     Expand or Fill, 0,
     0, 0);

   Gtk_New (General_Preferences.Label_General, -("General"));
   Set_Alignment (General_Preferences.Label_General, 0.5, 0.5);
   Set_Padding (General_Preferences.Label_General, 0, 0);
   Set_Justify (General_Preferences.Label_General, Justify_Center);
   Set_Line_Wrap (General_Preferences.Label_General, False);
   Set_Tab (General_Preferences.Notebook1, 0, General_Preferences.Label_General);

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
   Attach (General_Preferences.Table9, General_Preferences.File_Name_Bg_Combo, 1, 2, 1, 2,
     0, 0,
     0, 0);

   Gtk_New (General_Preferences.Frame17, -"Source");
   Set_Shadow_Type (General_Preferences.Frame17, Shadow_Etched_In);
   Pack_Start (General_Preferences.Vbox18, General_Preferences.Frame17, False, False, 0);

   Gtk_New (General_Preferences.Table10, 6, 3, False);
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

   Gtk_New (General_Preferences.Syntax_Highlight_Check, -"Syntax Highlighting");
   Set_Active (General_Preferences.Syntax_Highlight_Check, True);
   Attach (General_Preferences.Table10, General_Preferences.Syntax_Highlight_Check, 0, 1, 2, 3,
     Fill, 0,
     0, 0);

   Gtk_New (General_Preferences.Strip_Cr_Check, -"Strip Carriage Return");
   Set_Active (General_Preferences.Strip_Cr_Check, True);
   Attach (General_Preferences.Table10, General_Preferences.Strip_Cr_Check, 1, 2, 2, 3,
     Fill, 0,
     0, 0);

   Gtk_New (General_Preferences.Tooltips_Check, -"Automatic Display of Variables");
   Set_Active (General_Preferences.Tooltips_Check, True);
   Attach (General_Preferences.Table10, General_Preferences.Tooltips_Check, 2, 3, 1, 2,
     0, 0,
     0, 0);

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
   Pack_Start (General_Preferences.Hbox7, General_Preferences.Asm_Highlight_Combo, False, True, 0);

   Gtk_New (General_Preferences.Label_Source, -("Source"));
   Set_Alignment (General_Preferences.Label_Source, 0.5, 0.5);
   Set_Padding (General_Preferences.Label_Source, 0, 0);
   Set_Justify (General_Preferences.Label_Source, Justify_Center);
   Set_Line_Wrap (General_Preferences.Label_Source, False);
   Set_Tab (General_Preferences.Notebook1, 1, General_Preferences.Label_Source);

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

   Gtk_New (General_Preferences.Label_Data, -("Data"));
   Set_Alignment (General_Preferences.Label_Data, 0.5, 0.5);
   Set_Padding (General_Preferences.Label_Data, 0, 0);
   Set_Justify (General_Preferences.Label_Data, Justify_Center);
   Set_Line_Wrap (General_Preferences.Label_Data, False);
   Set_Tab (General_Preferences.Notebook1, 2, General_Preferences.Label_Data);

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

   Gtk_New (General_Preferences.Debug_Font_Combo);
   Attach (General_Preferences.Table8, General_Preferences.Debug_Font_Combo, 1, 2, 1, 2,
     Fill, 0,
     0, 0);

   Gtk_New (General_Preferences.Debug_Highlight_Combo);
   Attach (General_Preferences.Table8, General_Preferences.Debug_Highlight_Combo, 1, 2, 0, 1,
     0, 0,
     0, 0);

   Gtk_New (General_Preferences.Label_Command, -("Command"));
   Set_Alignment (General_Preferences.Label_Command, 0.5, 0.5);
   Set_Padding (General_Preferences.Label_Command, 0, 0);
   Set_Justify (General_Preferences.Label_Command, Justify_Center);
   Set_Line_Wrap (General_Preferences.Label_Command, False);
   Set_Tab (General_Preferences.Notebook1, 3, General_Preferences.Label_Command);

   Gtk_New (General_Preferences.Frame19);
   Set_Border_Width (General_Preferences.Frame19, 2);
   Set_Shadow_Type (General_Preferences.Frame19, Shadow_Etched_In);
   Add (General_Preferences.Notebook1, General_Preferences.Frame19);

   Gtk_New (General_Preferences.Table11, 5, 2, False);
   Set_Row_Spacings (General_Preferences.Table11, 2);
   Set_Col_Spacings (General_Preferences.Table11, 2);
   Add (General_Preferences.Frame19, General_Preferences.Table11);

   Gtk_New (General_Preferences.Label206, -("Font"));
   Set_Alignment (General_Preferences.Label206, 7.45058e-09, 0.5);
   Set_Padding (General_Preferences.Label206, 0, 0);
   Set_Justify (General_Preferences.Label206, Justify_Center);
   Set_Line_Wrap (General_Preferences.Label206, False);
   Attach (General_Preferences.Table11, General_Preferences.Label206, 0, 1, 0, 1,
     Fill, 0,
     0, 0);

   Gtk_New (General_Preferences.Label207, -("Default Color"));
   Set_Alignment (General_Preferences.Label207, 7.45058e-09, 0.5);
   Set_Padding (General_Preferences.Label207, 0, 0);
   Set_Justify (General_Preferences.Label207, Justify_Center);
   Set_Line_Wrap (General_Preferences.Label207, False);
   Attach (General_Preferences.Table11, General_Preferences.Label207, 0, 1, 1, 2,
     Fill, 0,
     0, 0);

   Gtk_New (General_Preferences.Memory_Font_Combo);
   Attach (General_Preferences.Table11, General_Preferences.Memory_Font_Combo, 1, 2, 0, 1,
     0, 0,
     0, 0);

   Gtk_New (General_Preferences.Memory_Default_Combo);
   Attach (General_Preferences.Table11, General_Preferences.Memory_Default_Combo, 1, 2, 1, 2,
     0, 0,
     0, 0);

   Gtk_New (General_Preferences.Label210, -("Modified"));
   Set_Alignment (General_Preferences.Label210, 7.45058e-09, 0.5);
   Set_Padding (General_Preferences.Label210, 0, 0);
   Set_Justify (General_Preferences.Label210, Justify_Center);
   Set_Line_Wrap (General_Preferences.Label210, False);
   Attach (General_Preferences.Table11, General_Preferences.Label210, 0, 1, 4, 5,
     Fill, 0,
     0, 0);

   Gtk_New (General_Preferences.Label209, -("Selection"));
   Set_Alignment (General_Preferences.Label209, 7.45058e-09, 0.5);
   Set_Padding (General_Preferences.Label209, 0, 0);
   Set_Justify (General_Preferences.Label209, Justify_Center);
   Set_Line_Wrap (General_Preferences.Label209, False);
   Attach (General_Preferences.Table11, General_Preferences.Label209, 0, 1, 3, 4,
     Fill, 0,
     0, 0);

   Gtk_New (General_Preferences.Label208, -("Color Highlighting"));
   Set_Alignment (General_Preferences.Label208, 7.45058e-09, 0.5);
   Set_Padding (General_Preferences.Label208, 0, 0);
   Set_Justify (General_Preferences.Label208, Justify_Center);
   Set_Line_Wrap (General_Preferences.Label208, False);
   Attach (General_Preferences.Table11, General_Preferences.Label208, 0, 1, 2, 3,
     Fill, 0,
     0, 0);

   Gtk_New (General_Preferences.Memory_Highlight_Combo);
   Attach (General_Preferences.Table11, General_Preferences.Memory_Highlight_Combo, 1, 2, 2, 3,
     0, 0,
     0, 0);

   Gtk_New (General_Preferences.Memory_Selection_Combo);
   Attach (General_Preferences.Table11, General_Preferences.Memory_Selection_Combo, 1, 2, 3, 4,
     0, 0,
     0, 0);

   Gtk_New (General_Preferences.Memory_Modified_Combo);
   Attach (General_Preferences.Table11, General_Preferences.Memory_Modified_Combo, 1, 2, 4, 5,
     0, 0,
     0, 0);

   Gtk_New (General_Preferences.Label_Memory, -("Memory"));
   Set_Alignment (General_Preferences.Label_Memory, 0.5, 0.5);
   Set_Padding (General_Preferences.Label_Memory, 0, 0);
   Set_Justify (General_Preferences.Label_Memory, Justify_Center);
   Set_Line_Wrap (General_Preferences.Label_Memory, False);
   Set_Tab (General_Preferences.Notebook1, 4, General_Preferences.Label_Memory);

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

   Gtk_New (General_Preferences.List_Processes_Entry);
   Set_Editable (General_Preferences.List_Processes_Entry, True);
   Set_Max_Length (General_Preferences.List_Processes_Entry, 0);
   Set_Text (General_Preferences.List_Processes_Entry, -"");
   Set_Visibility (General_Preferences.List_Processes_Entry, True);
   Attach (General_Preferences.Table6, General_Preferences.List_Processes_Entry, 1, 2, 1, 2,
     Expand or Fill, 0,
     3, 0);

   Gtk_New (General_Preferences.Remote_Shell_Entry);
   Set_Editable (General_Preferences.Remote_Shell_Entry, True);
   Set_Max_Length (General_Preferences.Remote_Shell_Entry, 0);
   Set_Text (General_Preferences.Remote_Shell_Entry, -"");
   Set_Visibility (General_Preferences.Remote_Shell_Entry, True);
   Attach (General_Preferences.Table6, General_Preferences.Remote_Shell_Entry, 1, 2, 2, 3,
     Expand or Fill, 0,
     3, 0);

   Gtk_New (General_Preferences.Remote_Copy_Entry);
   Set_Editable (General_Preferences.Remote_Copy_Entry, True);
   Set_Max_Length (General_Preferences.Remote_Copy_Entry, 0);
   Set_Text (General_Preferences.Remote_Copy_Entry, -"");
   Set_Visibility (General_Preferences.Remote_Copy_Entry, True);
   Attach (General_Preferences.Table6, General_Preferences.Remote_Copy_Entry, 1, 2, 3, 4,
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

   Gtk_New (General_Preferences.Label44, -("List Processes"));
   Set_Alignment (General_Preferences.Label44, 7.45058e-09, 0.5);
   Set_Padding (General_Preferences.Label44, 3, 0);
   Set_Justify (General_Preferences.Label44, Justify_Center);
   Set_Line_Wrap (General_Preferences.Label44, False);
   Attach (General_Preferences.Table6, General_Preferences.Label44, 0, 1, 1, 2,
     Fill, 0,
     0, 0);

   Gtk_New (General_Preferences.Label45, -("Remote Shell"));
   Set_Alignment (General_Preferences.Label45, 7.45058e-09, 0.5);
   Set_Padding (General_Preferences.Label45, 3, 0);
   Set_Justify (General_Preferences.Label45, Justify_Center);
   Set_Line_Wrap (General_Preferences.Label45, False);
   Attach (General_Preferences.Table6, General_Preferences.Label45, 0, 1, 2, 3,
     Fill, 0,
     0, 0);

   Gtk_New (General_Preferences.Label48, -("Remote Copy"));
   Set_Alignment (General_Preferences.Label48, 7.45058e-09, 0.5);
   Set_Padding (General_Preferences.Label48, 3, 0);
   Set_Justify (General_Preferences.Label48, Justify_Center);
   Set_Line_Wrap (General_Preferences.Label48, False);
   Attach (General_Preferences.Table6, General_Preferences.Label48, 0, 1, 3, 4,
     Fill, 0,
     0, 0);

   Gtk_New (General_Preferences.Label_Helpers, -("Helpers"));
   Set_Alignment (General_Preferences.Label_Helpers, 0.5, 0.5);
   Set_Padding (General_Preferences.Label_Helpers, 0, 0);
   Set_Justify (General_Preferences.Label_Helpers, Justify_Center);
   Set_Line_Wrap (General_Preferences.Label_Helpers, False);
   Set_Tab (General_Preferences.Notebook1, 5, General_Preferences.Label_Helpers);

   Gtk_New (General_Preferences.Hbuttonbox6);
   Set_Spacing (General_Preferences.Hbuttonbox6, 30);
   Set_Layout (General_Preferences.Hbuttonbox6, Buttonbox_Spread);
   Set_Child_Size (General_Preferences.Hbuttonbox6, 85, 27);
   Set_Child_Ipadding (General_Preferences.Hbuttonbox6, 7, 0);
   Pack_Start (General_Preferences.Vbox2, General_Preferences.Hbuttonbox6, False, False, 3);

   Gtk_New (General_Preferences.Ok_Button, -"OK");
   Set_Flags (General_Preferences.Ok_Button, Can_Default);
   Widget_Callback.Object_Connect
     (General_Preferences.Ok_Button, "clicked",
      Widget_Callback.To_Marshaller (On_Ok_Button_Clicked'Access), General_Preferences);
   Add (General_Preferences.Hbuttonbox6, General_Preferences.Ok_Button);

   Gtk_New (General_Preferences.Test_Button, -"Test");
   Set_Flags (General_Preferences.Test_Button, Can_Default);
   Widget_Callback.Object_Connect
     (General_Preferences.Test_Button, "clicked",
      Widget_Callback.To_Marshaller (On_Test_Button_Clicked'Access), General_Preferences);
   Add (General_Preferences.Hbuttonbox6, General_Preferences.Test_Button);

   Gtk_New (General_Preferences.Cancel_Button, -"Cancel");
   Set_Flags (General_Preferences.Cancel_Button, Can_Default);
   Widget_Callback.Object_Connect
     (General_Preferences.Cancel_Button, "clicked",
      Widget_Callback.To_Marshaller (On_Cancel_Button_Clicked'Access), General_Preferences);
   Add (General_Preferences.Hbuttonbox6, General_Preferences.Cancel_Button);

   General_Preferences.Main_Window := Gtk_Window (Main_Window);
end Initialize;

   -------------------
   -- Color_Changed --
   -------------------

   procedure Color_Changed
     (Combo : access Gvd_Color_Combo_Record'Class;
      Args  : Gtk.Arguments.Gtk_Args)
   is
      Name : String := To_String (Args, 2);
      Tmp_Gc   : Gdk_Gc;
      Color    : Gdk_Color;
      Pixmap   : Gtk_Pixmap;
   begin
      Color := Parse (Name);
      Alloc (Get_Colormap (Combo), Color);

      Realize (Combo);
      Pixmap := Gtk_Pixmap (Get_Child (Get_Button (Combo)));

      Gdk_New (Tmp_Gc, Get_Window (Combo));
      Set_Foreground (Tmp_Gc, Color);
      Draw_Rectangle (Get_Pixmap (Pixmap), Tmp_Gc, True, 5, 20, 16, 4);
      Draw (Pixmap, Full_Area);
      Unref (Tmp_Gc);
   end Color_Changed;

end General_Preferences_Pkg;
