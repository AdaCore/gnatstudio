-----------------------------------------------------------------------
--                 Odd - The Other Display Debugger                  --
--                                                                   --
--                         Copyright (C) 2000                        --
--                 Emmanuel Briot and Arnaud Charlet                 --
--                                                                   --
-- Odd is free  software;  you can redistribute it and/or modify  it --
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
with Gtk.Adjustment; use Gtk.Adjustment;
with Gtk.Widget;      use Gtk.Widget;
with Gtk.Enums;       use Gtk.Enums;
with Odd_Intl; use Odd_Intl;
with Breakpoints_Pkg.Callbacks; use Breakpoints_Pkg.Callbacks;
with Gdk.Color;       use Gdk.Color;
with Gdk.Pixmap;      use Gdk.Pixmap;
with Odd.Pixmaps;     use Odd.Pixmaps;
with Gtkada.Handlers; use Gtkada.Handlers;
with Gtk.Style;       use Gtk.Style;
with Odd.Code_Editors; use Odd.Code_Editors;
with Odd.Strings;     use Odd.Strings;
with Odd.Types;       use Odd.Types;
with Debugger;        use Debugger;

package body Breakpoints_Pkg is

pragma Suppress (All_Checks);
--  Checks are expensive (in code size) in this unit, and not needed,
--  since the following code is generated automatically.

procedure Gtk_New (Breakpoints : out Breakpoints_Access) is
begin
   Breakpoints := new Breakpoints_Record;
   Breakpoints_Pkg.Initialize (Breakpoints);
end Gtk_New;

procedure Initialize (Breakpoints : access Breakpoints_Record'Class) is
   Vbox15_Group : Widget_SList.GSList;
   File_Combo_Items : String_List.Glist;
   Line_Spin_Adj : Gtk_Adjustment;
   Subprogram_Combo_Items : String_List.Glist;
   --  Address_Combo_Items : String_List.Glist;
   Regexp_Combo_Items : String_List.Glist;
   Watchpoint_Type_Items : String_List.Glist;
   Exception_Name_Items : String_List.Glist;
   Vbox9_Group : Widget_SList.GSList;

begin
   Gtk.Window.Initialize (Breakpoints, Window_Toplevel);
   Set_Title (Breakpoints, -"Breakpoints");
   Set_Policy (Breakpoints, True, True, False);
   Set_Position (Breakpoints, Win_Pos_Center);
   Set_Modal (Breakpoints, False);
   Set_Default_Size (Breakpoints, 430, 460);

   Gtk_New_Vbox (Breakpoints.Vbox1, False, 4);
   Add (Breakpoints, Breakpoints.Vbox1);

   Gtk_New (Breakpoints.Notebook1);
   Pack_Start (Breakpoints.Vbox1, Breakpoints.Notebook1, False, True, 0);
   Set_Scrollable (Breakpoints.Notebook1, False);
   Set_Show_Border (Breakpoints.Notebook1, True);
   Set_Show_Tabs (Breakpoints.Notebook1, True);
   Set_Tab_Hborder (Breakpoints.Notebook1, 2);
   Set_Tab_Vborder (Breakpoints.Notebook1, 2);
   Set_Tab_Pos (Breakpoints.Notebook1, Pos_Top);

   Gtk_New_Hbox (Breakpoints.Hbox2, False, 0);
   Add (Breakpoints.Notebook1, Breakpoints.Hbox2);

   Gtk_New_Vbox (Breakpoints.Vbox2, False, 0);
   Pack_Start (Breakpoints.Hbox2, Breakpoints.Vbox2, True, True, 0);
   Set_Border_Width (Breakpoints.Vbox2, 7);

   Gtk_New (Breakpoints.Frame12);
   Pack_Start (Breakpoints.Vbox2, Breakpoints.Frame12, True, True, 0);
   Set_Shadow_Type (Breakpoints.Frame12, Shadow_Etched_In);

   Gtk_New_Vbox (Breakpoints.Vbox15, False, 0);
   Add (Breakpoints.Frame12, Breakpoints.Vbox15);
   Set_Border_Width (Breakpoints.Vbox15, 3);

   Gtk_New (Breakpoints.Location_Selected, Vbox15_Group, -"Source location");
   Vbox15_Group := Group (Breakpoints.Location_Selected);
   Pack_Start (Breakpoints.Vbox15, Breakpoints.Location_Selected, False, False, 0);
   Widget_Callback.Object_Connect
     (Breakpoints.Location_Selected, "toggled",
      Widget_Callback.To_Marshaller (On_Location_Selected_Toggled'Access), Breakpoints);
   Set_Active (Breakpoints.Location_Selected, False);

   Gtk_New
     (Breakpoints.Alignment5, 0.5, 0.5, 0.88,
      0.88);
   Pack_Start (Breakpoints.Vbox15, Breakpoints.Alignment5, False, False, 1);

   Gtk_New_Hbox (Breakpoints.Hbox5, False, 0);
   Add (Breakpoints.Alignment5, Breakpoints.Hbox5);

   Gtk_New (Breakpoints.Label61, -("File:"));
   Pack_Start (Breakpoints.Hbox5, Breakpoints.Label61, False, False, 0);
   Set_Alignment (Breakpoints.Label61, 0.5, 0.5);
   Set_Padding (Breakpoints.Label61, 5, 0);
   Set_Justify (Breakpoints.Label61, Justify_Center);
   Set_Line_Wrap (Breakpoints.Label61, False);

   Gtk_New (Breakpoints.File_Combo);
   Pack_Start (Breakpoints.Hbox5, Breakpoints.File_Combo, True, True, 0);
   Set_Case_Sensitive (Breakpoints.File_Combo, False);
   Set_Use_Arrows (Breakpoints.File_Combo, True);
   Set_Use_Arrows_Always (Breakpoints.File_Combo, False);
   String_List.Append (File_Combo_Items, -"");
   Combo.Set_Popdown_Strings (Breakpoints.File_Combo, File_Combo_Items);
   Free_String_List (File_Combo_Items);

   Breakpoints.Combo_Entry5 := Get_Entry (Breakpoints.File_Combo);
   Set_Editable (Breakpoints.Combo_Entry5, True);
   Set_Max_Length (Breakpoints.Combo_Entry5, 0);
   Set_Text (Breakpoints.Combo_Entry5, -"");
   Set_Visibility (Breakpoints.Combo_Entry5, True);

   Gtk_New (Breakpoints.Label62, -("Line:"));
   Pack_Start (Breakpoints.Hbox5, Breakpoints.Label62, False, False, 0);
   Set_Alignment (Breakpoints.Label62, 1.0, 0.5);
   Set_Padding (Breakpoints.Label62, 8, 0);
   Set_Justify (Breakpoints.Label62, Justify_Center);
   Set_Line_Wrap (Breakpoints.Label62, False);

   Gtk_New (Line_Spin_Adj, 1.0, 0.0, 100.0, 1.0, 10.0, 10.0);
   Gtk_New (Breakpoints.Line_Spin, Line_Spin_Adj, 1.0, 0);
   Pack_Start (Breakpoints.Hbox5, Breakpoints.Line_Spin, True, True, 0);
   Set_Numeric (Breakpoints.Line_Spin, False);
   Set_Snap_To_Ticks (Breakpoints.Line_Spin, False);
   Set_Update_Policy (Breakpoints.Line_Spin, Update_Always);
   Set_Value (Breakpoints.Line_Spin, 1.0);
   Set_Wrap (Breakpoints.Line_Spin, False);

   Gtk_New (Breakpoints.Subprogram_Selected, Vbox15_Group, -"Subprogram name");
   Vbox15_Group := Group (Breakpoints.Subprogram_Selected);
   Pack_Start (Breakpoints.Vbox15, Breakpoints.Subprogram_Selected, False, False, 0);
   Widget_Callback.Object_Connect
     (Breakpoints.Subprogram_Selected, "toggled",
      Widget_Callback.To_Marshaller (On_Subprogam_Selected_Toggled'Access), Breakpoints);
   Set_Active (Breakpoints.Subprogram_Selected, False);

   Gtk_New
     (Breakpoints.Alignment6, 0.5, 0.5, 0.88,
      0.88);
   Pack_Start (Breakpoints.Vbox15, Breakpoints.Alignment6, True, True, 0);

   Gtk_New (Breakpoints.Subprogram_Combo);
   Set_Sensitive (Breakpoints.Subprogram_Combo, False);
   Add (Breakpoints.Alignment6, Breakpoints.Subprogram_Combo);
   Set_Case_Sensitive (Breakpoints.Subprogram_Combo, False);
   Set_Use_Arrows (Breakpoints.Subprogram_Combo, True);
   Set_Use_Arrows_Always (Breakpoints.Subprogram_Combo, False);
   String_List.Append (Subprogram_Combo_Items, -"");
   Combo.Set_Popdown_Strings (Breakpoints.Subprogram_Combo, Subprogram_Combo_Items);
   Free_String_List (Subprogram_Combo_Items);

   Breakpoints.Entry20 := Get_Entry (Breakpoints.Subprogram_Combo);
   Set_Editable (Breakpoints.Entry20, True);
   Set_Max_Length (Breakpoints.Entry20, 0);
   Set_Text (Breakpoints.Entry20, -"");
   Set_Visibility (Breakpoints.Entry20, True);

   Gtk_New (Breakpoints.Address_Selected, Vbox15_Group, -"Address");
   Vbox15_Group := Group (Breakpoints.Address_Selected);
   Pack_Start (Breakpoints.Vbox15, Breakpoints.Address_Selected, False, False, 0);
   Widget_Callback.Object_Connect
     (Breakpoints.Address_Selected, "toggled",
      Widget_Callback.To_Marshaller (On_Address_Selected_Toggled'Access), Breakpoints);
   Set_Active (Breakpoints.Address_Selected, False);

   Gtk_New
     (Breakpoints.Alignment7, 0.5, 0.5, 0.88,
      0.88);
   Pack_Start (Breakpoints.Vbox15, Breakpoints.Alignment7, True, True, 0);

   Gtk_New (Breakpoints.Address_Combo);
   Set_Sensitive (Breakpoints.Address_Combo, False);
   Add (Breakpoints.Alignment7, Breakpoints.Address_Combo);
   Set_Case_Sensitive (Breakpoints.Address_Combo, False);
   Set_Use_Arrows (Breakpoints.Address_Combo, True);
   Set_Use_Arrows_Always (Breakpoints.Address_Combo, False);
   --  String_List.Append (Address_Combo_Items, -"");
   --  Combo.Set_Popdown_Strings (Breakpoints.Address_Combo, Address_Combo_Items);
   --  Free_String_List (Address_Combo_Items);

   Breakpoints.Entry21 := Get_Entry (Breakpoints.Address_Combo);
   Set_Editable (Breakpoints.Entry21, True);
   Set_Max_Length (Breakpoints.Entry21, 0);
   Set_Text (Breakpoints.Entry21, -"");
   Set_Visibility (Breakpoints.Entry21, True);

   Gtk_New (Breakpoints.Regexp_Selected, Vbox15_Group, -"Regular expression");
   Vbox15_Group := Group (Breakpoints.Regexp_Selected);
   Pack_Start (Breakpoints.Vbox15, Breakpoints.Regexp_Selected, False, False, 0);
   Widget_Callback.Object_Connect
     (Breakpoints.Regexp_Selected, "toggled",
      Widget_Callback.To_Marshaller (On_Regexp_Selected_Toggled'Access), Breakpoints);
   Set_Active (Breakpoints.Regexp_Selected, False);

   Gtk_New
     (Breakpoints.Alignment8, 0.5, 0.5, 0.88,
      0.88);
   Pack_Start (Breakpoints.Vbox15, Breakpoints.Alignment8, True, True, 0);

   Gtk_New (Breakpoints.Regexp_Combo);
   Set_Sensitive (Breakpoints.Regexp_Combo, False);
   Add (Breakpoints.Alignment8, Breakpoints.Regexp_Combo);
   Set_Case_Sensitive (Breakpoints.Regexp_Combo, False);
   Set_Use_Arrows (Breakpoints.Regexp_Combo, True);
   Set_Use_Arrows_Always (Breakpoints.Regexp_Combo, False);
   String_List.Append (Regexp_Combo_Items, -"");
   Combo.Set_Popdown_Strings (Breakpoints.Regexp_Combo, Regexp_Combo_Items);
   Free_String_List (Regexp_Combo_Items);

   Breakpoints.Entry22 := Get_Entry (Breakpoints.Regexp_Combo);
   Set_Editable (Breakpoints.Entry22, True);
   Set_Max_Length (Breakpoints.Entry22, 0);
   Set_Text (Breakpoints.Entry22, -"");
   Set_Visibility (Breakpoints.Entry22, True);

   Gtk_New (Breakpoints.Temporary_Location, -"Temporary breakpoint");
   Pack_Start (Breakpoints.Vbox2, Breakpoints.Temporary_Location, False, False, 5);
   Set_Active (Breakpoints.Temporary_Location, False);

   Gtk_New_Vseparator (Breakpoints.Vseparator1);
   Pack_Start (Breakpoints.Hbox2, Breakpoints.Vseparator1, False, False, 0);

   Gtk_New (Breakpoints.Vbuttonbox2);
   Pack_Start (Breakpoints.Hbox2, Breakpoints.Vbuttonbox2, False, False, 0);
   Set_Spacing (Breakpoints.Vbuttonbox2, 10);
   Set_Layout (Breakpoints.Vbuttonbox2, Buttonbox_Start);
   Set_Child_Size (Breakpoints.Vbuttonbox2, 85, 27);
   Set_Child_Ipadding (Breakpoints.Vbuttonbox2, 7, 0);

   Gtk_New (Breakpoints.Add_Location, -"Add");
   Set_Flags (Breakpoints.Add_Location, Can_Default);
   Widget_Callback.Object_Connect
     (Breakpoints.Add_Location, "clicked",
      Widget_Callback.To_Marshaller (On_Add_Location_Clicked'Access), Breakpoints);
   Add (Breakpoints.Vbuttonbox2, Breakpoints.Add_Location);

   Gtk_New (Breakpoints.Advanced_Location, -"Advanced...");
   Set_Flags (Breakpoints.Advanced_Location, Can_Default);
   Widget_Callback.Object_Connect
     (Breakpoints.Advanced_Location, "clicked",
      Widget_Callback.To_Marshaller (On_Advanced_Location_Clicked'Access), Breakpoints);
   Add (Breakpoints.Vbuttonbox2, Breakpoints.Advanced_Location);

   Gtk_New (Breakpoints.Location, -("Location"));
   Set_Alignment (Breakpoints.Location, 0.5, 0.5);
   Set_Padding (Breakpoints.Location, 0, 0);
   Set_Justify (Breakpoints.Location, Justify_Center);
   Set_Line_Wrap (Breakpoints.Location, False);
   Set_Tab (Breakpoints.Notebook1, 0, Breakpoints.Location);

   Gtk_New_Hbox (Breakpoints.Hbox3, False, 0);
   Add (Breakpoints.Notebook1, Breakpoints.Hbox3);

   Gtk_New_Vbox (Breakpoints.Vbox7, False, 0);
   Pack_Start (Breakpoints.Hbox3, Breakpoints.Vbox7, True, True, 0);
   Set_Border_Width (Breakpoints.Vbox7, 7);

   Gtk_New (Breakpoints.Label9, -("Break when the variable:"));
   Pack_Start (Breakpoints.Vbox7, Breakpoints.Label9, False, False, 5);
   Set_Alignment (Breakpoints.Label9, 0.0, 0.5);
   Set_Padding (Breakpoints.Label9, 0, 0);
   Set_Justify (Breakpoints.Label9, Justify_Left);
   Set_Line_Wrap (Breakpoints.Label9, False);

   Gtk_New (Breakpoints.Watchpoint_Name);
   Pack_Start (Breakpoints.Vbox7, Breakpoints.Watchpoint_Name, False, False, 0);
   Set_Editable (Breakpoints.Watchpoint_Name, True);
   Set_Max_Length (Breakpoints.Watchpoint_Name, 0);
   Set_Text (Breakpoints.Watchpoint_Name, -"");
   Set_Visibility (Breakpoints.Watchpoint_Name, True);

   Gtk_New (Breakpoints.Label10, -("is"));
   Pack_Start (Breakpoints.Vbox7, Breakpoints.Label10, False, False, 5);
   Set_Alignment (Breakpoints.Label10, 0.0, 0.5);
   Set_Padding (Breakpoints.Label10, 0, 0);
   Set_Justify (Breakpoints.Label10, Justify_Left);
   Set_Line_Wrap (Breakpoints.Label10, False);

   Gtk_New (Breakpoints.Watchpoint_Type);
   Pack_Start (Breakpoints.Vbox7, Breakpoints.Watchpoint_Type, False, False, 0);
   Set_Case_Sensitive (Breakpoints.Watchpoint_Type, False);
   Set_Use_Arrows (Breakpoints.Watchpoint_Type, True);
   Set_Use_Arrows_Always (Breakpoints.Watchpoint_Type, False);
   String_List.Append (Watchpoint_Type_Items, -"written");
   String_List.Append (Watchpoint_Type_Items, -"read");
   String_List.Append (Watchpoint_Type_Items, -"read or written");
   Combo.Set_Popdown_Strings (Breakpoints.Watchpoint_Type, Watchpoint_Type_Items);
   Free_String_List (Watchpoint_Type_Items);

   Breakpoints.Combo_Entry3 := Get_Entry (Breakpoints.Watchpoint_Type);
   Set_Editable (Breakpoints.Combo_Entry3, False);
   Set_Max_Length (Breakpoints.Combo_Entry3, 0);
   Set_Text (Breakpoints.Combo_Entry3, -"read");
   Set_Visibility (Breakpoints.Combo_Entry3, True);

   Gtk_New_Vseparator (Breakpoints.Vseparator2);
   Pack_Start (Breakpoints.Hbox3, Breakpoints.Vseparator2, False, False, 0);

   Gtk_New (Breakpoints.Vbuttonbox3);
   Pack_Start (Breakpoints.Hbox3, Breakpoints.Vbuttonbox3, False, True, 0);
   Set_Spacing (Breakpoints.Vbuttonbox3, 10);
   Set_Layout (Breakpoints.Vbuttonbox3, Buttonbox_Start);
   Set_Child_Size (Breakpoints.Vbuttonbox3, 85, 27);
   Set_Child_Ipadding (Breakpoints.Vbuttonbox3, 7, 0);

   Gtk_New (Breakpoints.Add_Watchpoint, -"Add");
   Set_Flags (Breakpoints.Add_Watchpoint, Can_Default);
   Widget_Callback.Object_Connect
     (Breakpoints.Add_Watchpoint, "clicked",
      Widget_Callback.To_Marshaller (On_Add_Watchpoint_Clicked'Access), Breakpoints);
   Add (Breakpoints.Vbuttonbox3, Breakpoints.Add_Watchpoint);

   Gtk_New (Breakpoints.Advanced_Watchpoint, -"Advanced...");
   Set_Flags (Breakpoints.Advanced_Watchpoint, Can_Default);
   Widget_Callback.Object_Connect
     (Breakpoints.Advanced_Watchpoint, "clicked",
      Widget_Callback.To_Marshaller (On_Advanced_Watchpoint_Clicked'Access), Breakpoints);
   Add (Breakpoints.Vbuttonbox3, Breakpoints.Advanced_Watchpoint);

   Gtk_New (Breakpoints.Watchpoint, -("WatchPoint"));
   Set_Alignment (Breakpoints.Watchpoint, 0.5, 0.5);
   Set_Padding (Breakpoints.Watchpoint, 0, 0);
   Set_Justify (Breakpoints.Watchpoint, Justify_Center);
   Set_Line_Wrap (Breakpoints.Watchpoint, False);
   Set_Tab (Breakpoints.Notebook1, 1, Breakpoints.Watchpoint);

   Gtk_New_Hbox (Breakpoints.Hbox4, False, 0);
   Add (Breakpoints.Notebook1, Breakpoints.Hbox4);

   Gtk_New_Vbox (Breakpoints.Vbox8, False, 0);
   Pack_Start (Breakpoints.Hbox4, Breakpoints.Vbox8, True, True, 0);
   Set_Border_Width (Breakpoints.Vbox8, 7);

   Gtk_New (Breakpoints.Label11, -("Break on exception:"));
   Pack_Start (Breakpoints.Vbox8, Breakpoints.Label11, False, False, 0);
   Set_Alignment (Breakpoints.Label11, 0.0, 0.5);
   Set_Padding (Breakpoints.Label11, 0, 0);
   Set_Justify (Breakpoints.Label11, Justify_Left);
   Set_Line_Wrap (Breakpoints.Label11, False);

   Gtk_New (Breakpoints.Exception_Name);
   Pack_Start (Breakpoints.Vbox8, Breakpoints.Exception_Name, False, False, 0);
   Set_Case_Sensitive (Breakpoints.Exception_Name, False);
   Set_Use_Arrows (Breakpoints.Exception_Name, True);
   Set_Use_Arrows_Always (Breakpoints.Exception_Name, False);
   String_List.Append (Exception_Name_Items, -"All exceptions");
   Combo.Set_Popdown_Strings (Breakpoints.Exception_Name, Exception_Name_Items);
   Free_String_List (Exception_Name_Items);

   Breakpoints.Combo_Entry4 := Get_Entry (Breakpoints.Exception_Name);
   Set_Editable (Breakpoints.Combo_Entry4, True);
   Set_Max_Length (Breakpoints.Combo_Entry4, 0);
   Set_Text (Breakpoints.Combo_Entry4, -"All exceptions");
   Set_Visibility (Breakpoints.Combo_Entry4, True);

   Gtk_New (Breakpoints.Temporary_Exception, -"Temporary breakpoint");
   Pack_Start (Breakpoints.Vbox8, Breakpoints.Temporary_Exception, False, False, 0);
   Set_Active (Breakpoints.Temporary_Exception, False);

   Gtk_New (Breakpoints.Frame4, -"Action");
   Pack_Start (Breakpoints.Vbox8, Breakpoints.Frame4, False, False, 7);
   Set_Shadow_Type (Breakpoints.Frame4, Shadow_Etched_In);

   Gtk_New_Vbox (Breakpoints.Vbox9, False, 0);
   Add (Breakpoints.Frame4, Breakpoints.Vbox9);
   Set_Border_Width (Breakpoints.Vbox9, 7);

   Gtk_New (Breakpoints.Stop_Always_Exception, Vbox9_Group, -"Stop always");
   Vbox9_Group := Group (Breakpoints.Stop_Always_Exception);
   Pack_Start (Breakpoints.Vbox9, Breakpoints.Stop_Always_Exception, False, False, 0);
   Set_Active (Breakpoints.Stop_Always_Exception, True);

   Gtk_New (Breakpoints.Stop_Not_Handled_Exception, Vbox9_Group, -"Stop if not handled");
   Vbox9_Group := Group (Breakpoints.Stop_Not_Handled_Exception);
   Pack_Start (Breakpoints.Vbox9, Breakpoints.Stop_Not_Handled_Exception, False, False, 0);
   Set_Active (Breakpoints.Stop_Not_Handled_Exception, False);

   Gtk_New_Vseparator (Breakpoints.Vseparator3);
   Pack_Start (Breakpoints.Hbox4, Breakpoints.Vseparator3, False, False, 0);

   Gtk_New (Breakpoints.Vbuttonbox4);
   Pack_Start (Breakpoints.Hbox4, Breakpoints.Vbuttonbox4, False, False, 0);
   Set_Spacing (Breakpoints.Vbuttonbox4, 10);
   Set_Layout (Breakpoints.Vbuttonbox4, Buttonbox_Start);
   Set_Child_Size (Breakpoints.Vbuttonbox4, 85, 27);
   Set_Child_Ipadding (Breakpoints.Vbuttonbox4, 7, 0);

   Gtk_New (Breakpoints.Add_Exception, -"Add");
   Set_Flags (Breakpoints.Add_Exception, Can_Default);
   Widget_Callback.Object_Connect
     (Breakpoints.Add_Exception, "clicked",
      Widget_Callback.To_Marshaller (On_Add_Exception_Clicked'Access), Breakpoints);
   Add (Breakpoints.Vbuttonbox4, Breakpoints.Add_Exception);

   Gtk_New (Breakpoints.Advanced_Exception, -"Advanced...");
   Set_Flags (Breakpoints.Advanced_Exception, Can_Default);
   Widget_Callback.Object_Connect
     (Breakpoints.Advanced_Exception, "clicked",
      Widget_Callback.To_Marshaller (On_Advanced_Exception_Clicked'Access), Breakpoints);
   Add (Breakpoints.Vbuttonbox4, Breakpoints.Advanced_Exception);

   Gtk_New (Breakpoints.Except, -("Exception"));
   Set_Alignment (Breakpoints.Except, 0.5, 0.5);
   Set_Padding (Breakpoints.Except, 0, 0);
   Set_Justify (Breakpoints.Except, Justify_Center);
   Set_Line_Wrap (Breakpoints.Except, False);
   Set_Tab (Breakpoints.Notebook1, 2, Breakpoints.Except);

   Gtk_New (Breakpoints.Frame11, -"Breakpoints");
   Pack_Start (Breakpoints.Vbox1, Breakpoints.Frame11, True, True, 0);
   Set_Shadow_Type (Breakpoints.Frame11, Shadow_Etched_In);

   Gtk_New_Vbox (Breakpoints.Vbox16, False, 0);
   Add (Breakpoints.Frame11, Breakpoints.Vbox16);

   Gtk_New (Breakpoints.Scrolledwindow2);
   Pack_Start (Breakpoints.Vbox16, Breakpoints.Scrolledwindow2, True, True, 0);
   Set_Policy (Breakpoints.Scrolledwindow2, Policy_Automatic, Policy_Always);

   Gtk_New (Breakpoints.Clist1, 7);
   Add (Breakpoints.Scrolledwindow2, Breakpoints.Clist1);
   Set_Selection_Mode (Breakpoints.Clist1, Selection_Single);
   Set_Shadow_Type (Breakpoints.Clist1, Shadow_In);
   Set_Show_Titles (Breakpoints.Clist1, True);
   --  Set_Column_Width (Breakpoints.Clist1, 0, 80);
   --  Set_Column_Width (Breakpoints.Clist1, 1, 80);

   Set_Column_Title (Breakpoints.Clist1, 0, -"Num");
   Set_Column_Title (Breakpoints.Clist1, 1, -"Enb");
   Set_Column_Title (Breakpoints.Clist1, 2, -"Type");
   Set_Column_Title (Breakpoints.Clist1, 3, -"Disp");
   Set_Column_Title (Breakpoints.Clist1, 4, -"File/Variable");
   Set_Column_Title (Breakpoints.Clist1, 5, -"Line");
   Set_Column_Title (Breakpoints.Clist1, 6, -"Exception");
   Set_Column_Title (Breakpoints.Clist1, 7, -"Action");

   --  Gtk_New (Breakpoints.Label15, -("label15"));
   --  Set_Alignment (Breakpoints.Label15, 0.5, 0.5);
   --  Set_Padding (Breakpoints.Label15, 0, 0);
   --  Set_Justify (Breakpoints.Label15, Justify_Center);
   --  Set_Line_Wrap (Breakpoints.Label15, False);
   --  Set_Column_Widget (Breakpoints.Clist1, 0, Breakpoints.Label15);

   --  Gtk_New (Breakpoints.Label16, -("label16"));
   --  Set_Alignment (Breakpoints.Label16, 0.5, 0.5);
   --  Set_Padding (Breakpoints.Label16, 0, 0);
   --  Set_Justify (Breakpoints.Label16, Justify_Center);
   --  Set_Line_Wrap (Breakpoints.Label16, False);
   --  Set_Column_Widget (Breakpoints.Clist1, 1, Breakpoints.Label16);

   Gtk_New (Breakpoints.Hbuttonbox8);
   Pack_Start (Breakpoints.Vbox16, Breakpoints.Hbuttonbox8, False, False, 0);
   Set_Spacing (Breakpoints.Hbuttonbox8, 30);
   Set_Layout (Breakpoints.Hbuttonbox8, Buttonbox_Spread);
   Set_Child_Size (Breakpoints.Hbuttonbox8, 85, 27);
   Set_Child_Ipadding (Breakpoints.Hbuttonbox8, 7, 0);

   Gtk_New (Breakpoints.Remove, -"Remove");
   Set_Flags (Breakpoints.Remove, Can_Default);
   Widget_Callback.Object_Connect
     (Breakpoints.Remove, "clicked",
      Widget_Callback.To_Marshaller (On_Remove_Clicked'Access), Breakpoints);
   Add (Breakpoints.Hbuttonbox8, Breakpoints.Remove);

   Gtk_New (Breakpoints.View, -"View");
   Set_Flags (Breakpoints.View, Can_Default);
   Widget_Callback.Object_Connect
     (Breakpoints.View, "clicked",
      Widget_Callback.To_Marshaller (On_View_Clicked'Access), Breakpoints);
   Add (Breakpoints.Hbuttonbox8, Breakpoints.View);

   Gtk_New (Breakpoints.Hbuttonbox4);
   Pack_Start (Breakpoints.Vbox1, Breakpoints.Hbuttonbox4, False, False, 0);
   Set_Spacing (Breakpoints.Hbuttonbox4, 30);
   Set_Layout (Breakpoints.Hbuttonbox4, Buttonbox_Spread);
   Set_Child_Size (Breakpoints.Hbuttonbox4, 85, 27);
   Set_Child_Ipadding (Breakpoints.Hbuttonbox4, 7, 0);

   Gtk_New (Breakpoints.Ok_Button, -"Close");
   Set_Flags (Breakpoints.Ok_Button, Can_Default);
   Widget_Callback.Object_Connect
     (Breakpoints.Ok_Button, "clicked",
      Widget_Callback.To_Marshaller (On_Ok_Bp_Clicked'Access), Breakpoints);
   Add (Breakpoints.Hbuttonbox4, Breakpoints.Ok_Button);

   Realize (Breakpoints);
   Create_From_Xpm_D
     (Breakpoints.Enabled_Pixmap,
      Get_Window (Breakpoints),
      Breakpoints.Enabled_Mask,
      White (Get_System),
      break_xpm);

   --  Grey background when the combo boxes are insensitive
   Set_Background_Gc
     (Get_Style (Breakpoints.Entry21),
      State_Insensitive,
      Get_Background_Gc (Get_Style (Breakpoints), State_Normal));

   --  Return in the combo boxes should activate them

   Disable_Activate (Breakpoints.File_Combo);
   Widget_Callback.Object_Connect
     (Get_Entry (Breakpoints.File_Combo), "activate",
      Widget_Callback.To_Marshaller (On_Add_Location_Clicked'Access),
      Breakpoints);
   Disable_Activate (Breakpoints.Address_Combo);
   Widget_Callback.Object_Connect
     (Get_Entry (Breakpoints.Address_Combo), "activate",
      Widget_Callback.To_Marshaller (On_Add_Location_Clicked'Access),
      Breakpoints);
   Disable_Activate (Breakpoints.Subprogram_Combo);
   Widget_Callback.Object_Connect
     (Get_Entry (Breakpoints.Subprogram_Combo), "activate",
      Widget_Callback.To_Marshaller (On_Add_Location_Clicked'Access),
      Breakpoints);
   Disable_Activate (Breakpoints.Regexp_Combo);
   Widget_Callback.Object_Connect
     (Get_Entry (Breakpoints.Regexp_Combo), "activate",
      Widget_Callback.To_Marshaller (On_Add_Location_Clicked'Access),
      Breakpoints);

   --  ??? Temporary
   Set_Sensitive (Breakpoints.Hbox3, False);
   Set_Sensitive (Breakpoints.View, False);
   Set_Sensitive (Breakpoints.Advanced_Location, False);
   Set_Sensitive (Breakpoints.Advanced_Exception, False);

end Initialize;

procedure Breakpoint_Editor
  (Editor     : in out Breakpoints_Access;
   Process    : access Odd.Process.Debugger_Process_Tab_Record'Class)
is
   Exception_Name_Items : String_List.Glist;
   Exception_Arr        : Exception_Array :=
     List_Exceptions (Process.Debugger);
begin
   if Editor = null then
      Gtk_New (Editor);

      --  ??? Else we should raise the window, in case it was hidden by some
      --  other window.
   end if;

   Editor.Process := Odd.Process.Debugger_Process_Tab (Process);
   Update_Breakpoint_List (Editor);

   --  Initialize the contents of the combo boxes

   Set_Text
     (Get_Entry (Editor.File_Combo),
      Base_File_Name (Get_Current_File (Process.Editor_Text)));

   --  List of exceptions

   if Exception_Arr'Length > 0 then
      Set_Sensitive (Editor.Hbox4, True);
      String_List.Append (Exception_Name_Items, -"All exceptions");
      for J in Exception_Arr'Range loop
         String_List.Append (Exception_Name_Items, Exception_Arr (J).Name.all);
      end loop;
      Combo.Set_Popdown_Strings
        (Editor.Exception_Name, Exception_Name_Items);
      Free_String_List (Exception_Name_Items);

   else
      Set_Sensitive (Editor.Hbox4, False);
   end if;

   Free (Exception_Arr);

   Show_All (Editor);
end Breakpoint_Editor;

end Breakpoints_Pkg;
