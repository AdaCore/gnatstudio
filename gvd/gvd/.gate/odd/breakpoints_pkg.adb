with Glib; use Glib;
with Gtk; use Gtk;
with Gdk.Types;       use Gdk.Types;
with Gtk.Widget;      use Gtk.Widget;
with Gtk.Enums;       use Gtk.Enums;
with Gtkada.Handlers; use Gtkada.Handlers;
with Callbacks_Odd; use Callbacks_Odd;
with Odd_Intl; use Odd_Intl;
with Breakpoints_Pkg.Callbacks; use Breakpoints_Pkg.Callbacks;

package body Breakpoints_Pkg is

procedure Gtk_New (Breakpoints : out Breakpoints_Access) is
begin
   Breakpoints := new Breakpoints_Record;
   Breakpoints_Pkg.Initialize (Breakpoints);
end Gtk_New;

procedure Initialize (Breakpoints : access Breakpoints_Record'Class) is
   pragma Suppress (All_Checks);
   Vbox15_Group : Widget_SList.GSList;
   File_Combo_Items : String_List.Glist;
   Line_Spin_Adj : Gtk_Adjustment;
   Subprogram_Combo_Items : String_List.Glist;
   Address_Combo_Items : String_List.Glist;
   Regexp_Combo_Items : String_List.Glist;
   Watchpoint_Type_Items : String_List.Glist;
   Exception_Name_Items : String_List.Glist;
   Vbox9_Group : Widget_SList.GSList;

begin
   Gtk.Window.Initialize (Breakpoints, Window_Toplevel);
   Set_Title (Breakpoints, -"Breakpoints");
   Set_Policy (Breakpoints, False, True, True);
   Set_Position (Breakpoints, Win_Pos_Center);
   Set_Modal (Breakpoints, False);
   Return_Callback.Connect
     (Breakpoints, "delete_event", On_Breakpoints_Delete_Event'Access);
   Return_Callback.Connect
     (Breakpoints, "key_press_event", On_Breakpoints_Key_Press_Event'Access);

   Gtk_New_Hbox (Breakpoints.Main_Box, False, 0);
   Add (Breakpoints, Breakpoints.Main_Box);

   Gtk_New_Vbox (Breakpoints.Vbox1, False, 4);
   Pack_Start (Breakpoints.Main_Box, Breakpoints.Vbox1, True, True, 0);

   Gtk_New (Breakpoints.Notebook1);
   Set_Scrollable (Breakpoints.Notebook1, False);
   Set_Show_Border (Breakpoints.Notebook1, True);
   Set_Show_Tabs (Breakpoints.Notebook1, True);
   Set_Tab_Hborder (Breakpoints.Notebook1, 2);
   Set_Tab_Vborder (Breakpoints.Notebook1, 2);
   Set_Tab_Pos (Breakpoints.Notebook1, Pos_Top);
   Pack_Start (Breakpoints.Vbox1, Breakpoints.Notebook1, False, True, 0);

   Gtk_New_Hbox (Breakpoints.Hbox2, False, 0);
   Add (Breakpoints.Notebook1, Breakpoints.Hbox2);

   Gtk_New_Vbox (Breakpoints.Vbox2, False, 0);
   Set_Border_Width (Breakpoints.Vbox2, 7);
   Pack_Start (Breakpoints.Hbox2, Breakpoints.Vbox2, True, True, 0);

   Gtk_New (Breakpoints.Frame12);
   Set_Shadow_Type (Breakpoints.Frame12, Shadow_Etched_In);
   Pack_Start (Breakpoints.Vbox2, Breakpoints.Frame12, True, True, 0);

   Gtk_New_Vbox (Breakpoints.Vbox15, False, 0);
   Set_Border_Width (Breakpoints.Vbox15, 3);
   Add (Breakpoints.Frame12, Breakpoints.Vbox15);

   Gtk_New (Breakpoints.Location_Selected, Vbox15_Group, -"Source location");
   Vbox15_Group := Group (Breakpoints.Location_Selected);
   Set_Active (Breakpoints.Location_Selected, False);
   Pack_Start (Breakpoints.Vbox15, Breakpoints.Location_Selected, False, False, 0);
   Widget_Callback.Object_Connect
     (Breakpoints.Location_Selected, "toggled",
      Widget_Callback.To_Marshaller (On_Location_Selected_Toggled'Access), Breakpoints);

   Gtk_New
     (Breakpoints.Alignment5, 0.5, 0.5, 0.88, 
      0.88);
   Pack_Start (Breakpoints.Vbox15, Breakpoints.Alignment5, False, False, 1);

   Gtk_New_Hbox (Breakpoints.Hbox5, False, 0);
   Add (Breakpoints.Alignment5, Breakpoints.Hbox5);

   Gtk_New (Breakpoints.Label61, -("File:"));
   Set_Alignment (Breakpoints.Label61, 0.5, 0.5);
   Set_Padding (Breakpoints.Label61, 5, 0);
   Set_Justify (Breakpoints.Label61, Justify_Center);
   Set_Line_Wrap (Breakpoints.Label61, False);
   Pack_Start (Breakpoints.Hbox5, Breakpoints.Label61, False, False, 0);

   Gtk_New (Breakpoints.File_Combo);
   Set_Case_Sensitive (Breakpoints.File_Combo, False);
   Set_Use_Arrows (Breakpoints.File_Combo, True);
   Set_Use_Arrows_Always (Breakpoints.File_Combo, False);
   String_List.Append (File_Combo_Items, -"");
   Combo.Set_Popdown_Strings (Breakpoints.File_Combo, File_Combo_Items);
   Free_String_List (File_Combo_Items);
   Pack_Start (Breakpoints.Hbox5, Breakpoints.File_Combo, True, True, 0);

   Breakpoints.Combo_Entry5 := Get_Entry (Breakpoints.File_Combo);
   Set_Editable (Breakpoints.Combo_Entry5, True);
   Set_Max_Length (Breakpoints.Combo_Entry5, 0);
   Set_Text (Breakpoints.Combo_Entry5, -"");
   Set_Visibility (Breakpoints.Combo_Entry5, True);

   Gtk_New (Breakpoints.Label62, -("Line:"));
   Set_Alignment (Breakpoints.Label62, 1.0, 0.5);
   Set_Padding (Breakpoints.Label62, 8, 0);
   Set_Justify (Breakpoints.Label62, Justify_Center);
   Set_Line_Wrap (Breakpoints.Label62, False);
   Pack_Start (Breakpoints.Hbox5, Breakpoints.Label62, False, False, 0);

   Gtk_New (Line_Spin_Adj, 1.0, 0.0, 1e+08.0, 1.0, 10.0, 10.0);
   Gtk_New (Breakpoints.Line_Spin, Line_Spin_Adj, 1.0, 0);
   Set_Numeric (Breakpoints.Line_Spin, False);
   Set_Snap_To_Ticks (Breakpoints.Line_Spin, False);
   Set_Update_Policy (Breakpoints.Line_Spin, Update_Always);
   Set_Value (Breakpoints.Line_Spin, 1.0);
   Set_Wrap (Breakpoints.Line_Spin, False);
   Pack_Start (Breakpoints.Hbox5, Breakpoints.Line_Spin, True, True, 0);

   Gtk_New (Breakpoints.Subprogram_Selected, Vbox15_Group, -"Subprogram Name");
   Vbox15_Group := Group (Breakpoints.Subprogram_Selected);
   Set_Active (Breakpoints.Subprogram_Selected, False);
   Pack_Start (Breakpoints.Vbox15, Breakpoints.Subprogram_Selected, False, False, 0);
   Widget_Callback.Object_Connect
     (Breakpoints.Subprogram_Selected, "toggled",
      Widget_Callback.To_Marshaller (On_Subprogam_Selected_Toggled'Access), Breakpoints);

   Gtk_New
     (Breakpoints.Alignment6, 0.5, 0.5, 0.88, 
      0.88);
   Pack_Start (Breakpoints.Vbox15, Breakpoints.Alignment6, True, True, 0);

   Gtk_New (Breakpoints.Subprogram_Combo);
   Set_Case_Sensitive (Breakpoints.Subprogram_Combo, False);
   Set_Use_Arrows (Breakpoints.Subprogram_Combo, True);
   Set_Use_Arrows_Always (Breakpoints.Subprogram_Combo, False);
   String_List.Append (Subprogram_Combo_Items, -"");
   Combo.Set_Popdown_Strings (Breakpoints.Subprogram_Combo, Subprogram_Combo_Items);
   Free_String_List (Subprogram_Combo_Items);
   Set_Sensitive (Breakpoints.Subprogram_Combo, False);
   Add (Breakpoints.Alignment6, Breakpoints.Subprogram_Combo);

   Breakpoints.Entry20 := Get_Entry (Breakpoints.Subprogram_Combo);
   Set_Editable (Breakpoints.Entry20, True);
   Set_Max_Length (Breakpoints.Entry20, 0);
   Set_Text (Breakpoints.Entry20, -"");
   Set_Visibility (Breakpoints.Entry20, True);

   Gtk_New (Breakpoints.Address_Selected, Vbox15_Group, -"Address");
   Vbox15_Group := Group (Breakpoints.Address_Selected);
   Set_Active (Breakpoints.Address_Selected, False);
   Pack_Start (Breakpoints.Vbox15, Breakpoints.Address_Selected, False, False, 0);
   Widget_Callback.Object_Connect
     (Breakpoints.Address_Selected, "toggled",
      Widget_Callback.To_Marshaller (On_Address_Selected_Toggled'Access), Breakpoints);

   Gtk_New
     (Breakpoints.Alignment7, 0.5, 0.5, 0.88, 
      0.88);
   Pack_Start (Breakpoints.Vbox15, Breakpoints.Alignment7, True, True, 0);

   Gtk_New (Breakpoints.Address_Combo);
   Set_Case_Sensitive (Breakpoints.Address_Combo, False);
   Set_Use_Arrows (Breakpoints.Address_Combo, True);
   Set_Use_Arrows_Always (Breakpoints.Address_Combo, False);
   String_List.Append (Address_Combo_Items, -"");
   Combo.Set_Popdown_Strings (Breakpoints.Address_Combo, Address_Combo_Items);
   Free_String_List (Address_Combo_Items);
   Set_Sensitive (Breakpoints.Address_Combo, False);
   Add (Breakpoints.Alignment7, Breakpoints.Address_Combo);

   Breakpoints.Entry21 := Get_Entry (Breakpoints.Address_Combo);
   Set_Editable (Breakpoints.Entry21, True);
   Set_Max_Length (Breakpoints.Entry21, 0);
   Set_Text (Breakpoints.Entry21, -"");
   Set_Visibility (Breakpoints.Entry21, True);

   Gtk_New (Breakpoints.Regexp_Selected, Vbox15_Group, -"Regular expression");
   Vbox15_Group := Group (Breakpoints.Regexp_Selected);
   Set_Active (Breakpoints.Regexp_Selected, False);
   Pack_Start (Breakpoints.Vbox15, Breakpoints.Regexp_Selected, False, False, 0);
   Widget_Callback.Object_Connect
     (Breakpoints.Regexp_Selected, "toggled",
      Widget_Callback.To_Marshaller (On_Regexp_Selected_Toggled'Access), Breakpoints);

   Gtk_New
     (Breakpoints.Alignment8, 0.5, 0.5, 0.88, 
      0.88);
   Pack_Start (Breakpoints.Vbox15, Breakpoints.Alignment8, True, True, 0);

   Gtk_New (Breakpoints.Regexp_Combo);
   Set_Case_Sensitive (Breakpoints.Regexp_Combo, False);
   Set_Use_Arrows (Breakpoints.Regexp_Combo, True);
   Set_Use_Arrows_Always (Breakpoints.Regexp_Combo, False);
   String_List.Append (Regexp_Combo_Items, -"");
   Combo.Set_Popdown_Strings (Breakpoints.Regexp_Combo, Regexp_Combo_Items);
   Free_String_List (Regexp_Combo_Items);
   Set_Sensitive (Breakpoints.Regexp_Combo, False);
   Add (Breakpoints.Alignment8, Breakpoints.Regexp_Combo);

   Breakpoints.Entry22 := Get_Entry (Breakpoints.Regexp_Combo);
   Set_Editable (Breakpoints.Entry22, True);
   Set_Max_Length (Breakpoints.Entry22, 0);
   Set_Text (Breakpoints.Entry22, -"");
   Set_Visibility (Breakpoints.Entry22, True);

   Gtk_New (Breakpoints.Temporary_Location, -"Temporary breakpoint");
   Set_Active (Breakpoints.Temporary_Location, False);
   Pack_Start (Breakpoints.Vbox2, Breakpoints.Temporary_Location, False, False, 5);

   Gtk_New_Vseparator (Breakpoints.Vseparator1);
   Pack_Start (Breakpoints.Hbox2, Breakpoints.Vseparator1, False, False, 0);

   Gtk_New (Breakpoints.Vbuttonbox2);
   Set_Spacing (Breakpoints.Vbuttonbox2, 10);
   Set_Layout (Breakpoints.Vbuttonbox2, Buttonbox_Start);
   Set_Child_Size (Breakpoints.Vbuttonbox2, 85, 27);
   Set_Child_Ipadding (Breakpoints.Vbuttonbox2, 7, 0);
   Pack_Start (Breakpoints.Hbox2, Breakpoints.Vbuttonbox2, False, False, 0);

   Gtk_New (Breakpoints.Add_Location, -"Add");
   Set_Relief (Breakpoints.Add_Location, Relief_Normal);
   Set_Flags (Breakpoints.Add_Location, Can_Default);
   Widget_Callback.Object_Connect
     (Breakpoints.Add_Location, "clicked",
      Widget_Callback.To_Marshaller (On_Add_Location_Clicked'Access), Breakpoints);
   Add (Breakpoints.Vbuttonbox2, Breakpoints.Add_Location);

   Gtk_New (Breakpoints.Location, -("Location"));
   Set_Alignment (Breakpoints.Location, 0.5, 0.5);
   Set_Padding (Breakpoints.Location, 0, 0);
   Set_Justify (Breakpoints.Location, Justify_Center);
   Set_Line_Wrap (Breakpoints.Location, False);
   Set_Tab (Breakpoints.Notebook1, 0, Breakpoints.Location);

   Gtk_New_Hbox (Breakpoints.Hbox3, False, 0);
   Add (Breakpoints.Notebook1, Breakpoints.Hbox3);

   Gtk_New_Vbox (Breakpoints.Vbox7, False, 0);
   Set_Border_Width (Breakpoints.Vbox7, 7);
   Pack_Start (Breakpoints.Hbox3, Breakpoints.Vbox7, True, True, 0);

   Gtk_New (Breakpoints.Label9, -("Break when the variable:"));
   Set_Alignment (Breakpoints.Label9, 0.0, 0.5);
   Set_Padding (Breakpoints.Label9, 0, 0);
   Set_Justify (Breakpoints.Label9, Justify_Left);
   Set_Line_Wrap (Breakpoints.Label9, False);
   Pack_Start (Breakpoints.Vbox7, Breakpoints.Label9, False, False, 5);

   Gtk_New (Breakpoints.Watchpoint_Name);
   Set_Editable (Breakpoints.Watchpoint_Name, True);
   Set_Max_Length (Breakpoints.Watchpoint_Name, 0);
   Set_Text (Breakpoints.Watchpoint_Name, -"");
   Set_Visibility (Breakpoints.Watchpoint_Name, True);
   Pack_Start (Breakpoints.Vbox7, Breakpoints.Watchpoint_Name, False, False, 0);

   Gtk_New (Breakpoints.Label10, -("is"));
   Set_Alignment (Breakpoints.Label10, 0.0, 0.5);
   Set_Padding (Breakpoints.Label10, 0, 0);
   Set_Justify (Breakpoints.Label10, Justify_Left);
   Set_Line_Wrap (Breakpoints.Label10, False);
   Pack_Start (Breakpoints.Vbox7, Breakpoints.Label10, False, False, 5);

   Gtk_New (Breakpoints.Watchpoint_Type);
   Set_Case_Sensitive (Breakpoints.Watchpoint_Type, False);
   Set_Use_Arrows (Breakpoints.Watchpoint_Type, True);
   Set_Use_Arrows_Always (Breakpoints.Watchpoint_Type, False);
   String_List.Append (Watchpoint_Type_Items, -"written");
   String_List.Append (Watchpoint_Type_Items, -"read");
   String_List.Append (Watchpoint_Type_Items, -"read or written");
   Combo.Set_Popdown_Strings (Breakpoints.Watchpoint_Type, Watchpoint_Type_Items);
   Free_String_List (Watchpoint_Type_Items);
   Pack_Start (Breakpoints.Vbox7, Breakpoints.Watchpoint_Type, False, False, 0);

   Breakpoints.Combo_Entry3 := Get_Entry (Breakpoints.Watchpoint_Type);
   Set_Editable (Breakpoints.Combo_Entry3, False);
   Set_Max_Length (Breakpoints.Combo_Entry3, 0);
   Set_Text (Breakpoints.Combo_Entry3, -"read");
   Set_Visibility (Breakpoints.Combo_Entry3, True);

   Gtk_New_Vseparator (Breakpoints.Vseparator2);
   Pack_Start (Breakpoints.Hbox3, Breakpoints.Vseparator2, False, False, 0);

   Gtk_New (Breakpoints.Vbuttonbox3);
   Set_Spacing (Breakpoints.Vbuttonbox3, 10);
   Set_Layout (Breakpoints.Vbuttonbox3, Buttonbox_Start);
   Set_Child_Size (Breakpoints.Vbuttonbox3, 85, 27);
   Set_Child_Ipadding (Breakpoints.Vbuttonbox3, 7, 0);
   Pack_Start (Breakpoints.Hbox3, Breakpoints.Vbuttonbox3, False, True, 0);

   Gtk_New (Breakpoints.Add_Watchpoint, -"Add");
   Set_Relief (Breakpoints.Add_Watchpoint, Relief_Normal);
   Set_Flags (Breakpoints.Add_Watchpoint, Can_Default);
   Widget_Callback.Object_Connect
     (Breakpoints.Add_Watchpoint, "clicked",
      Widget_Callback.To_Marshaller (On_Add_Watchpoint_Clicked'Access), Breakpoints);
   Add (Breakpoints.Vbuttonbox3, Breakpoints.Add_Watchpoint);

   Gtk_New (Breakpoints.Watchpoint, -("Variable"));
   Set_Alignment (Breakpoints.Watchpoint, 0.5, 0.5);
   Set_Padding (Breakpoints.Watchpoint, 0, 0);
   Set_Justify (Breakpoints.Watchpoint, Justify_Center);
   Set_Line_Wrap (Breakpoints.Watchpoint, False);
   Set_Tab (Breakpoints.Notebook1, 1, Breakpoints.Watchpoint);

   Gtk_New_Hbox (Breakpoints.Hbox4, False, 0);
   Add (Breakpoints.Notebook1, Breakpoints.Hbox4);

   Gtk_New_Vbox (Breakpoints.Vbox8, False, 0);
   Set_Border_Width (Breakpoints.Vbox8, 7);
   Pack_Start (Breakpoints.Hbox4, Breakpoints.Vbox8, True, True, 0);

   Gtk_New (Breakpoints.Label11, -("Break on exception:"));
   Set_Alignment (Breakpoints.Label11, 0.0, 0.5);
   Set_Padding (Breakpoints.Label11, 0, 0);
   Set_Justify (Breakpoints.Label11, Justify_Left);
   Set_Line_Wrap (Breakpoints.Label11, False);
   Pack_Start (Breakpoints.Vbox8, Breakpoints.Label11, False, False, 0);

   Gtk_New_Hbox (Breakpoints.Hbox14, False, 8);
   Pack_Start (Breakpoints.Vbox8, Breakpoints.Hbox14, False, True, 0);

   Gtk_New (Breakpoints.Exception_Name);
   Set_Case_Sensitive (Breakpoints.Exception_Name, False);
   Set_Use_Arrows (Breakpoints.Exception_Name, True);
   Set_Use_Arrows_Always (Breakpoints.Exception_Name, False);
   String_List.Append (Exception_Name_Items, -"");
   Combo.Set_Popdown_Strings (Breakpoints.Exception_Name, Exception_Name_Items);
   Free_String_List (Exception_Name_Items);
   Pack_Start (Breakpoints.Hbox14, Breakpoints.Exception_Name, True, True, 0);

   Breakpoints.Combo_Entry25 := Get_Entry (Breakpoints.Exception_Name);
   Set_Editable (Breakpoints.Combo_Entry25, True);
   Set_Max_Length (Breakpoints.Combo_Entry25, 0);
   Set_Text (Breakpoints.Combo_Entry25, -"All exceptions");
   Set_Visibility (Breakpoints.Combo_Entry25, True);

   Gtk_New (Breakpoints.Load_Exception_List, -"Load List");
   Set_Relief (Breakpoints.Load_Exception_List, Relief_Normal);
   Pack_Start (Breakpoints.Hbox14, Breakpoints.Load_Exception_List, False, False, 0);
   Widget_Callback.Object_Connect
     (Breakpoints.Load_Exception_List, "clicked",
      Widget_Callback.To_Marshaller (On_Load_Exception_List_Clicked'Access), Breakpoints);

   Gtk_New (Breakpoints.Temporary_Exception, -"Temporary breakpoint");
   Set_Active (Breakpoints.Temporary_Exception, False);
   Pack_Start (Breakpoints.Vbox8, Breakpoints.Temporary_Exception, False, False, 0);

   Gtk_New (Breakpoints.Frame4, -"Action");
   Set_Shadow_Type (Breakpoints.Frame4, Shadow_Etched_In);
   Pack_Start (Breakpoints.Vbox8, Breakpoints.Frame4, False, False, 7);

   Gtk_New_Vbox (Breakpoints.Vbox9, False, 0);
   Set_Border_Width (Breakpoints.Vbox9, 7);
   Add (Breakpoints.Frame4, Breakpoints.Vbox9);

   Gtk_New (Breakpoints.Stop_Always_Exception, Vbox9_Group, -"Stop always");
   Vbox9_Group := Group (Breakpoints.Stop_Always_Exception);
   Set_Active (Breakpoints.Stop_Always_Exception, True);
   Pack_Start (Breakpoints.Vbox9, Breakpoints.Stop_Always_Exception, False, False, 0);

   Gtk_New (Breakpoints.Stop_Not_Handled_Exception, Vbox9_Group, -"Stop if not handled");
   Vbox9_Group := Group (Breakpoints.Stop_Not_Handled_Exception);
   Set_Active (Breakpoints.Stop_Not_Handled_Exception, False);
   Pack_Start (Breakpoints.Vbox9, Breakpoints.Stop_Not_Handled_Exception, False, False, 0);

   Gtk_New_Vseparator (Breakpoints.Vseparator3);
   Pack_Start (Breakpoints.Hbox4, Breakpoints.Vseparator3, False, False, 0);

   Gtk_New (Breakpoints.Vbuttonbox4);
   Set_Spacing (Breakpoints.Vbuttonbox4, 10);
   Set_Layout (Breakpoints.Vbuttonbox4, Buttonbox_Start);
   Set_Child_Size (Breakpoints.Vbuttonbox4, 85, 27);
   Set_Child_Ipadding (Breakpoints.Vbuttonbox4, 7, 0);
   Pack_Start (Breakpoints.Hbox4, Breakpoints.Vbuttonbox4, False, False, 0);

   Gtk_New (Breakpoints.Add_Exception, -"Add");
   Set_Relief (Breakpoints.Add_Exception, Relief_Normal);
   Set_Flags (Breakpoints.Add_Exception, Can_Default);
   Widget_Callback.Object_Connect
     (Breakpoints.Add_Exception, "clicked",
      Widget_Callback.To_Marshaller (On_Add_Exception_Clicked'Access), Breakpoints);
   Add (Breakpoints.Vbuttonbox4, Breakpoints.Add_Exception);

   Gtk_New (Breakpoints.Except, -("Exception"));
   Set_Alignment (Breakpoints.Except, 0.5, 0.5);
   Set_Padding (Breakpoints.Except, 0, 0);
   Set_Justify (Breakpoints.Except, Justify_Center);
   Set_Line_Wrap (Breakpoints.Except, False);
   Set_Tab (Breakpoints.Notebook1, 2, Breakpoints.Except);

   Gtk_New (Breakpoints.Frame11, -"Breakpoints");
   Set_Shadow_Type (Breakpoints.Frame11, Shadow_Etched_In);
   Pack_Start (Breakpoints.Vbox1, Breakpoints.Frame11, True, True, 0);

   Gtk_New_Vbox (Breakpoints.Vbox16, False, 0);
   Add (Breakpoints.Frame11, Breakpoints.Vbox16);

   Gtk_New (Breakpoints.Label72, -("Click in the 'Enb' column to change the status"));
   Set_Alignment (Breakpoints.Label72, 0.05, 0.5);
   Set_Padding (Breakpoints.Label72, 0, 0);
   Set_Justify (Breakpoints.Label72, Justify_Center);
   Set_Line_Wrap (Breakpoints.Label72, False);
   Pack_Start (Breakpoints.Vbox16, Breakpoints.Label72, False, False, 0);

   Gtk_New (Breakpoints.Scrolledwindow2);
   Set_Policy (Breakpoints.Scrolledwindow2, Policy_Automatic, Policy_Always);
   Pack_Start (Breakpoints.Vbox16, Breakpoints.Scrolledwindow2, True, True, 0);

   Gtk_New (Breakpoints.Breakpoint_List, 2);
   Set_Selection_Mode (Breakpoints.Breakpoint_List, Selection_Single);
   Set_Shadow_Type (Breakpoints.Breakpoint_List, Shadow_In);
   Set_Show_Titles (Breakpoints.Breakpoint_List, True);
   Set_Column_Width (Breakpoints.Breakpoint_List, 0, 80);
   Set_Column_Width (Breakpoints.Breakpoint_List, 1, 186);
   Add (Breakpoints.Scrolledwindow2, Breakpoints.Breakpoint_List);

   Gtk_New (Breakpoints.Label15, -("label15"));
   Set_Alignment (Breakpoints.Label15, 0.5, 0.5);
   Set_Padding (Breakpoints.Label15, 0, 0);
   Set_Justify (Breakpoints.Label15, Justify_Center);
   Set_Line_Wrap (Breakpoints.Label15, False);
   Set_Column_Widget (Breakpoints.Breakpoint_List, 0, Breakpoints.Label15);

   Gtk_New (Breakpoints.Label16, -("label16"));
   Set_Alignment (Breakpoints.Label16, 0.5, 0.5);
   Set_Padding (Breakpoints.Label16, 0, 0);
   Set_Justify (Breakpoints.Label16, Justify_Center);
   Set_Line_Wrap (Breakpoints.Label16, False);
   Set_Column_Widget (Breakpoints.Breakpoint_List, 1, Breakpoints.Label16);

   Gtk_New (Breakpoints.Hbuttonbox8);
   Set_Spacing (Breakpoints.Hbuttonbox8, 30);
   Set_Layout (Breakpoints.Hbuttonbox8, Buttonbox_Spread);
   Set_Child_Size (Breakpoints.Hbuttonbox8, 85, 27);
   Set_Child_Ipadding (Breakpoints.Hbuttonbox8, 7, 0);
   Pack_Start (Breakpoints.Vbox16, Breakpoints.Hbuttonbox8, False, False, 0);

   Gtk_New (Breakpoints.Remove, -"Remove");
   Set_Relief (Breakpoints.Remove, Relief_Normal);
   Set_Flags (Breakpoints.Remove, Can_Default);
   Widget_Callback.Object_Connect
     (Breakpoints.Remove, "clicked",
      Widget_Callback.To_Marshaller (On_Remove_Clicked'Access), Breakpoints);
   Add (Breakpoints.Hbuttonbox8, Breakpoints.Remove);

   Gtk_New (Breakpoints.View, -"View");
   Set_Relief (Breakpoints.View, Relief_Normal);
   Set_Flags (Breakpoints.View, Can_Default);
   Widget_Callback.Object_Connect
     (Breakpoints.View, "clicked",
      Widget_Callback.To_Marshaller (On_View_Clicked'Access), Breakpoints);
   Add (Breakpoints.Hbuttonbox8, Breakpoints.View);

   Gtk_New (Breakpoints.Advanced_Location, -"Advanced");
   Set_Relief (Breakpoints.Advanced_Location, Relief_Normal);
   Set_Flags (Breakpoints.Advanced_Location, Can_Default);
   Widget_Callback.Object_Connect
     (Breakpoints.Advanced_Location, "clicked",
      Widget_Callback.To_Marshaller (On_Advanced_Location_Clicked'Access), Breakpoints);
   Add (Breakpoints.Hbuttonbox8, Breakpoints.Advanced_Location);

   Gtk_New (Breakpoints.Ok_Button, -"Close");
   Set_Relief (Breakpoints.Ok_Button, Relief_Normal);
   Set_Flags (Breakpoints.Ok_Button, Can_Default);
   Widget_Callback.Object_Connect
     (Breakpoints.Ok_Button, "clicked",
      Widget_Callback.To_Marshaller (On_Ok_Bp_Clicked'Access), Breakpoints);
   Add (Breakpoints.Hbuttonbox8, Breakpoints.Ok_Button);

end Initialize;

end Breakpoints_Pkg;
