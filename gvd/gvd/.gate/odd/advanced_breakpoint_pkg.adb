with Glib; use Glib;
with Gtk; use Gtk;
with Gdk.Types;       use Gdk.Types;
with Gtk.Widget;      use Gtk.Widget;
with Gtk.Enums;       use Gtk.Enums;
with Gtkada.Handlers; use Gtkada.Handlers;
with Callbacks_Odd; use Callbacks_Odd;
with Odd_Intl; use Odd_Intl;
with Advanced_Breakpoint_Pkg.Callbacks; use Advanced_Breakpoint_Pkg.Callbacks;

package body Advanced_Breakpoint_Pkg is

procedure Gtk_New (Advanced_Breakpoint : out Advanced_Breakpoint_Access) is
begin
   Advanced_Breakpoint := new Advanced_Breakpoint_Record;
   Advanced_Breakpoint_Pkg.Initialize (Advanced_Breakpoint);
end Gtk_New;

procedure Initialize (Advanced_Breakpoint : access Advanced_Breakpoint_Record'Class) is
   Condition_Combo_Items : String_List.Glist;
   Ignore_Count_Combo_Adj : Gtk_Adjustment;

begin
   Gtk.Window.Initialize (Advanced_Breakpoint, Window_Toplevel);
   Set_Title (Advanced_Breakpoint, -"Advanced Breakpoint Settings");
   Set_Policy (Advanced_Breakpoint, False, True, False);
   Set_Position (Advanced_Breakpoint, Win_Pos_Center);
   Set_Modal (Advanced_Breakpoint, True);

   Gtk_New_Vbox (Advanced_Breakpoint.Vbox11, False, 0);
   Add (Advanced_Breakpoint, Advanced_Breakpoint.Vbox11);

   Gtk_New (Advanced_Breakpoint.Condition_Frame, -"Condition");
   Pack_Start (Advanced_Breakpoint.Vbox11, Advanced_Breakpoint.Condition_Frame, False, False, 0);
   Set_Shadow_Type (Advanced_Breakpoint.Condition_Frame, Shadow_Etched_In);

   Gtk_New_Vbox (Advanced_Breakpoint.Vbox5, False, 0);
   Add (Advanced_Breakpoint.Condition_Frame, Advanced_Breakpoint.Vbox5);
   Set_Border_Width (Advanced_Breakpoint.Vbox5, 7);

   Gtk_New (Advanced_Breakpoint.Label7, -("Break only when following condition is true:"));
   Pack_Start (Advanced_Breakpoint.Vbox5, Advanced_Breakpoint.Label7, False, False, 0);
   Set_Alignment (Advanced_Breakpoint.Label7, 0.0, 0.5);
   Set_Padding (Advanced_Breakpoint.Label7, 0, 0);
   Set_Justify (Advanced_Breakpoint.Label7, Justify_Left);
   Set_Line_Wrap (Advanced_Breakpoint.Label7, False);

   Gtk_New (Advanced_Breakpoint.Condition_Combo);
   Pack_Start (Advanced_Breakpoint.Vbox5, Advanced_Breakpoint.Condition_Combo, False, False, 0);
   Set_Case_Sensitive (Advanced_Breakpoint.Condition_Combo, False);
   Set_Use_Arrows (Advanced_Breakpoint.Condition_Combo, True);
   Set_Use_Arrows_Always (Advanced_Breakpoint.Condition_Combo, False);
   String_List.Append (Condition_Combo_Items, -"");
   Combo.Set_Popdown_Strings (Advanced_Breakpoint.Condition_Combo, Condition_Combo_Items);
   Free_String_List (Condition_Combo_Items);

   Advanced_Breakpoint.Combo_Entry2 := Get_Entry (Advanced_Breakpoint.Condition_Combo);
   Set_Editable (Advanced_Breakpoint.Combo_Entry2, True);
   Set_Max_Length (Advanced_Breakpoint.Combo_Entry2, 0);
   Set_Text (Advanced_Breakpoint.Combo_Entry2, -"");
   Set_Visibility (Advanced_Breakpoint.Combo_Entry2, True);

   Gtk_New (Advanced_Breakpoint.Ignore_Count_Frame, -"Ignore");
   Pack_Start (Advanced_Breakpoint.Vbox11, Advanced_Breakpoint.Ignore_Count_Frame, False, False, 0);
   Set_Shadow_Type (Advanced_Breakpoint.Ignore_Count_Frame, Shadow_Etched_In);

   Gtk_New_Vbox (Advanced_Breakpoint.Vbox6, False, 0);
   Add (Advanced_Breakpoint.Ignore_Count_Frame, Advanced_Breakpoint.Vbox6);
   Set_Border_Width (Advanced_Breakpoint.Vbox6, 7);

   Gtk_New (Advanced_Breakpoint.Label8, -("Enter the number of times to skip before stopping:"));
   Pack_Start (Advanced_Breakpoint.Vbox6, Advanced_Breakpoint.Label8, False, False, 0);
   Set_Alignment (Advanced_Breakpoint.Label8, 0.0, 0.5);
   Set_Padding (Advanced_Breakpoint.Label8, 0, 0);
   Set_Justify (Advanced_Breakpoint.Label8, Justify_Center);
   Set_Line_Wrap (Advanced_Breakpoint.Label8, False);

   Gtk_New (Ignore_Count_Combo_Adj, 0.0, 0.0, 10000.0, 1.0, 10.0, 10.0);
   Gtk_New (Advanced_Breakpoint.Ignore_Count_Combo, Ignore_Count_Combo_Adj, 1.0, 0);
   Pack_Start (Advanced_Breakpoint.Vbox6, Advanced_Breakpoint.Ignore_Count_Combo, False, False, 0);
   Set_Numeric (Advanced_Breakpoint.Ignore_Count_Combo, False);
   Set_Snap_To_Ticks (Advanced_Breakpoint.Ignore_Count_Combo, True);
   Set_Update_Policy (Advanced_Breakpoint.Ignore_Count_Combo, Update_Always);
   Set_Value (Advanced_Breakpoint.Ignore_Count_Combo, 0.0);
   Set_Wrap (Advanced_Breakpoint.Ignore_Count_Combo, False);

   Gtk_New (Advanced_Breakpoint.Command_Frame, -"Commands");
   Pack_Start (Advanced_Breakpoint.Vbox11, Advanced_Breakpoint.Command_Frame, True, True, 0);
   Set_Shadow_Type (Advanced_Breakpoint.Command_Frame, Shadow_Etched_In);

   Gtk_New_Vbox (Advanced_Breakpoint.Vbox12, False, 0);
   Add (Advanced_Breakpoint.Command_Frame, Advanced_Breakpoint.Vbox12);
   Set_Border_Width (Advanced_Breakpoint.Vbox12, 7);

   Gtk_New (Advanced_Breakpoint.Label13, -("Enter command to execute when program stops:"));
   Pack_Start (Advanced_Breakpoint.Vbox12, Advanced_Breakpoint.Label13, False, False, 0);
   Set_Alignment (Advanced_Breakpoint.Label13, 0.0, 0.5);
   Set_Padding (Advanced_Breakpoint.Label13, 0, 0);
   Set_Justify (Advanced_Breakpoint.Label13, Justify_Left);
   Set_Line_Wrap (Advanced_Breakpoint.Label13, False);

   Gtk_New (Advanced_Breakpoint.Scrolledwindow1);
   Pack_Start (Advanced_Breakpoint.Vbox12, Advanced_Breakpoint.Scrolledwindow1, False, False, 0);
   Set_Policy (Advanced_Breakpoint.Scrolledwindow1, Policy_Never, Policy_Always);

   Gtk_New (Advanced_Breakpoint.Command_Descr);
   Add (Advanced_Breakpoint.Scrolledwindow1, Advanced_Breakpoint.Command_Descr);
   Set_Editable (Advanced_Breakpoint.Command_Descr, False);

   Gtk_New (Advanced_Breakpoint.Hbuttonbox3);
   Pack_Start (Advanced_Breakpoint.Vbox12, Advanced_Breakpoint.Hbuttonbox3, False, False, 0);
   Set_Spacing (Advanced_Breakpoint.Hbuttonbox3, 30);
   Set_Layout (Advanced_Breakpoint.Hbuttonbox3, Buttonbox_Spread);
   Set_Child_Size (Advanced_Breakpoint.Hbuttonbox3, 85, 27);
   Set_Child_Ipadding (Advanced_Breakpoint.Hbuttonbox3, 7, 0);

   Gtk_New (Advanced_Breakpoint.Record_Button, -"Record");
   Set_Flags (Advanced_Breakpoint.Record_Button, Can_Default);
   Button_Callback.Connect
     (Advanced_Breakpoint.Record_Button, "clicked",
      Button_Callback.To_Marshaller (On_Start_Record_Clicked'Access));
   Add (Advanced_Breakpoint.Hbuttonbox3, Advanced_Breakpoint.Record_Button);

   Gtk_New (Advanced_Breakpoint.End_Button, -"Stop recording");
   Set_Flags (Advanced_Breakpoint.End_Button, Can_Default);
   Button_Callback.Connect
     (Advanced_Breakpoint.End_Button, "clicked",
      Button_Callback.To_Marshaller (On_Stop_Record_Clicked'Access));
   Add (Advanced_Breakpoint.Hbuttonbox3, Advanced_Breakpoint.End_Button);

   Gtk_New (Advanced_Breakpoint.Hbuttonbox5);
   Pack_Start (Advanced_Breakpoint.Vbox11, Advanced_Breakpoint.Hbuttonbox5, False, False, 0);
   Set_Spacing (Advanced_Breakpoint.Hbuttonbox5, 30);
   Set_Layout (Advanced_Breakpoint.Hbuttonbox5, Buttonbox_Spread);
   Set_Child_Size (Advanced_Breakpoint.Hbuttonbox5, 85, 27);
   Set_Child_Ipadding (Advanced_Breakpoint.Hbuttonbox5, 7, 0);

   Gtk_New (Advanced_Breakpoint.Ok_Button, -"OK");
   Set_Flags (Advanced_Breakpoint.Ok_Button, Can_Default);
   Button_Callback.Connect
     (Advanced_Breakpoint.Ok_Button, "clicked",
      Button_Callback.To_Marshaller (On_Ok_Advanced_Bp_Clicked'Access));
   Add (Advanced_Breakpoint.Hbuttonbox5, Advanced_Breakpoint.Ok_Button);

   Gtk_New (Advanced_Breakpoint.Cancel_Button, -"Cancel");
   Set_Flags (Advanced_Breakpoint.Cancel_Button, Can_Default);
   Button_Callback.Connect
     (Advanced_Breakpoint.Cancel_Button, "clicked",
      Button_Callback.To_Marshaller (On_Cancel_Advanced_Bp_Clicked'Access));
   Add (Advanced_Breakpoint.Hbuttonbox5, Advanced_Breakpoint.Cancel_Button);

end Initialize;

end Advanced_Breakpoint_Pkg;
