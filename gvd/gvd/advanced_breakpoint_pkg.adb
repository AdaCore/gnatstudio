-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                      Copyright (C) 2000-2003                      --
--                              ACT-Europe                           --
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
with Gtk.Stock; use Gtk.Stock;
with Gtk.Widget;      use Gtk.Widget;
with Gtk.Enums;       use Gtk.Enums;
with Gtkada.Handlers; use Gtkada.Handlers;
with Gtk.Adjustment; use Gtk.Adjustment;
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
   pragma Suppress (All_Checks);
   Condition_Combo_Items : String_List.Glist;
   Ignore_Count_Combo_Adj : Gtk_Adjustment;
   Vbox30_Group : Widget_SList.GSList;
   Vbox31_Group : Widget_SList.GSList;

begin
   Gtk.Window.Initialize (Advanced_Breakpoint, Window_Toplevel);
   Set_Title (Advanced_Breakpoint, -"Advanced Breakpoint Settings");
   Set_Policy (Advanced_Breakpoint, False, False, False);
   Set_Position (Advanced_Breakpoint, Win_Pos_Center);
   Set_Modal (Advanced_Breakpoint, True);

   Gtk_New_Vbox (Advanced_Breakpoint.Vbox34, False, 0);
   Add (Advanced_Breakpoint, Advanced_Breakpoint.Vbox34);

   Gtk_New (Advanced_Breakpoint.Main_Notebook);
   Set_Scrollable (Advanced_Breakpoint.Main_Notebook, False);
   Set_Show_Border (Advanced_Breakpoint.Main_Notebook, True);
   Set_Show_Tabs (Advanced_Breakpoint.Main_Notebook, True);
   Set_Tab_Hborder (Advanced_Breakpoint.Main_Notebook, 2);
   Set_Tab_Vborder (Advanced_Breakpoint.Main_Notebook, 2);
   Set_Tab_Pos (Advanced_Breakpoint.Main_Notebook, Pos_Top);
   Pack_Start (Advanced_Breakpoint.Vbox34, Advanced_Breakpoint.Main_Notebook, True, True, 0);

   Gtk_New_Vbox (Advanced_Breakpoint.Condition_Box, False, 0);
   Add (Advanced_Breakpoint.Main_Notebook, Advanced_Breakpoint.Condition_Box);

   Gtk_New (Advanced_Breakpoint.Condition_Frame, -"Condition");
   Set_Shadow_Type (Advanced_Breakpoint.Condition_Frame, Shadow_Etched_In);
   Pack_Start (Advanced_Breakpoint.Condition_Box, Advanced_Breakpoint.Condition_Frame, False, True, 0);

   Gtk_New_Vbox (Advanced_Breakpoint.Vbox32, False, 0);
   Set_Border_Width (Advanced_Breakpoint.Vbox32, 7);
   Add (Advanced_Breakpoint.Condition_Frame, Advanced_Breakpoint.Vbox32);

   Gtk_New (Advanced_Breakpoint.Label104, -("Break only when following condition is true:"));
   Set_Alignment (Advanced_Breakpoint.Label104, 0.0, 0.5);
   Set_Padding (Advanced_Breakpoint.Label104, 0, 0);
   Set_Justify (Advanced_Breakpoint.Label104, Justify_Left);
   Set_Line_Wrap (Advanced_Breakpoint.Label104, False);
   Pack_Start (Advanced_Breakpoint.Vbox32, Advanced_Breakpoint.Label104, False, False, 0);

   Gtk_New (Advanced_Breakpoint.Condition_Combo);
   Set_Case_Sensitive (Advanced_Breakpoint.Condition_Combo, False);
   Set_Use_Arrows (Advanced_Breakpoint.Condition_Combo, True);
   Set_Use_Arrows_Always (Advanced_Breakpoint.Condition_Combo, False);
   String_List.Append (Condition_Combo_Items, -"");
   Combo.Set_Popdown_Strings (Advanced_Breakpoint.Condition_Combo, Condition_Combo_Items);
   Free_String_List (Condition_Combo_Items);
   Pack_Start (Advanced_Breakpoint.Vbox32, Advanced_Breakpoint.Condition_Combo, False, False, 0);

   Advanced_Breakpoint.Combo_Entry2 := Get_Entry (Advanced_Breakpoint.Condition_Combo);
   Set_Editable (Advanced_Breakpoint.Combo_Entry2, True);
   Set_Max_Length (Advanced_Breakpoint.Combo_Entry2, 0);
   Set_Text (Advanced_Breakpoint.Combo_Entry2, -"");
   Set_Visibility (Advanced_Breakpoint.Combo_Entry2, True);

   Gtk_New (Advanced_Breakpoint.Ignore_Count_Frame, -"Ignore");
   Set_Shadow_Type (Advanced_Breakpoint.Ignore_Count_Frame, Shadow_Etched_In);
   Pack_Start (Advanced_Breakpoint.Condition_Box, Advanced_Breakpoint.Ignore_Count_Frame, False, True, 0);

   Gtk_New_Vbox (Advanced_Breakpoint.Vbox33, False, 0);
   Set_Border_Width (Advanced_Breakpoint.Vbox33, 7);
   Add (Advanced_Breakpoint.Ignore_Count_Frame, Advanced_Breakpoint.Vbox33);

   Gtk_New (Advanced_Breakpoint.Label105, -("Enter the number of times to skip before stopping:"));
   Set_Alignment (Advanced_Breakpoint.Label105, 0.0, 0.5);
   Set_Padding (Advanced_Breakpoint.Label105, 0, 0);
   Set_Justify (Advanced_Breakpoint.Label105, Justify_Center);
   Set_Line_Wrap (Advanced_Breakpoint.Label105, False);
   Pack_Start (Advanced_Breakpoint.Vbox33, Advanced_Breakpoint.Label105, False, False, 0);

   Gtk_New (Ignore_Count_Combo_Adj, 0.0, 0.0, 10000.0, 1.0, 10.0, 10.0);
   Gtk_New (Advanced_Breakpoint.Ignore_Count_Combo, Ignore_Count_Combo_Adj, 1.0, 0);
   Set_Numeric (Advanced_Breakpoint.Ignore_Count_Combo, False);
   Set_Snap_To_Ticks (Advanced_Breakpoint.Ignore_Count_Combo, True);
   Set_Update_Policy (Advanced_Breakpoint.Ignore_Count_Combo, Update_Always);
   Set_Value (Advanced_Breakpoint.Ignore_Count_Combo, 0.0);
   Set_Wrap (Advanced_Breakpoint.Ignore_Count_Combo, False);
   Pack_Start (Advanced_Breakpoint.Vbox33, Advanced_Breakpoint.Ignore_Count_Combo, False, False, 0);

   Gtk_New (Advanced_Breakpoint.Command_Frame, -"Commands");
   Set_Shadow_Type (Advanced_Breakpoint.Command_Frame, Shadow_Etched_In);
   Pack_Start (Advanced_Breakpoint.Condition_Box, Advanced_Breakpoint.Command_Frame, False, True, 0);

   Gtk_New_Vbox (Advanced_Breakpoint.Vbox35, False, 0);
   Set_Border_Width (Advanced_Breakpoint.Vbox35, 7);
   Add (Advanced_Breakpoint.Command_Frame, Advanced_Breakpoint.Vbox35);

   Gtk_New (Advanced_Breakpoint.Label106, -("Enter commands to execute when program stops:"));
   Set_Alignment (Advanced_Breakpoint.Label106, 0.0, 0.5);
   Set_Padding (Advanced_Breakpoint.Label106, 0, 0);
   Set_Justify (Advanced_Breakpoint.Label106, Justify_Left);
   Set_Line_Wrap (Advanced_Breakpoint.Label106, False);
   Pack_Start (Advanced_Breakpoint.Vbox35, Advanced_Breakpoint.Label106, False, False, 0);

   Gtk_New (Advanced_Breakpoint.Scrolledwindow12);
   Set_Policy (Advanced_Breakpoint.Scrolledwindow12, Policy_Never, Policy_Always);
   Pack_Start (Advanced_Breakpoint.Vbox35, Advanced_Breakpoint.Scrolledwindow12, False, False, 0);

   Gtk_New (Advanced_Breakpoint.Command_Descr);
   Set_Editable (Advanced_Breakpoint.Command_Descr, True);
   Add (Advanced_Breakpoint.Scrolledwindow12, Advanced_Breakpoint.Command_Descr);

   Gtk_New (Advanced_Breakpoint.Hbuttonbox12);
   Set_Spacing (Advanced_Breakpoint.Hbuttonbox12, 30);
   Set_Layout (Advanced_Breakpoint.Hbuttonbox12, Buttonbox_Spread);
   Set_Child_Size (Advanced_Breakpoint.Hbuttonbox12, 85, 27);
   Set_Child_Ipadding (Advanced_Breakpoint.Hbuttonbox12, 7, 0);
   Pack_Start (Advanced_Breakpoint.Vbox35, Advanced_Breakpoint.Hbuttonbox12, False, False, 0);

   Gtk_New (Advanced_Breakpoint.Record_Button, -"Record");
   Set_Relief (Advanced_Breakpoint.Record_Button, Relief_Normal);
   Set_Flags (Advanced_Breakpoint.Record_Button, Can_Default);
   Button_Callback.Connect
     (Advanced_Breakpoint.Record_Button, "clicked",
      Button_Callback.To_Marshaller (On_Start_Record_Clicked'Access));
   Add (Advanced_Breakpoint.Hbuttonbox12, Advanced_Breakpoint.Record_Button);

   Gtk_New (Advanced_Breakpoint.End_Button, -"Stop recording");
   Set_Relief (Advanced_Breakpoint.End_Button, Relief_Normal);
   Set_Flags (Advanced_Breakpoint.End_Button, Can_Default);
   Button_Callback.Connect
     (Advanced_Breakpoint.End_Button, "clicked",
      Button_Callback.To_Marshaller (On_Stop_Record_Clicked'Access));
   Add (Advanced_Breakpoint.Hbuttonbox12, Advanced_Breakpoint.End_Button);

   Gtk_New (Advanced_Breakpoint.Label102, -("Conditions"));
   Set_Alignment (Advanced_Breakpoint.Label102, 0.5, 0.5);
   Set_Padding (Advanced_Breakpoint.Label102, 0, 0);
   Set_Justify (Advanced_Breakpoint.Label102, Justify_Center);
   Set_Line_Wrap (Advanced_Breakpoint.Label102, False);
   Set_Tab (Advanced_Breakpoint.Main_Notebook, 0, Advanced_Breakpoint.Label102);
   Set_Flags (Advanced_Breakpoint.Label102, Can_Default);

   Gtk_New_Vbox (Advanced_Breakpoint.Scope_Box, False, 0);
   Add (Advanced_Breakpoint.Main_Notebook, Advanced_Breakpoint.Scope_Box);

   Gtk_New (Advanced_Breakpoint.Frame13, -"Scope: tasks that can hit the breakpoint");
   Set_Border_Width (Advanced_Breakpoint.Frame13, 3);
   Set_Shadow_Type (Advanced_Breakpoint.Frame13, Shadow_Etched_In);
   Pack_Start (Advanced_Breakpoint.Scope_Box, Advanced_Breakpoint.Frame13, False, True, 5);

   Gtk_New_Vbox (Advanced_Breakpoint.Vbox30, True, 3);
   Add (Advanced_Breakpoint.Frame13, Advanced_Breakpoint.Vbox30);

   Gtk_New (Advanced_Breakpoint.Scope_Task, Vbox30_Group, -"Running task");
   Vbox30_Group := Group (Advanced_Breakpoint.Scope_Task);
   Set_Active (Advanced_Breakpoint.Scope_Task, True);
   Pack_Start (Advanced_Breakpoint.Vbox30, Advanced_Breakpoint.Scope_Task, False, False, 0);

   Gtk_New (Advanced_Breakpoint.Scope_Pd, Vbox30_Group, -"All tasks in current Protection Domain");
   Vbox30_Group := Group (Advanced_Breakpoint.Scope_Pd);
   Set_Active (Advanced_Breakpoint.Scope_Pd, False);
   Pack_Start (Advanced_Breakpoint.Vbox30, Advanced_Breakpoint.Scope_Pd, False, False, 0);

   Gtk_New (Advanced_Breakpoint.Scope_Any, Vbox30_Group, -"Any task");
   Vbox30_Group := Group (Advanced_Breakpoint.Scope_Any);
   Set_Active (Advanced_Breakpoint.Scope_Any, False);
   Pack_Start (Advanced_Breakpoint.Vbox30, Advanced_Breakpoint.Scope_Any, False, False, 0);

   Gtk_New (Advanced_Breakpoint.Frame14, -"Action: tasks to break");
   Set_Border_Width (Advanced_Breakpoint.Frame14, 3);
   Set_Shadow_Type (Advanced_Breakpoint.Frame14, Shadow_Etched_In);
   Pack_Start (Advanced_Breakpoint.Scope_Box, Advanced_Breakpoint.Frame14, False, True, 0);

   Gtk_New_Vbox (Advanced_Breakpoint.Vbox31, True, 3);
   Add (Advanced_Breakpoint.Frame14, Advanced_Breakpoint.Vbox31);

   Gtk_New (Advanced_Breakpoint.Action_Task, Vbox31_Group, -"Task that hits the breakpoint");
   Vbox31_Group := Group (Advanced_Breakpoint.Action_Task);
   Set_Active (Advanced_Breakpoint.Action_Task, True);
   Pack_Start (Advanced_Breakpoint.Vbox31, Advanced_Breakpoint.Action_Task, False, False, 0);

   Gtk_New (Advanced_Breakpoint.Action_Pd, Vbox31_Group, -"All tasks in current Protection Domain");
   Vbox31_Group := Group (Advanced_Breakpoint.Action_Pd);
   Set_Active (Advanced_Breakpoint.Action_Pd, False);
   Pack_Start (Advanced_Breakpoint.Vbox31, Advanced_Breakpoint.Action_Pd, False, False, 0);

   Gtk_New (Advanced_Breakpoint.Action_All, Vbox31_Group, -"All breakable tasks");
   Vbox31_Group := Group (Advanced_Breakpoint.Action_All);
   Set_Active (Advanced_Breakpoint.Action_All, False);
   Pack_Start (Advanced_Breakpoint.Vbox31, Advanced_Breakpoint.Action_All, False, False, 0);

   Gtk_New (Advanced_Breakpoint.Set_Default, -"Set these values as session's default");
   Set_Border_Width (Advanced_Breakpoint.Set_Default, 2);
   Set_Active (Advanced_Breakpoint.Set_Default, False);
   Pack_Start (Advanced_Breakpoint.Scope_Box, Advanced_Breakpoint.Set_Default, False, False, 2);

   Gtk_New (Advanced_Breakpoint.Scope, -("Scope/Action"));
   Set_Alignment (Advanced_Breakpoint.Scope, 0.5, 0.5);
   Set_Padding (Advanced_Breakpoint.Scope, 0, 0);
   Set_Justify (Advanced_Breakpoint.Scope, Justify_Center);
   Set_Line_Wrap (Advanced_Breakpoint.Scope, False);
   Set_Tab (Advanced_Breakpoint.Main_Notebook, 1, Advanced_Breakpoint.Scope);

   Gtk_New (Advanced_Breakpoint.Hbuttonbox13);
   Set_Spacing (Advanced_Breakpoint.Hbuttonbox13, 30);
   Set_Layout (Advanced_Breakpoint.Hbuttonbox13, Buttonbox_Spread);
   Set_Child_Size (Advanced_Breakpoint.Hbuttonbox13, 85, 27);
   Set_Child_Ipadding (Advanced_Breakpoint.Hbuttonbox13, 7, 0);
   Pack_Start (Advanced_Breakpoint.Vbox34, Advanced_Breakpoint.Hbuttonbox13, True, True, 0);

   Gtk_New_From_Stock (Advanced_Breakpoint.Apply, Stock_Apply);
   Set_Relief (Advanced_Breakpoint.Apply, Relief_Normal);
   Set_Flags (Advanced_Breakpoint.Apply, Can_Default);
   Widget_Callback.Object_Connect
     (Advanced_Breakpoint.Apply, "clicked",
      Widget_Callback.To_Marshaller (On_Apply_Clicked'Access), Advanced_Breakpoint);
   Add (Advanced_Breakpoint.Hbuttonbox13, Advanced_Breakpoint.Apply);

   Gtk_New_From_Stock (Advanced_Breakpoint.Close, Stock_Close);
   Set_Relief (Advanced_Breakpoint.Close, Relief_Normal);
   Set_Flags (Advanced_Breakpoint.Close, Can_Default);
   Widget_Callback.Object_Connect
     (Advanced_Breakpoint.Close, "clicked",
      Widget_Callback.To_Marshaller (On_Close_Clicked'Access), Advanced_Breakpoint);
   Add (Advanced_Breakpoint.Hbuttonbox13, Advanced_Breakpoint.Close);

end Initialize;

end Advanced_Breakpoint_Pkg;
