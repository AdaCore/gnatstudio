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

with Glib; use Glib;
with Gtk; use Gtk;
with Gdk.Types;       use Gdk.Types;
with Gtk.Widget;      use Gtk.Widget;
with Gtk.Enums;       use Gtk.Enums;
with Gtkada.Handlers; use Gtkada.Handlers;
with Callbacks_Odd; use Callbacks_Odd;
with Odd_Intl; use Odd_Intl;
with Breakpoints_Pkg.Callbacks; use Breakpoints_Pkg.Callbacks;
with Gtk.Main;

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
   Combo1_Items : String_List.Glist;
   Combo3_Items : String_List.Glist;
   Combo4_Items : String_List.Glist;
   Vbox9_Group : Widget_SList.GSList;
   Combo5_Items : String_List.Glist;

begin
   Gtk.Window.Initialize (Breakpoints, Window_Toplevel);
   Set_Title (Breakpoints, -"Breakpoints");
   Set_Policy (Breakpoints, True, True, False);
   Set_Position (Breakpoints, Win_Pos_Center);
   Set_Modal (Breakpoints, True);

   Gtk_New_Vbox (Breakpoints.Vbox1, False, 0);
   Add (Breakpoints, Breakpoints.Vbox1);

   Gtk_New (Breakpoints.Notebook1);
   Pack_Start (Breakpoints.Vbox1, Breakpoints.Notebook1, True, True, 0);
   Set_Scrollable (Breakpoints.Notebook1, False);
   Set_Show_Border (Breakpoints.Notebook1, True);
   Set_Show_Tabs (Breakpoints.Notebook1, True);
   Set_Tab_Hborder (Breakpoints.Notebook1, 2);
   Set_Tab_Vborder (Breakpoints.Notebook1, 2);
   Set_Tab_Pos (Breakpoints.Notebook1, Pos_Top);

   Gtk_New_Vbox (Breakpoints.Vbox2, False, 0);
   Add (Breakpoints.Notebook1, Breakpoints.Vbox2);
   Set_Border_Width (Breakpoints.Vbox2, 7);

   Gtk_New (Breakpoints.Label4, -("Enter line number, subprogram or address to break at:"));
   Pack_Start (Breakpoints.Vbox2, Breakpoints.Label4, False, False, 5);
   Set_Alignment (Breakpoints.Label4, 0.0, 0.5);
   Set_Padding (Breakpoints.Label4, 0, 0);
   Set_Justify (Breakpoints.Label4, Justify_Left);
   Set_Line_Wrap (Breakpoints.Label4, False);

   Gtk_New (Breakpoints.Combo1);
   Pack_Start (Breakpoints.Vbox2, Breakpoints.Combo1, False, False, 0);
   Set_Case_Sensitive (Breakpoints.Combo1, False);
   Set_Use_Arrows (Breakpoints.Combo1, True);
   Set_Use_Arrows_Always (Breakpoints.Combo1, False);
   String_List.Append (Combo1_Items, -"");
   Combo.Set_Popdown_Strings (Breakpoints.Combo1, Combo1_Items);
   Free_String_List (Combo1_Items);

   Breakpoints.Combo_Entry1 := Get_Entry (Breakpoints.Combo1);
   Set_Editable (Breakpoints.Combo_Entry1, True);
   Set_Max_Length (Breakpoints.Combo_Entry1, 0);
   Set_Text (Breakpoints.Combo_Entry1, -"");
   Set_Visibility (Breakpoints.Combo_Entry1, True);

   Gtk_New (Breakpoints.Temporary_Location, -"Temporary breakpoint");
   Pack_Start (Breakpoints.Vbox2, Breakpoints.Temporary_Location, False, False, 5);
   Set_Active (Breakpoints.Temporary_Location, False);

   Gtk_New (Breakpoints.Location, -("Location"));
   Set_Alignment (Breakpoints.Location, 0.5, 0.5);
   Set_Padding (Breakpoints.Location, 0, 0);
   Set_Justify (Breakpoints.Location, Justify_Center);
   Set_Line_Wrap (Breakpoints.Location, False);
   Set_Tab (Breakpoints.Notebook1, 0, Breakpoints.Location);

   Gtk_New_Vbox (Breakpoints.Vbox7, False, 0);
   Add (Breakpoints.Notebook1, Breakpoints.Vbox7);
   Set_Border_Width (Breakpoints.Vbox7, 7);

   Gtk_New (Breakpoints.Label9, -("Break when the variable:"));
   Pack_Start (Breakpoints.Vbox7, Breakpoints.Label9, False, False, 5);
   Set_Alignment (Breakpoints.Label9, 0.0, 0.5);
   Set_Padding (Breakpoints.Label9, 0, 0);
   Set_Justify (Breakpoints.Label9, Justify_Left);
   Set_Line_Wrap (Breakpoints.Label9, False);

   Gtk_New (Breakpoints.Entry1);
   Pack_Start (Breakpoints.Vbox7, Breakpoints.Entry1, False, False, 0);
   Set_Editable (Breakpoints.Entry1, True);
   Set_Max_Length (Breakpoints.Entry1, 0);
   Set_Text (Breakpoints.Entry1, -"");
   Set_Visibility (Breakpoints.Entry1, True);

   Gtk_New (Breakpoints.Label10, -("is"));
   Pack_Start (Breakpoints.Vbox7, Breakpoints.Label10, False, False, 5);
   Set_Alignment (Breakpoints.Label10, 0.0, 0.5);
   Set_Padding (Breakpoints.Label10, 0, 0);
   Set_Justify (Breakpoints.Label10, Justify_Left);
   Set_Line_Wrap (Breakpoints.Label10, False);

   Gtk_New (Breakpoints.Combo3);
   Pack_Start (Breakpoints.Vbox7, Breakpoints.Combo3, False, False, 0);
   Set_Case_Sensitive (Breakpoints.Combo3, False);
   Set_Use_Arrows (Breakpoints.Combo3, True);
   Set_Use_Arrows_Always (Breakpoints.Combo3, False);
   String_List.Append (Combo3_Items, -"written");
   String_List.Append (Combo3_Items, -"read");
   String_List.Append (Combo3_Items, -"read or written");
   Combo.Set_Popdown_Strings (Breakpoints.Combo3, Combo3_Items);
   Free_String_List (Combo3_Items);

   Breakpoints.Combo_Entry3 := Get_Entry (Breakpoints.Combo3);
   Set_Editable (Breakpoints.Combo_Entry3, False);
   Set_Max_Length (Breakpoints.Combo_Entry3, 0);
   Set_Text (Breakpoints.Combo_Entry3, -"read");
   Set_Visibility (Breakpoints.Combo_Entry3, True);

   Gtk_New (Breakpoints.Watchpoint, -("WatchPoint"));
   Set_Alignment (Breakpoints.Watchpoint, 0.5, 0.5);
   Set_Padding (Breakpoints.Watchpoint, 0, 0);
   Set_Justify (Breakpoints.Watchpoint, Justify_Center);
   Set_Line_Wrap (Breakpoints.Watchpoint, False);
   Set_Tab (Breakpoints.Notebook1, 1, Breakpoints.Watchpoint);

   Gtk_New_Vbox (Breakpoints.Vbox8, False, 0);
   Add (Breakpoints.Notebook1, Breakpoints.Vbox8);
   Set_Border_Width (Breakpoints.Vbox8, 7);

   Gtk_New (Breakpoints.Label11, -("Break on exception:"));
   Pack_Start (Breakpoints.Vbox8, Breakpoints.Label11, False, False, 0);
   Set_Alignment (Breakpoints.Label11, 0.0, 0.5);
   Set_Padding (Breakpoints.Label11, 0, 0);
   Set_Justify (Breakpoints.Label11, Justify_Left);
   Set_Line_Wrap (Breakpoints.Label11, False);

   Gtk_New (Breakpoints.Combo4);
   Pack_Start (Breakpoints.Vbox8, Breakpoints.Combo4, False, False, 0);
   Set_Case_Sensitive (Breakpoints.Combo4, False);
   Set_Use_Arrows (Breakpoints.Combo4, True);
   Set_Use_Arrows_Always (Breakpoints.Combo4, False);
   String_List.Append (Combo4_Items, -"All exceptions");
   String_List.Append (Combo4_Items, -"Assert failure");
   Combo.Set_Popdown_Strings (Breakpoints.Combo4, Combo4_Items);
   Free_String_List (Combo4_Items);

   Breakpoints.Combo_Entry4 := Get_Entry (Breakpoints.Combo4);
   Set_Editable (Breakpoints.Combo_Entry4, True);
   Set_Max_Length (Breakpoints.Combo_Entry4, 0);
   Set_Text (Breakpoints.Combo_Entry4, -"All exceptions");
   Set_Visibility (Breakpoints.Combo_Entry4, True);

   Gtk_New (Breakpoints.Frame4, -"Action");
   Pack_Start (Breakpoints.Vbox8, Breakpoints.Frame4, False, False, 7);
   Set_Shadow_Type (Breakpoints.Frame4, Shadow_Etched_In);

   Gtk_New_Vbox (Breakpoints.Vbox9, False, 0);
   Add (Breakpoints.Frame4, Breakpoints.Vbox9);
   Set_Border_Width (Breakpoints.Vbox9, 7);

   Gtk_New (Breakpoints.Stop_Always, Vbox9_Group, -"Stop always");
   Vbox9_Group := Group (Breakpoints.Stop_Always);
   Pack_Start (Breakpoints.Vbox9, Breakpoints.Stop_Always, False, False, 0);
   Set_Active (Breakpoints.Stop_Always, True);

   Gtk_New (Breakpoints.Stop_Not_Handled, Vbox9_Group, -"Stop if not handled");
   Vbox9_Group := Group (Breakpoints.Stop_Not_Handled);
   Pack_Start (Breakpoints.Vbox9, Breakpoints.Stop_Not_Handled, False, False, 0);
   Set_Active (Breakpoints.Stop_Not_Handled, False);

   Gtk_New (Breakpoints.Except, -("Exception"));
   Set_Alignment (Breakpoints.Except, 0.5, 0.5);
   Set_Padding (Breakpoints.Except, 0, 0);
   Set_Justify (Breakpoints.Except, Justify_Center);
   Set_Line_Wrap (Breakpoints.Except, False);
   Set_Tab (Breakpoints.Notebook1, 2, Breakpoints.Except);

   Gtk_New_Vbox (Breakpoints.Vbox10, False, 0);
   Add (Breakpoints.Notebook1, Breakpoints.Vbox10);
   Set_Border_Width (Breakpoints.Vbox10, 7);

   Gtk_New (Breakpoints.Label12, -("Break on all functions matching "));
   Pack_Start (Breakpoints.Vbox10, Breakpoints.Label12, False, False, 0);
   Set_Alignment (Breakpoints.Label12, 0.0, 0.5);
   Set_Padding (Breakpoints.Label12, 0, 0);
   Set_Justify (Breakpoints.Label12, Justify_Left);
   Set_Line_Wrap (Breakpoints.Label12, False);

   Gtk_New (Breakpoints.Combo5);
   Pack_Start (Breakpoints.Vbox10, Breakpoints.Combo5, False, False, 0);
   Set_Case_Sensitive (Breakpoints.Combo5, False);
   Set_Use_Arrows (Breakpoints.Combo5, True);
   Set_Use_Arrows_Always (Breakpoints.Combo5, False);
   String_List.Append (Combo5_Items, -"");
   Combo.Set_Popdown_Strings (Breakpoints.Combo5, Combo5_Items);
   Free_String_List (Combo5_Items);

   Breakpoints.Combo_Entry5 := Get_Entry (Breakpoints.Combo5);
   Set_Editable (Breakpoints.Combo_Entry5, True);
   Set_Max_Length (Breakpoints.Combo_Entry5, 0);
   Set_Text (Breakpoints.Combo_Entry5, -"");
   Set_Visibility (Breakpoints.Combo_Entry5, True);

   Gtk_New (Breakpoints.Temporary_Regexp, -"Temporary breakpoint");
   Pack_Start (Breakpoints.Vbox10, Breakpoints.Temporary_Regexp, False, False, 0);
   Set_Active (Breakpoints.Temporary_Regexp, False);

   Gtk_New (Breakpoints.Regexp, -("Regexp"));
   Set_Alignment (Breakpoints.Regexp, 0.5, 0.5);
   Set_Padding (Breakpoints.Regexp, 0, 0);
   Set_Justify (Breakpoints.Regexp, Justify_Center);
   Set_Line_Wrap (Breakpoints.Regexp, False);
   Set_Tab (Breakpoints.Notebook1, 3, Breakpoints.Regexp);

   Gtk_New (Breakpoints.Advanced_Button, -"Advanced properties...");
   Pack_Start (Breakpoints.Vbox1, Breakpoints.Advanced_Button, False, False, 0);
   Button_Callback.Connect
     (Breakpoints.Advanced_Button, "clicked",
      Button_Callback.To_Marshaller (On_Advanced_Bp_Clicked'Access));
   Set_Border_Width (Breakpoints.Advanced_Button, 6);

   Gtk_New_Hbox (Breakpoints.Hbox1, False, 0);
   Pack_Start (Breakpoints.Vbox1, Breakpoints.Hbox1, True, True, 0);

   Gtk_New (Breakpoints.Scrolledwindow2);
   Pack_Start (Breakpoints.Hbox1, Breakpoints.Scrolledwindow2, True, True, 0);
   Set_Policy (Breakpoints.Scrolledwindow2, Policy_Automatic, Policy_Always);

   Gtk_New (Breakpoints.Clist1, 2);
   Add (Breakpoints.Scrolledwindow2, Breakpoints.Clist1);
   Set_Selection_Mode (Breakpoints.Clist1, Selection_Single);
   Set_Shadow_Type (Breakpoints.Clist1, Shadow_In);
   Set_Show_Titles (Breakpoints.Clist1, False);
   Set_Column_Width (Breakpoints.Clist1, 0, 80);
   Set_Column_Width (Breakpoints.Clist1, 1, 80);

   Gtk_New (Breakpoints.Label15, -("label15"));
   Set_Alignment (Breakpoints.Label15, 0.5, 0.5);
   Set_Padding (Breakpoints.Label15, 0, 0);
   Set_Justify (Breakpoints.Label15, Justify_Center);
   Set_Line_Wrap (Breakpoints.Label15, False);
   Set_Column_Widget (Breakpoints.Clist1, 0, Breakpoints.Label15);

   Gtk_New (Breakpoints.Label16, -("label16"));
   Set_Alignment (Breakpoints.Label16, 0.5, 0.5);
   Set_Padding (Breakpoints.Label16, 0, 0);
   Set_Justify (Breakpoints.Label16, Justify_Center);
   Set_Line_Wrap (Breakpoints.Label16, False);
   Set_Column_Widget (Breakpoints.Clist1, 1, Breakpoints.Label16);

   Gtk_New (Breakpoints.Vbuttonbox1);
   Pack_Start (Breakpoints.Hbox1, Breakpoints.Vbuttonbox1, False, False, 0);
   Set_Spacing (Breakpoints.Vbuttonbox1, 10);
   Set_Layout (Breakpoints.Vbuttonbox1, Buttonbox_Default_Style);
   Set_Child_Size (Breakpoints.Vbuttonbox1, 85, 27);
   Set_Child_Ipadding (Breakpoints.Vbuttonbox1, 7, 0);

   Gtk_New (Breakpoints.Add_Button, -"Add");
   Set_Flags (Breakpoints.Add_Button, Can_Default);
   Button_Callback.Connect
     (Breakpoints.Add_Button, "clicked",
      Button_Callback.To_Marshaller (On_Add_Bp_Clicked'Access));
   Add (Breakpoints.Vbuttonbox1, Breakpoints.Add_Button);

   Gtk_New (Breakpoints.Remove_Button, -"Remove");
   Set_Flags (Breakpoints.Remove_Button, Can_Default);
   Button_Callback.Connect
     (Breakpoints.Remove_Button, "clicked",
      Button_Callback.To_Marshaller (On_Remove_Bp_Clicked'Access));
   Add (Breakpoints.Vbuttonbox1, Breakpoints.Remove_Button);

   Gtk_New (Breakpoints.Remove_All_Button, -"Remove All");
   Set_Flags (Breakpoints.Remove_All_Button, Can_Default);
   Button_Callback.Connect
     (Breakpoints.Remove_All_Button, "clicked",
      Button_Callback.To_Marshaller (On_Remove_All_Bp_Clicked'Access));
   Add (Breakpoints.Vbuttonbox1, Breakpoints.Remove_All_Button);

   Gtk_New_Hseparator (Breakpoints.Hseparator1);
   Pack_Start (Breakpoints.Vbox1, Breakpoints.Hseparator1, False, False, 0);

   Gtk_New (Breakpoints.Hbuttonbox4);
   Pack_Start (Breakpoints.Vbox1, Breakpoints.Hbuttonbox4, False, False, 0);
   Set_Spacing (Breakpoints.Hbuttonbox4, 30);
   Set_Layout (Breakpoints.Hbuttonbox4, Buttonbox_Spread);
   Set_Child_Size (Breakpoints.Hbuttonbox4, 85, 27);
   Set_Child_Ipadding (Breakpoints.Hbuttonbox4, 7, 0);

   Gtk_New (Breakpoints.Ok_Button, -"OK");
   Set_Flags (Breakpoints.Ok_Button, Can_Default);
   Button_Callback.Connect
     (Breakpoints.Ok_Button, "clicked",
      Button_Callback.To_Marshaller (On_Ok_Bp_Clicked'Access));
   Add (Breakpoints.Hbuttonbox4, Breakpoints.Ok_Button);

   Gtk_New (Breakpoints.Cancel_Button, -"Cancel");
   Set_Flags (Breakpoints.Cancel_Button, Can_Default);
   Button_Callback.Connect
     (Breakpoints.Cancel_Button, "clicked",
      Button_Callback.To_Marshaller (On_Cancel_Bp_Clicked'Access));
   Add (Breakpoints.Hbuttonbox4, Breakpoints.Cancel_Button);

end Initialize;

procedure Breakpoint_Editor
  (Editor     : in out Breakpoints_Access;
   Descriptor : out Breakpoint_Descriptor) is
begin
   if Editor = null then
      Gtk_New (Editor);
   end if;

   Show_All (Editor);
   Gtk.Main.Main;
   Hide (Editor);
end Breakpoint_Editor;

procedure Free (Descriptor : in out Breakpoint_Descriptor) is
begin
   null;
end Free;

end Breakpoints_Pkg;
