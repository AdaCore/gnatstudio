-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                      Copyright (C) 2000-2001                      --
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
with Gtk.Widget;      use Gtk.Widget;
with Gtk.Enums;       use Gtk.Enums;
with Gtk.Main;
with Callbacks_Odd; use Callbacks_Odd;
with Odd_Intl; use Odd_Intl;
with Open_Program_Pkg.Callbacks; use Open_Program_Pkg.Callbacks;
with Basic_Types; use Basic_Types;

package body Open_Program_Pkg is

procedure Gtk_New (Open_Program : out Open_Program_Access) is
begin
   Open_Program := new Open_Program_Record;
   Open_Program_Pkg.Initialize (Open_Program);
end Gtk_New;

procedure Initialize (Open_Program : access Open_Program_Record'Class) is
   pragma Suppress (All_Checks);
   Table7_Group : Widget_SList.GSList;
   Program_Combo_Items : String_List.Glist;
   Host_Combo_Items : String_List.Glist;
   Protocol_Combo_Items : String_List.Glist;
   Program_Host_Combo_Items : String_List.Glist;
   Debugger_Combo_Items : String_List.Glist;

begin
   Gtk.Window.Initialize (Open_Program, Window_Toplevel);
   Set_Title (Open_Program, -"Open Program in a New Debugger");
   Set_Policy (Open_Program, False, True, False);
   Set_Position (Open_Program, Win_Pos_Center);
   Set_Modal (Open_Program, True);

   Gtk_New_Vbox (Open_Program.Vbox13, False, 0);
   Add (Open_Program, Open_Program.Vbox13);

   Gtk_New (Open_Program.Frame8);
   Set_Shadow_Type (Open_Program.Frame8, Shadow_Etched_In);
   Pack_Start (Open_Program.Vbox13, Open_Program.Frame8, True, True, 0);

   Gtk_New (Open_Program.Table7, 6, 7, False);
   Set_Row_Spacings (Open_Program.Table7, 3);
   Set_Col_Spacings (Open_Program.Table7, 3);
   Add (Open_Program.Frame8, Open_Program.Table7);

   Gtk_New (Open_Program.Gdb_Button, Table7_Group, -"GDB");
   Table7_Group := Group (Open_Program.Gdb_Button);
   Set_Active (Open_Program.Gdb_Button, False);
   Attach (Open_Program.Table7, Open_Program.Gdb_Button, 1, 2, 3, 4,
     0, 0,
     0, 0);
   Radio_Button_Callback.Connect
     (Open_Program.Gdb_Button, "toggled",
      Radio_Button_Callback.To_Marshaller (On_Radio_Button_Toggled'Access));

   Gtk_New (Open_Program.Dbx_Button, Table7_Group, -"DBX");
   Table7_Group := Group (Open_Program.Dbx_Button);
   Set_Active (Open_Program.Dbx_Button, False);
   Set_Sensitive (Open_Program.Dbx_Button, False);
   Attach (Open_Program.Table7, Open_Program.Dbx_Button, 2, 3, 3, 4,
     0, 0,
     0, 0);
   Radio_Button_Callback.Connect
     (Open_Program.Dbx_Button, "toggled",
      Radio_Button_Callback.To_Marshaller (On_Radio_Button_Toggled'Access));

   Gtk_New (Open_Program.Xdb_Button, Table7_Group, -"XDB");
   Table7_Group := Group (Open_Program.Xdb_Button);
   Set_Active (Open_Program.Xdb_Button, False);
   Set_Sensitive (Open_Program.Xdb_Button, False);
   Attach (Open_Program.Table7, Open_Program.Xdb_Button, 3, 4, 3, 4,
     0, 0,
     0, 0);
   Radio_Button_Callback.Connect
     (Open_Program.Xdb_Button, "toggled",
      Radio_Button_Callback.To_Marshaller (On_Radio_Button_Toggled'Access));

   Gtk_New (Open_Program.Jdb_Button, Table7_Group, -"JDB");
   Table7_Group := Group (Open_Program.Jdb_Button);
   Set_Active (Open_Program.Jdb_Button, False);
   Attach (Open_Program.Table7, Open_Program.Jdb_Button, 4, 5, 3, 4,
     0, 0,
     0, 0);
   Radio_Button_Callback.Connect
     (Open_Program.Jdb_Button, "toggled",
      Radio_Button_Callback.To_Marshaller (On_Radio_Button_Toggled'Access));

   Gtk_New (Open_Program.Pydb_Button, Table7_Group, -"PYDB");
   Table7_Group := Group (Open_Program.Pydb_Button);
   Set_Active (Open_Program.Pydb_Button, False);
   Set_Sensitive (Open_Program.Pydb_Button, False);
   Attach (Open_Program.Table7, Open_Program.Pydb_Button, 5, 6, 3, 4,
     0, 0,
     0, 0);
   Radio_Button_Callback.Connect
     (Open_Program.Pydb_Button, "toggled",
      Radio_Button_Callback.To_Marshaller (On_Radio_Button_Toggled'Access));

   Gtk_New (Open_Program.Perl_Button, Table7_Group, -"Perl");
   Table7_Group := Group (Open_Program.Perl_Button);
   Set_Active (Open_Program.Perl_Button, False);
   Set_Sensitive (Open_Program.Perl_Button, False);
   Attach (Open_Program.Table7, Open_Program.Perl_Button, 6, 7, 3, 4,
     0, 0,
     0, 0);
   Radio_Button_Callback.Connect
     (Open_Program.Perl_Button, "toggled",
      Radio_Button_Callback.To_Marshaller (On_Radio_Button_Toggled'Access));

   Gtk_New (Open_Program.Program_Combo);
   Set_Case_Sensitive (Open_Program.Program_Combo, False);
   Set_Use_Arrows (Open_Program.Program_Combo, True);
   Set_Use_Arrows_Always (Open_Program.Program_Combo, False);
   String_List.Append (Program_Combo_Items, -"");
   Combo.Set_Popdown_Strings (Open_Program.Program_Combo, Program_Combo_Items);
   Free_String_List (Program_Combo_Items);
   Attach (Open_Program.Table7, Open_Program.Program_Combo, 1, 6, 0, 1,
     Expand or Fill, 0,
     0, 0);

   Open_Program.Program_Entry := Get_Entry (Open_Program.Program_Combo);
   Set_Editable (Open_Program.Program_Entry, True);
   Set_Max_Length (Open_Program.Program_Entry, 0);
   Set_Text (Open_Program.Program_Entry, -"");
   Set_Visibility (Open_Program.Program_Entry, True);

   Gtk_New (Open_Program.Open_Button, -"...");
   Attach (Open_Program.Table7, Open_Program.Open_Button, 6, 7, 0, 1,
     0, 0,
     0, 0);
   Button_Callback.Connect
     (Open_Program.Open_Button, "clicked",
      Button_Callback.To_Marshaller (On_Open_Button_Clicked'Access));

   Gtk_New (Open_Program.Host_Combo);
   Set_Case_Sensitive (Open_Program.Host_Combo, False);
   Set_Use_Arrows (Open_Program.Host_Combo, True);
   Set_Use_Arrows_Always (Open_Program.Host_Combo, False);
   String_List.Append (Host_Combo_Items, -"");
   Combo.Set_Popdown_Strings (Open_Program.Host_Combo, Host_Combo_Items);
   Free_String_List (Host_Combo_Items);
   Attach (Open_Program.Table7, Open_Program.Host_Combo, 1, 6, 1, 2,
     Expand or Fill, 0,
     0, 0);

   Open_Program.Host_Entry := Get_Entry (Open_Program.Host_Combo);
   Set_Editable (Open_Program.Host_Entry, True);
   Set_Max_Length (Open_Program.Host_Entry, 0);
   Set_Text (Open_Program.Host_Entry, -"");
   Set_Visibility (Open_Program.Host_Entry, True);

   Gtk_New (Open_Program.Label57, -("Debugger"));
   Set_Alignment (Open_Program.Label57, 0.0, 0.5);
   Set_Padding (Open_Program.Label57, 0, 0);
   Set_Justify (Open_Program.Label57, Justify_Center);
   Set_Line_Wrap (Open_Program.Label57, False);
   Attach (Open_Program.Table7, Open_Program.Label57, 0, 1, 3, 4,
     Fill, 0,
     0, 0);

   Gtk_New (Open_Program.Label55, -("Program File"));
   Set_Alignment (Open_Program.Label55, 7.45058e-09, 0.5);
   Set_Padding (Open_Program.Label55, 0, 0);
   Set_Justify (Open_Program.Label55, Justify_Center);
   Set_Line_Wrap (Open_Program.Label55, False);
   Attach (Open_Program.Table7, Open_Program.Label55, 0, 1, 0, 1,
     Fill, 0,
     0, 0);

   Gtk_New (Open_Program.Label56, -("Debugger Host"));
   Set_Alignment (Open_Program.Label56, 7.45058e-09, 0.5);
   Set_Padding (Open_Program.Label56, 0, 0);
   Set_Justify (Open_Program.Label56, Justify_Center);
   Set_Line_Wrap (Open_Program.Label56, False);
   Attach (Open_Program.Table7, Open_Program.Label56, 0, 1, 1, 2,
     Fill, 0,
     0, 0);

   Gtk_New (Open_Program.Protocol_Combo);
   Set_Case_Sensitive (Open_Program.Protocol_Combo, False);
   Set_Use_Arrows (Open_Program.Protocol_Combo, True);
   Set_Use_Arrows_Always (Open_Program.Protocol_Combo, False);
   String_List.Append (Protocol_Combo_Items, -"wtx");
   String_List.Append (Protocol_Combo_Items, -"vxworks");
   String_List.Append (Protocol_Combo_Items, -"remote");
   Combo.Set_Popdown_Strings (Open_Program.Protocol_Combo, Protocol_Combo_Items);
   Free_String_List (Protocol_Combo_Items);
   Attach (Open_Program.Table7, Open_Program.Protocol_Combo, 4, 6, 2, 3,
     Expand or Fill, 0,
     0, 0);

   Open_Program.Protocol_Entry := Get_Entry (Open_Program.Protocol_Combo);
   Set_Editable (Open_Program.Protocol_Entry, True);
   Set_Max_Length (Open_Program.Protocol_Entry, 0);
   Set_Text (Open_Program.Protocol_Entry, -"");
   Set_Visibility (Open_Program.Protocol_Entry, True);

   Gtk_New (Open_Program.Label60, -("Protocol"));
   Set_Alignment (Open_Program.Label60, 0.5, 0.5);
   Set_Padding (Open_Program.Label60, 0, 0);
   Set_Justify (Open_Program.Label60, Justify_Center);
   Set_Line_Wrap (Open_Program.Label60, False);
   Attach (Open_Program.Table7, Open_Program.Label60, 3, 4, 2, 3,
     0, 0,
     0, 0);

   Gtk_New (Open_Program.Program_Host_Combo);
   Set_Case_Sensitive (Open_Program.Program_Host_Combo, False);
   Set_Use_Arrows (Open_Program.Program_Host_Combo, True);
   Set_Use_Arrows_Always (Open_Program.Program_Host_Combo, False);
   String_List.Append (Program_Host_Combo_Items, -"");
   Combo.Set_Popdown_Strings (Open_Program.Program_Host_Combo, Program_Host_Combo_Items);
   Free_String_List (Program_Host_Combo_Items);
   Attach (Open_Program.Table7, Open_Program.Program_Host_Combo, 1, 3, 2, 3,
     Expand or Fill, 0,
     0, 0);

   Open_Program.Target_Entry := Get_Entry (Open_Program.Program_Host_Combo);
   Set_Editable (Open_Program.Target_Entry, True);
   Set_Max_Length (Open_Program.Target_Entry, 0);
   Set_Text (Open_Program.Target_Entry, -"");
   Set_Visibility (Open_Program.Target_Entry, True);

   Gtk_New (Open_Program.Label59, -("Program Host"));
   Set_Alignment (Open_Program.Label59, 7.45058e-09, 0.5);
   Set_Padding (Open_Program.Label59, 0, 0);
   Set_Justify (Open_Program.Label59, Justify_Center);
   Set_Line_Wrap (Open_Program.Label59, False);
   Attach (Open_Program.Table7, Open_Program.Label59, 0, 1, 2, 3,
     Fill, 0,
     0, 0);

   Gtk_New (Open_Program.Label73, -("Debugger Name"));
   Set_Alignment (Open_Program.Label73, 0.0, 0.5);
   Set_Padding (Open_Program.Label73, 0, 0);
   Set_Justify (Open_Program.Label73, Justify_Center);
   Set_Line_Wrap (Open_Program.Label73, False);
   Attach (Open_Program.Table7, Open_Program.Label73, 0, 1, 4, 5,
     Fill, 0,
     0, 0);

   Gtk_New (Open_Program.Debugger_Combo);
   Set_Case_Sensitive (Open_Program.Debugger_Combo, False);
   Set_Use_Arrows (Open_Program.Debugger_Combo, True);
   Set_Use_Arrows_Always (Open_Program.Debugger_Combo, False);
   String_List.Append (Debugger_Combo_Items, -"");
   Combo.Set_Popdown_Strings (Open_Program.Debugger_Combo, Debugger_Combo_Items);
   Free_String_List (Debugger_Combo_Items);
   Attach (Open_Program.Table7, Open_Program.Debugger_Combo, 1, 6, 4, 5,
     Expand or Fill, 0,
     0, 0);

   Open_Program.Debugger_Entry := Get_Entry (Open_Program.Debugger_Combo);
   Set_Editable (Open_Program.Debugger_Entry, True);
   Set_Max_Length (Open_Program.Debugger_Entry, 0);
   Set_Text (Open_Program.Debugger_Entry, -"");
   Set_Visibility (Open_Program.Debugger_Entry, True);

   Gtk_New (Open_Program.Replace_Check, -"Replace Current Debugger");
   Set_Active (Open_Program.Replace_Check, True);
   Attach (Open_Program.Table7, Open_Program.Replace_Check, 0, 7, 5, 6,
     Fill, 0,
     0, 0);

   Gtk_New (Open_Program.Hbuttonbox7);
   Set_Spacing (Open_Program.Hbuttonbox7, 30);
   Set_Layout (Open_Program.Hbuttonbox7, Buttonbox_Spread);
   Set_Child_Size (Open_Program.Hbuttonbox7, 85, 27);
   Set_Child_Ipadding (Open_Program.Hbuttonbox7, 7, 0);
   Pack_Start (Open_Program.Vbox13, Open_Program.Hbuttonbox7, False, True, 0);

   Gtk_New (Open_Program.Ok_Button, -"OK");
   Set_Flags (Open_Program.Ok_Button, Can_Default);
   Button_Callback.Connect
     (Open_Program.Ok_Button, "clicked",
      Button_Callback.To_Marshaller (On_Ok_Open_Clicked'Access));
   Add (Open_Program.Hbuttonbox7, Open_Program.Ok_Button);

   Gtk_New (Open_Program.Cancel_Button, -"Cancel");
   Set_Flags (Open_Program.Cancel_Button, Can_Default);
   Button_Callback.Connect
     (Open_Program.Cancel_Button, "clicked",
      Button_Callback.To_Marshaller (On_Cancel_Open_Clicked'Access));
   Add (Open_Program.Hbuttonbox7, Open_Program.Cancel_Button);

end Initialize;

procedure Open_Program
  (Open       : in out Open_Program_Access;
   Descriptor : out Program_Descriptor)
is
begin
   if Open = null then
      Gtk_New (Open);
   end if;

   Show_All (Open);
   Gtk.Main.Main;

   if not Open.Valid then
      Descriptor.Launch := None;
      Hide (Open);
      return;
   end if;

   if Get_Active (Open.Gdb_Button) then
      Descriptor.Debugger := Gdb_Type;
   elsif Get_Active (Open.Dbx_Button) then
      Descriptor.Debugger := Dbx_Type;
   elsif Get_Active (Open.Xdb_Button) then
      Descriptor.Debugger := Xdb_Type;
   elsif Get_Active (Open.Jdb_Button) then
      Descriptor.Debugger := Jdb_Type;
   elsif Get_Active (Open.Pydb_Button) then
      Descriptor.Debugger := Pydb_Type;
   elsif Get_Active (Open.Perl_Button) then
      Descriptor.Debugger := Perl_Type;
   end if;

   Descriptor.Program := new String' (Get_Text (Open.Program_Entry));
   Descriptor.Remote_Host := new String' (Get_Text (Open.Host_Entry));
   Descriptor.Remote_Target := new String' (Get_Text (Open.Target_Entry));
   Descriptor.Protocol := new String' (Get_Text (Open.Protocol_Entry));
   Descriptor.Debugger_Name := new String' (Get_Text (Open.Debugger_Entry));

   if Get_Active (Open.Replace_Check) then
      Descriptor.Launch := Current_Debugger;
   else
      Descriptor.Launch := New_Debugger;
   end if;

   Hide (Open);
end Open_Program;

procedure Free (Descriptor : in out Program_Descriptor) is
begin
   Free (Descriptor.Program);
   Free (Descriptor.Remote_Host);
   Free (Descriptor.Remote_Target);
   Free (Descriptor.Protocol);
   Free (Descriptor.Debugger_Name);
end Free;

end Open_Program_Pkg;
