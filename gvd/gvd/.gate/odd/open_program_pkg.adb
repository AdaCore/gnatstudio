with Glib; use Glib;
with Gtk; use Gtk;
with Gdk.Types;       use Gdk.Types;
with Gtk.Widget;      use Gtk.Widget;
with Gtk.Enums;       use Gtk.Enums;
with Gtkada.Handlers; use Gtkada.Handlers;
with Callbacks_Odd; use Callbacks_Odd;
with Odd_Intl; use Odd_Intl;
with Open_Program_Pkg.Callbacks; use Open_Program_Pkg.Callbacks;

package body Open_Program_Pkg is

procedure Gtk_New (Open_Program : out Open_Program_Access) is
begin
   Open_Program := new Open_Program_Record;
   Open_Program_Pkg.Initialize (Open_Program);
end Gtk_New;

procedure Initialize (Open_Program : access Open_Program_Record'Class) is
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
   Pack_Start (Open_Program.Vbox13, Open_Program.Frame8, True, True, 0);
   Set_Shadow_Type (Open_Program.Frame8, Shadow_Etched_In);

   Gtk_New (Open_Program.Table7, 5, 7, False);
   Add (Open_Program.Frame8, Open_Program.Table7);
   Set_Row_Spacings (Open_Program.Table7, 3);
   Set_Col_Spacings (Open_Program.Table7, 3);

   Gtk_New (Open_Program.Gdb_Button, Table7_Group, -"GDB");
   Table7_Group := Group (Open_Program.Gdb_Button);
   Attach (Open_Program.Table7, Open_Program.Gdb_Button, 1, 2, 3, 4,
     0, 0,
     0, 0);
   Radio_Button_Callback.Connect
     (Open_Program.Gdb_Button, "toggled",
      Radio_Button_Callback.To_Marshaller (On_Radio_Button_Toggled'Access));
   Set_Active (Open_Program.Gdb_Button, False);

   Gtk_New (Open_Program.Dbx_Button, Table7_Group, -"DBX");
   Table7_Group := Group (Open_Program.Dbx_Button);
   Set_Sensitive (Open_Program.Dbx_Button, False);
   Attach (Open_Program.Table7, Open_Program.Dbx_Button, 2, 3, 3, 4,
     0, 0,
     0, 0);
   Radio_Button_Callback.Connect
     (Open_Program.Dbx_Button, "toggled",
      Radio_Button_Callback.To_Marshaller (On_Radio_Button_Toggled'Access));
   Set_Active (Open_Program.Dbx_Button, False);

   Gtk_New (Open_Program.Xdb_Button, Table7_Group, -"XDB");
   Table7_Group := Group (Open_Program.Xdb_Button);
   Set_Sensitive (Open_Program.Xdb_Button, False);
   Attach (Open_Program.Table7, Open_Program.Xdb_Button, 3, 4, 3, 4,
     0, 0,
     0, 0);
   Radio_Button_Callback.Connect
     (Open_Program.Xdb_Button, "toggled",
      Radio_Button_Callback.To_Marshaller (On_Radio_Button_Toggled'Access));
   Set_Active (Open_Program.Xdb_Button, False);

   Gtk_New (Open_Program.Jdb_Button, Table7_Group, -"JDB");
   Table7_Group := Group (Open_Program.Jdb_Button);
   Attach (Open_Program.Table7, Open_Program.Jdb_Button, 4, 5, 3, 4,
     0, 0,
     0, 0);
   Radio_Button_Callback.Connect
     (Open_Program.Jdb_Button, "toggled",
      Radio_Button_Callback.To_Marshaller (On_Radio_Button_Toggled'Access));
   Set_Active (Open_Program.Jdb_Button, False);

   Gtk_New (Open_Program.Pydb_Button, Table7_Group, -"PYDB");
   Table7_Group := Group (Open_Program.Pydb_Button);
   Set_Sensitive (Open_Program.Pydb_Button, False);
   Attach (Open_Program.Table7, Open_Program.Pydb_Button, 5, 6, 3, 4,
     0, 0,
     0, 0);
   Radio_Button_Callback.Connect
     (Open_Program.Pydb_Button, "toggled",
      Radio_Button_Callback.To_Marshaller (On_Radio_Button_Toggled'Access));
   Set_Active (Open_Program.Pydb_Button, False);

   Gtk_New (Open_Program.Perl_Button, Table7_Group, -"Perl");
   Table7_Group := Group (Open_Program.Perl_Button);
   Set_Sensitive (Open_Program.Perl_Button, False);
   Attach (Open_Program.Table7, Open_Program.Perl_Button, 6, 7, 3, 4,
     0, 0,
     0, 0);
   Radio_Button_Callback.Connect
     (Open_Program.Perl_Button, "toggled",
      Radio_Button_Callback.To_Marshaller (On_Radio_Button_Toggled'Access));
   Set_Active (Open_Program.Perl_Button, False);

   Gtk_New (Open_Program.Program_Combo);
   Attach (Open_Program.Table7, Open_Program.Program_Combo, 1, 6, 0, 1,
     Expand or Fill, 0,
     0, 0);
   Set_Case_Sensitive (Open_Program.Program_Combo, False);
   Set_Use_Arrows (Open_Program.Program_Combo, True);
   Set_Use_Arrows_Always (Open_Program.Program_Combo, False);
   String_List.Append (Program_Combo_Items, -"");
   Combo.Set_Popdown_Strings (Open_Program.Program_Combo, Program_Combo_Items);
   Free_String_List (Program_Combo_Items);

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
   Attach (Open_Program.Table7, Open_Program.Host_Combo, 1, 6, 1, 2,
     Expand or Fill, 0,
     0, 0);
   Set_Case_Sensitive (Open_Program.Host_Combo, False);
   Set_Use_Arrows (Open_Program.Host_Combo, True);
   Set_Use_Arrows_Always (Open_Program.Host_Combo, False);
   String_List.Append (Host_Combo_Items, -"");
   Combo.Set_Popdown_Strings (Open_Program.Host_Combo, Host_Combo_Items);
   Free_String_List (Host_Combo_Items);

   Open_Program.Host_Entry := Get_Entry (Open_Program.Host_Combo);
   Set_Editable (Open_Program.Host_Entry, True);
   Set_Max_Length (Open_Program.Host_Entry, 0);
   Set_Text (Open_Program.Host_Entry, -"");
   Set_Visibility (Open_Program.Host_Entry, True);

   Gtk_New (Open_Program.Label57, -("Debugger"));
   Attach (Open_Program.Table7, Open_Program.Label57, 0, 1, 3, 4,
     Fill, 0,
     0, 0);
   Set_Alignment (Open_Program.Label57, 0.0, 0.5);
   Set_Padding (Open_Program.Label57, 0, 0);
   Set_Justify (Open_Program.Label57, Justify_Center);
   Set_Line_Wrap (Open_Program.Label57, False);

   Gtk_New (Open_Program.Label55, -("Program File"));
   Attach (Open_Program.Table7, Open_Program.Label55, 0, 1, 0, 1,
     Fill, 0,
     0, 0);
   Set_Alignment (Open_Program.Label55, 7.45058e-09, 0.5);
   Set_Padding (Open_Program.Label55, 0, 0);
   Set_Justify (Open_Program.Label55, Justify_Center);
   Set_Line_Wrap (Open_Program.Label55, False);

   Gtk_New (Open_Program.Label56, -("Debugger Host"));
   Attach (Open_Program.Table7, Open_Program.Label56, 0, 1, 1, 2,
     Fill, 0,
     0, 0);
   Set_Alignment (Open_Program.Label56, 7.45058e-09, 0.5);
   Set_Padding (Open_Program.Label56, 0, 0);
   Set_Justify (Open_Program.Label56, Justify_Center);
   Set_Line_Wrap (Open_Program.Label56, False);

   Gtk_New (Open_Program.Protocol_Combo);
   Attach (Open_Program.Table7, Open_Program.Protocol_Combo, 4, 6, 2, 3,
     Expand or Fill, 0,
     0, 0);
   Set_Case_Sensitive (Open_Program.Protocol_Combo, False);
   Set_Use_Arrows (Open_Program.Protocol_Combo, True);
   Set_Use_Arrows_Always (Open_Program.Protocol_Combo, False);
   String_List.Append (Protocol_Combo_Items, -"wtx");
   String_List.Append (Protocol_Combo_Items, -"vxworks");
   String_List.Append (Protocol_Combo_Items, -"remote");
   Combo.Set_Popdown_Strings (Open_Program.Protocol_Combo, Protocol_Combo_Items);
   Free_String_List (Protocol_Combo_Items);

   Open_Program.Protocol_Entry := Get_Entry (Open_Program.Protocol_Combo);
   Set_Editable (Open_Program.Protocol_Entry, True);
   Set_Max_Length (Open_Program.Protocol_Entry, 0);
   Set_Text (Open_Program.Protocol_Entry, -"");
   Set_Visibility (Open_Program.Protocol_Entry, True);

   Gtk_New (Open_Program.Label60, -("Protocol"));
   Attach (Open_Program.Table7, Open_Program.Label60, 3, 4, 2, 3,
     0, 0,
     0, 0);
   Set_Alignment (Open_Program.Label60, 0.5, 0.5);
   Set_Padding (Open_Program.Label60, 0, 0);
   Set_Justify (Open_Program.Label60, Justify_Center);
   Set_Line_Wrap (Open_Program.Label60, False);

   Gtk_New (Open_Program.Program_Host_Combo);
   Attach (Open_Program.Table7, Open_Program.Program_Host_Combo, 1, 3, 2, 3,
     Expand or Fill, 0,
     0, 0);
   Set_Case_Sensitive (Open_Program.Program_Host_Combo, False);
   Set_Use_Arrows (Open_Program.Program_Host_Combo, True);
   Set_Use_Arrows_Always (Open_Program.Program_Host_Combo, False);
   String_List.Append (Program_Host_Combo_Items, -"");
   Combo.Set_Popdown_Strings (Open_Program.Program_Host_Combo, Program_Host_Combo_Items);
   Free_String_List (Program_Host_Combo_Items);

   Open_Program.Target_Entry := Get_Entry (Open_Program.Program_Host_Combo);
   Set_Editable (Open_Program.Target_Entry, True);
   Set_Max_Length (Open_Program.Target_Entry, 0);
   Set_Text (Open_Program.Target_Entry, -"");
   Set_Visibility (Open_Program.Target_Entry, True);

   Gtk_New (Open_Program.Label59, -("Program Host"));
   Attach (Open_Program.Table7, Open_Program.Label59, 0, 1, 2, 3,
     Fill, 0,
     0, 0);
   Set_Alignment (Open_Program.Label59, 7.45058e-09, 0.5);
   Set_Padding (Open_Program.Label59, 0, 0);
   Set_Justify (Open_Program.Label59, Justify_Center);
   Set_Line_Wrap (Open_Program.Label59, False);

   Gtk_New (Open_Program.Label73, -("Debugger Name"));
   Attach (Open_Program.Table7, Open_Program.Label73, 0, 1, 4, 5,
     Fill, 0,
     0, 0);
   Set_Alignment (Open_Program.Label73, 0.0, 0.5);
   Set_Padding (Open_Program.Label73, 0, 0);
   Set_Justify (Open_Program.Label73, Justify_Center);
   Set_Line_Wrap (Open_Program.Label73, False);

   Gtk_New (Open_Program.Debugger_Combo);
   Attach (Open_Program.Table7, Open_Program.Debugger_Combo, 1, 6, 4, 5,
     Expand or Fill, 0,
     0, 0);
   Set_Case_Sensitive (Open_Program.Debugger_Combo, False);
   Set_Use_Arrows (Open_Program.Debugger_Combo, True);
   Set_Use_Arrows_Always (Open_Program.Debugger_Combo, False);
   String_List.Append (Debugger_Combo_Items, -"");
   Combo.Set_Popdown_Strings (Open_Program.Debugger_Combo, Debugger_Combo_Items);
   Free_String_List (Debugger_Combo_Items);

   Open_Program.Debugger_Entry := Get_Entry (Open_Program.Debugger_Combo);
   Set_Editable (Open_Program.Debugger_Entry, True);
   Set_Max_Length (Open_Program.Debugger_Entry, 0);
   Set_Text (Open_Program.Debugger_Entry, -"");
   Set_Visibility (Open_Program.Debugger_Entry, True);

   Gtk_New (Open_Program.Hbuttonbox7);
   Pack_Start (Open_Program.Vbox13, Open_Program.Hbuttonbox7, False, True, 0);
   Set_Spacing (Open_Program.Hbuttonbox7, 30);
   Set_Layout (Open_Program.Hbuttonbox7, Buttonbox_Spread);
   Set_Child_Size (Open_Program.Hbuttonbox7, 85, 27);
   Set_Child_Ipadding (Open_Program.Hbuttonbox7, 7, 0);

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

   Gtk_New (Open_Program.Help_Button, -"Help");
   Set_Flags (Open_Program.Help_Button, Can_Default);
   Button_Callback.Connect
     (Open_Program.Help_Button, "clicked",
      Button_Callback.To_Marshaller (On_Help_Open_Clicked'Access));
   Add (Open_Program.Hbuttonbox7, Open_Program.Help_Button);

end Initialize;

end Open_Program_Pkg;
