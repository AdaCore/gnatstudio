with Glib; use Glib;
with Gtk; use Gtk;
with Gdk.Types;       use Gdk.Types;
with Gtk.Widget;      use Gtk.Widget;
with Gtk.Enums;       use Gtk.Enums;
with Gtkada.Handlers; use Gtkada.Handlers;
with Callbacks_Odd; use Callbacks_Odd;
with Odd_Intl; use Odd_Intl;
with Memory_View_Pkg.Callbacks; use Memory_View_Pkg.Callbacks;

package body Memory_View_Pkg is

procedure Gtk_New (Memory_View : out Memory_View_Access) is
begin
   Memory_View := new Memory_View_Record;
   Memory_View_Pkg.Initialize (Memory_View);
end Gtk_New;

procedure Initialize (Memory_View : access Memory_View_Record'Class) is
   Size_Items : String_List.Glist;
   Format_Items : String_List.Glist;

begin
   Gtk.Window.Initialize (Memory_View, Window_Toplevel);
   Window_Callback.Connect
     (Memory_View, "size_allocate", On_Memory_View_Size_Allocate'Access);
   Set_Title (Memory_View, -"Memory View");
   Set_Policy (Memory_View, False, True, False);
   Set_Position (Memory_View, Win_Pos_None);
   Set_Modal (Memory_View, False);

   Gtk_New_Vbox (Memory_View.Vbox20, False, 0);
   Return_Callback.Connect
     (Memory_View.Vbox20, "key_press_event", On_Vbox20_Key_Press_Event'Access);
   Add (Memory_View, Memory_View.Vbox20);

   Gtk_New (Memory_View.Frame, -"(no executable)");
   Pack_Start (Memory_View.Vbox20, Memory_View.Frame, False, False, 0);
   Set_Shadow_Type (Memory_View.Frame, Shadow_Etched_In);

   Gtk_New_Hbox (Memory_View.Hbox8, False, 0);
   Add (Memory_View.Frame, Memory_View.Hbox8);

   Gtk_New_Hbox (Memory_View.Hbox11, False, 0);
   Pack_Start (Memory_View.Hbox8, Memory_View.Hbox11, True, True, 0);

   Gtk_New_Vbox (Memory_View.Vbox23, True, 0);
   Pack_Start (Memory_View.Hbox11, Memory_View.Vbox23, False, False, 7);

   Gtk_New (Memory_View.Label95, -("Address"));
   Pack_Start (Memory_View.Vbox23, Memory_View.Label95, False, False, 0);
   Set_Alignment (Memory_View.Label95, 0.5, 0.5);
   Set_Padding (Memory_View.Label95, 0, 0);
   Set_Justify (Memory_View.Label95, Justify_Center);
   Set_Line_Wrap (Memory_View.Label95, False);

   Gtk_New (Memory_View.Label96, -("Value"));
   Set_Sensitive (Memory_View.Label96, False);
   Pack_Start (Memory_View.Vbox23, Memory_View.Label96, False, False, 0);
   Set_Alignment (Memory_View.Label96, 0.5, 0.5);
   Set_Padding (Memory_View.Label96, 0, 0);
   Set_Justify (Memory_View.Label96, Justify_Center);
   Set_Line_Wrap (Memory_View.Label96, False);

   Gtk_New_Vbox (Memory_View.Vbox24, True, 0);
   Pack_Start (Memory_View.Hbox11, Memory_View.Vbox24, True, True, 0);

   Gtk_New (Memory_View.Address_Entry);
   Pack_Start (Memory_View.Vbox24, Memory_View.Address_Entry, False, False, 0);
   Entry_Callback.Connect
     (Memory_View.Address_Entry, "activate",
      Entry_Callback.To_Marshaller (On_Address_Entry_Activate'Access));
   Set_Editable (Memory_View.Address_Entry, True);
   Set_Max_Length (Memory_View.Address_Entry, 0);
   Set_Text (Memory_View.Address_Entry, -"");
   Set_Visibility (Memory_View.Address_Entry, True);

   Gtk_New (Memory_View.Value_Entry);
   Set_Sensitive (Memory_View.Value_Entry, False);
   Pack_Start (Memory_View.Vbox24, Memory_View.Value_Entry, False, False, 0);
   Entry_Callback.Connect
     (Memory_View.Value_Entry, "insert_text", On_Value_Insert_Text'Access);
   Set_Editable (Memory_View.Value_Entry, True);
   Set_Max_Length (Memory_View.Value_Entry, 0);
   Set_Text (Memory_View.Value_Entry, -"");
   Set_Visibility (Memory_View.Value_Entry, True);

   Gtk_New (Memory_View.Vbuttonbox6);
   Pack_Start (Memory_View.Hbox8, Memory_View.Vbuttonbox6, False, False, 0);
   Set_Spacing (Memory_View.Vbuttonbox6, 10);
   Set_Layout (Memory_View.Vbuttonbox6, Buttonbox_Spread);
   Set_Child_Size (Memory_View.Vbuttonbox6, 85, 27);
   Set_Child_Ipadding (Memory_View.Vbuttonbox6, 7, 0);

   Gtk_New (Memory_View.Address_View, -"View");
   Set_Flags (Memory_View.Address_View, Can_Default);
   Button_Callback.Connect
     (Memory_View.Address_View, "clicked",
      Button_Callback.To_Marshaller (On_Address_View_Clicked'Access));
   Add (Memory_View.Vbuttonbox6, Memory_View.Address_View);

   Gtk_New (Memory_View.Value_Update, -"Update");
   Set_Sensitive (Memory_View.Value_Update, False);
   Set_Flags (Memory_View.Value_Update, Can_Default);
   Button_Callback.Connect
     (Memory_View.Value_Update, "clicked",
      Button_Callback.To_Marshaller (On_Value_Update_Clicked'Access));
   Add (Memory_View.Vbuttonbox6, Memory_View.Value_Update);

   Gtk_New_Hbox (Memory_View.Hbox12, False, 0);
   Pack_Start (Memory_View.Vbox20, Memory_View.Hbox12, False, False, 4);

   Gtk_New (Memory_View.Vbuttonbox5);
   Pack_Start (Memory_View.Hbox12, Memory_View.Vbuttonbox5, False, False, 0);
   Set_Spacing (Memory_View.Vbuttonbox5, 10);
   Set_Layout (Memory_View.Vbuttonbox5, Buttonbox_Spread);
   Set_Child_Size (Memory_View.Vbuttonbox5, 85, 27);
   Set_Child_Ipadding (Memory_View.Vbuttonbox5, 7, 0);

   Gtk_New (Memory_View.Pgup);
   Set_Flags (Memory_View.Pgup, Can_Default);
   Pack_Start (Memory_View.Hbox12, Memory_View.Pgup, False, False, 0);
   Button_Callback.Connect
     (Memory_View.Pgup, "clicked",
      Button_Callback.To_Marshaller (On_Pgup_Clicked'Access));

   Memory_View.Pixmap1 := Create_Pixmap ("up_xpm", Memory_View);
   Add (Memory_View.Pgup, Memory_View.Pixmap1);
   Set_Alignment (Memory_View.Pixmap1, 0.5, 0.5);
   Set_Padding (Memory_View.Pixmap1, 0, 0);

   Gtk_New (Memory_View.Pgdn);
   Set_Flags (Memory_View.Pgdn, Can_Default);
   Pack_Start (Memory_View.Hbox12, Memory_View.Pgdn, False, False, 0);
   Button_Callback.Connect
     (Memory_View.Pgdn, "clicked",
      Button_Callback.To_Marshaller (On_Pgdn_Clicked'Access));

   Memory_View.Pixmap2 := Create_Pixmap ("down_xpm", Memory_View);
   Add (Memory_View.Pgdn, Memory_View.Pixmap2);
   Set_Alignment (Memory_View.Pixmap2, 0.5, 0.5);
   Set_Padding (Memory_View.Pixmap2, 0, 0);

   Gtk_New_Vseparator (Memory_View.Vseparator5);
   Pack_Start (Memory_View.Hbox12, Memory_View.Vseparator5, True, True, 7);

   Gtk_New (Memory_View.Label99, -("Page size:"));
   Pack_Start (Memory_View.Hbox12, Memory_View.Label99, True, True, 1);
   Set_Alignment (Memory_View.Label99, 0.5, 0.5);
   Set_Padding (Memory_View.Label99, 0, 0);
   Set_Justify (Memory_View.Label99, Justify_Center);
   Set_Line_Wrap (Memory_View.Label99, False);

   Gtk_New (Memory_View.Page_Size_Entry);
   Pack_Start (Memory_View.Hbox12, Memory_View.Page_Size_Entry, False, False, 0);
   Entry_Callback.Connect
     (Memory_View.Page_Size_Entry, "activate",
      Entry_Callback.To_Marshaller (On_Page_Size_Entry_Activate'Access));
   Set_Editable (Memory_View.Page_Size_Entry, True);
   Set_Max_Length (Memory_View.Page_Size_Entry, 0);
   Set_Text (Memory_View.Page_Size_Entry, -"256");
   Set_Visibility (Memory_View.Page_Size_Entry, True);

   Gtk_New (Memory_View.Label100, -("bytes"));
   Pack_Start (Memory_View.Hbox12, Memory_View.Label100, False, True, 0);
   Set_Alignment (Memory_View.Label100, 0.5, 0.5);
   Set_Padding (Memory_View.Label100, 0, 0);
   Set_Justify (Memory_View.Label100, Justify_Center);
   Set_Line_Wrap (Memory_View.Label100, False);

   Gtk_New_Vseparator (Memory_View.Vseparator6);
   Pack_Start (Memory_View.Hbox12, Memory_View.Vseparator6, True, True, 7);

   Gtk_New (Memory_View.Label98, -("Unit size: "));
   Pack_Start (Memory_View.Hbox12, Memory_View.Label98, True, True, 0);
   Set_Alignment (Memory_View.Label98, 0.5, 0.5);
   Set_Padding (Memory_View.Label98, 0, 0);
   Set_Justify (Memory_View.Label98, Justify_Center);
   Set_Line_Wrap (Memory_View.Label98, False);

   Gtk_New (Memory_View.Size);
   Pack_Start (Memory_View.Hbox12, Memory_View.Size, False, True, 0);
   Set_Case_Sensitive (Memory_View.Size, False);
   Set_Use_Arrows (Memory_View.Size, True);
   Set_Use_Arrows_Always (Memory_View.Size, False);
   String_List.Append (Size_Items, -"Byte");
   String_List.Append (Size_Items, -"Halfword");
   String_List.Append (Size_Items, -"Word");
   Combo.Set_Popdown_Strings (Memory_View.Size, Size_Items);
   Free_String_List (Size_Items);

   Memory_View.Size_Entry := Get_Entry (Memory_View.Size);
   Entry_Callback.Connect
     (Memory_View.Size_Entry, "changed",
      Entry_Callback.To_Marshaller (On_Size_Entry_Changed'Access));
   Set_Editable (Memory_View.Size_Entry, False);
   Set_Max_Length (Memory_View.Size_Entry, 0);
   Set_Text (Memory_View.Size_Entry, -"Byte");
   Set_Visibility (Memory_View.Size_Entry, True);

   Gtk_New (Memory_View.Label97, -("Format: "));
   Pack_Start (Memory_View.Hbox12, Memory_View.Label97, True, True, 4);
   Set_Alignment (Memory_View.Label97, 0.5, 0.5);
   Set_Padding (Memory_View.Label97, 0, 0);
   Set_Justify (Memory_View.Label97, Justify_Center);
   Set_Line_Wrap (Memory_View.Label97, False);

   Gtk_New (Memory_View.Format);
   Pack_Start (Memory_View.Hbox12, Memory_View.Format, False, True, 0);
   Set_Case_Sensitive (Memory_View.Format, False);
   Set_Use_Arrows (Memory_View.Format, True);
   Set_Use_Arrows_Always (Memory_View.Format, False);
   String_List.Append (Format_Items, -"Hex");
   String_List.Append (Format_Items, -"Decimal");
   String_List.Append (Format_Items, -"Octal");
   String_List.Append (Format_Items, -"ASCII");
   Combo.Set_Popdown_Strings (Memory_View.Format, Format_Items);
   Free_String_List (Format_Items);

   Memory_View.Data_Entry := Get_Entry (Memory_View.Format);
   Entry_Callback.Connect
     (Memory_View.Data_Entry, "changed",
      Entry_Callback.To_Marshaller (On_Data_Entry_Changed'Access));
   Set_Editable (Memory_View.Data_Entry, False);
   Set_Max_Length (Memory_View.Data_Entry, 0);
   Set_Text (Memory_View.Data_Entry, -"Hex");
   Set_Visibility (Memory_View.Data_Entry, True);

   Gtk_New (Memory_View.Scrolledwindow);
   Pack_Start (Memory_View.Vbox20, Memory_View.Scrolledwindow, True, True, 0);
   Set_Policy (Memory_View.Scrolledwindow, Policy_Automatic, Policy_Automatic);

   Gtk_New (Memory_View.View);
   Set_USize (Memory_View.View, 686, 320);
   Return_Callback.Connect
     (Memory_View.View, "key_press_event", On_View_Key_Press_Event'Access);
   Text_Callback.Connect
     (Memory_View.View, "move_cursor", On_View_Move_Cursor'Access);
   Return_Callback.Connect
     (Memory_View.View, "button_release_event", On_View_Button_Release_Event'Access);
   Return_Callback.Connect
     (Memory_View.View, "button_press_event", On_View_Button_Press_Event'Access);
   Text_Callback.Connect
     (Memory_View.View, "insert_text", On_View_Insert_Text'Access);
   Add (Memory_View.Scrolledwindow, Memory_View.View);
   Set_Editable (Memory_View.View, True);

   Gtk_New (Memory_View.Hbuttonbox11);
   Pack_Start (Memory_View.Vbox20, Memory_View.Hbuttonbox11, False, False, 0);
   Set_Spacing (Memory_View.Hbuttonbox11, 30);
   Set_Layout (Memory_View.Hbuttonbox11, Buttonbox_Spread);
   Set_Child_Size (Memory_View.Hbuttonbox11, 85, 27);
   Set_Child_Ipadding (Memory_View.Hbuttonbox11, 7, 0);

   Gtk_New (Memory_View.Reset, -"Reset");
   Set_Flags (Memory_View.Reset, Can_Default);
   Button_Callback.Connect
     (Memory_View.Reset, "clicked",
      Button_Callback.To_Marshaller (On_Reset_Clicked'Access));
   Add (Memory_View.Hbuttonbox11, Memory_View.Reset);

   Gtk_New (Memory_View.Submit, -"Submit changes");
   Set_Flags (Memory_View.Submit, Can_Default);
   Button_Callback.Connect
     (Memory_View.Submit, "clicked",
      Button_Callback.To_Marshaller (On_Submit_Clicked'Access));
   Add (Memory_View.Hbuttonbox11, Memory_View.Submit);

   Gtk_New (Memory_View.Cancel, -"Cancel");
   Set_Flags (Memory_View.Cancel, Can_Default);
   Button_Callback.Connect
     (Memory_View.Cancel, "clicked",
      Button_Callback.To_Marshaller (On_Cancel_Clicked'Access));
   Add (Memory_View.Hbuttonbox11, Memory_View.Cancel);

   Gtk_New (Memory_View.Help, -"Help");
   Set_Flags (Memory_View.Help, Can_Default);
   Button_Callback.Connect
     (Memory_View.Help, "clicked",
      Button_Callback.To_Marshaller (On_Help_Clicked'Access));
   Add (Memory_View.Hbuttonbox11, Memory_View.Help);

end Initialize;

end Memory_View_Pkg;
