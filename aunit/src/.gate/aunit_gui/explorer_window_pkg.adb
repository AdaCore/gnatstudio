with Glib; use Glib;
with Gtk; use Gtk;
with Gdk.Types;       use Gdk.Types;
with Gtk.Widget;      use Gtk.Widget;
with Gtk.Enums;       use Gtk.Enums;
with Gtkada.Handlers; use Gtkada.Handlers;
with Callbacks_Aunit_Gui; use Callbacks_Aunit_Gui;
with Aunit_Gui_Intl; use Aunit_Gui_Intl;
with Explorer_Window_Pkg.Callbacks; use Explorer_Window_Pkg.Callbacks;

package body Explorer_Window_Pkg is

procedure Gtk_New (Explorer_Window : out Explorer_Window_Access) is
begin
   Explorer_Window := new Explorer_Window_Record;
   Explorer_Window_Pkg.Initialize (Explorer_Window);
end Gtk_New;

procedure Initialize (Explorer_Window : access Explorer_Window_Record'Class) is
   pragma Suppress (All_Checks);
begin
   Gtk.Window.Initialize (Explorer_Window, Window_Toplevel);
   Set_Title (Explorer_Window, -"Explore");
   Set_Policy (Explorer_Window, False, True, False);
   Set_Position (Explorer_Window, Win_Pos_None);
   Set_Modal (Explorer_Window, False);
   Return_Callback.Connect
     (Explorer_Window, "delete_event", On_Explorer_Window_Delete_Event'Access);

   Gtk_New_Vbox (Explorer_Window.Vbox7, False, 0);
   Add (Explorer_Window, Explorer_Window.Vbox7);

   Gtk_New (Explorer_Window.Scrolledwindow1);
   Set_Policy (Explorer_Window.Scrolledwindow1, Policy_Automatic, Policy_Automatic);
   Pack_Start (Explorer_Window.Vbox7, Explorer_Window.Scrolledwindow1, True, True, 3);

   Gtk_New (Explorer_Window.Clist, 2);
   Set_Selection_Mode (Explorer_Window.Clist, Selection_Extended);
   Set_Shadow_Type (Explorer_Window.Clist, Shadow_In);
   Set_Show_Titles (Explorer_Window.Clist, False);
   Set_Column_Width (Explorer_Window.Clist, 0, 80);
   Set_Column_Width (Explorer_Window.Clist, 1, 80);
   C_List_Callback.Connect
     (Explorer_Window.Clist, "select_row", On_Clist_Select_Row'Access);
   Add (Explorer_Window.Scrolledwindow1, Explorer_Window.Clist);

   Gtk_New (Explorer_Window.Label4, -("label4"));
   Set_Alignment (Explorer_Window.Label4, 0.5, 0.5);
   Set_Padding (Explorer_Window.Label4, 0, 0);
   Set_Justify (Explorer_Window.Label4, Justify_Center);
   Set_Line_Wrap (Explorer_Window.Label4, False);
   Set_Column_Widget (Explorer_Window.Clist, 0, Explorer_Window.Label4);

   Gtk_New (Explorer_Window.Label5, -("label5"));
   Set_Alignment (Explorer_Window.Label5, 0.5, 0.5);
   Set_Padding (Explorer_Window.Label5, 0, 0);
   Set_Justify (Explorer_Window.Label5, Justify_Center);
   Set_Line_Wrap (Explorer_Window.Label5, False);
   Set_Column_Widget (Explorer_Window.Clist, 1, Explorer_Window.Label5);

   Gtk_New (Explorer_Window.Hbuttonbox2);
   Set_Spacing (Explorer_Window.Hbuttonbox2, 30);
   Set_Layout (Explorer_Window.Hbuttonbox2, Buttonbox_Spread);
   Set_Child_Size (Explorer_Window.Hbuttonbox2, 85, 27);
   Set_Child_Ipadding (Explorer_Window.Hbuttonbox2, 7, 0);
   Pack_Start (Explorer_Window.Vbox7, Explorer_Window.Hbuttonbox2, False, True, 0);

   Gtk_New (Explorer_Window.Ok, -"OK");
   Set_Flags (Explorer_Window.Ok, Can_Default);
   Button_Callback.Connect
     (Explorer_Window.Ok, "clicked",
      Button_Callback.To_Marshaller (On_Ok_Clicked'Access));
   Add (Explorer_Window.Hbuttonbox2, Explorer_Window.Ok);

   Gtk_New (Explorer_Window.Cancel, -"Cancel");
   Set_Flags (Explorer_Window.Cancel, Can_Default);
   Button_Callback.Connect
     (Explorer_Window.Cancel, "clicked",
      Button_Callback.To_Marshaller (On_Cancel_Clicked'Access));
   Add (Explorer_Window.Hbuttonbox2, Explorer_Window.Cancel);

end Initialize;

end Explorer_Window_Pkg;
