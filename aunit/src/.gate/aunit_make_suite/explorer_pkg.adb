with Glib; use Glib;
with Gtk; use Gtk;
with Gdk.Types;       use Gdk.Types;
with Gtk.Widget;      use Gtk.Widget;
with Gtk.Enums;       use Gtk.Enums;
with Gtkada.Handlers; use Gtkada.Handlers;
with Callbacks_Aunit_Make_Suite; use Callbacks_Aunit_Make_Suite;
with Aunit_Make_Suite_Intl; use Aunit_Make_Suite_Intl;
with Explorer_Pkg.Callbacks; use Explorer_Pkg.Callbacks;

package body Explorer_Pkg is

procedure Gtk_New (Explorer : out Explorer_Access) is
begin
   Explorer := new Explorer_Record;
   Explorer_Pkg.Initialize (Explorer);
end Gtk_New;

procedure Initialize (Explorer : access Explorer_Record'Class) is
   pragma Suppress (All_Checks);
begin
   Gtk.Window.Initialize (Explorer, Window_Toplevel);
   Set_Title (Explorer, -"explore");
   Set_Policy (Explorer, False, True, False);
   Set_Position (Explorer, Win_Pos_None);
   Set_Modal (Explorer, False);

   Gtk_New_Vbox (Explorer.Vbox5, False, 0);
   Add (Explorer, Explorer.Vbox5);

   Gtk_New (Explorer.Scrolledwindow3);
   Set_Policy (Explorer.Scrolledwindow3, Policy_Automatic, Policy_Automatic);
   Pack_Start (Explorer.Vbox5, Explorer.Scrolledwindow3, True, True, 0);

   Gtk_New (Explorer.Clist, 2);
   Set_Selection_Mode (Explorer.Clist, Selection_Extended);
   Set_Shadow_Type (Explorer.Clist, Shadow_In);
   Set_Show_Titles (Explorer.Clist, False);
   Set_Column_Width (Explorer.Clist, 0, 80);
   Set_Column_Width (Explorer.Clist, 1, 80);
   C_List_Callback.Connect
     (Explorer.Clist, "select_row", On_Clist_Select_Row'Access);
   Add (Explorer.Scrolledwindow3, Explorer.Clist);

   Gtk_New (Explorer.Label3, -("label3"));
   Set_Alignment (Explorer.Label3, 0.5, 0.5);
   Set_Padding (Explorer.Label3, 0, 0);
   Set_Justify (Explorer.Label3, Justify_Center);
   Set_Line_Wrap (Explorer.Label3, False);
   Set_Column_Widget (Explorer.Clist, 0, Explorer.Label3);

   Gtk_New (Explorer.Label4, -("label4"));
   Set_Alignment (Explorer.Label4, 0.5, 0.5);
   Set_Padding (Explorer.Label4, 0, 0);
   Set_Justify (Explorer.Label4, Justify_Center);
   Set_Line_Wrap (Explorer.Label4, False);
   Set_Column_Widget (Explorer.Clist, 1, Explorer.Label4);

   Gtk_New (Explorer.Hbuttonbox2);
   Set_Spacing (Explorer.Hbuttonbox2, 30);
   Set_Layout (Explorer.Hbuttonbox2, Buttonbox_Spread);
   Set_Child_Size (Explorer.Hbuttonbox2, 85, 27);
   Set_Child_Ipadding (Explorer.Hbuttonbox2, 7, 0);
   Pack_Start (Explorer.Vbox5, Explorer.Hbuttonbox2, False, True, 0);

   Gtk_New (Explorer.Ok, -"OK");
   Set_Flags (Explorer.Ok, Can_Default);
   Button_Callback.Connect
     (Explorer.Ok, "clicked",
      Button_Callback.To_Marshaller (On_Ok_Clicked'Access));
   Add (Explorer.Hbuttonbox2, Explorer.Ok);

   Gtk_New (Explorer.Close, -"Close");
   Set_Flags (Explorer.Close, Can_Default);
   Button_Callback.Connect
     (Explorer.Close, "clicked",
      Button_Callback.To_Marshaller (On_Close_Clicked'Access));
   Add (Explorer.Hbuttonbox2, Explorer.Close);

end Initialize;

end Explorer_Pkg;
