with Glib; use Glib;
with Gtk; use Gtk;
with Gdk.Types;       use Gdk.Types;
with Gtk.Widget;      use Gtk.Widget;
with Gtk.Enums;       use Gtk.Enums;
with Gtkada.Handlers; use Gtkada.Handlers;
with Callbacks_Aunit_Gui; use Callbacks_Aunit_Gui;
with Aunit_Gui_Intl; use Aunit_Gui_Intl;
with Make_Suite_Window_Pkg.Callbacks; use Make_Suite_Window_Pkg.Callbacks;

package body Make_Suite_Window_Pkg is

procedure Gtk_New (Make_Suite_Window : out Make_Suite_Window_Access) is
begin
   Make_Suite_Window := new Make_Suite_Window_Record;
   Make_Suite_Window_Pkg.Initialize (Make_Suite_Window);
end Gtk_New;

procedure Initialize (Make_Suite_Window : access Make_Suite_Window_Record'Class) is
   pragma Suppress (All_Checks);
begin
   Gtk.Window.Initialize (Make_Suite_Window, Window_Toplevel);
   Set_Title (Make_Suite_Window, -"Make new suite");
   Set_Policy (Make_Suite_Window, False, True, False);
   Set_Position (Make_Suite_Window, Win_Pos_None);
   Set_Modal (Make_Suite_Window, False);
   Return_Callback.Connect
     (Make_Suite_Window, "delete_event", On_Make_Suite_Window_Delete_Event'Access);

   Gtk_New_Vbox (Make_Suite_Window.Vbox11, False, 0);
   Add (Make_Suite_Window, Make_Suite_Window.Vbox11);

   Gtk_New_Hbox (Make_Suite_Window.Hbox5, False, 0);
   Pack_Start (Make_Suite_Window.Vbox11, Make_Suite_Window.Hbox5, False, True, 3);

   Gtk_New_Vbox (Make_Suite_Window.Vbox12, True, 0);
   Pack_Start (Make_Suite_Window.Hbox5, Make_Suite_Window.Vbox12, False, False, 5);

   Gtk_New (Make_Suite_Window.Label7, -("Suite name :"));
   Set_Alignment (Make_Suite_Window.Label7, 1.0, 0.5);
   Set_Padding (Make_Suite_Window.Label7, 0, 0);
   Set_Justify (Make_Suite_Window.Label7, Justify_Center);
   Set_Line_Wrap (Make_Suite_Window.Label7, False);
   Pack_Start (Make_Suite_Window.Vbox12, Make_Suite_Window.Label7, False, False, 0);

   Gtk_New_Vbox (Make_Suite_Window.Vbox13, True, 0);
   Pack_Start (Make_Suite_Window.Hbox5, Make_Suite_Window.Vbox13, True, True, 3);

   Gtk_New (Make_Suite_Window.Entry1);
   Set_Editable (Make_Suite_Window.Entry1, True);
   Set_Max_Length (Make_Suite_Window.Entry1, 0);
   Set_Text (Make_Suite_Window.Entry1, -"New_Suite");
   Set_Visibility (Make_Suite_Window.Entry1, True);
   Pack_Start (Make_Suite_Window.Vbox13, Make_Suite_Window.Entry1, False, False, 1);

   Gtk_New_Vbox (Make_Suite_Window.Vbox14, False, 0);
   Pack_Start (Make_Suite_Window.Vbox11, Make_Suite_Window.Vbox14, True, True, 0);

   Gtk_New (Make_Suite_Window.Label8, -("The following tests will be added to the new suite :"));
   Set_Alignment (Make_Suite_Window.Label8, 0.0, 0.5);
   Set_Padding (Make_Suite_Window.Label8, 6, 0);
   Set_Justify (Make_Suite_Window.Label8, Justify_Center);
   Set_Line_Wrap (Make_Suite_Window.Label8, False);
   Pack_Start (Make_Suite_Window.Vbox14, Make_Suite_Window.Label8, False, False, 0);

   Gtk_New_Hbox (Make_Suite_Window.Hbox6, False, 0);
   Pack_Start (Make_Suite_Window.Vbox14, Make_Suite_Window.Hbox6, True, True, 0);

   Gtk_New (Make_Suite_Window.Scrolledwindow2);
   Set_Policy (Make_Suite_Window.Scrolledwindow2, Policy_Automatic, Policy_Automatic);
   Pack_Start (Make_Suite_Window.Hbox6, Make_Suite_Window.Scrolledwindow2, True, True, 0);

   Gtk_New (Make_Suite_Window.Test_List, 3);
   Set_Selection_Mode (Make_Suite_Window.Test_List, Selection_Single);
   Set_Shadow_Type (Make_Suite_Window.Test_List, Shadow_In);
   Set_Show_Titles (Make_Suite_Window.Test_List, False);
   Set_Column_Width (Make_Suite_Window.Test_List, 0, 80);
   Set_Column_Width (Make_Suite_Window.Test_List, 1, 80);
   Set_Column_Width (Make_Suite_Window.Test_List, 2, 80);
   Add (Make_Suite_Window.Scrolledwindow2, Make_Suite_Window.Test_List);

   Gtk_New (Make_Suite_Window.Label9, -("label5"));
   Set_Alignment (Make_Suite_Window.Label9, 0.5, 0.5);
   Set_Padding (Make_Suite_Window.Label9, 0, 0);
   Set_Justify (Make_Suite_Window.Label9, Justify_Center);
   Set_Line_Wrap (Make_Suite_Window.Label9, False);
   Set_Column_Widget (Make_Suite_Window.Test_List, 0, Make_Suite_Window.Label9);

   Gtk_New (Make_Suite_Window.Label10, -("label6"));
   Set_Alignment (Make_Suite_Window.Label10, 0.5, 0.5);
   Set_Padding (Make_Suite_Window.Label10, 0, 0);
   Set_Justify (Make_Suite_Window.Label10, Justify_Center);
   Set_Line_Wrap (Make_Suite_Window.Label10, False);
   Set_Column_Widget (Make_Suite_Window.Test_List, 1, Make_Suite_Window.Label10);

   Gtk_New (Make_Suite_Window.Label11, -("label7"));
   Set_Alignment (Make_Suite_Window.Label11, 0.5, 0.5);
   Set_Padding (Make_Suite_Window.Label11, 0, 0);
   Set_Justify (Make_Suite_Window.Label11, Justify_Center);
   Set_Line_Wrap (Make_Suite_Window.Label11, False);
   Set_Column_Widget (Make_Suite_Window.Test_List, 2, Make_Suite_Window.Label11);

   Gtk_New (Make_Suite_Window.Vbuttonbox1);
   Set_Spacing (Make_Suite_Window.Vbuttonbox1, 10);
   Set_Layout (Make_Suite_Window.Vbuttonbox1, Buttonbox_Spread);
   Set_Child_Size (Make_Suite_Window.Vbuttonbox1, 85, 27);
   Set_Child_Ipadding (Make_Suite_Window.Vbuttonbox1, 7, 0);
   Pack_Start (Make_Suite_Window.Hbox6, Make_Suite_Window.Vbuttonbox1, False, True, 0);

   Gtk_New (Make_Suite_Window.Add, -"Add");
   Set_Flags (Make_Suite_Window.Add, Can_Default);
   Button_Callback.Connect
     (Make_Suite_Window.Add, "clicked",
      Button_Callback.To_Marshaller (On_Add_Clicked'Access));
   Add (Make_Suite_Window.Vbuttonbox1, Make_Suite_Window.Add);

   Gtk_New (Make_Suite_Window.Remove, -"Remove");
   Set_Flags (Make_Suite_Window.Remove, Can_Default);
   Button_Callback.Connect
     (Make_Suite_Window.Remove, "clicked",
      Button_Callback.To_Marshaller (On_Remove_Clicked'Access));
   Add (Make_Suite_Window.Vbuttonbox1, Make_Suite_Window.Remove);

   Gtk_New (Make_Suite_Window.Hbuttonbox3);
   Set_Spacing (Make_Suite_Window.Hbuttonbox3, 30);
   Set_Layout (Make_Suite_Window.Hbuttonbox3, Buttonbox_Spread);
   Set_Child_Size (Make_Suite_Window.Hbuttonbox3, 85, 27);
   Set_Child_Ipadding (Make_Suite_Window.Hbuttonbox3, 7, 0);
   Pack_Start (Make_Suite_Window.Vbox11, Make_Suite_Window.Hbuttonbox3, False, False, 0);

   Gtk_New (Make_Suite_Window.Ok, -"OK");
   Set_Flags (Make_Suite_Window.Ok, Can_Default);
   Button_Callback.Connect
     (Make_Suite_Window.Ok, "clicked",
      Button_Callback.To_Marshaller (On_Ok_Clicked'Access));
   Add (Make_Suite_Window.Hbuttonbox3, Make_Suite_Window.Ok);

   Gtk_New (Make_Suite_Window.Cancel, -"Cancel");
   Set_Flags (Make_Suite_Window.Cancel, Can_Default);
   Button_Callback.Connect
     (Make_Suite_Window.Cancel, "clicked",
      Button_Callback.To_Marshaller (On_Cancel_Clicked'Access));
   Add (Make_Suite_Window.Hbuttonbox3, Make_Suite_Window.Cancel);

   Gtk_New (Make_Suite_Window.Help, -"Help");
   Set_Flags (Make_Suite_Window.Help, Can_Default);
   Button_Callback.Connect
     (Make_Suite_Window.Help, "clicked",
      Button_Callback.To_Marshaller (On_Help_Clicked'Access));
   Add (Make_Suite_Window.Hbuttonbox3, Make_Suite_Window.Help);

end Initialize;

end Make_Suite_Window_Pkg;
