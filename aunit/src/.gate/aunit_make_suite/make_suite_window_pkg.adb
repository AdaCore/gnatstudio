with Glib; use Glib;
with Gtk; use Gtk;
with Gdk.Types;       use Gdk.Types;
with Gtk.Widget;      use Gtk.Widget;
with Gtk.Enums;       use Gtk.Enums;
with Gtkada.Handlers; use Gtkada.Handlers;
with Callbacks_Aunit_Make_Suite; use Callbacks_Aunit_Make_Suite;
with Aunit_Make_Suite_Intl; use Aunit_Make_Suite_Intl;
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
   Return_Callback.Connect
     (Make_Suite_Window, "delete_event", On_Make_Suite_Window_Delete_Event'Access);
   Set_Title (Make_Suite_Window, -"Make new suite");
   Set_Policy (Make_Suite_Window, False, True, False);
   Set_Position (Make_Suite_Window, Win_Pos_None);
   Set_Modal (Make_Suite_Window, False);

   Gtk_New_Vbox (Make_Suite_Window.Vbox1, False, 0);
   Add (Make_Suite_Window, Make_Suite_Window.Vbox1);

   Gtk_New_Hbox (Make_Suite_Window.Hbox1, False, 0);
   Pack_Start (Make_Suite_Window.Vbox1, Make_Suite_Window.Hbox1, False, True, 3);

   Gtk_New_Vbox (Make_Suite_Window.Vbox2, True, 0);
   Pack_Start (Make_Suite_Window.Hbox1, Make_Suite_Window.Vbox2, False, False, 5);

   Gtk_New (Make_Suite_Window.Label1, -("Suite name :"));
   Pack_Start (Make_Suite_Window.Vbox2, Make_Suite_Window.Label1, False, False, 0);
   Set_Alignment (Make_Suite_Window.Label1, 1.0, 0.5);
   Set_Padding (Make_Suite_Window.Label1, 0, 0);
   Set_Justify (Make_Suite_Window.Label1, Justify_Center);
   Set_Line_Wrap (Make_Suite_Window.Label1, False);

   Gtk_New_Vbox (Make_Suite_Window.Vbox3, True, 0);
   Pack_Start (Make_Suite_Window.Hbox1, Make_Suite_Window.Vbox3, True, True, 3);

   Gtk_New (Make_Suite_Window.Name_Entry);
   Pack_Start (Make_Suite_Window.Vbox3, Make_Suite_Window.Name_Entry, False, False, 1);
   Set_Editable (Make_Suite_Window.Name_Entry, True);
   Set_Max_Length (Make_Suite_Window.Name_Entry, 0);
   Set_Text (Make_Suite_Window.Name_Entry, -"New_Suite");
   Set_Visibility (Make_Suite_Window.Name_Entry, True);

   Gtk_New_Vbox (Make_Suite_Window.Vbox4, False, 0);
   Pack_Start (Make_Suite_Window.Vbox1, Make_Suite_Window.Vbox4, True, True, 0);

   Gtk_New (Make_Suite_Window.Label2, -("The following tests will be added to the new suite :"));
   Pack_Start (Make_Suite_Window.Vbox4, Make_Suite_Window.Label2, False, False, 0);
   Set_Alignment (Make_Suite_Window.Label2, 0.0, 0.5);
   Set_Padding (Make_Suite_Window.Label2, 6, 0);
   Set_Justify (Make_Suite_Window.Label2, Justify_Center);
   Set_Line_Wrap (Make_Suite_Window.Label2, False);

   Gtk_New_Hbox (Make_Suite_Window.Hbox2, False, 0);
   Pack_Start (Make_Suite_Window.Vbox4, Make_Suite_Window.Hbox2, True, True, 0);

   Gtk_New (Make_Suite_Window.Scrolledwindow2);
   Pack_Start (Make_Suite_Window.Hbox2, Make_Suite_Window.Scrolledwindow2, True, True, 0);
   Set_Policy (Make_Suite_Window.Scrolledwindow2, Policy_Automatic, Policy_Automatic);

   Gtk_New (Make_Suite_Window.Test_List, 3);
   Add (Make_Suite_Window.Scrolledwindow2, Make_Suite_Window.Test_List);
   Set_Selection_Mode (Make_Suite_Window.Test_List, Selection_Single);
   Set_Shadow_Type (Make_Suite_Window.Test_List, Shadow_In);
   Set_Show_Titles (Make_Suite_Window.Test_List, False);
   Set_Column_Width (Make_Suite_Window.Test_List, 0, 80);
   Set_Column_Width (Make_Suite_Window.Test_List, 1, 80);
   Set_Column_Width (Make_Suite_Window.Test_List, 2, 80);

   Gtk_New (Make_Suite_Window.Label5, -("label5"));
   Set_Alignment (Make_Suite_Window.Label5, 0.5, 0.5);
   Set_Padding (Make_Suite_Window.Label5, 0, 0);
   Set_Justify (Make_Suite_Window.Label5, Justify_Center);
   Set_Line_Wrap (Make_Suite_Window.Label5, False);
   Set_Column_Widget (Make_Suite_Window.Test_List, 0, Make_Suite_Window.Label5);

   Gtk_New (Make_Suite_Window.Label6, -("label6"));
   Set_Alignment (Make_Suite_Window.Label6, 0.5, 0.5);
   Set_Padding (Make_Suite_Window.Label6, 0, 0);
   Set_Justify (Make_Suite_Window.Label6, Justify_Center);
   Set_Line_Wrap (Make_Suite_Window.Label6, False);
   Set_Column_Widget (Make_Suite_Window.Test_List, 1, Make_Suite_Window.Label6);

   Gtk_New (Make_Suite_Window.Label7, -("label7"));
   Set_Alignment (Make_Suite_Window.Label7, 0.5, 0.5);
   Set_Padding (Make_Suite_Window.Label7, 0, 0);
   Set_Justify (Make_Suite_Window.Label7, Justify_Center);
   Set_Line_Wrap (Make_Suite_Window.Label7, False);
   Set_Column_Widget (Make_Suite_Window.Test_List, 2, Make_Suite_Window.Label7);

   Gtk_New (Make_Suite_Window.Vbuttonbox1);
   Pack_Start (Make_Suite_Window.Hbox2, Make_Suite_Window.Vbuttonbox1, False, True, 0);
   Set_Spacing (Make_Suite_Window.Vbuttonbox1, 10);
   Set_Layout (Make_Suite_Window.Vbuttonbox1, Buttonbox_Spread);
   Set_Child_Size (Make_Suite_Window.Vbuttonbox1, 85, 27);
   Set_Child_Ipadding (Make_Suite_Window.Vbuttonbox1, 7, 0);

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

   Gtk_New (Make_Suite_Window.Hbuttonbox1);
   Pack_Start (Make_Suite_Window.Vbox1, Make_Suite_Window.Hbuttonbox1, False, False, 0);
   Set_Spacing (Make_Suite_Window.Hbuttonbox1, 30);
   Set_Layout (Make_Suite_Window.Hbuttonbox1, Buttonbox_Spread);
   Set_Child_Size (Make_Suite_Window.Hbuttonbox1, 85, 27);
   Set_Child_Ipadding (Make_Suite_Window.Hbuttonbox1, 7, 0);

   Gtk_New (Make_Suite_Window.Ok, -"OK");
   Set_Flags (Make_Suite_Window.Ok, Can_Default);
   Button_Callback.Connect
     (Make_Suite_Window.Ok, "clicked",
      Button_Callback.To_Marshaller (On_Ok_Clicked'Access));
   Add (Make_Suite_Window.Hbuttonbox1, Make_Suite_Window.Ok);

   Gtk_New (Make_Suite_Window.Cancel, -"Cancel");
   Set_Flags (Make_Suite_Window.Cancel, Can_Default);
   Button_Callback.Connect
     (Make_Suite_Window.Cancel, "clicked",
      Button_Callback.To_Marshaller (On_Cancel_Clicked'Access));
   Add (Make_Suite_Window.Hbuttonbox1, Make_Suite_Window.Cancel);

   Gtk_New (Make_Suite_Window.Help, -"Help");
   Set_Flags (Make_Suite_Window.Help, Can_Default);
   Button_Callback.Connect
     (Make_Suite_Window.Help, "clicked",
      Button_Callback.To_Marshaller (On_Help_Clicked'Access));
   Add (Make_Suite_Window.Hbuttonbox1, Make_Suite_Window.Help);

end Initialize;

end Make_Suite_Window_Pkg;
