with Glib; use Glib;
with Gtk; use Gtk;
with Gdk.Types;       use Gdk.Types;
with Gtk.Widget;      use Gtk.Widget;
with Gtk.Enums;       use Gtk.Enums;
with Gtkada.Handlers; use Gtkada.Handlers;
with Callbacks_Aunit_Gui; use Callbacks_Aunit_Gui;
with Aunit_Gui_Intl; use Aunit_Gui_Intl;
with Make_Test_Window_Pkg.Callbacks; use Make_Test_Window_Pkg.Callbacks;

package body Make_Test_Window_Pkg is

procedure Gtk_New (Make_Test_Window : out Make_Test_Window_Access) is
begin
   Make_Test_Window := new Make_Test_Window_Record;
   Make_Test_Window_Pkg.Initialize (Make_Test_Window);
end Gtk_New;

procedure Initialize (Make_Test_Window : access Make_Test_Window_Record'Class) is
   pragma Suppress (All_Checks);
begin
   Gtk.Window.Initialize (Make_Test_Window, Window_Toplevel);
   Set_Title (Make_Test_Window, -"New test unit");
   Set_Policy (Make_Test_Window, False, True, False);
   Set_Position (Make_Test_Window, Win_Pos_None);
   Set_Modal (Make_Test_Window, False);
   Return_Callback.Connect
     (Make_Test_Window, "delete_event", On_Make_Test_Window_Delete_Event'Access);

   Gtk_New_Vbox (Make_Test_Window.Vbox16, False, 0);
   Add (Make_Test_Window, Make_Test_Window.Vbox16);

   Gtk_New_Hbox (Make_Test_Window.Hbox7, False, 0);
   Pack_Start (Make_Test_Window.Vbox16, Make_Test_Window.Hbox7, True, True, 0);

   Gtk_New_Vbox (Make_Test_Window.Vbox17, True, 0);
   Pack_Start (Make_Test_Window.Hbox7, Make_Test_Window.Vbox17, False, False, 5);

   Gtk_New (Make_Test_Window.Label14, -("Unit name : "));
   Set_Alignment (Make_Test_Window.Label14, 1.0, 0.5);
   Set_Padding (Make_Test_Window.Label14, 0, 0);
   Set_Justify (Make_Test_Window.Label14, Justify_Center);
   Set_Line_Wrap (Make_Test_Window.Label14, False);
   Pack_Start (Make_Test_Window.Vbox17, Make_Test_Window.Label14, False, False, 0);

   Gtk_New (Make_Test_Window.Label15, -("Description : "));
   Set_Alignment (Make_Test_Window.Label15, 1.0, 0.5);
   Set_Padding (Make_Test_Window.Label15, 0, 0);
   Set_Justify (Make_Test_Window.Label15, Justify_Center);
   Set_Line_Wrap (Make_Test_Window.Label15, False);
   Pack_Start (Make_Test_Window.Vbox17, Make_Test_Window.Label15, False, False, 0);

   Gtk_New (Make_Test_Window.Label16);
   Set_Alignment (Make_Test_Window.Label16, 0.5, 0.5);
   Set_Padding (Make_Test_Window.Label16, 0, 0);
   Set_Justify (Make_Test_Window.Label16, Justify_Center);
   Set_Line_Wrap (Make_Test_Window.Label16, False);
   Pack_Start (Make_Test_Window.Vbox17, Make_Test_Window.Label16, False, False, 0);

   Gtk_New (Make_Test_Window.Label17);
   Set_Alignment (Make_Test_Window.Label17, 0.5, 0.5);
   Set_Padding (Make_Test_Window.Label17, 0, 0);
   Set_Justify (Make_Test_Window.Label17, Justify_Center);
   Set_Line_Wrap (Make_Test_Window.Label17, False);
   Pack_Start (Make_Test_Window.Vbox17, Make_Test_Window.Label17, False, False, 0);

   Gtk_New_Vbox (Make_Test_Window.Vbox18, True, 0);
   Pack_Start (Make_Test_Window.Hbox7, Make_Test_Window.Vbox18, True, True, 3);

   Gtk_New (Make_Test_Window.Name_Entry);
   Set_Editable (Make_Test_Window.Name_Entry, True);
   Set_Max_Length (Make_Test_Window.Name_Entry, 0);
   Set_Text (Make_Test_Window.Name_Entry, -"New_Test");
   Set_Visibility (Make_Test_Window.Name_Entry, True);
   Pack_Start (Make_Test_Window.Vbox18, Make_Test_Window.Name_Entry, False, False, 0);
   Entry_Callback.Connect
     (Make_Test_Window.Name_Entry, "activate",
      Entry_Callback.To_Marshaller (On_Name_Entry_Activate'Access));

   Gtk_New (Make_Test_Window.Description_Entry);
   Set_Editable (Make_Test_Window.Description_Entry, True);
   Set_Max_Length (Make_Test_Window.Description_Entry, 0);
   Set_Text (Make_Test_Window.Description_Entry, -"(no description)");
   Set_Visibility (Make_Test_Window.Description_Entry, True);
   Pack_Start (Make_Test_Window.Vbox18, Make_Test_Window.Description_Entry, False, False, 0);
   Entry_Callback.Connect
     (Make_Test_Window.Description_Entry, "activate",
      Entry_Callback.To_Marshaller (On_Description_Entry_Activate'Access));

   Gtk_New (Make_Test_Window.Override_Tear_Down, -"Override Tear_Down");
   Set_Active (Make_Test_Window.Override_Tear_Down, False);
   Pack_Start (Make_Test_Window.Vbox18, Make_Test_Window.Override_Tear_Down, False, False, 0);

   Gtk_New (Make_Test_Window.Override_Set_Up, -"Override Set_up");
   Set_Active (Make_Test_Window.Override_Set_Up, False);
   Pack_Start (Make_Test_Window.Vbox18, Make_Test_Window.Override_Set_Up, False, False, 0);

   Gtk_New (Make_Test_Window.Hbuttonbox5);
   Set_Spacing (Make_Test_Window.Hbuttonbox5, 30);
   Set_Layout (Make_Test_Window.Hbuttonbox5, Buttonbox_Spread);
   Set_Child_Size (Make_Test_Window.Hbuttonbox5, 85, 27);
   Set_Child_Ipadding (Make_Test_Window.Hbuttonbox5, 7, 0);
   Pack_Start (Make_Test_Window.Vbox16, Make_Test_Window.Hbuttonbox5, True, True, 0);

   Gtk_New (Make_Test_Window.Ok, -"OK");
   Set_Flags (Make_Test_Window.Ok, Can_Default);
   Button_Callback.Connect
     (Make_Test_Window.Ok, "clicked",
      Button_Callback.To_Marshaller (On_Ok_Clicked'Access));
   Add (Make_Test_Window.Hbuttonbox5, Make_Test_Window.Ok);

   Gtk_New (Make_Test_Window.Cancel, -"Cancel");
   Set_Flags (Make_Test_Window.Cancel, Can_Default);
   Button_Callback.Connect
     (Make_Test_Window.Cancel, "clicked",
      Button_Callback.To_Marshaller (On_Cancel_Clicked'Access));
   Add (Make_Test_Window.Hbuttonbox5, Make_Test_Window.Cancel);

   Gtk_New (Make_Test_Window.Help, -"Help");
   Set_Flags (Make_Test_Window.Help, Can_Default);
   Button_Callback.Connect
     (Make_Test_Window.Help, "clicked",
      Button_Callback.To_Marshaller (On_Help_Clicked'Access));
   Add (Make_Test_Window.Hbuttonbox5, Make_Test_Window.Help);

end Initialize;

end Make_Test_Window_Pkg;
