with Glib; use Glib;
with Gtk; use Gtk;
with Gdk.Types;       use Gdk.Types;
with Gtk.Widget;      use Gtk.Widget;
with Gtk.Enums;       use Gtk.Enums;
with Gtkada.Handlers; use Gtkada.Handlers;
with Callbacks_Aunit_Gui; use Callbacks_Aunit_Gui;
with Aunit_Gui_Intl; use Aunit_Gui_Intl;
with Make_Harness_Window_Pkg.Callbacks; use Make_Harness_Window_Pkg.Callbacks;

package body Make_Harness_Window_Pkg is

procedure Gtk_New (Make_Harness_Window : out Make_Harness_Window_Access) is
begin
   Make_Harness_Window := new Make_Harness_Window_Record;
   Make_Harness_Window_Pkg.Initialize (Make_Harness_Window);
end Gtk_New;

procedure Initialize (Make_Harness_Window : access Make_Harness_Window_Record'Class) is
   pragma Suppress (All_Checks);
begin
   Gtk.Window.Initialize (Make_Harness_Window, Window_Toplevel);
   Set_Title (Make_Harness_Window, -"Make new harness");
   Set_Policy (Make_Harness_Window, False, True, False);
   Set_Position (Make_Harness_Window, Win_Pos_None);
   Set_Modal (Make_Harness_Window, False);
   Return_Callback.Connect
     (Make_Harness_Window, "delete_event", On_Make_Harness_Window_Delete_Event'Access);

   Gtk_New_Vbox (Make_Harness_Window.Vbox3, False, 0);
   Add (Make_Harness_Window, Make_Harness_Window.Vbox3);

   Gtk_New_Hbox (Make_Harness_Window.Hbox2, False, 0);
   Pack_Start (Make_Harness_Window.Vbox3, Make_Harness_Window.Hbox2, True, True, 0);

   Gtk_New_Vbox (Make_Harness_Window.Vbox8, True, 0);
   Pack_Start (Make_Harness_Window.Hbox2, Make_Harness_Window.Vbox8, True, True, 3);

   Gtk_New (Make_Harness_Window.Label6, -("Procedure name :"));
   Set_Alignment (Make_Harness_Window.Label6, 1.0, 0.5);
   Set_Padding (Make_Harness_Window.Label6, 0, 0);
   Set_Justify (Make_Harness_Window.Label6, Justify_Center);
   Set_Line_Wrap (Make_Harness_Window.Label6, False);
   Pack_Start (Make_Harness_Window.Vbox8, Make_Harness_Window.Label6, False, False, 0);

   Gtk_New_Vbox (Make_Harness_Window.Vbox6, True, 0);
   Pack_Start (Make_Harness_Window.Vbox8, Make_Harness_Window.Vbox6, True, True, 0);

   Gtk_New (Make_Harness_Window.Label1, -("Suite file :"));
   Set_Alignment (Make_Harness_Window.Label1, 1.0, 0.5);
   Set_Padding (Make_Harness_Window.Label1, 0, 0);
   Set_Justify (Make_Harness_Window.Label1, Justify_Center);
   Set_Line_Wrap (Make_Harness_Window.Label1, False);
   Pack_Start (Make_Harness_Window.Vbox6, Make_Harness_Window.Label1, False, False, 0);

   Gtk_New_Vbox (Make_Harness_Window.Vbox9, True, 0);
   Pack_Start (Make_Harness_Window.Hbox2, Make_Harness_Window.Vbox9, True, True, 0);

   Gtk_New_Hbox (Make_Harness_Window.Hbox4, False, 0);
   Pack_Start (Make_Harness_Window.Vbox9, Make_Harness_Window.Hbox4, False, False, 0);

   Gtk_New (Make_Harness_Window.Procedure_Entry);
   Set_Editable (Make_Harness_Window.Procedure_Entry, True);
   Set_Max_Length (Make_Harness_Window.Procedure_Entry, 0);
   Set_Text (Make_Harness_Window.Procedure_Entry, -"Harness");
   Set_Visibility (Make_Harness_Window.Procedure_Entry, True);
   Pack_Start (Make_Harness_Window.Hbox4, Make_Harness_Window.Procedure_Entry, True, True, 3);
   Entry_Callback.Connect
     (Make_Harness_Window.Procedure_Entry, "activate",
      Entry_Callback.To_Marshaller (On_Procedure_Entry_Activate'Access));

   Gtk_New_Hbox (Make_Harness_Window.Hbox3, False, 0);
   Pack_Start (Make_Harness_Window.Vbox9, Make_Harness_Window.Hbox3, True, True, 0);

   Gtk_New (Make_Harness_Window.File_Name_Entry);
   Set_Editable (Make_Harness_Window.File_Name_Entry, True);
   Set_Max_Length (Make_Harness_Window.File_Name_Entry, 0);
   Set_Text (Make_Harness_Window.File_Name_Entry, -"");
   Set_Visibility (Make_Harness_Window.File_Name_Entry, True);
   Pack_Start (Make_Harness_Window.Hbox3, Make_Harness_Window.File_Name_Entry, True, True, 3);
   Entry_Callback.Connect
     (Make_Harness_Window.File_Name_Entry, "activate",
      Entry_Callback.To_Marshaller (On_Name_Entry_Activate'Access));

   Gtk_New_Vbox (Make_Harness_Window.Vbox10, True, 0);
   Pack_Start (Make_Harness_Window.Hbox3, Make_Harness_Window.Vbox10, False, False, 3);

   Gtk_New (Make_Harness_Window.Browse, -"Browse");
   Pack_Start (Make_Harness_Window.Vbox10, Make_Harness_Window.Browse, False, False, 0);
   Button_Callback.Connect
     (Make_Harness_Window.Browse, "clicked",
      Button_Callback.To_Marshaller (On_Browse_Clicked'Access));

   Gtk_New (Make_Harness_Window.Hbuttonbox1);
   Set_Spacing (Make_Harness_Window.Hbuttonbox1, 30);
   Set_Layout (Make_Harness_Window.Hbuttonbox1, Buttonbox_Spread);
   Set_Child_Size (Make_Harness_Window.Hbuttonbox1, 85, 27);
   Set_Child_Ipadding (Make_Harness_Window.Hbuttonbox1, 7, 0);
   Pack_Start (Make_Harness_Window.Vbox3, Make_Harness_Window.Hbuttonbox1, False, True, 0);

   Gtk_New (Make_Harness_Window.Ok, -"OK");
   Set_Flags (Make_Harness_Window.Ok, Can_Default);
   Button_Callback.Connect
     (Make_Harness_Window.Ok, "clicked",
      Button_Callback.To_Marshaller (On_Ok_Clicked'Access));
   Add (Make_Harness_Window.Hbuttonbox1, Make_Harness_Window.Ok);

   Gtk_New (Make_Harness_Window.Cancel, -"Cancel");
   Set_Flags (Make_Harness_Window.Cancel, Can_Default);
   Button_Callback.Connect
     (Make_Harness_Window.Cancel, "clicked",
      Button_Callback.To_Marshaller (On_Cancel_Clicked'Access));
   Add (Make_Harness_Window.Hbuttonbox1, Make_Harness_Window.Cancel);

   Gtk_New (Make_Harness_Window.Help, -"Help");
   Set_Flags (Make_Harness_Window.Help, Can_Default);
   Button_Callback.Connect
     (Make_Harness_Window.Help, "clicked",
      Button_Callback.To_Marshaller (On_Help_Clicked'Access));
   Add (Make_Harness_Window.Hbuttonbox1, Make_Harness_Window.Help);

   Gtk_New (Make_Harness_Window.Statusbar);
   Pack_Start (Make_Harness_Window.Vbox3, Make_Harness_Window.Statusbar, False, False, 0);

end Initialize;

end Make_Harness_Window_Pkg;
