with Glib; use Glib;
with Gtk; use Gtk;
with Gdk.Types;       use Gdk.Types;
with Gtk.Widget;      use Gtk.Widget;
with Gtk.Enums;       use Gtk.Enums;
with Gtkada.Handlers; use Gtkada.Handlers;
with Callbacks_Odd; use Callbacks_Odd;
with Odd_Intl; use Odd_Intl;
with Open_Session_Pkg.Callbacks; use Open_Session_Pkg.Callbacks;

package body Open_Session_Pkg is

procedure Gtk_New (Open_Session : out Open_Session_Access) is
begin
   Open_Session := new Open_Session_Record;
   Open_Session_Pkg.Initialize (Open_Session);
end Gtk_New;

procedure Initialize (Open_Session : access Open_Session_Record'Class) is
begin
   Gtk.Window.Initialize (Open_Session, Window_Toplevel);
   Set_Title (Open_Session, -"Open Session");
   Set_Policy (Open_Session, False, True, False);
   Set_Position (Open_Session, Win_Pos_Center);
   Set_Modal (Open_Session, True);

   Gtk_New_Vbox (Open_Session.Vbox17, False, 0);
   Add (Open_Session, Open_Session.Vbox17);

   Gtk_New_Hbox (Open_Session.Hbox7, False, 0);
   Pack_Start (Open_Session.Vbox17, Open_Session.Hbox7, True, True, 0);

   Gtk_New_Vbox (Open_Session.Vbox18, False, 0);
   Pack_Start (Open_Session.Hbox7, Open_Session.Vbox18, True, True, 0);

   Gtk_New (Open_Session.Scrolledwindow10);
   Pack_Start (Open_Session.Vbox18, Open_Session.Scrolledwindow10, True, True, 0);
   Set_Policy (Open_Session.Scrolledwindow10, Policy_Automatic, Policy_Automatic);

   Gtk_New (Open_Session.Viewport1);
   Add (Open_Session.Scrolledwindow10, Open_Session.Viewport1);
   Set_Shadow_Type (Open_Session.Viewport1, Shadow_In);

   Gtk_New (Open_Session.List);
   List_Callback.Connect
     (Open_Session.List, "select_child", On_List_Select_Child'Access);
   Add (Open_Session.Viewport1, Open_Session.List);
   Set_Selection_Mode (Open_Session.List, Selection_Single);

   Gtk_New_Hbox (Open_Session.Hbox6, False, 0);
   Pack_Start (Open_Session.Vbox18, Open_Session.Hbox6, False, False, 7);

   Gtk_New (Open_Session.Label73, -("Session :"));
   Pack_Start (Open_Session.Hbox6, Open_Session.Label73, False, False, 7);
   Set_Alignment (Open_Session.Label73, 0.5, 0.5);
   Set_Padding (Open_Session.Label73, 0, 0);
   Set_Justify (Open_Session.Label73, Justify_Center);
   Set_Line_Wrap (Open_Session.Label73, False);

   Gtk_New (Open_Session.Entry1);
   Pack_Start (Open_Session.Hbox6, Open_Session.Entry1, True, True, 0);
   Set_Editable (Open_Session.Entry1, True);
   Set_Max_Length (Open_Session.Entry1, 0);
   Set_Text (Open_Session.Entry1, -"");
   Set_Visibility (Open_Session.Entry1, True);

   Gtk_New_Vseparator (Open_Session.Vseparator4);
   Pack_Start (Open_Session.Hbox7, Open_Session.Vseparator4, False, False, 7);

   Gtk_New_Vbox (Open_Session.Vbox19, False, 0);
   Pack_Start (Open_Session.Hbox7, Open_Session.Vbox19, True, True, 0);

   Gtk_New (Open_Session.Scrolledwindow11);
   Pack_Start (Open_Session.Vbox19, Open_Session.Scrolledwindow11, True, True, 0);
   Set_Policy (Open_Session.Scrolledwindow11, Policy_Automatic, Policy_Automatic);

   Gtk_New (Open_Session.Viewport2);
   Add (Open_Session.Scrolledwindow11, Open_Session.Viewport2);
   Set_Shadow_Type (Open_Session.Viewport2, Shadow_In);

   Gtk_New_Vbox (Open_Session.File_Buttons, False, 0);
   Add (Open_Session.Viewport2, Open_Session.File_Buttons);

   Gtk_New (Open_Session.Hbuttonbox10);
   Pack_Start (Open_Session.Vbox19, Open_Session.Hbuttonbox10, False, False, 0);
   Set_Spacing (Open_Session.Hbuttonbox10, 30);
   Set_Layout (Open_Session.Hbuttonbox10, Buttonbox_Spread);
   Set_Child_Size (Open_Session.Hbuttonbox10, 85, 27);
   Set_Child_Ipadding (Open_Session.Hbuttonbox10, 7, 0);

   Gtk_New (Open_Session.Select_All, -"Select all");
   Set_Flags (Open_Session.Select_All, Can_Default);
   Button_Callback.Connect
     (Open_Session.Select_All, "clicked",
      Button_Callback.To_Marshaller (On_Select_All_Clicked'Access));
   Add (Open_Session.Hbuttonbox10, Open_Session.Select_All);

   Gtk_New (Open_Session.Unselect_All, -"Unselect all");
   Set_Flags (Open_Session.Unselect_All, Can_Default);
   Button_Callback.Connect
     (Open_Session.Unselect_All, "clicked",
      Button_Callback.To_Marshaller (On_Unselect_All_Clicked'Access));
   Add (Open_Session.Hbuttonbox10, Open_Session.Unselect_All);

   Gtk_New_Hseparator (Open_Session.Hseparator1);
   Pack_Start (Open_Session.Vbox17, Open_Session.Hseparator1, False, False, 0);

   Gtk_New (Open_Session.Hbuttonbox9);
   Add (Open_Session.Vbox17, Open_Session.Hbuttonbox9);
   Set_Spacing (Open_Session.Hbuttonbox9, 30);
   Set_Layout (Open_Session.Hbuttonbox9, Buttonbox_Spread);
   Set_Child_Size (Open_Session.Hbuttonbox9, 85, 27);
   Set_Child_Ipadding (Open_Session.Hbuttonbox9, 7, 0);

   Gtk_New (Open_Session.Ok_Button, -"OK");
   Set_Flags (Open_Session.Ok_Button, Can_Default);
   Button_Callback.Connect
     (Open_Session.Ok_Button, "clicked",
      Button_Callback.To_Marshaller (On_Ok_Button_Clicked'Access));
   Add (Open_Session.Hbuttonbox9, Open_Session.Ok_Button);

   Gtk_New (Open_Session.Cancel_Button, -"Cancel");
   Set_Flags (Open_Session.Cancel_Button, Can_Default);
   Button_Callback.Connect
     (Open_Session.Cancel_Button, "clicked",
      Button_Callback.To_Marshaller (On_Cancel_Button_Clicked'Access));
   Add (Open_Session.Hbuttonbox9, Open_Session.Cancel_Button);

   Gtk_New (Open_Session.Help_Button, -"Help");
   Set_Flags (Open_Session.Help_Button, Can_Default);
   Button_Callback.Connect
     (Open_Session.Help_Button, "clicked",
      Button_Callback.To_Marshaller (On_Help_Button_Clicked'Access));
   Add (Open_Session.Hbuttonbox9, Open_Session.Help_Button);

end Initialize;

end Open_Session_Pkg;
