with Glib; use Glib;
with Gtk; use Gtk;
with Gdk.Types;       use Gdk.Types;
with Gtk.Widget;      use Gtk.Widget;
with Gtk.Enums;       use Gtk.Enums;
with Gtkada.Handlers; use Gtkada.Handlers;
with Callbacks_Prj_Edit; use Callbacks_Prj_Edit;
with Prj_Edit_Intl; use Prj_Edit_Intl;

package body Wizard_Window_Pkg is

procedure Gtk_New (Wizard_Window : out Wizard_Window_Access) is
begin
   Wizard_Window := new Wizard_Window_Record;
   Wizard_Window_Pkg.Initialize (Wizard_Window);
end Gtk_New;

procedure Initialize (Wizard_Window : access Wizard_Window_Record'Class) is
   pragma Suppress (All_Checks);
begin
   Gtk.Window.Initialize (Wizard_Window, Window_Toplevel);
   Set_Title (Wizard_Window, -"");
   Set_Policy (Wizard_Window, True, True, True);
   Set_Position (Wizard_Window, Win_Pos_None);
   Set_Modal (Wizard_Window, False);

   Gtk_New_Vbox (Wizard_Window.Vbox1, False, 0);
   Add (Wizard_Window, Wizard_Window.Vbox1);

   Gtk_New_Hbox (Wizard_Window.Page_Box, False, 0);
   Set_Border_Width (Wizard_Window.Page_Box, 4);
   Pack_Start (Wizard_Window.Vbox1, Wizard_Window.Page_Box, True, True, 0);

   Gtk_New (Wizard_Window.Eventbox1);
   Pack_Start (Wizard_Window.Page_Box, Wizard_Window.Eventbox1, False, True, 0);

   Gtk_New_Vbox (Wizard_Window.Toc_Box, False, 6);
   Set_Border_Width (Wizard_Window.Toc_Box, 7);
   Add (Wizard_Window.Eventbox1, Wizard_Window.Toc_Box);

   Gtk_New (Wizard_Window.Notebook);
   Set_Scrollable (Wizard_Window.Notebook, False);
   Set_Show_Border (Wizard_Window.Notebook, False);
   Set_Show_Tabs (Wizard_Window.Notebook, False);
   Set_Tab_Hborder (Wizard_Window.Notebook, 2);
   Set_Tab_Vborder (Wizard_Window.Notebook, 2);
   Set_Tab_Pos (Wizard_Window.Notebook, Pos_Top);
   Pack_Start (Wizard_Window.Page_Box, Wizard_Window.Notebook, True, True, 0);

   Gtk_New (Wizard_Window.Label1, -("label1"));
   Set_Alignment (Wizard_Window.Label1, 0.5, 0.5);
   Set_Padding (Wizard_Window.Label1, 0, 0);
   Set_Justify (Wizard_Window.Label1, Justify_Center);
   Set_Line_Wrap (Wizard_Window.Label1, False);
   Set_Tab (Wizard_Window.Notebook, 0, Wizard_Window.Label1);

   Gtk_New_Hseparator (Wizard_Window.Hseparator1);
   Pack_Start (Wizard_Window.Vbox1, Wizard_Window.Hseparator1, False, False, 0);

   Gtk_New (Wizard_Window.Hbuttonbox1);
   Set_Spacing (Wizard_Window.Hbuttonbox1, 30);
   Set_Layout (Wizard_Window.Hbuttonbox1, Buttonbox_End);
   Set_Child_Size (Wizard_Window.Hbuttonbox1, 85, 27);
   Set_Child_Ipadding (Wizard_Window.Hbuttonbox1, 7, 0);
   Pack_Start (Wizard_Window.Vbox1, Wizard_Window.Hbuttonbox1, False, False, 0);

   Gtk_New (Wizard_Window.Previous);
   Set_Flags (Wizard_Window.Previous, Can_Default);
   Add (Wizard_Window.Hbuttonbox1, Wizard_Window.Previous);

   Gtk_New_Hbox (Wizard_Window.Hbox1, False, 0);
   Add (Wizard_Window.Previous, Wizard_Window.Hbox1);

   Wizard_Window.Pixmap1 := Create_Pixmap ("stock_left_arrow_xpm", Wizard_Window);
   Set_Alignment (Wizard_Window.Pixmap1, 0.5, 0.5);
   Set_Padding (Wizard_Window.Pixmap1, 0, 0);
   Pack_Start (Wizard_Window.Hbox1, Wizard_Window.Pixmap1, False, True, 0);

   Gtk_New (Wizard_Window.Label2, -("Prev"));
   Set_Alignment (Wizard_Window.Label2, 0.5, 0.5);
   Set_Padding (Wizard_Window.Label2, 0, 0);
   Set_Justify (Wizard_Window.Label2, Justify_Center);
   Set_Line_Wrap (Wizard_Window.Label2, False);
   Pack_Start (Wizard_Window.Hbox1, Wizard_Window.Label2, True, True, 0);

   Gtk_New (Wizard_Window.Next);
   Set_Flags (Wizard_Window.Next, Can_Default);
   Add (Wizard_Window.Hbuttonbox1, Wizard_Window.Next);

   Gtk_New_Hbox (Wizard_Window.Hbox2, False, 0);
   Add (Wizard_Window.Next, Wizard_Window.Hbox2);

   Gtk_New (Wizard_Window.Label3, -("Next"));
   Set_Alignment (Wizard_Window.Label3, 0.5, 0.5);
   Set_Padding (Wizard_Window.Label3, 0, 0);
   Set_Justify (Wizard_Window.Label3, Justify_Center);
   Set_Line_Wrap (Wizard_Window.Label3, False);
   Pack_Start (Wizard_Window.Hbox2, Wizard_Window.Label3, True, True, 0);

   Wizard_Window.Pixmap2 := Create_Pixmap ("stock_right_arrow_xpm", Wizard_Window);
   Set_Alignment (Wizard_Window.Pixmap2, 0.5, 0.5);
   Set_Padding (Wizard_Window.Pixmap2, 0, 0);
   Pack_Start (Wizard_Window.Hbox2, Wizard_Window.Pixmap2, False, True, 0);

   Gtk_New (Wizard_Window.Finish);
   Set_Flags (Wizard_Window.Finish, Can_Default);
   Add (Wizard_Window.Hbuttonbox1, Wizard_Window.Finish);

   Gtk_New_Hbox (Wizard_Window.Hbox5, False, 0);
   Add (Wizard_Window.Finish, Wizard_Window.Hbox5);

   Wizard_Window.Pixmap4 := Create_Pixmap ("stock_button_apply_xpm", Wizard_Window);
   Set_Alignment (Wizard_Window.Pixmap4, 0.5, 0.5);
   Set_Padding (Wizard_Window.Pixmap4, 0, 0);
   Pack_Start (Wizard_Window.Hbox5, Wizard_Window.Pixmap4, False, True, 0);

   Gtk_New (Wizard_Window.Label5, -("Apply"));
   Set_Alignment (Wizard_Window.Label5, 0.5, 0.5);
   Set_Padding (Wizard_Window.Label5, 0, 0);
   Set_Justify (Wizard_Window.Label5, Justify_Center);
   Set_Line_Wrap (Wizard_Window.Label5, False);
   Pack_Start (Wizard_Window.Hbox5, Wizard_Window.Label5, True, True, 0);

   Gtk_New (Wizard_Window.Cancel);
   Set_Flags (Wizard_Window.Cancel, Can_Default);
   Add (Wizard_Window.Hbuttonbox1, Wizard_Window.Cancel);

   Gtk_New_Hbox (Wizard_Window.Hbox4, False, 0);
   Add (Wizard_Window.Cancel, Wizard_Window.Hbox4);

   Wizard_Window.Pixmap3 := Create_Pixmap ("stock_button_cancel_xpm", Wizard_Window);
   Set_Alignment (Wizard_Window.Pixmap3, 0.5, 0.5);
   Set_Padding (Wizard_Window.Pixmap3, 0, 0);
   Pack_Start (Wizard_Window.Hbox4, Wizard_Window.Pixmap3, False, True, 0);

   Gtk_New (Wizard_Window.Label4, -("Cancel"));
   Set_Alignment (Wizard_Window.Label4, 0.5, 0.5);
   Set_Padding (Wizard_Window.Label4, 0, 0);
   Set_Justify (Wizard_Window.Label4, Justify_Center);
   Set_Line_Wrap (Wizard_Window.Label4, False);
   Pack_Start (Wizard_Window.Hbox4, Wizard_Window.Label4, True, True, 0);

end Initialize;

end Wizard_Window_Pkg;
