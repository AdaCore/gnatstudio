with Glib; use Glib;
with Gtk; use Gtk;
with Gdk.Types;       use Gdk.Types;
with Gtk.Widget;      use Gtk.Widget;
with Gtk.Enums;       use Gtk.Enums;
with Gtkada.Handlers; use Gtkada.Handlers;
with Callbacks_Switches_Glade; use Callbacks_Switches_Glade;
with Switches_Glade_Intl; use Switches_Glade_Intl;
with Variable_Editor_Pkg.Callbacks; use Variable_Editor_Pkg.Callbacks;

package body Variable_Editor_Pkg is

procedure Gtk_New (Variable_Editor : out Variable_Editor_Access) is
begin
   Variable_Editor := new Variable_Editor_Record;
   Variable_Editor_Pkg.Initialize (Variable_Editor);
end Gtk_New;

procedure Initialize (Variable_Editor : access Variable_Editor_Record'Class) is
   pragma Suppress (All_Checks);
begin
   Gtk.Window.Initialize (Variable_Editor, Window_Toplevel);
   Set_Title (Variable_Editor, -"List of variables");
   Set_Policy (Variable_Editor, True, True, True);
   Set_Position (Variable_Editor, Win_Pos_None);
   Set_Modal (Variable_Editor, False);
   Set_Default_Size (Variable_Editor, 640, 400);

   Gtk_New_Vbox (Variable_Editor.Vbox31, False, 0);
   Add (Variable_Editor, Variable_Editor.Vbox31);

   Gtk_New (Variable_Editor.Scrolledwindow3);
   Set_Policy (Variable_Editor.Scrolledwindow3, Policy_Automatic, Policy_Always);
   Pack_Start (Variable_Editor.Vbox31, Variable_Editor.Scrolledwindow3, True, True, 0);

   Gtk_New (Variable_Editor.Viewport2);
   Set_Shadow_Type (Variable_Editor.Viewport2, Shadow_In);
   Add (Variable_Editor.Scrolledwindow3, Variable_Editor.Viewport2);

   Gtk_New (Variable_Editor.List_Variables, 2, 5, False);
   Set_Row_Spacings (Variable_Editor.List_Variables, 0);
   Set_Col_Spacings (Variable_Editor.List_Variables, 6);
   Add (Variable_Editor.Viewport2, Variable_Editor.List_Variables);

   Gtk_New (Variable_Editor.Label52, -("Name"));
   Set_Alignment (Variable_Editor.Label52, 0.5, 0.5);
   Set_Padding (Variable_Editor.Label52, 0, 0);
   Set_Justify (Variable_Editor.Label52, Justify_Center);
   Set_Line_Wrap (Variable_Editor.Label52, False);
   Attach (Variable_Editor.List_Variables, Variable_Editor.Label52, 2, 3, 0, 1,
     0, 0,
     0, 0);

   Gtk_New (Variable_Editor.Label53, -("Current Value"));
   Set_Alignment (Variable_Editor.Label53, 0.5, 0.5);
   Set_Padding (Variable_Editor.Label53, 0, 0);
   Set_Justify (Variable_Editor.Label53, Justify_Center);
   Set_Line_Wrap (Variable_Editor.Label53, False);
   Attach (Variable_Editor.List_Variables, Variable_Editor.Label53, 3, 4, 0, 1,
     0, 0,
     0, 0);

   Gtk_New (Variable_Editor.Label54, -("Environment Var."));
   Set_Alignment (Variable_Editor.Label54, 0.5, 0.5);
   Set_Padding (Variable_Editor.Label54, 0, 0);
   Set_Justify (Variable_Editor.Label54, Justify_Center);
   Set_Line_Wrap (Variable_Editor.Label54, False);
   Attach (Variable_Editor.List_Variables, Variable_Editor.Label54, 4, 5, 0, 1,
     0, 0,
     0, 0);

   Gtk_New_Hseparator (Variable_Editor.Hseparator3);
   Attach (Variable_Editor.List_Variables, Variable_Editor.Hseparator3, 0, 5, 1, 2,
     Expand or Fill, Fill,
     0, 5);

   Gtk_New (Variable_Editor.Hbuttonbox2);
   Set_Spacing (Variable_Editor.Hbuttonbox2, 30);
   Set_Layout (Variable_Editor.Hbuttonbox2, Buttonbox_Spread);
   Set_Child_Size (Variable_Editor.Hbuttonbox2, 85, 27);
   Set_Child_Ipadding (Variable_Editor.Hbuttonbox2, 7, 0);
   Pack_Start (Variable_Editor.Vbox31, Variable_Editor.Hbuttonbox2, False, False, 0);

   Gtk_New (Variable_Editor.Add_Button);
   Set_Flags (Variable_Editor.Add_Button, Can_Default);
   Widget_Callback.Object_Connect
     (Variable_Editor.Add_Button, "clicked",
      Widget_Callback.To_Marshaller (On_Add_Clicked'Access), Variable_Editor);
   Add (Variable_Editor.Hbuttonbox2, Variable_Editor.Add_Button);

   Gtk_New_Hbox (Variable_Editor.Hbox4, False, 0);
   Add (Variable_Editor.Add_Button, Variable_Editor.Hbox4);

   Variable_Editor.Pixmap1 := Create_Pixmap ("stock_add_xpm", Variable_Editor);
   Set_Alignment (Variable_Editor.Pixmap1, 0.5, 0.5);
   Set_Padding (Variable_Editor.Pixmap1, 0, 0);
   Pack_Start (Variable_Editor.Hbox4, Variable_Editor.Pixmap1, False, False, 0);

   Gtk_New (Variable_Editor.Label57, -("Add..."));
   Set_Alignment (Variable_Editor.Label57, 0.5, 0.5);
   Set_Padding (Variable_Editor.Label57, 0, 0);
   Set_Justify (Variable_Editor.Label57, Justify_Center);
   Set_Line_Wrap (Variable_Editor.Label57, False);
   Pack_Start (Variable_Editor.Hbox4, Variable_Editor.Label57, True, True, 0);

   Gtk_New (Variable_Editor.Close_Button);
   Set_Flags (Variable_Editor.Close_Button, Can_Default);
   Widget_Callback.Object_Connect
     (Variable_Editor.Close_Button, "clicked",
      Widget_Callback.To_Marshaller (On_Close_Clicked'Access), Variable_Editor);
   Add (Variable_Editor.Hbuttonbox2, Variable_Editor.Close_Button);

   Gtk_New_Hbox (Variable_Editor.Hbox5, False, 0);
   Add (Variable_Editor.Close_Button, Variable_Editor.Hbox5);

   Variable_Editor.Pixmap2 := Create_Pixmap ("stock_close_xpm", Variable_Editor);
   Set_Alignment (Variable_Editor.Pixmap2, 0.5, 0.5);
   Set_Padding (Variable_Editor.Pixmap2, 0, 0);
   Pack_Start (Variable_Editor.Hbox5, Variable_Editor.Pixmap2, False, False, 0);

   Gtk_New (Variable_Editor.Label58, -("Close"));
   Set_Alignment (Variable_Editor.Label58, 0.5, 0.5);
   Set_Padding (Variable_Editor.Label58, 0, 0);
   Set_Justify (Variable_Editor.Label58, Justify_Center);
   Set_Line_Wrap (Variable_Editor.Label58, False);
   Pack_Start (Variable_Editor.Hbox5, Variable_Editor.Label58, True, True, 0);

end Initialize;

end Variable_Editor_Pkg;
