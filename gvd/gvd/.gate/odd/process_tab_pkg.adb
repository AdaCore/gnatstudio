with Glib; use Glib;
with Gtk; use Gtk;
with Gdk.Types;       use Gdk.Types;
with Gtk.Widget;      use Gtk.Widget;
with Gtk.Enums;       use Gtk.Enums;
with Gtkada.Handlers; use Gtkada.Handlers;
with Callbacks_Odd; use Callbacks_Odd;
with Odd_Intl; use Odd_Intl;
with Process_Tab_Pkg.Callbacks; use Process_Tab_Pkg.Callbacks;

package body Process_Tab_Pkg is

procedure Gtk_New (Process_Tab : out Process_Tab_Access) is
begin
   Process_Tab := new Process_Tab_Record;
   Process_Tab_Pkg.Initialize (Process_Tab);
end Gtk_New;

procedure Initialize (Process_Tab : access Process_Tab_Record'Class) is
begin
   Gtk.Window.Initialize (Process_Tab, Window_Toplevel);
   Set_Title (Process_Tab, -"window1");
   Set_Policy (Process_Tab, False, True, False);
   Set_Position (Process_Tab, Win_Pos_None);
   Set_Modal (Process_Tab, False);

   Gtk_New_Vpaned (Process_Tab.Process_Paned);
   Add (Process_Tab, Process_Tab.Process_Paned);
   Set_Handle_Size (Process_Tab.Process_Paned, 10);
   Set_Gutter_Size (Process_Tab.Process_Paned, 6);

   Gtk_New_Vpaned (Process_Tab.Vpaned6);
   Add (Process_Tab.Process_Paned, Process_Tab.Vpaned6);
   Set_Handle_Size (Process_Tab.Vpaned6, 10);
   Set_Gutter_Size (Process_Tab.Vpaned6, 6);

   Gtk_New (Process_Tab.Scrolledwindow9);
   Add (Process_Tab.Vpaned6, Process_Tab.Scrolledwindow9);
   Set_Policy (Process_Tab.Scrolledwindow9, Policy_Automatic, Policy_Automatic);

   Gtk_New (Process_Tab.Data_Canvas);
   Add (Process_Tab.Scrolledwindow9, Process_Tab.Data_Canvas);
   Set_Shadow_Type (Process_Tab.Data_Canvas, Shadow_In);

   Gtk_New (Process_Tab.Thread_Notebook);
   Add (Process_Tab.Vpaned6, Process_Tab.Thread_Notebook);
   Set_Scrollable (Process_Tab.Thread_Notebook, True);
   Set_Show_Border (Process_Tab.Thread_Notebook, True);
   Set_Show_Tabs (Process_Tab.Thread_Notebook, True);
   Set_Tab_Hborder (Process_Tab.Thread_Notebook, 2);
   Set_Tab_Vborder (Process_Tab.Thread_Notebook, 2);
   Set_Tab_Pos (Process_Tab.Thread_Notebook, Pos_Top);

   Gtk_New (Process_Tab.Scrolledwindow8);
   Add (Process_Tab.Thread_Notebook, Process_Tab.Scrolledwindow8);
   Set_Policy (Process_Tab.Scrolledwindow8, Policy_Never, Policy_Always);

   Gtk_New (Process_Tab.Editor_Text, 1);
   Add (Process_Tab.Scrolledwindow8, Process_Tab.Editor_Text);
   Set_Selection_Mode (Process_Tab.Editor_Text, Selection_Single);
   Set_Shadow_Type (Process_Tab.Editor_Text, Shadow_In);
   Set_Show_Titles (Process_Tab.Editor_Text, False);
   Set_Column_Width (Process_Tab.Editor_Text, 0, 80);

   Gtk_New (Process_Tab.Label54, -("label54"));
   Set_Alignment (Process_Tab.Label54, 0.5, 0.5);
   Set_Padding (Process_Tab.Label54, 0, 0);
   Set_Justify (Process_Tab.Label54, Justify_Center);
   Set_Line_Wrap (Process_Tab.Label54, False);
   Set_Column_Widget (Process_Tab.Editor_Text, 0, Process_Tab.Label54);

   Gtk_New (Process_Tab.Label52, -("Main Thread"));
   Set_Alignment (Process_Tab.Label52, 0.5, 0.5);
   Set_Padding (Process_Tab.Label52, 0, 0);
   Set_Justify (Process_Tab.Label52, Justify_Center);
   Set_Line_Wrap (Process_Tab.Label52, False);
   Set_Tab (Process_Tab.Thread_Notebook, 0, Process_Tab.Label52);

   Gtk_New (Process_Tab.Scrolledwindow7);
   Add (Process_Tab.Process_Paned, Process_Tab.Scrolledwindow7);
   Set_Policy (Process_Tab.Scrolledwindow7, Policy_Never, Policy_Always);

   Gtk_New (Process_Tab.Debugger_Text);
   Widget_Callback.Object_Connect
     (Process_Tab.Debugger_Text, "insert_text", On_Debugger_Text_Insert_Text'Access, Process_Tab);
   Widget_Callback.Object_Connect
     (Process_Tab.Debugger_Text, "delete_text", On_Debugger_Text_Delete_Text'Access, Process_Tab);
   Add (Process_Tab.Scrolledwindow7, Process_Tab.Debugger_Text);
   Set_Editable (Process_Tab.Debugger_Text, True);

end Initialize;

end Process_Tab_Pkg;
