with Glib; use Glib;
with Gtk; use Gtk;
with Gdk.Types;       use Gdk.Types;
with Gtk.Widget;      use Gtk.Widget;
with Gtk.Enums;       use Gtk.Enums;
with Gtkada.Handlers; use Gtkada.Handlers;
with Callbacks_Odd; use Callbacks_Odd;
with Odd_Intl; use Odd_Intl;
with Main_Debug_Window_Pkg.Callbacks; use Main_Debug_Window_Pkg.Callbacks;

package body Main_Debug_Window_Pkg is

procedure Gtk_New (Main_Debug_Window : out Main_Debug_Window_Access) is
begin
   Main_Debug_Window := new Main_Debug_Window_Record;
   Main_Debug_Window_Pkg.Initialize (Main_Debug_Window);
end Gtk_New;

procedure Initialize (Main_Debug_Window : access Main_Debug_Window_Record'Class) is
   pragma Suppress (All_Checks);
begin
   Gtk.Window.Initialize (Main_Debug_Window, Window_Toplevel);
   Set_Title (Main_Debug_Window, -"The GNU Visual Debugger");
   Set_Policy (Main_Debug_Window, False, True, False);
   Set_Position (Main_Debug_Window, Win_Pos_None);
   Set_Modal (Main_Debug_Window, False);
   Set_Default_Size (Main_Debug_Window, 700, 700);
   Return_Callback.Connect
     (Main_Debug_Window, "delete_event", On_Main_Debug_Window_Delete_Event'Access);

   Gtk_New_Vbox (Main_Debug_Window.Vbox, False, 0);
   Add (Main_Debug_Window, Main_Debug_Window.Vbox);

   Gtk_New (Main_Debug_Window.Factory);
   Set_Shadow_Type (Main_Debug_Window.Factory, Shadow_Out);
   Pack_Start (Main_Debug_Window.Vbox, Main_Debug_Window.Factory, False, False, 0);

   Gtk_New_Vbox (Main_Debug_Window.Toolbar_Box, False, 0);
   Pack_Start (Main_Debug_Window.Vbox, Main_Debug_Window.Toolbar_Box, False, False, 0);

   Gtk_New (Main_Debug_Window.Frame);
   Set_Shadow_Type (Main_Debug_Window.Frame, Shadow_Etched_In);
   Pack_Start (Main_Debug_Window.Vbox, Main_Debug_Window.Frame, True, True, 0);

   Gtk_New (Main_Debug_Window.Process_Notebook);
   Set_Scrollable (Main_Debug_Window.Process_Notebook, True);
   Set_Show_Border (Main_Debug_Window.Process_Notebook, True);
   Set_Show_Tabs (Main_Debug_Window.Process_Notebook, False);
   Set_Tab_Hborder (Main_Debug_Window.Process_Notebook, 2);
   Set_Tab_Vborder (Main_Debug_Window.Process_Notebook, 2);
   Set_Tab_Pos (Main_Debug_Window.Process_Notebook, Pos_Top);
   Widget_Callback.Object_Connect
     (Main_Debug_Window.Process_Notebook, "switch_page", On_Process_Notebook_Switch_Page'Access, Main_Debug_Window);
   Add (Main_Debug_Window.Frame, Main_Debug_Window.Process_Notebook);

   Gtk_New (Main_Debug_Window.Statusbar);
   Pack_Start (Main_Debug_Window.Vbox, Main_Debug_Window.Statusbar, False, False, 0);

end Initialize;

end Main_Debug_Window_Pkg;
