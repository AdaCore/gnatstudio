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
   pragma Suppress (All_Checks);
begin
   Gtk.Window.Initialize (Process_Tab, Window_Toplevel);
   Set_Title (Process_Tab, -"Data");
   Set_Policy (Process_Tab, False, True, False);
   Set_Position (Process_Tab, Win_Pos_None);
   Set_Modal (Process_Tab, False);
   Set_Default_Size (Process_Tab, 500, 300);
   Return_Callback.Connect
     (Process_Tab, "delete_event", On_Process_Tab_Delete_Event'Access);

   Gtk_New_Hbox (Process_Tab.Process_Mdi, False, 0);
   Add (Process_Tab, Process_Tab.Process_Mdi);

   Gtk_New_Hbox (Process_Tab.Editor_Text, False, 0);
   Pack_Start (Process_Tab.Process_Mdi, Process_Tab.Editor_Text, True, True, 0);
   Return_Callback.Object_Connect
     (Process_Tab.Editor_Text, "delete_event", On_Editor_Text_Delete_Event'Access, Process_Tab);

end Initialize;

end Process_Tab_Pkg;
