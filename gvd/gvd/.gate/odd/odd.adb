with Gtk; use Gtk;
with Gtk.Main;
with Gtk.Widget; use Gtk.Widget;
with Main_Debug_Window_Pkg; use Main_Debug_Window_Pkg;
with Odd_Preferences_Pkg; use Odd_Preferences_Pkg;
with Process_Tab_Pkg; use Process_Tab_Pkg;
with Breakpoints_Pkg; use Breakpoints_Pkg;
with Advanced_Breakpoint_Pkg; use Advanced_Breakpoint_Pkg;
with Open_Program_Pkg; use Open_Program_Pkg;
with Open_Session_Pkg; use Open_Session_Pkg;
with Memory_View_Pkg; use Memory_View_Pkg;

procedure Odd is
begin
   Gtk.Main.Set_Locale;
   Gtk.Main.Init;
   Gtk_New (Main_Debug_Window);
   Show_All (Main_Debug_Window);
   Gtk_New (Odd_Preferences);
   Show_All (Odd_Preferences);
   Gtk_New (Process_Tab);
   Show_All (Process_Tab);
   Gtk_New (Breakpoints);
   Show_All (Breakpoints);
   Gtk_New (Advanced_Breakpoint);
   Show_All (Advanced_Breakpoint);
   Gtk_New (Open_Program);
   Show_All (Open_Program);
   Gtk_New (Open_Session);
   Show_All (Open_Session);
   Gtk_New (Memory_View);
   Show_All (Memory_View);
   Gtk.Main.Main;
end Odd;
