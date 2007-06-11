with Gtk; use Gtk;
with Gtk.Main;
with Gtk.Window; use Gtk.Window;
with Gui_Builder_Pkg; use Gui_Builder_Pkg;
with RAD.Editor; use RAD.Editor;

procedure Radical is
   Window : Gtk_Window;
begin
   Gtk.Main.Set_Locale;
   Gtk.Main.Init;
   Gtk_New (Gui_Builder);
   Show_All (Gui_Builder);
   Gtk_New (Window);
   Set_Name (Window, "window_test");
   Add_Mouse_Signals (Window);
   Add_Draw_Signals (Window);
   Add_Key_Signals (Window);
   Show_All (Window);
   Gtk.Main.Main;
end Radical;
