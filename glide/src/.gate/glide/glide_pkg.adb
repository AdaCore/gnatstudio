with Glib; use Glib;
with Gtk; use Gtk;
with Gdk.Types;       use Gdk.Types;
with Gtk.Widget;      use Gtk.Widget;
with Gtk.Enums;       use Gtk.Enums;
with Gtkada.Handlers; use Gtkada.Handlers;
with Callbacks_Glide; use Callbacks_Glide;
with Glide_Intl; use Glide_Intl;

package body Glide_Pkg is

procedure Gtk_New (Glide : out Glide_Access) is
begin
   Glide := new Glide_Record;
   Glide_Pkg.Initialize (Glide);
end Gtk_New;

procedure Initialize (Glide : access Glide_Record'Class) is
   pragma Suppress (All_Checks);
begin
   Gtk.Window.Initialize (Glide, Window_Toplevel);
   Set_Title (Glide, -"GLIDE");
   Set_Policy (Glide, False, True, False);
   Set_Position (Glide, Win_Pos_None);
   Set_Modal (Glide, False);
   Set_Default_Size (Glide, 640, 480);

   Gtk_New_Vbox (Glide.Vbox, False, 0);
   Add (Glide, Glide.Vbox);

   Gtk_New (Glide.Factory);
   Pack_Start (Glide.Vbox, Glide.Factory, False, False, 0);

   Gtk_New_Vbox (Glide.Mdi, False, 0);
   Pack_Start (Glide.Vbox, Glide.Mdi, True, True, 0);

   Gtk_New (Glide.Console_Sw);
   Set_Policy (Glide.Console_Sw, Policy_Never, Policy_Always);
   Pack_Start (Glide.Mdi, Glide.Console_Sw, True, True, 0);

   Gtk_New (Glide.Console);
   Set_Editable (Glide.Console, False);
   Add (Glide.Console_Sw, Glide.Console);

   Gtk_New (Glide.Statusbar);
   Pack_Start (Glide.Vbox, Glide.Statusbar, False, False, 0);

end Initialize;

end Glide_Pkg;
