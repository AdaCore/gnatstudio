
with Gtk.Main;     use Gtk.Main;
with Creation_Wizard; use Creation_Wizard;

procedure Prj_Wizard is
   Wiz  : Creation_Wizard.Prj_Wizard;
begin
   Gtk.Main.Init;

   Gtk_New (Wiz);
   Set_Page (Wiz, 1);
   Show_All (Wiz);

   Main;
end Prj_Wizard;
