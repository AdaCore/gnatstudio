
with Gtk.Main;     use Gtk.Main;
with Creation_Wizard; use Creation_Wizard;
with Glide_Kernel;  use Glide_Kernel;

procedure Prj_Wizard is
   Wiz  : Creation_Wizard.Prj_Wizard;
   Kernel : Kernel_Handle;
begin
   Gtk.Main.Init;
   Gtk_New (Kernel, null);

   Gtk_New (Wiz, Kernel);
   Set_Current_Page (Wiz, 1);
   Show_All (Wiz);

   Main;
end Prj_Wizard;
