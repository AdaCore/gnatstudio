
with Wizards; use Wizards;
with Gtk.Main; use Gtk.Main;
with Gtk.Label; use Gtk.Label;
with Gtk.Handlers; use Gtk.Handlers;
with Gdk.Pixmap; use Gdk.Pixmap;
with Gdk.Bitmap; use Gdk.Bitmap;
with Gdk.Color; use Gdk.Color;
with Gtk.Widget; use Gtk.Widget;

procedure Test_Wizard is
   package Wiz_Cb is new Gtk.Handlers.Callback (Wizard_Record);
   procedure Custom_Next (Wiz : access Wizard_Record'Class);

   procedure Custom_Next (Wiz : access Wizard_Record'Class) is
   begin
      if Get_Current_Page (Wiz) = 2 then
         Set_Page (Wiz, 4);
         Emit_Stop_By_Name (Next_Button (Wiz), "clicked");
      end if;
   end Custom_Next;

   Wiz  : Wizard;
   Lab  : Gtk_Label;
   Pix  : Gdk_Pixmap;
   Mask : Gdk_Bitmap;

begin
   Gtk.Main.Init;

   Create_From_Xpm
     (Pix, null, Get_Default_Colormap, Mask, Null_Color, "splash.xpm");

   Gtk_New (Wiz, "Project setup", "#0476bc");
   Add_Logo (Wiz, Pix, Mask);
   Wiz_Cb.Object_Connect
     (Next_Button (Wiz), "clicked",
      Wiz_Cb.To_Marshaller (Custom_Next'Unrestricted_Access), Wiz);

   Gtk_New (Lab, "On Page 1");
   Add_Page (Wiz, Lab, "Step1");

   Gtk_New (Lab, "On Page 11 (next is page 4)");
   Add_Page (Wiz, Lab, "Step11", Level => 2);

   Gtk_New (Lab, "On Page 3");
   Add_Page (Wiz, Lab, "Step3");

   Gtk_New (Lab, "On Page 4");
   Add_Page (Wiz, Lab, "Step4");

   Set_Page (Wiz, 1);
   Show_All (Wiz);

   Main;
end Test_Wizard;
