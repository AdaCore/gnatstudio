
with Gdk.Bitmap;   use Gdk.Bitmap;
with Gdk.Color;    use Gdk.Color;
with Gdk.Pixmap;   use Gdk.Pixmap;
with Gtk.Box;      use Gtk.Box;
with Gtk.GEntry;   use Gtk.GEntry;
with Gtk.Button;   use Gtk.Button;
with Gtk.Label;    use Gtk.Label;
with Gtkada.Types; use Gtkada.Types;
with Wizards;      use Wizards;
with Gtk.Widget;   use Gtk.Widget;

package body Creation_Wizard is

   Logo_Xpm : aliased Chars_Ptr_Array (0 .. 0);
   pragma Import (C, Logo_Xpm, "logo_xpm");

   function First_Page return Gtk_Widget;
   --  Return the widget to use for the contents of the first page

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Wiz : out Prj_Wizard) is
   begin
      Wiz := new Prj_Wizard_Record;
      Creation_Wizard.Initialize (Wiz);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Wiz : access Prj_Wizard_Record'Class) is
      Pix  : Gdk_Pixmap;
      Mask : Gdk_Bitmap;
   begin
      Wizards.Initialize (Wiz, "Project setup", "#0476bc");

      Create_From_Xpm_D
        (Pix, null, Get_Default_Colormap, Mask, Null_Color, Logo_Xpm);
      Add_Logo (Wiz, Pix, Mask);

      Add_Page (Wiz, First_Page, "Naming the project");
   end Initialize;

   ----------------
   -- First_Page --
   ----------------

   function First_Page return Gtk_Widget is
      Box    : Gtk_Box;
      Hbox   : Gtk_Box;
      Label  : Gtk_Label;
      Button : Gtk_Button;
      Ent    : Gtk_Entry;
   begin
      Gtk_New_Vbox (Box, Homogeneous => False, Spacing => 8);
      Set_Border_Width (Box, 5);

      Gtk_New (Label, "Creating a new project");
      Pack_Start (Box, Label, Fill => True, Expand => True);

      Gtk_New_Hbox (Hbox, Homogeneous => False, Spacing => 8);
      Pack_End (Box, Hbox, Expand => False, Fill => False);
      Gtk_New (Ent, 255);
      Pack_Start (Hbox, Ent, Expand => True, Fill => True);
      Gtk_New (Button, "...");
      Pack_Start (Hbox, Button, Expand => False, Fill => False);

      Gtk_New (Label, "Enter the directory where to copy the file to:");
      Pack_End (Box, Label, Expand => False, Fill => False);

      Gtk_New_Hbox (Hbox, Homogeneous => False, Spacing => 8);
      Pack_End (Box, Hbox, Expand => False, Fill => False);
      Gtk_New (Ent, 255);
      Pack_Start (Hbox, Ent, Expand => True, Fill => True);
      Gtk_New (Button, "...");
      Pack_Start (Hbox, Button, Expand => False, Fill => False);

      Gtk_New (Label, "Enter the name of the project to create:");
      Pack_End (Box, Label, Expand => False, Fill => False);

      return Gtk_Widget (Box);
   end First_Page;

end Creation_Wizard;
