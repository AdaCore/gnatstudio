with Gtk.Window; use Gtk.Window;
with Gtk.Box; use Gtk.Box;
with Gtk.Item_Factory; use Gtk.Item_Factory;
with Gtkada.MDI; use Gtkada.MDI;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Text; use Gtk.Text;
with Gtk.Status_Bar; use Gtk.Status_Bar;
with Gtk.Button; use Gtk.Button;

package Glide_Pkg is

   type Glide_Record is new Gtk_Window_Record with record
      Vbox : Gtk_Vbox;
      Factory : Gtk_Item_Factory;
      Mdi : MDI_Window;
      Console_Sw : Gtk_Scrolled_Window;
      Console : Gtk_Text;
      Statusbar : Gtk_Statusbar;
   end record;
   type Glide_Access is access all Glide_Record'Class;

   procedure Gtk_New (Glide : out Glide_Access);
   procedure Initialize (Glide : access Glide_Record'Class);

end Glide_Pkg;
