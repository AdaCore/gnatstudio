with GVD.Process;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Text; use Gtk.Text;
with GVD.Main_Window;

package Glide_Page is

   type Glide_Page_Record is new GVD.Process.Debugger_Process_Tab_Record with
   record
      Console_Sw : Gtk_Scrolled_Window;
      Console : Gtk_Text;
   end record;
   type Glide_Page is access all Glide_Page_Record'Class;

   procedure Gtk_New
     (Page   : out Glide_Page;
      Window : access GVD.Main_Window.GVD_Main_Window_Record'Class);

   procedure Initialize
     (Page   : access Glide_Page_Record'Class;
      Window : access GVD.Main_Window.GVD_Main_Window_Record'Class);

end Glide_Page;
