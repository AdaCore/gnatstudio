with GVD.Process;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Text; use Gtk.Text;
with Glide_Main_Window; use Glide_Main_Window;
with Gtk.Box; use Gtk.Box;
with Src_Editor_Box; use Src_Editor_Box;

package Glide_Page is

   type Glide_Page_Record is new GVD.Process.Debugger_Process_Tab_Record with
   record
      Console_Sw : Gtk_Scrolled_Window;
      Console    : Gtk_Text;
   end record;
   type Glide_Page is access all Glide_Page_Record'Class;

   procedure Gtk_New
     (Page   : out Glide_Page;
      Window : access Glide_Window_Record'Class);

   procedure Initialize
     (Page   : access Glide_Page_Record'Class;
      Window : access Glide_Window_Record'Class);

   type Source_Box_Record is new Gtk_Hbox_Record with record
      Editor : Source_Editor_Box;
   end record;
   type Source_Box is access all Source_Box_Record'Class;

   procedure Gtk_New
     (Box    : out Source_Box;
      Editor : Source_Editor_Box);
   --  Create a new source box.

   procedure Initialize
     (Box    : access Source_Box_Record'Class;
      Editor : Source_Editor_Box);
   --  Internal initialization function.

end Glide_Page;
