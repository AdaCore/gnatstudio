with Gtk.Window; use Gtk.Window;
with Gtk.Box; use Gtk.Box;
with Gtk.Button; use Gtk.Button;
package Process_Tab_Pkg is

   type Process_Tab_Record is new Gtk_Window_Record with record
      Process_Mdi : Gtk_Hbox;
      Editor_Text : Gtk_Hbox;
   end record;
   type Process_Tab_Access is access all Process_Tab_Record'Class;

   procedure Gtk_New (Process_Tab : out Process_Tab_Access);
   procedure Initialize (Process_Tab : access Process_Tab_Record'Class);

end Process_Tab_Pkg;
