with Gtk.Main;

package body Src_Cb is

   procedure Exit_Main (Object : access Gtk_Widget_Record'Class) is
   begin
      Gtk.Main.Main_Quit;
   end Exit_Main;

end Src_Cb;
