with Gtk.Widget; use Gtk.Widget;
with Gtk.Main; use Gtk.Main;

package body Print_Dialog_Pkg.Callbacks is

   -----------------------------
   -- On_Print_Button_Clicked --
   -----------------------------

   procedure On_Print_Button_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
      Dialog : Print_Dialog_Access := Print_Dialog_Access (Object);
   begin
      Dialog.Variable := new String' (Get_Text (Dialog.Combo_Entry1));
      Main_Quit;
   end On_Print_Button_Clicked;

   ------------------------------
   -- On_Cancel_Button_Clicked --
   ------------------------------

   procedure On_Cancel_Button_Clicked
     (Object : access Gtk_Button_Record'Class)
   is
   begin
      Main_Quit;
   end On_Cancel_Button_Clicked;

end Print_Dialog_Pkg.Callbacks;
