with Glib;         use Glib;
with Gtk.Object;   use Gtk.Object;
with Gtkada.Types; use Gtkada.Types;

package body Common is

   Gui_Builder_Window_Quark : GQuark := Unknown_Quark;

   ---------------------------
   -- Is_Gui_Builder_Window --
   ---------------------------

   function Is_Gui_Builder_Window
     (Object : access Gtk.Object.Gtk_Object_Record'Class)
      return Boolean
   is
   begin
      if Gui_Builder_Window_Quark = Unknown_Quark then
         Gui_Builder_Window_Quark := Quark_From_String ("GBuilder_window");
      end if;

      return Boolean_User_Data.Get (Object, Gui_Builder_Window_Quark);
   exception
      when Data_Error =>
         return False;
   end Is_Gui_Builder_Window;

   ----------------------------
   -- Set_Gui_Builder_Window --
   ----------------------------

   procedure Set_Gui_Builder_Window
     (Object : access Gtk.Object.Gtk_Object_Record'Class)
   is
   begin
      if Gui_Builder_Window_Quark = Unknown_Quark then
         Gui_Builder_Window_Quark := Quark_From_String ("GBuilder_window");
      end if;

      Boolean_User_Data.Set (Object, True, Gui_Builder_Window_Quark);
   end Set_Gui_Builder_Window;
end Common;

