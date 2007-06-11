with Gtk.Object;

package Common is

   package Boolean_User_Data is new Gtk.Object.User_Data (Boolean);

   function Is_Gui_Builder_Window
     (Object : access Gtk.Object.Gtk_Object_Record'Class)
      return Boolean;
   --  Return True if Object is a window currently build in the GUI builder
   --  ??? Could this apply to GObject_Record instead ?

   procedure Set_Gui_Builder_Window
     (Object : access Gtk.Object.Gtk_Object_Record'Class);
   --  Indicate that Object is currently build in the GUI builder

end Common;
