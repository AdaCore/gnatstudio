with Gtk.Item_Factory;
with Gtk.Window;
with Src_Editor_Box;

package Src_Menu is

   Root : constant String := "<root>";

   procedure Create_Menu
     (Menu : out Gtk.Item_Factory.Gtk_Item_Factory;
      Win  : Gtk.Window.Gtk_Window;
      Box  : Src_Editor_Box.Source_Editor_Box);
   --  Create the menu for the source editor...

end Src_Menu;
