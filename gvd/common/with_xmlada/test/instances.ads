with XML_Gtk.Readers;
with Glib.XML;

package Instances is
   package XML_Int is new Glib.XML (Integer);
   package Gtk_Readers is new XML_Gtk.Readers (Integer, 0, XML_Int);
end Instances;
