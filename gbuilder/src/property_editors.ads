
with Glib;
with Glib.Object;
with Gtk.Notebook;
with Gtk.Table;
with Gtk.Tooltips;

package Property_Editors is

   type Property_Editor_Record is new Gtk.Notebook.Gtk_Notebook_Record
     with private;
   type Property_Editor is access all Property_Editor_Record'Class;

   procedure Gtk_New (Editor : out Property_Editor);
   --  Create a new property editor

   procedure Initialize (Editor : access Property_Editor_Record'Class);

   procedure Inspect
     (Editor : access Property_Editor_Record;
      Widget : access Glib.Object.GObject_Record'Class);
   --  Set the widget whose properties we want to edit.

private
   type Property_Editor_Record is new Gtk.Notebook.Gtk_Notebook_Record with
      record
         Prop_Table   : Gtk.Table.Gtk_Table;
         Signal_Table : Gtk.Table.Gtk_Table;
         Tooltips     : Gtk.Tooltips.Gtk_Tooltips;
      end record;
end Property_Editors;
