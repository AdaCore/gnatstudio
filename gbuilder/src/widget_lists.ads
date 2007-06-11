with Glib;
with Gtk.Notebook;
with Gtk.Tooltips;
with Gtk.Widget;

with Interfaces.C.Strings;

package Widget_Lists is

   type Widget_List_Record is new Gtk.Widget.Gtk_Widget_Record with private;
   type Widget_List is access all Widget_List_Record'Class;

   procedure Gtk_New (List : out Widget_List);
   --  Create a new instance of the widget list

   procedure Initialize (List : access Widget_List_Record'Class);
   --  Internal function for creating new widgets.

   procedure Set_Page_Title
     (List  : access Widget_List_Record;
      Page  : Glib.Gint;
      Title : String);
   --  Set the title for a specific page of list.
   --  If the page doesn't exist, it is created.

   procedure Add_Widget
     (List        : access Widget_List_Record;
      Page        : Glib.Gint;
      Name        : String;
      Pixmap      : Interfaces.C.Strings.chars_ptr_array;
      Object_Type : Glib.GType);
   --  Add a new entry in List for a widget called Name in a specific
   --  page.
   --  If the page doesn't exist, it is created.

private
   type Widget_List_Record is new Gtk.Notebook.Gtk_Notebook_Record with
      record
         Tooltips : Gtk.Tooltips.Gtk_Tooltips;
      end record;
end Widget_Lists;
