with Gtk.Drawing_Area;
with Gtk.Object;

package Placeholders is

   type Placeholder_Record is new Gtk.Drawing_Area.Gtk_Drawing_Area_Record
     with private;
   type Placeholder is access all Placeholder_Record'Class;

   procedure Gtk_New (Holder : out Placeholder);
   --  Create a new placeholder

   procedure Initialize (Holder : access Placeholder_Record'Class);
   --  Internal subprogram for creating new widgets

   function Is_Placeholder
     (Object : access Gtk.Object.Gtk_Object_Record'Class)
      return Boolean;
   --  Return true if Objet is a placeholder

   procedure Set_Placeholder
     (Object : access Gtk.Object.Gtk_Object_Record'Class);
   --  Indicate that Object is a placeholder

private
   type Placeholder_Record is new
     Gtk.Drawing_Area.Gtk_Drawing_Area_Record with null record;

end Placeholders;
