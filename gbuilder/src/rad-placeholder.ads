with Gtk_Drawing_Area; use Gtk_Drawing_Area;

package RAD.Placeholder is

   type RAD_Placeholder_Record is new Gtk_Drawing_Area_Record with
     null record;
   type RAD_Placeholder is access all RAD_Placeholder_Record'Class;
   --  RAD_Placeholder is a generic widget the draws a visual area in
   --  places that are not yet filled in the GUI.

   procedure Gtk_New (Placeholder : out RAD_Placeholder);
   --  Create a new placeholder.
   --  Connect all the default signals for a RAD widget

   procedure Initialize (Placeholder : access RAD_Placeholder_Record'Class);
   --  Internal initialize procedure.

end RAD.Placeholder;
