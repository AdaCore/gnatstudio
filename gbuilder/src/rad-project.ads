with Gtk.Widget; use Gtk.Widget;
with Gdk;

--  This file defines all the data needed to handle a project and to
--  handle each widget

package RAD.Project is

   --  Project wide options

   type RAD_Project is record
      Selected_Widgets   : Widget_List.Glist;
      Dragging_Widget    : Gtk_Widget;
      Drag_Action        : RAD_Drag_Action := Drag_None;
      Placeholder_Pixmap : Gdk.Gdk_Pixmap  := null;
   end record;

   --  Widget specific data

   type RAD_Flags is mod 2 ** 32;
   No_Flags           : constant RAD_Flags := 0;

   Visible            : constant RAD_Flags := 2 ** 0;
   --  If the widget is initially visible

   Sensitive          : constant RAD_Flags := 2 ** 1;
   --  If the widget is initially sensitive

   Grab_Default       : constant RAD_Flags := 2 ** 2;
   --  If it grabs the default

   Grab_Focus         : constant RAD_Flags := 2 ** 3;
   --  If it grabs the focus

   Style_Is_Unnamed   : constant RAD_Flags := 2 ** 4;
   --  If it's using its own unnamed style

   Style_Propagate    : constant RAD_Flags := 2 ** 5;
   --  If it propgates style to its children

   Active             : constant RAD_Flags := 2 ** 6;
   --  If it is initially active (for toggles)

   X_Set              : constant RAD_Flags := 2 ** 7;
   --  If the x pos is set explicitly

   Y_Set              : constant RAD_Flags := 2 ** 8;
   --  If the y pos is set explicitly

   Width_Set          : constant RAD_Flags := 2 ** 9;
   --  If the width is set explicitly

   Height_Set         : constant RAD_Flags := 2 ** 10;
   --  If the height is set explicitly

   Size_Not_Allocated : constant RAD_Flags := 2 ** 11;
   --  Internally used so that a widget's size and position properties aren't
   --  displayed until its area has been allocated.

   type Field_Visibility is (Private, Protected, Public);

   type Widget_Data is record
      RAD_Widget   : Boolean := True;
      Placeholder  : Boolean := False;
      Flags        : RAD_Flags := No_Flags;
      X            : Gint;
      Y            : Gint;
      Width        : Gint;
      Height       : Gint;

      --  ???
      --  gint           events;
      --  gchar         *tooltip;
      --  GList         *signals;
      --  GList         *accelerators;
      --  GbStyle       *gbstyle;

      Visibility   : Field_Visibility := Public;
   end Widget_Data;

end RAD.Project;
