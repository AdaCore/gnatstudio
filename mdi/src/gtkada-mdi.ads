with Glib;
with Gdk.GC;
with Gdk.Types;
with Gtk.Event_Box;
with Gtk.Layout;
with Gtk.Widget;
with GNAT.OS_Lib;

--  TODO:
--  - emit "delete_event" when clicking on the "x" button
--  - handles docking (widgets that can't move)
--  - handles floating widgets (have their own window)
--  - handles multiple views of the MDI (through several top-level windows)
--  - have an icon-bar somewhere (or notebook) to list all the children
--  - Provide automatic initial placement of widgets
--  - Provide non-opaque resizing

package Gtkada.MDI is

   type MDI_Window_Record is new Gtk.Widget.Gtk_Widget_Record with private;
   type MDI_Window is access all MDI_Window_Record'Class;
   --  Although this widget is implemented as a gtk_layout, you shouldn't
   --  use the standard Gtk_Layout functions like Put and Move yourself.

   procedure Gtk_New (MDI : out MDI_Window);
   --  Create a new MDI window

   procedure Initialize (MDI : access MDI_Window_Record'Class);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   procedure Put (MDI : access MDI_Window_Record;
                  Child : access Gtk.Widget.Gtk_Widget_Record'Class;
                  Name : String);
   --  Add a new child to the MDI window.
   --  You shouldn't access Child directly afterwards if it was a Gtk_Window,
   --  but should manipulate its child instead. However, as a special
   --  exception, you can still pass Child as a parameter to the subprograms
   --  in this package to manipulate it (for instance in Raise_Child,...)

   procedure Raise_Child
     (MDI : access MDI_Window_Record;
      Child : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Put Child in the foreground.

   procedure Cascade_Children (MDI : access MDI_Window_Record);
   --  All the children are stacked so that the focus widget is on top.
   --  They overlap each other, but all the title bars are left visible

   procedure Tile_Children (MDI : access MDI_Window_Record);
   --  The available space in the MDI is shared equally between all children.
   --  They do not overlap each other.

private
   type State_Type is (Normal, Iconified);

   type MDI_Child_Record is new Gtk.Event_Box.Gtk_Event_Box_Record with record
      Initial : Gtk.Widget.Gtk_Widget;
      --  The widget we used to build this child. This is used in case it
      --  was a window, since we need to be able to reparent it in the future,
      --  just in case.

      X, Y : Glib.Gint;
      GC   : Gdk.GC.Gdk_GC;
      Title_GC : Gdk.GC.Gdk_GC;
      State : State_Type := Normal;
      Name : GNAT.OS_Lib.String_Access;

      Uniconified_Width, Uniconified_Height : Glib.Gint;
      Uniconified_X, Uniconified_Y : Glib.Gint;
   end record;
   type MDI_Child is access all MDI_Child_Record'Class;

   procedure Gtk_New (Child : out MDI_Child;
                      Widget : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Create a new MDI child that contains widget.

   procedure Initialize (Child : access MDI_Child_Record;
                         Widget : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Internal initialization function.
   --  See the section "Creating your own widgets" in the documentation.

   type MDI_Window_Record is new Gtk.Layout.Gtk_Layout_Record with record
      X_Root, Y_Root : Glib.Gint;
      --  Root coordinates of the button_press event that generated a move

      Selected_Child : MDI_Child;
      --  The child being moved

      Current_Cursor : Gdk.Types.Gdk_Cursor_Type := Gdk.Types.Left_Ptr;

      Initial_Width, Initial_Height : Glib.Gint;
   end record;

end Gtkada.MDI;
