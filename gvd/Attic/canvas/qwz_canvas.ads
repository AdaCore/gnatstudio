
with Gdk.Font;
with Gdk.GC;
with Gdk.Pixmap;
with Gdk.Window;
with Glib;
with Gtk.Drawing_Area;
with Gtk.Viewport;

package Qwz_Canvas is

   type Interactive_Canvas_Record is new Gtk.Viewport.Gtk_Viewport_Record
     with private;
   type Interactive_Canvas is access all Interactive_Canvas_Record'Class;
   --  A canvas on which items are put.
   --  Each item can be moved interactively by the user, and links can be
   --  drawn automatically from an item to another.

   type Canvas_Item_Record is abstract tagged private;
   type Canvas_Item is access all Canvas_Item_Record'Class;
   --  An item that can be put on the canvas.
   --  This is an abstract type, as it does not provide any default drawing
   --  routine. Instead, the end-user should extend this type and implement
   --  a subprogram to draw on the pixmap returned by the Pixmap subprogram.

   -------------------
   -- Customization --
   -------------------

   Default_Annotation_Font   : constant String := "Helvetica";
   --  Font used when displaying link annotation.
   --  This is the name of a postscript font, as defined in Gtk.Extra.PsFont.

   Default_Annotation_Height : constant := 7;
   --  Default Height use for the annotation font.

   Default_Grid_Size         : constant := 15;
   --  Number of pixels between two dots on the grid.
   --  This is used for both horizontal and vertical orientation.

   Default_Arc_Link_Offset   : constant := 25;
   --  Distance between two parallel arcs for two links.

   Default_Arrow_Angle       : constant := 30;
   --  Half angle for the arrows in degres

   Default_Arrow_Length      : constant := 6;
   --  Length of the arrows in pixels.

   Default_Motion_Threshold  : constant := 4;
   --  Mimimum motion the mouse must have before we start moving the select
   --  item. If the mouse has moved less than that amount of pixel in any
   --  direction, then the mouse click is considered as being a selection
   --  only and is transfered to the item itself.

   -----------------------
   -- Creating a canvas --
   -----------------------

   procedure Gtk_New (Canvas : out Interactive_Canvas);
   --  Create a new empty Canvas.
   --  The canvas includes a grid in its background.

   procedure Initialize (Canvas : access Interactive_Canvas_Record'Class);
   --  Internal function used to initialize the canvas.

   procedure Configure
     (Canvas : access Interactive_Canvas_Record;
      Grid_Size         : Glib.Guint := Default_Grid_Size;
      Annotation_Font   : String := Default_Annotation_Font;
      Annotation_Height : Glib.Gint := Default_Annotation_Height;
      Arc_Link_Offset   : Glib.Gint := Default_Arc_Link_Offset;
      Arrow_Angle       : Glib.Gint := Default_Arrow_Angle;
      Arrow_Length      : Glib.Gint := Default_Arrow_Length;
      Motion_Threshold  : Glib.Gint := Default_Motion_Threshold);
   --  Change the parameters for the canvas.

   procedure Align_On_Grid (Canvas : access Interactive_Canvas_Record;
                            Align  : Boolean := True);
   --  Choose whether the items should be aligned on the grid when moved.
   --  Existing items are not moved even if you set this parameter to True,
   --  this will only take effect the next time the items are moved.

   procedure Add_Link (Canvas : access Interactive_Canvas_Record;
                       Src    : access Canvas_Item_Record'Class;
                       Dest   : access Canvas_Item_Record'Class;
                       Descr  : in String := "");
   --  Add an oriented link between two items.
   --  If Descr is not the empty string, it will be displayed in the middle
   --  of the link, and should indicate what the link means.
   --  This package automatically chose whether the link should be a straight
   --  line or an arc, so as to avoid overloading links.
   --  An arrow is drawn at the end of the link, on the border of Dest.

   procedure Put (Canvas : access Interactive_Canvas_Record;
                  Item   : access Canvas_Item_Record'Class;
                  X, Y   : Glib.Gint);
   --  Add a new item to the canvas.
   --  The item is added at a specific location.

   procedure Remove (Canvas : access Interactive_Canvas_Record;
                     Item   : access Canvas_Item_Record'Class);
   --  Remove an item and all the links to and from it from the canvas.
   --  The item itself is not freed.
   --  Nothing is done if the item is not part of the canvas.

   ------------------------
   -- Items manipulation --
   ------------------------

   procedure Initialize (Item   : access Canvas_Item_Record'Class;
                         Win    : Gdk.Window.Gdk_Window;
                         Width  : Glib.Gint;
                         Height : Glib.Gint);
   --  Function used to initialize the private data of the item.
   --  Each child of Canvas_Item should call this function, so as to create
   --  the canvas and register the size.

   function Pixmap (Item : access Canvas_Item_Record'Class)
                   return Gdk.Pixmap.Gdk_Pixmap;
   --  Return the pixmap on which the contents of the Item should be drawn.
   --  Drawing is left to the end-user.

   procedure On_Button_Click (Item   : access Canvas_Item_Record;
                              Button : Glib.Guint;
                              X, Y   : Glib.Gint);
   --  Function called whenever the item was clicked on.
   --  Note that this function is not called when the item is moved, and thus
   --  is only called when the click was short.
   --  If it returns True, the canvas it redrawn afterwards (in case the item
   --  has changed for instance).

private

   type String_Access is access String;

   type Link_Side is (Straight, Right, Left);
   --  The type of each link.
   --  For Straight, the link is drawn as a straight line between the two
   --  items. The other two cases indicates curve links, to a specific side.

   type Link;
   type Link_Access is access Link;
   type Link is
      record
         Src    : Canvas_Item;
         Dest   : Canvas_Item;
         Descr  : String_Access;

         Side   : Link_Side;
         Offset : Glib.Gint;
         --  How the link is drawn.
         --  Offset is used, along with Side, to calculate the "equation" of
         --  the arc. Basically, Offset * Grid_Size is the distance in the
         --  middle of the link between where a straight link would be and
         --  where the arc is.

         Next   : Link_Access;
      end record;

   type Canvas_Item_List_Record;
   type Canvas_Item_List is access Canvas_Item_List_Record;
   type Canvas_Item_List_Record is
      record
         Item : Canvas_Item;
         Next : Canvas_Item_List;
      end record;

   type Interactive_Canvas_Record is new Gtk.Viewport.Gtk_Viewport_Record with
      record
         Links             : Link_Access := null;
         Children          : Canvas_Item_List := null;
         Selected_Child    : Canvas_Item;

         Last_X_Event      : Glib.Gint;
         Last_Y_Event      : Glib.Gint;
         --  Last position where the mouse was pressed or moved in the canvas.

         Mouse_Has_Moved   : Boolean;
         --  True if mouse has moved while the button was clicked. This is used
         --  to distinguish between item motion and item selection.

         Drawing_Area      : Gtk.Drawing_Area.Gtk_Drawing_Area;

         Grid_Size         : Glib.Guint := Default_Grid_Size;
         Annotation_Font   : String_Access;
         Annotation_Height : Glib.Gint := Default_Annotation_Height;
         Arc_Link_Offset   : Float := Float (Default_Arc_Link_Offset);
         Arrow_Angle       : Float;
         Arrow_Length      : Float := Float (Default_Arrow_Length);
         Motion_Threshold  : Glib.Gint := Default_Motion_Threshold;
         Align_On_Grid     : Boolean := False;

         --  The following variables are initialized as soon as a Gdk_Window
         --  has been created for the canvas, in the Realized subprograms.

         Clear_GC : Gdk.GC.Gdk_GC := Gdk.GC.Null_GC;
         Black_GC : Gdk.GC.Gdk_GC := Gdk.GC.Null_GC;
         Anim_GC  : Gdk.GC.Gdk_GC := Gdk.GC.Null_GC;
         Font     : Gdk.Font.Gdk_Font := Gdk.Font.Null_Font;
      end record;

   type Canvas_Item_Record is abstract tagged
      record
         X         : Glib.Gint;
         Y         : Glib.Gint;
         Width     : Glib.Gint;
         Height    : Glib.Gint;
         Pixmap    : Gdk.Pixmap.Gdk_Pixmap;
      end record;

   pragma Inline (Pixmap);
end Qwz_Canvas;
