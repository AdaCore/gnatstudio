-----------------------------------------------------------------------
--                 Odd - The Other Display Debugger                  --
--                                                                   --
--                         Copyright (C) 2000                        --
--                 Emmanuel Briot and Arnaud Charlet                 --
--                                                                   --
-- Odd is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

--  Generic items used to display things in the canvas.

with Gdk.Font;
with Gdk.Pixmap;
with Gdk.Bitmap;
with Gdk.GC;
with Glib;
with Language;
with Glib;
with Unchecked_Deallocation;
with Odd.Types;

package Items is

   --  Description of the types and values that are parsed by Odd.
   --
   --  When a user wants to display an item in the canvas, its type is
   --  first parsed, and then the value itself is parsed.
   --
   --  Doing this in two steps means that parsing the value can be done
   --  much faster this way, which is mandatory since this needs to be done
   --  every time the debugger is stopped for auto-display variables.
   --
   --  The items are organized in a tree. Each item in the tree contains both
   --  the description of the type and its current value. Whereas the type
   --  itself is never freed, the values are deleted every time we need to
   --  parse a new value.

   procedure Set_Hidden_Pixmap
     (Pixmap : Gdk.Pixmap.Gdk_Pixmap;
      Mask   : Gdk.Bitmap.Gdk_Bitmap);
   --  Set the pixmap used for hidden items.
   --  This must be called, or hidden items will not be displayed at all.

   procedure Set_Unknown_Pixmap
     (Pixmap : Gdk.Pixmap.Gdk_Pixmap;
      Mask   : Gdk.Bitmap.Gdk_Bitmap);
   --  Set the pixmap used when the value of an item is unknown.
   --  This must be called.

   ------------------
   -- Generic_Type --
   ------------------

   type Generic_Type is abstract tagged private;
   type Generic_Type_Access is access all Generic_Type'Class;
   --  general type for the items.

   type Generic_Iterator is tagged private;
   --  Iterator used to traverse all the children of an item. For a record
   --  type, this would point to each of the fields.

   Unknown_Type_Prefix : constant String := "???";
   --  Prefix to indicate that a type has not been parsed yet, and needs some
   --  more parsing.

   ---------------
   -- Iterators --
   ---------------

   function Start (Item : access Generic_Type) return Generic_Iterator'Class;
   --  Return an iterator that points to the first child of the item.

   function At_End (Iter : Generic_Iterator) return Boolean;
   --  Return True if the iterator points after the last child of the item, ie
   --  if there is no more child

   procedure Next (Iter : in out Generic_Iterator);
   --  Points to the next child.

   function Data (Iter : Generic_Iterator) return Generic_Type_Access;
   --  Return the value pointed to by the iterator

   ---------------------
   -- Drawing Context --
   ---------------------

   type Display_Mode is (Value, Type_Only, Type_Value);
   --  What information should be displayed in the item.

   function Show_Value (Mode : in Display_Mode) return Boolean;
   --  Whether we should display the value of the item

   function Show_Type (Mode : in Display_Mode) return Boolean;
   --  Whether we should display the type of the item

   type Drawing_Context is record
      GC          : Gdk.GC.Gdk_GC;
      Xref_GC     : Gdk.GC.Gdk_GC;
      Modified_GC : Gdk.GC.Gdk_GC;
      Font        : Gdk.Font.Gdk_Font;
      Type_Font   : Gdk.Font.Gdk_Font;
      Pixmap      : Gdk.Pixmap.Gdk_Pixmap;
      Mode        : Display_Mode;
      Lang        : Language.Language_Access;
   end record;
   --  This structure contains all the information needed to draw items on
   --  the canvas.
   --  GC is the standard graphic context used to draw text and boxes.
   --  Xref_GC is used for fields that can be clicked on by the user to display
   --  new items (such as access types).
   --  Modified_GC is the graphic context used to highlight the items whose
   --  value has changed since the last update.
   --  Font is the default font to use.
   --  Pixmap is the pixmap to draw to.
   --
   --  All the contexts should be reset to their initial settings on exit of
   --  the Paint subprograms.

   -----------------------------
   -- Printing and Displaying --
   -----------------------------

   procedure Print
     (Value  : Generic_Type;
      Indent : Natural := 0) is abstract;
   --  Print Value on Standard_Output.
   --  Indent is the indentation level.
   --  This function is intended for debug purposes only.

   procedure Paint
     (Item    : in out Generic_Type;
      Context : Drawing_Context;
      X, Y    : Glib.Gint := 0) is abstract;
   --  Paint the item on the pixmap, that will be used to show the item in the
   --  canvas.
   --  The item should be drawn so that its upper-left corner is at coordinates
   --  (X, Y) in Pixmap.
   --  Xref_GC is the graphic context to use when the text being displayed
   --  is clickable by the user.

   procedure Size_Request
     (Item           : in out Generic_Type;
      Context        : Drawing_Context;
      Hide_Big_Items : Boolean := False) is abstract;
   --  Compute the size that Item needs to display itself on the screen.
   --  The two fields Width and Height are initialized by this function.
   --  This function is always guaranteed to be called when an item is resized,
   --  its value is changed, or the font is changed.
   --  If Hide_Big_Items is True, then items bigger than a certain limit will
   --  be automatically hidden before their size is computed.

   --------------------------------
   -- Manipulating the structure --
   --------------------------------

   procedure Free
     (Item : access Generic_Type;
      Only_Value : Boolean := False);
   --  Free the memory occupied by Item and its components.
   --  if Only_Value is True, then only clear the value fields, but keep alive
   --  the structure that describes the type.

   function Clone (Item : Generic_Type'Class) return Generic_Type_Access;
   --  return a deep copy of Item.

   function Get_Width (Item : Generic_Type) return Glib.Gint;
   --  Return the width that Item needs to display itself on the screen.

   function Get_Height (Item : Generic_Type) return Glib.Gint;
   --  Return the height that Item needs to display itself on the screen.

   function Get_X (Item : Generic_Type) return Glib.Gint;
   --  Return the coordinates in the pixmap where the item was displayed.

   function Get_Y (Item : Generic_Type) return Glib.Gint;
   --  Return the coordinates in the pixmap where the item was displayed.

   procedure Set_Visibility
     (Item      : access Generic_Type;
      Visible   : Boolean;
      Recursive : Boolean := False);
   --  Whether the item should be visible or not.
   --  This function also applies to components of complex items if Recursive
   --  is True.

   function Get_Visibility (Item : Generic_Type) return Boolean;
   --  Return the visibility state of an item.

   procedure Component_Is_Visible
     (Item       : access Generic_Type;
      Component  : access Generic_Type'Class;
      Is_Visible : out Boolean;
      Found      : out Boolean);
   --  Return True if Component (A child or grand-child of Item) is currently
   --  displayed on the screen.
   --  Found is set to False if Component is not part of the item hierarchy. In
   --  that case, Is_Visible's value is irrelevant.

   procedure Propagate_Width
     (Item  : in out Generic_Type;
      Width : Glib.Gint);
   --  Set a specific width for the item.
   --  This width is propagated, with appropriate modifications, to the
   --  children of Item.

   function Get_Component_Name
     (Item : access Generic_Type;
      Lang : access Language.Language_Root'Class;
      Name : String;
      X, Y : Glib.Gint) return String is abstract;
   --  Return a string that described the fields at coordinates (X, Y).
   --  (X, Y) must be relative to the upper-left corner of item.
   --
   --  For instance, clicking on a record field of an an array might return
   --  something like (for the Ada language):  "Name (3, 2).Field"
   --  The resulting string can be used as is by a debugger that understands
   --  Lang.
   --  Note also that only visible fields are returned, and that the selected
   --  field is not necessarily the innermost one (if you click for instance
   --  in a record box, outside of any field, then the record itself is
   --  returned).
   --
   --  Name is the name of item iself, in case we need to add a suffix to it.

   function Get_Component
     (Item : access Generic_Type;
      X, Y : Glib.Gint) return Generic_Type_Access is abstract;
   --  As above, but return the component itself.

   procedure Set_Selected
     (Item     : access Generic_Type;
      Selected : Boolean := True);
   --  Set the selected status of item.
   --  If Selected if False, then all the children are also unselected.
   --  The item is not redrawn.

   function Get_Selected (Item : access Generic_Type) return Boolean;
   --  Return the selected status of Item.

   procedure Set_Valid
     (Item  : access Generic_Type;
      Valid : Boolean := True);
   --  Indicate whether the value given in Item is valid (ie there was no
   --  error when getting the value from the debugger, ...)

   function Is_Valid (Item : access Generic_Type) return Boolean;
   --  Return True if the value given in Item is valid, ie was correctly
   --  parsed

   function Replace
     (Parent       : access Generic_Type;
      Current      : access Generic_Type'Class;
      Replace_With : access Generic_Type'Class) return Generic_Type_Access
      is abstract;
   --  Substitute a field/value/element in Parent.
   --  The field that is currently equal to Current is replaced with
   --  Replace_With. Current is then Freed completly.
   --  This is used when we discover that a type previously parsed doesn't in
   --  fact match the value, and should be dynamically changed.
   --  Replace_With is returned; null is returned if Current did not belong to
   --  Parent, or if nothing could be done.

   procedure Reset_Recursive (Item : access Generic_Type);
   --  Reset the boolean that indicates whether the item has changed since the
   --  last update. All the children of Item are reset as well.

   procedure Set_Type_Name
     (Item : access Generic_Type;
      Name : String);
   --  Change the type of the item

   function Get_Type_Name
     (Item    : access Generic_Type;
      Context : Drawing_Context)
     return String;
   --  Return the type of Item.
   --  If the type has not been evaluated yet (lazy evaluation), this is done
   --  at this point.

   ---------------
   -- Constants --
   ---------------

   Line_Spacing : constant Glib.Gint := 1;
   --  Space between lines in the display of items in a pixmap.
   --  This is the extra space added between two lines of an array or two
   --  fields of a record

   Border_Spacing : constant Glib.Gint := 2;
   --  Space between the rectangle and the item on each side, for complex
   --  items.

   Left_Border : constant Glib.Gint := 5;
   --  Space of the column on the left of records and arrays, where the user
   --  can click to select the whole array or record.

   Big_Item_Height : constant Glib.Gint := 150;
   --  Items taller than this value will start hidden.

   -------------------------
   -- Read-only variables --
   -------------------------
   --  These variables should be set only once, and then read only.

   Hidden_Pixmap : Gdk.Pixmap.Gdk_Pixmap;
   Hidden_Mask   : Gdk.Bitmap.Gdk_Bitmap;
   Hidden_Height : Glib.Gint;
   Hidden_Width  : Glib.Gint;
   --  pixmap used when items are hidden

   Unknown_Pixmap : Gdk.Pixmap.Gdk_Pixmap;
   Unknown_Mask   : Gdk.Bitmap.Gdk_Bitmap;
   Unknown_Height : Glib.Gint;
   Unknown_Width  : Glib.Gint;

   procedure Clone_Dispatching
     (Item  : Generic_Type;
      Clone : out Generic_Type_Access);
   --  Deep copy of the contents of Item into Clone.
   --  Clone must have been allocated first, and you should rather use the
   --  subprogram Clone above.

private

   procedure Display_Pixmap
     (On_Pixmap : Gdk.Pixmap.Gdk_Pixmap;
      GC        : Gdk.GC.Gdk_GC;
      Pixmap    : Gdk.Pixmap.Gdk_Pixmap;
      Mask      : Gdk.Bitmap.Gdk_Bitmap;
      X, Y      : Glib.Gint);
   --  Display a masked pixmap at specific coordinates.

   type Generic_Type is abstract tagged record
      Width, Height : Glib.Gint := 0;
      --  These two fields are allocated by calls to Size_Request

      X, Y : Glib.Gint := Glib.Gint'Last;
      --  The coordinates in the top-level entity window where a component
      --  was displayed. This is used when we need to redraw a single component
      --  for instance when it was selected.

      Visible : Boolean := True;
      --  Whether the item's contents is shown or hidden. Note that some
      --  types (Simple_Type'Class) can not be hidden.

      Selected : Boolean := False;
      --  Whether the item is selected.

      Valid    : Boolean := False;
      --  Whether the value stored is valid, ie there was no error from the
      --  debugger when we got it.

      Type_Name : Odd.Types.String_Access := null;
      --  The type of the item.
      --  As a special case, this starts with Unknown_Type_Prefix if some extra
      --  info needs to be extracted from the debugger. In that case, the
      --  format is
      --     Unknown_Type_Prefix & "entity" & ASCII.LF & "default",
      --  as for the arguments for Get_Type_Info. This is used so that we do
      --  not need to query extra information every time.
   end record;

   procedure Free_Internal is new Unchecked_Deallocation
     (Generic_Type'Class, Generic_Type_Access);

   type Generic_Iterator is tagged null record;

end Items;
