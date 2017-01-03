------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2000-2017, AdaCore                     --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

--  Generic items used to display things in the canvas.

with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;
with Browsers;               use Browsers;
with Browsers.Canvas;        use Browsers.Canvas;
with GNAT.Strings;           use GNAT.Strings;
with Glib;
with Gtkada.Canvas_View;     use Gtkada.Canvas_View;
with Gtkada.Style;           use Gtkada.Style;
with Language;               use Language;

package Items is

   --  Description of the types and values that are parsed by GVD.
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

   --------------------
   --  The Data view --
   --------------------

   type Debugger_Data_View_Record is new Browsers.Canvas.General_Browser_Record
   with record
      Modified : Drawing_Style;  --  when value has changed
      Freeze   : Drawing_Style;  --  item's value is not refreshed

      --  Settings to draw items
      --  Modified_Color: used to highlight the items whose value has changed
      --    since the last update.
   end record;
   type Debugger_Data_View is access all Debugger_Data_View_Record'Class;

   function Item_Hidden
     (View : not null access Debugger_Data_View_Record'Class)
      return Container_Item;
   --  Return the item that shows when an item is hidden

   ------------------
   -- Generic_Type --
   ------------------

   type Generic_Type is abstract tagged private;
   type Generic_Type_Access is access all Generic_Type'Class;
   --  general type for the items.

   Unknown_Type_Prefix : constant String := "???";
   --  Prefix to indicate that a type has not been parsed yet, and needs some
   --  more parsing.

   ---------------
   -- Iterators --
   ---------------

   type Generic_Iterator is abstract tagged null record;
   --  Iterator used to traverse all the children of an item. For a record
   --  type, this would point to each of the fields.

   function Start
     (Item : not null access Generic_Type) return Generic_Iterator'Class;
   --  Return an iterator that points to the first child of the item.
   --  The values returned by this iterator might depend on a previous call to
   --  Set_Show_Details.

   function At_End (Iter : Generic_Iterator) return Boolean is (True);
   --  Return True if the iterator points after the last child of the item, ie
   --  if there is no more child

   procedure Next (Iter : in out Generic_Iterator) is null;
   --  Points to the next child.

   function Field_Name
     (Self : Generic_Iterator;
      Lang : not null access Language_Root'Class;
      Base : String := "") return String is abstract;
   --  Return the name of the current field.
   --  Base is the name of the parent item, or the empty string to display
   --  short names only.

   function Data (Iter : Generic_Iterator) return Generic_Type_Access
      is (null);
   --  Return the value pointed to by the iterator

   type Empty_Iterator is new Generic_Iterator with private;
   --  An iterator that returns nothing

   function Create_Empty_Iterator return Generic_Iterator'Class;
   --  Return an iterator that will return no element

   ---------------------
   -- Drawing Context --
   ---------------------

   type Display_Mode is (Value, Type_Only, Type_Value);
   --  What information should be displayed in the item.

   function Show_Value (Mode : Display_Mode) return Boolean;
   --  Whether we should display the value of the item

   function Show_Type (Mode : Display_Mode) return Boolean;
   --  Whether we should display the type of the item

   -----------------------------
   -- Printing and Displaying --
   -----------------------------

   type Component_Item_Record is new Rect_Item_Record with record
      Name      : Ada.Strings.Unbounded.Unbounded_String;
      Component : Generic_Type_Access;
   end record;
   type Component_Item is access all Component_Item_Record'Class;
   --  The GUI representation of a Generic_Type

   function New_Component_Item
     (Styles    : not null access Browser_Styles;
      Component : not null access Generic_Type'Class;
      Name      : String) return Component_Item;
   procedure Initialize_Component_Item
     (Self      : not null access Component_Item_Record'Class;
      Styles    : not null access Browser_Styles;
      Component : not null access Generic_Type'Class;
      Name      : String);
   --  Build a new component item

   function Build_Display
     (Self   : not null access Generic_Type;
      Name   : String;
      View   : not null access Debugger_Data_View_Record'Class;
      Lang   : Language.Language_Access;
      Mode   : Display_Mode) return Component_Item is abstract;
   --  Build the contents of the item, to show Self.

   type Collapsible_Item_Record is new Rect_Item_Record
     and Clickable_Item with record
         For_Component : access Generic_Type'Class;
     end record;
   type Collapsible_Item is access all Collapsible_Item_Record'Class;
   overriding procedure On_Click
     (Self    : not null access Collapsible_Item_Record;
      View    : not null access GPS_Canvas_View_Record'Class;
      Details : Gtkada.Canvas_View.Event_Details_Access);
   --  An box that can be double-clicked to be hidden, and thus save screen
   --  estate

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

   procedure Set_Visibility
     (Item      : access Generic_Type;
      Visible   : Boolean;
      Recursive : Boolean := False);
   function Get_Visibility (Item : Generic_Type) return Boolean;
   --  Whether the item should be visible or not.
   --  This function also applies to components of complex items if Recursive
   --  is True.

   function Is_Changed
     (Self : not null access Generic_Type) return Boolean;
   --  Whether Self (or one of its components) was modified during the last
   --  update (i.e. between two debugger commands).
   --  The default is to check whether any of the components returned by the
   --  Start iterator has changed

   procedure Set_Valid
     (Item  : access Generic_Type;
      Valid : Boolean := True);
   function Is_Valid (Item : access Generic_Type) return Boolean;
   --  Indicate whether the value given in Item is valid (ie there was no
   --  error when getting the value from the debugger, ...)

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
   function Get_Type_Name
     (Item    : access Generic_Type;
      Lang    : Language.Language_Access) return String;
   --  Return the type of Item.
   --  If the type has not been evaluated yet (lazy evaluation), this is done
   --  at this point.

   function Get_Type_Descr
     (Self    : not null access Generic_Type) return String is abstract;
   --  A debug function to display the type of an item.

   function Get_Simple_Value
     (Self    : not null access Generic_Type)
      return String is ("");
   --  Return the value for Self, when a simple type.
   --  For instance, it would return a string for a numeric, string or access
   --  value, but for a record and class it would return the empty string.
   --  This is the string that should be used for each row of the variables
   --  view.

   function Structurally_Equivalent
     (Item1 : access Generic_Type; Item2 : access Generic_Type'Class)
     return Boolean is abstract;
   --  Return True if Item1 and Item2 are structurally equivalent.
   --  Any access type is structurally equivalent to any other access type,
   --  whereas two records are structurally equivalent only if their fields are
   --  structurally equivalent.

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

   procedure Clone_Dispatching
     (Item  : Generic_Type;
      Clone : in out Generic_Type_Access);
   --  Deep copy of the contents of Item into Clone.
   --  Clone must have been allocated first, and you should rather use the
   --  subprogram Clone above.

private

   type Generic_Type is abstract tagged record
      Visible : Boolean := True;
      --  Whether the item's contents is shown or hidden. Note that some
      --  types (Simple_Type'Class) can not be hidden.

      Valid    : Boolean := False;
      --  Whether the value stored is valid, ie there was no error from the
      --  debugger when we got it.

      Type_Name : GNAT.Strings.String_Access := null;
      --  The type of the item.
      --  As a special case, this starts with Unknown_Type_Prefix if some extra
      --  info needs to be extracted from the debugger. In that case, the
      --  format is
      --     Unknown_Type_Prefix & "entity" & ASCII.LF & "default",
      --  as for the arguments for Get_Type_Info. This is used so that we do
      --  not need to query extra information every time.
   end record;

   procedure Free_Internal is new Ada.Unchecked_Deallocation
     (Generic_Type'Class, Generic_Type_Access);

   type Empty_Iterator is new Generic_Iterator with null record;
   overriding procedure Next (It : in out Empty_Iterator) is null;
   overriding function At_End (It : Empty_Iterator) return Boolean is (True);
   overriding function Data (It : Empty_Iterator) return Generic_Type_Access
      is (null);
   overriding function Field_Name
     (Self : Empty_Iterator;
      Lang : not null access Language_Root'Class;
      Base : String := "") return String is ("");

   type Field_Descr is record
      Name : GNAT.Strings.String_Access;
      Typ  : Generic_Type_Access;
   end record;
   type Type_Array is array (Natural range <>) of Field_Descr;
   type Type_Array_Access is access all Type_Array;

   type Field_Iterator is new Generic_Iterator with record
      Fields : Type_Array_Access;   --  Not owned by the iterator !
      Idx    : Integer;
   end record;
   overriding function At_End (Self : Field_Iterator) return Boolean;
   overriding procedure Next (Self : in out Field_Iterator);
   overriding function Field_Name
     (Self : Field_Iterator;
      Lang : not null access Language_Root'Class;
      Base : String := "") return String;
   overriding function Data (Self : Field_Iterator) return Generic_Type_Access;
   --  An iterator on an array of fields

   procedure Free (Self : in out Type_Array_Access);
   --  Free the memory used by Self

   function Start (Self : Type_Array_Access) return Generic_Iterator'Class;
   --  Return an iterator on Self
end Items;
