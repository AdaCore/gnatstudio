------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2017-2019, AdaCore                     --
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

--  Generic items used to display debugger variables in the views.

with Ada.Finalization;
with Ada.Strings.Unbounded;       use Ada.Strings.Unbounded;
with System.Storage_Elements;

with Glib.Values;
with Language;                    use Language;

package GVD.Variables.Types is

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

   ----------------------
   -- GVD_Generic_Type --
   ----------------------

   type GVD_Generic_Type is abstract tagged private;
   type GVD_Generic_Type_Access is access all GVD_Generic_Type'Class;
   --  general type for the items.

   ---------------------
   -- GVD_Type_Holder --
   ---------------------

   --  Used for storing instances of Types with reference counting

   type GVD_Type_Holder is new Ada.Finalization.Controlled with private;

   function Id
     (Self : GVD_Type_Holder)
      return System.Storage_Elements.Integer_Address;
   --  Returns unique ID of the holdes's data

   function Get_Type (Self : GVD_Type_Holder) return GVD_Generic_Type_Access;
   --  DO NOT STORE the result of this function in any cases.

   function Clone (Self : GVD_Type_Holder) return GVD_Type_Holder;
   --  Return a deep copy of Item.

   Empty_GVD_Type_Holder : constant GVD_Type_Holder;

   --  Utilities to store/retrive type to/from GValue

   function Get_GVD_Type_Holder_GType return Glib.GType;

   function As_GVD_Type_Holder
     (Self : GVD_Type_Holder)
      return Glib.Values.GValue;

   procedure Set_Value
     (Value  : in out Glib.Values.GValue;
      Holder : GVD_Type_Holder);

   function Get_Value
     (Value : Glib.Values.GValue)
      return GVD_Type_Holder;

   --------------------------------
   -- Manipulating the structure --
   --------------------------------

   procedure Clear (Self : not null access GVD_Generic_Type) is null;
   --  Clear the value fields, but keep alive the structure that describes
   --  the type.

   procedure Set_Visibility
     (Self      : not null access GVD_Generic_Type;
      Visible   : Boolean;
      Recursive : Boolean := False);
   function Get_Visibility
     (Self : not null access GVD_Generic_Type) return Boolean;
   --  Whether the item should be visible or not.
   --  This function also applies to components of complex items if Recursive
   --  is True.

   function Is_Changed
     (Self : not null access GVD_Generic_Type) return Boolean;
   --  Whether Self (or one of its components) was modified during the last
   --  update (i.e. between two debugger commands).
   --  The default is to check whether any of the components returned by the
   --  Start iterator has changed

   procedure Set_Valid
     (Self  : not null access GVD_Generic_Type;
      Valid : Boolean := True);
   function Is_Valid (Self : not null access GVD_Generic_Type) return Boolean;
   --  Indicate whether the value given in Item is valid (ie there was no
   --  error when getting the value from the debugger, ...)

   function Replace
     (Parent       : not null access GVD_Generic_Type;
      Current      : GVD_Type_Holder'Class;
      Replace_With : GVD_Type_Holder'Class) return GVD_Type_Holder'Class
      is abstract;
   --  Substitute a field/value/element in Parent.
   --  The field that is currently equal to Current is replaced with
   --  Replace_With. Current is then Freed completly.
   --  This is used when we discover that a type previously parsed doesn't in
   --  fact match the value, and should be dynamically changed.
   --  Replace_With is returned; null is returned if Current did not belong to
   --  Parent, or if nothing could be done.

   procedure Reset_Recursive (Self : not null access GVD_Generic_Type);
   --  Reset the boolean that indicates whether the item has changed since the
   --  last update. All the children of Item are reset as well.

   procedure Set_Type_Name
     (Self : not null access GVD_Generic_Type;
      Name : String);
   function Get_Type_Name
     (Self : not null access GVD_Generic_Type)
      return String;
   --  Return the type of Item.
   --  If the type has not been evaluated yet (lazy evaluation), this is done
   --  at this point.

   function Get_Type_Descr
     (Self : not null access GVD_Generic_Type)
      return String is abstract;
   --  A debug function to display the type of an item.

   function Get_Simple_Value
     (Self : not null access GVD_Generic_Type)
      return String is ("");
   --  Return the value for Self, when a simple type.
   --  For instance, it would return a string for a numeric, string or access
   --  value, but for a record and class it would return the empty string.
   --  This is the string that should be used for each row of the variables
   --  view.

   function Get_Advanced_Value
     (Self      : not null access GVD_Generic_Type;
      Is_Nested : Boolean := False)
      return String;
   --  Return the complete value for Self.
   --  This is the string displayed in the tooltips.

   function Structurally_Equivalent
     (Self : not null access GVD_Generic_Type; Item : GVD_Type_Holder'Class)
     return Boolean is abstract;
   --  Return True if Item1 and Item2 are structurally equivalent.
   --  Any access type is structurally equivalent to any other access type,
   --  whereas two records are structurally equivalent only if their fields are
   --  structurally equivalent.

   type Generic_Iterator;

   function Start
     (Self : not null access GVD_Generic_Type) return Generic_Iterator'Class;
   --  Return an iterator that points to the first child of the item.
   --  The values returned by this iterator might depend on a previous call to
   --  Set_Show_Details.

   ---------------
   -- Iterators --
   ---------------

   type Generic_Iterator is
     abstract new Ada.Finalization.Controlled with null record;
   --  Iterator used to traverse all the children of an item. For a record
   --  type, this would point to each of the fields.

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

   function Data (Iter : Generic_Iterator) return GVD_Type_Holder'Class
      is (Empty_GVD_Type_Holder);
   --  Return the value pointed to by the iterator

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

private

   type GVD_Generic_Type is abstract tagged record
      Visible   : Boolean := True;
      --  Whether the item's contents is shown or hidden. Note that some
      --  types (Simple_Type'Class) can not be hidden.

      Valid     : Boolean := False;
      --  Whether the value stored is valid, ie there was no error from the
      --  debugger when we got it.

      Type_Name : Unbounded_String := Null_Unbounded_String;
      --  The type of the item.
      --  As a special case, this starts with Unknown_Type_Prefix if some extra
      --  info needs to be extracted from the debugger. In that case, the
      --  format is
      --     Unknown_Type_Prefix & "entity" & ASCII.LF & "default",
      --  as for the arguments for Get_Type_Info. This is used so that we do
      --  not need to query extra information every time.
   end record;

   procedure Free (Self : not null access GVD_Generic_Type);
   --  Free the memory occupied by Item and its components.

   procedure Clone
     (Self : not null access GVD_Generic_Type;
      Item : not null GVD_Generic_Type_Access);

   -----------------
   -- Type_Holder --
   -----------------

   type GVD_Type_Holder_Data is record
      Count    : Natural := 0;
      Instance : GVD_Generic_Type_Access := null;
   end record;
   type GVD_Type_Holder_Data_Access is access all GVD_Type_Holder_Data;
   --  The record to store Item's data.

   type GVD_Type_Holder is new Ada.Finalization.Controlled with record
      Data : GVD_Type_Holder_Data_Access := null;
   end record;

   overriding procedure Adjust   (Self : in out GVD_Type_Holder);
   overriding procedure Finalize (Self : in out GVD_Type_Holder);

   Empty_GVD_Type_Holder : constant GVD_Type_Holder :=
     (Ada.Finalization.Controlled with null);

   --------------------
   -- Empty_Iterator --
   --------------------

   type Empty_Iterator is new Generic_Iterator with null record;
   overriding function Field_Name
     (Self       : Empty_Iterator;
      Dummy_Lang : not null access Language_Root'Class;
      Dummy_Base : String := "") return String is ("");

   function Create_Empty_Iterator return Generic_Iterator'Class;
   --  Return an iterator that will return no element

end GVD.Variables.Types;
