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

with Unchecked_Deallocation;
with Glib;
with Gdk.Pixmap;
with Gdk.Bitmap;
with Gdk.GC;
with Gdk.Font;
with Language;

package Generic_Values is

   type String_Access is access all String;
   procedure Free is new Unchecked_Deallocation (String, String_Access);

   --  Description of the types and values that are parsed by Odd.
   --
   --  When a user wants to display an item in the canvas, its type is
   --  first parsed, and then the value itself is parsed.
   --
   --  Doing this in two steps means that parsing the value can be done
   --  much faster this way, which is what we need since this needs to be done
   --  every time the debugger is stopped.
   --
   --  The items are organized in a tree. Each item in the tree contains both
   --  the description of the type and its current value. Whereas the type
   --  itself is never freed, the values are delete every time we need to
   --  parse a new value.

   procedure Set_Hidden_Pixmap (Pixmap : Gdk.Pixmap.Gdk_Pixmap;
                                Mask   : Gdk.Bitmap.Gdk_Bitmap);
   --  Set the pixmap used for hidden items.
   --  This must be called, or hidden items will not be displayed at all.

   ------------------
   -- Generic_Type --
   ------------------

   type Generic_Type (<>) is abstract tagged private;
   type Generic_Type_Access is access all Generic_Type'Class;
   --  general type for the items.
   --  The unknown discriminant is issued so as to force the user to call the
   --  constructors defined in this package.

   procedure Print (Value  : Generic_Type;
                    Indent : Natural := 0) is abstract;
   --  Print Value on Standard_Output.
   --  Indent is the indentation level.
   --  This function is intended for debug purposes only.

   procedure Free (Item : access Generic_Type;
                   Only_Value : Boolean := False) is abstract;
   --  Free the memory occupied by Item and its components.
   --  if Only_Value is True, then only clear the value fields, but keep alive
   --  the structure that describes the type.

   function Clone (Item : Generic_Type) return Generic_Type_Access is abstract;
   --  Deep copy Item, and return a newly allocated record.
   --  Only the type-related fields are cloned, the value fields are reset to
   --  Null.

   procedure Paint (Item    : in out Generic_Type;
                    GC      : Gdk.GC.Gdk_GC;
                    Xref_Gc : Gdk.GC.Gdk_GC;
                    Font    : Gdk.Font.Gdk_Font;
                    Pixmap  : Gdk.Pixmap.Gdk_Pixmap;
                    X, Y    : Glib.Gint := 0)
      is abstract;
   --  Paint the item on the pixmap, that will be used to show the item in the
   --  canvas.
   --  The item should be drawn so that its upper-left corner is at coordinates
   --  (X, Y) in Pixmap.
   --  Xref_GC is the graphic context to use when the text being displayed
   --  is clickable by the user.

   procedure Size_Request
     (Item           : in out Generic_Type;
      Font           : Gdk.Font.Gdk_Font;
      Hide_Big_Items : Boolean := False)
      is abstract;
   --  Compute the size that Item needs to display itself on the screen.
   --  The two fields Width and Height are initialized by this function.
   --  This function is always guaranteed to be called when an item is resized,
   --  its value is changed, or the font is changed.
   --  If Hide_Big_Items is True, then items bigger than a certain limit will
   --  be automatically hidden before their size is computed.

   function Get_Width (Item : Generic_Type) return Glib.Gint;
   --  Return the width that Item needs to display itself on the screen.

   function Get_Height (Item : Generic_Type) return Glib.Gint;
   --  Return the height that Item needs to display itself on the screen.

   function Get_X (Item : Generic_Type) return Glib.Gint;
   --  Return the coordinates in the pixmap where the item was displayed.

   function Get_Y (Item : Generic_Type) return Glib.Gint;
   --  Return the coordinates in the pixmap where the item was displayed.

   procedure Set_Visibility (Item    : in out Generic_Type;
                             Visible : Boolean);
   --  Whether the item should be visible or not.
   --  This function also applies to components of complex items.

   function Get_Visibility (Item : Generic_Type) return Boolean;
   --  Return the visibility state of an item.

   procedure Propagate_Width (Item  : in out Generic_Type;
                              Width : Glib.Gint);
   --  Set a specific width for the item.
   --  This width is propagated, with appropriate modifications, to the
   --  children of Item.

   function Get_Component_Name (Item : access Generic_Type;
                                Lang : access Language.Language_Root'Class;
                                Name : String;
                                X, Y : Glib.Gint)
                               return String is abstract;
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

   function Get_Component (Item : access Generic_Type;
                           X, Y : Glib.Gint)
                          return Generic_Type_Access is abstract;
   --  As above, but return the component itself.

   procedure Set_Selected (Item     : access Generic_Type;
                           Selected : Boolean := True);
   --  Set the selected status of item.
   --  If Selected if False, then all the children are also unselected.
   --  The item is not redrawn.

   function Get_Selected (Item : access Generic_Type) return Boolean;
   --  Return the selected status of Item.

   -----------------
   -- Simple_Type --
   -----------------

   type Simple_Type is new Generic_Type with private;
   type Simple_Type_Access is access all Simple_Type'Class;
   --  For simple values, like integers and floats, whose value we don't need
   --  to analyze.

   function New_Simple_Type return Generic_Type_Access;
   --  Create a new simple value.

   function Get_Value (Item : Simple_Type) return String_Access;
   --  Return the current value of Item (or null if it is not known).

   procedure Set_Value (Item : in out Simple_Type; Value : String);
   --  Assign a new value to Item.
   --  String is copied internally.

   -----------------
   -- Range Types --
   -----------------

   type Range_Type is new Simple_Type with private;
   type Range_Type_Access is access all Range_Type'Class;
   --  A range value, as in Ada 'range 0 .. 10', ie a number between the min
   --  and max value.

   function New_Range_Type (Min, Max : Long_Integer)
                           return Generic_Type_Access;
   --  Create a new range item, whose specific range is given in parameter.

   ---------------
   -- Mod Types --
   ---------------

   type Mod_Type is new Simple_Type with private;
   type Mod_Type_Access is access all Mod_Type'Class;
   --  A modular type whose values are module a given value.

   function New_Mod_Type (Modulo : Long_Integer) return Generic_Type_Access;
   --  Create a new mod type which a given modulo value.

   ------------------
   -- Access Types --
   ------------------

   type Access_Type is new Simple_Type with private;
   type Access_Type_Access is access all Access_Type'Class;
   --  A simple address value, that can be derefenced by the user.

   function New_Access_Type return Generic_Type_Access;
   --  Create a new access type.

   ----------------
   -- Enum Types --
   ----------------

   type Enum_Type is new Simple_Type with private;
   type Enum_Type_Access is access all Enum_Type'Class;
   --  An enumeration value, ie an item whose value must be in a set of given
   --  elements.
   --  Currently, the set of possible values is not stored in the item.

   function New_Enum_Type return Generic_Type_Access;
   --  Create a new enum type.

   ----------------
   -- Array_Type --
   ----------------

   type Array_Type is new Generic_Type with private;
   type Array_Type_Access is access all Array_Type'Class;
   --  An array type.
   --  Arrays can have multiple dimensions.
   --  Currently, all indexes must be integers, altough we should also handle
   --  any Simple_Type indexes.
   --  The value of an array is stored in a sparse table, ie all equal adjacent
   --  values are merged into a single element of type Repeat_Type, as returned
   --  for instance by gdb in <repeat .. times>.
   --  Note also that for unidmensional arrays of characters, their value is
   --  stored as a single item (a string).

   type Dimension is record
      First, Last : Long_Integer;
   end record;

   function New_Array_Type
     (Num_Dimensions : Positive) return Generic_Type_Access;
   --  Create a new array type with a given number of dimensions.

   procedure Set_Dimensions
     (Item : in out Array_Type;
      Dim  : Positive;
      Size : Dimension);
   --  Set the bounds of the Dim-nth dimensions of the array.

   function Num_Dimensions
     (Item : Array_Type)
     return Positive;
   --  Return the number of dimensions in the array Item.

   function Get_Dimensions
     (Item : Array_Type;
      Dim  : Positive)
     return Dimension;
   --  Return the bounds of the Dim-nth dimension in Item.

   procedure Set_Item_Type
     (Item     : in out Array_Type;
      The_Type : access Generic_Type'Class);
   --  Set the type of items contained in Item.
   --  The_Type is not duplicated, we just keep an access to it.

   function Get_Item_Type (Item : Array_Type)
                          return Generic_Type_Access;
   --  Return the Item_Type for the array.
   --  The returned structure should not be modified!
   --  ??? Could we use an access to constant ???

   procedure Set_Value (Item       : in out Array_Type;
                        Elem_Value : access Generic_Type'Class;
                        Elem_Index : Long_Integer;
                        Repeat_Num : Positive := 1);
   --  Set a value in the array.
   --
   --  The sparse table that stores the values of Item is dynamically resized
   --  if need be.
   --
   --  Elem_Index should be the index in a one-dimensional table that would
   --  contain the same number of elements as Item (as in C).
   --  The new value is repeat Repeat_Num times in adjacent cells in the array.
   --  Elem_Value is not duplicated!

   function Get_Value
     (Item       : Array_Type;
      Elem_Index : Long_Integer) return Generic_Type_Access;
   --  Read a value in the array at a specific Index.
   --  Elem_Index

   procedure Shrink_Values (Item : in out Array_Type);
   --  Shrink the sparse table used to store the values of the array to its
   --  minimal size.
   --  This is never mandatory, but saves some memory in some cases.

   procedure Propagate_Width (Item  : in out Array_Type;
                              Width : Glib.Gint);

   -------------
   -- Records --
   -------------

   type Record_Type is new Generic_Type with private;
   type Record_Type_Access is access all Record_Type'Class;
   --  A record type (or struct in C).

   function New_Record_Type (Num_Fields : Natural) return Generic_Type_Access;
   --  Create a new record type with a specific number of fields.
   --  Num_Fields can be null for a 'null record'.

   function Num_Fields (Item : Record_Type) return Natural;
   --  Return the number of fields in the record, or 0 for a null record.

   procedure Set_Field_Name (Item          : in out Record_Type;
                             Index         : Positive;
                             Name          : String;
                             Variant_Parts : Natural := 0);
   --  Set the name of the Index-nth field in the record.
   --  If Variant_Parts is not 0, then the field in the record is considered
   --  as a field with a variant_part (ie whose value depends on another field
   --  in the record (Name)).

   function Get_Variant_Parts (Item  : Record_Type;
                               Field : Positive)
                              return Natural;
   --  Get the number of variant parts for a specific field in the record.

   function Get_Field_Name (Item  : in Record_Type;
                            Index : Positive)
                           return String_Access;
   --  Return the name of the Index-th field in Item.

   function Find_Variant_Part (Item     : Record_Type;
                               Field    : Positive;
                               Contains : String)
                              return Generic_Type_Access;
   --  Return the variant part of the field-th of Item, whose first field is
   --  Contains.
   --  null is returned if no such part is found.

   procedure Set_Variant_Field (Item          : in out Record_Type;
                                Index         : Positive;
                                Variant_Index : Positive;
                                Value         : access Record_Type'Class);
   --  Set the Variant_Index-nth part of the Index-nth element in the array.
   --  Nothing is done if the Index-nth field in Item does not have any
   --  variant part.

   procedure Set_Value (Item  : in out Record_Type;
                        Value : access Generic_Type'Class;
                        Field : String);
   --  Set the value of a specific field in the record.
   --  Value is not duplicated, we simply keep a pointer to it.

   procedure Set_Value (Item  : in out Record_Type;
                        Value : access Generic_Type'Class;
                        Field : Positive);
   --  Same as above, for a specific field index.

   function Get_Value (Item  : Record_Type;
                       Field : String)
                      return Generic_Type_Access;
   --  Get the value of a specific field.

   function Get_Value (Item  : Record_Type;
                       Field : Positive)
                      return Generic_Type_Access;
   --  Same as above, but for a specific field index.

   procedure Propagate_Width (Item  : in out Record_Type;
                              Width : Glib.Gint);

   ------------
   -- Unions --
   ------------

   type Union_Type is new Record_Type with private;
   type Union_Type_Access is access all Union_Type'Class;
   --  A union type, ie a set of fields that are stored at the same address in
   --  memory.

   function New_Union_Type (Num_Fields : Positive) return Generic_Type_Access;
   --  Create a new union type with a specific number of fields.

   -------------
   -- Classes --
   -------------

   type Class_Type is new Simple_Type with private;
   type Class_Type_Access is access all Class_Type'Class;
   --  This type represents a C++ class, or Ada tagged type.
   --  It can have one or more ancestors, whose contents is also displayed when
   --  the value of the variable is show.

   function New_Class_Type (Num_Ancestors : Natural)
                           return Generic_Type_Access;
   --  Create a new class type, with a specific number of ancestors (parent
   --  classes).

   procedure Add_Ancestor (Item     : in out Class_Type;
                           Num      : Positive;
                           Ancestor : Class_Type_Access);
   --  Defines one of the ancestors of item.
   --  When the value of item, its components are parsed in the following
   --  order: first, all the fields of the first ancestor, then all the fields
   --  of the second ancestor, ..., then the fields of Item.
   --  No copy of Ancestor is made, we just keep the pointer.

   procedure Set_Child (Item  : in out Class_Type;
                        Child : Record_Type_Access);
   --  Record the child component of Item (where the fields of Item are
   --  defined).

   function Get_Child (Item : Class_Type) return Generic_Type_Access;
   --  Return a pointer to the child.

   function Get_Ancestor (Item : Class_Type;
                          Num  : Positive)
                         return Generic_Type_Access;
   --  Return a pointer to the Num-th ancestor.

   function Get_Num_Ancestors (Item : Class_Type) return Natural;
   --  Return the number of ancestors.

   procedure Propagate_Width (Item  : in out Class_Type;
                              Width : Glib.Gint);

private

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
   end record;

   type Simple_Type is new Generic_Type with record
      Value : String_Access := null;
      --  The value, as displayed by the debugger
   end record;
   procedure Print (Value : Simple_Type; Indent : Natural := 0);
   procedure Free (Item : access Simple_Type;
                   Only_Value : Boolean := False);
   function Clone (Value : Simple_Type) return Generic_Type_Access;
   procedure Paint (Item    : in out Simple_Type;
                    GC      : Gdk.GC.Gdk_GC;
                    Xref_Gc : Gdk.GC.Gdk_GC;
                    Font    : Gdk.Font.Gdk_Font;
                    Pixmap  : Gdk.Pixmap.Gdk_Pixmap;
                    X, Y    : Glib.Gint := 0);
   procedure Size_Request
     (Item           : in out Simple_Type;
      Font           : Gdk.Font.Gdk_Font;
      Hide_Big_Items : Boolean := False);
   function Get_Component_Name (Item : access Simple_Type;
                                Lang : access Language.Language_Root'Class;
                                Name : String;
                                X, Y : Glib.Gint)
                               return String;
   function Get_Component (Item : access Simple_Type;
                           X, Y : Glib.Gint)
                          return Generic_Type_Access;


   type Range_Type is new Simple_Type with record
      Min, Max : Long_Integer;
   end record;
   procedure Print (Value : Range_Type; Indent : Natural := 0);
   function Clone (Value : Range_Type) return Generic_Type_Access;
   --  Free is inherited from Simple_Type.


   type Mod_Type is new Simple_Type with record
      Modulo : Long_Integer;
   end record;
   procedure Print (Value : Mod_Type; Indent : Natural := 0);
   function Clone (Value : Mod_Type) return Generic_Type_Access;
   --  Free is inherited from Simple_Type.


   type Access_Type is new Simple_Type with null record;
   procedure Print (Value : Access_Type; Indent : Natural := 0);
   function Clone (Value : Access_Type) return Generic_Type_Access;
   --  Free is inherited from Simple_Type.
   procedure Paint (Item    : in out Access_Type;
                    GC      : Gdk.GC.Gdk_GC;
                    Xref_Gc : Gdk.GC.Gdk_GC;
                    Font    : Gdk.Font.Gdk_Font;
                    Pixmap  : Gdk.Pixmap.Gdk_Pixmap;
                    X, Y    : Glib.Gint := 0);


   type Enum_Type is new Simple_Type with null record;
   procedure Print (Value : Enum_Type; Indent : Natural := 0);
   function Clone (Value : Enum_Type) return Generic_Type_Access;
   --  Free is inherited from Simple_Type.


   --  The structure for arrays is slightly complex, since we need to be able
   --  to handle multi-dimensional arrays as well as store the values in a
   --  spare table.

   type Dimension_Array is array (Positive range <>) of Dimension;
   --  Represents one of the dimension of the array.

   type Array_Item is record
      Index : Long_Integer;
      Value : Generic_Type_Access := null;
   end record;
   type Array_Item_Array is array (Positive range <>) of Array_Item;
   --  One of the item of in the array.
   --  We need to store both its value and its index. The index is calculated
   --  as the index in a one-dimensional array that would store the same number
   --  of values as the array, starting from 0 as in C.

   type Array_Item_Array_Access is access Array_Item_Array;
   procedure Free is new Unchecked_Deallocation
     (Array_Item_Array, Array_Item_Array_Access);

   type Array_Type (Num_Dimensions : Positive) is new Generic_Type with record
      Dimensions  : Dimension_Array (1 .. Num_Dimensions);
      Values      : Array_Item_Array_Access := null;
      Item_Type   : Generic_Type_Access := null;
      Last_Value  : Natural := 0;

      Index_Width : Glib.Gint := 0;

   end record;
   --  Last_Value is the last value that is relevant in Values, or 0 if the
   --  array is empty.

   procedure Print (Value : Array_Type; Indent : Natural := 0);
   procedure Free (Item : access Array_Type;
                   Only_Value : Boolean := False);
   function Clone (Value : Array_Type) return Generic_Type_Access;
   procedure Paint (Item    : in out Array_Type;
                    GC      : Gdk.GC.Gdk_GC;
                    Xref_Gc : Gdk.GC.Gdk_GC;
                    Font    : Gdk.Font.Gdk_Font;
                    Pixmap  : Gdk.Pixmap.Gdk_Pixmap;
                    X, Y    : Glib.Gint := 0);
   procedure Size_Request
     (Item           : in out Array_Type;
      Font           : Gdk.Font.Gdk_Font;
      Hide_Big_Items : Boolean := False);
   function Get_Component_Name (Item : access Array_Type;
                                Lang : access Language.Language_Root'Class;
                                Name : String;
                                X, Y : Glib.Gint)
                               return String;
   function Get_Component (Item : access Array_Type;
                           X, Y : Glib.Gint)
                          return Generic_Type_Access;

   ------------
   -- Repeat --
   ------------

   --  To handle the '0 <repeats .. times>' case.

   type Repeat_Type is new Generic_Type with record
      Repeat_Num : Integer;
      Value      : Generic_Type_Access := null;
   end record;
   type Repeat_Type_Access is access all Repeat_Type'Class;

   procedure Print (Value : Repeat_Type; Indent : Natural := 0);
   procedure Free (Item : access Repeat_Type;
                   Only_Value : Boolean := False);
   function Clone (Value : Repeat_Type) return Generic_Type_Access;

   procedure Paint (Item    : in out Repeat_Type;
                    GC      : Gdk.GC.Gdk_GC;
                    Xref_Gc : Gdk.GC.Gdk_GC;
                    Font    : Gdk.Font.Gdk_Font;
                    Pixmap  : Gdk.Pixmap.Gdk_Pixmap;
                    X, Y    : Glib.Gint := 0);

   procedure Size_Request
     (Item           : in out Repeat_Type;
      Font           : Gdk.Font.Gdk_Font;
      Hide_Big_Items : Boolean := False);
   function Get_Component_Name (Item : access Repeat_Type;
                                Lang : access Language.Language_Root'Class;
                                Name : String;
                                X, Y : Glib.Gint)
                               return String;
   function Get_Component (Item : access Repeat_Type;
                           X, Y : Glib.Gint)
                          return Generic_Type_Access;

   -----------------
   -- Record Type --
   -----------------

   type Record_Type_Array;
   type Record_Type_Array_Access is access Record_Type_Array;

   type Record_Field is record
      Name         : String_Access := null;
      Value        : Generic_Type_Access := null;
      Variant_Part : Record_Type_Array_Access := null;
   end record;
   type Record_Field_Array is array (Natural range <>) of Record_Field;
   --  One of the fields in a record.
   --
   --  For a record with a variant part, a single item is created for that
   --  part. Its Name is the name of the variable that selects one of the
   --  alternatives. Value is null.
   --  This is the only case where Variant_Part is not null and contains the
   --  list of all alternatives.

   type Record_Type (Num_Fields : Natural) is new Generic_Type with record
      Fields           : Record_Field_Array (1 .. Num_Fields);

      Gui_Fields_Width : Glib.Gint := 0;
      --  Width allocated for the field names column when drawing the item
      --  on a pixmap. This is calculated once when Size_Request is called.
   end record;
   --  Num_Fields can be 0 in case of a 'null record'. Thus, it has to be
   --  a Natural.

   type Record_Type_Array is array (Positive range <>) of Record_Type_Access;
   procedure Free is new Unchecked_Deallocation
     (Record_Type_Array, Record_Type_Array_Access);

   procedure Print (Value : Record_Type; Indent : Natural := 0);
   procedure Free (Item : access Record_Type;
                   Only_Value : Boolean := False);
   function Clone (Value : Record_Type) return Generic_Type_Access;
   procedure Paint (Item    : in out Record_Type;
                    GC      : Gdk.GC.Gdk_GC;
                    Xref_Gc : Gdk.GC.Gdk_GC;
                    Font    : Gdk.Font.Gdk_Font;
                    Pixmap  : Gdk.Pixmap.Gdk_Pixmap;
                    X, Y    : Glib.Gint := 0);
   procedure Size_Request
     (Item           : in out Record_Type;
      Font           : Gdk.Font.Gdk_Font;
      Hide_Big_Items : Boolean := False);
   function Get_Component_Name (Item : access Record_Type;
                                Lang : access Language.Language_Root'Class;
                                Name : String;
                                X, Y : Glib.Gint)
                               return String;
   function Get_Component (Item : access Record_Type;
                           X, Y : Glib.Gint)
                          return Generic_Type_Access;

   type Union_Type (Num_Fields : Natural) is new Record_Type (Num_Fields)
     with null record;
   procedure Print (Value : Union_Type; Indent : Natural := 0);
   function Clone (Value : Union_Type) return Generic_Type_Access;
   --  Free is inherited from Record_Type.



   type Class_Type_Array is array (Positive range <>) of Class_Type_Access;

   type Class_Type (Num_Ancestors : Natural) is new Simple_Type with record
      Ancestors : Class_Type_Array (1 .. Num_Ancestors) := (others => null);
      Child     : Record_Type_Access;
   end record;
   procedure Print (Value : Class_Type; Indent : Natural := 0);
   procedure Free (Item : access Class_Type;
                   Only_Value : Boolean := False);
   function Clone (Value : Class_Type) return Generic_Type_Access;
   procedure Paint (Item    : in out Class_Type;
                    GC      : Gdk.GC.Gdk_GC;
                    Xref_Gc : Gdk.GC.Gdk_GC;
                    Font    : Gdk.Font.Gdk_Font;
                    Pixmap  : Gdk.Pixmap.Gdk_Pixmap;
                    X, Y    : Glib.Gint := 0);
   procedure Size_Request
     (Item           : in out Class_Type;
      Font           : Gdk.Font.Gdk_Font;
      Hide_Big_Items : Boolean := False);
   function Get_Component_Name (Item : access Class_Type;
                                Lang : access Language.Language_Root'Class;
                                Name : String;
                                X, Y : Glib.Gint)
                               return String;
   function Get_Component (Item : access Class_Type;
                           X, Y : Glib.Gint)
                          return Generic_Type_Access;
end Generic_Values;
