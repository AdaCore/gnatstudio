-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                      Copyright (C) 2000-2001                      --
--                              ACT-Europe                           --
--                                                                   --
-- GVD is free  software;  you can redistribute it and/or modify  it --
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

--  Items used for record types.
--  See the package Items for more information on all the private subprograms.

with GVD.Types;
with Unchecked_Deallocation;

package Items.Records is

   -------------
   -- Records --
   -------------

   type Record_Type (<>) is new Generic_Type with private;
   type Record_Type_Access is access all Record_Type'Class;
   --  A record type (or struct in C).

   function New_Record_Type (Num_Fields : Natural) return Generic_Type_Access;
   --  Create a new record type with a specific number of fields.
   --  Num_Fields can be null for a 'null record'.

   function Num_Fields (Item : Record_Type) return Natural;
   --  Return the number of fields in the record, or 0 for a null record.

   procedure Set_Field_Name
     (Item          : in out Record_Type;
      Index         : Positive;
      Name          : String;
      Variant_Parts : Natural := 0);
   --  Set the name of the Index-nth field in the record.
   --  If Variant_Parts is not 0, then the field in the record is considered
   --  as a field with a variant_part (ie whose value depends on another field
   --  in the record (Name)).

   function Get_Variant_Parts
     (Item  : Record_Type;
      Field : Positive) return Natural;
   --  Get the number of variant parts for a specific field in the record.

   function Get_Field_Name
     (Item  : in Record_Type;
      Index : Positive) return GVD.Types.String_Access;
   --  Return the name of the Index-th field in Item.

   function Find_Variant_Part
     (Item     : Record_Type;
      Field    : Positive;
      Contains : String) return Items.Generic_Type_Access;
   --  Return the variant part of the field-th of Item, whose first field is
   --  Contains.
   --  null is returned if no such part is found.
   --  All variant parts become invalid, except for the one that is returned.

   procedure Set_Variant_Field
     (Item          : in out Record_Type;
      Index         : Positive;
      Variant_Index : Positive;
      Value         : access Record_Type'Class);
   --  Set the Variant_Index-nth part of the Index-nth element in the array.
   --  Nothing is done if the Index-nth field in Item does not have any
   --  variant part.

   procedure Set_Value
     (Item  : in out Record_Type;
      Value : access Generic_Type'Class;
      Field : String);
   --  Set the value of a specific field in the record.
   --  Value is not duplicated, we simply keep a pointer to it.

   procedure Set_Value
     (Item  : in out Record_Type;
      Value : access Generic_Type'Class;
      Field : Positive);
   --  Same as above, for a specific field index.

   function Get_Value
     (Item  : Record_Type;
      Field : String) return Generic_Type_Access;
   --  Get the value of a specific field.

   function Get_Value
     (Item  : Record_Type;
      Field : Positive) return Generic_Type_Access;
   --  Same as above, but for a specific field index.

   procedure Propagate_Width
     (Item  : in out Record_Type;
      Width : Glib.Gint);

   procedure Draw_Border
     (Item : access Record_Type;
      Draw : Boolean := True);
   --  If Draw is True (the default for new items), a border is drawn around
   --  the item when it is displayed on the screen.

   ------------
   -- Unions --
   ------------

   type Union_Type (<>) is new Record_Type with private;
   type Union_Type_Access is access all Union_Type'Class;
   --  A union type, ie a set of fields that are stored at the same address in
   --  memory.

   function New_Union_Type (Num_Fields : Positive) return Generic_Type_Access;
   --  Create a new union type with a specific number of fields.

private

   type Record_Type_Array;
   type Record_Type_Array_Access is access Record_Type_Array;

   type Record_Field is record
      Name         : GVD.Types.String_Access := null;
      Value        : Items.Generic_Type_Access := null;
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

      Type_Height : Glib.Gint := 0;
      --  Height of the first line used to display the type of the item.

      Border_Spacing : Glib.Gint := Items.Border_Spacing;
      --  Size to leave on each size between the border and the actual
      --  display of the item. If this is set to 0, then no border is drawn.
   end record;
   --  Num_Fields can be 0 in case of a 'null record'. Thus, it has to be
   --  a Natural.

   type Record_Type_Array is array (Positive range <>) of Record_Type_Access;
   procedure Free is new Unchecked_Deallocation
     (Record_Type_Array, Record_Type_Array_Access);

   procedure Print (Value : Record_Type; Indent : Natural := 0);
   procedure Free
     (Item : access Record_Type;
      Only_Value : Boolean := False);
   procedure Clone_Dispatching
     (Item  : Record_Type;
      Clone : out Generic_Type_Access);
   procedure Paint
     (Item    : in out Record_Type;
      Context : Drawing_Context;
      X, Y    : Glib.Gint := 0);
   procedure Size_Request
     (Item           : in out Record_Type;
      Context        : Drawing_Context;
      Hide_Big_Items : Boolean := False);
   function Get_Component_Name
     (Item : access Record_Type;
      Lang : access Language.Language_Root'Class;
      Name : String;
      X, Y : Glib.Gint) return String;
   function Get_Component
     (Item : access Record_Type;
      X, Y : Glib.Gint) return Generic_Type_Access;
   function Replace
     (Parent       : access Record_Type;
      Current      : access Generic_Type'Class;
      Replace_With : access Generic_Type'Class) return Generic_Type_Access;

   type Record_Iterator is new Generic_Iterator with record
      Item  : Record_Type_Access;
      Field : Natural;
      Variant : Natural;
   end record;
   function Start (Item : access Record_Type) return Generic_Iterator'Class;
   procedure Next (Iter : in out Record_Iterator);
   function At_End (Iter : Record_Iterator) return Boolean;
   function Data (Iter : Record_Iterator) return Generic_Type_Access;

   type Union_Type (Num_Fields : Natural) is new Record_Type (Num_Fields)
     with null record;
   procedure Print (Value : Union_Type; Indent : Natural := 0);
   --  Free is inherited from Record_Type.

end Items.Records;
