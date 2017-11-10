------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2017, AdaCore                          --
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

--  Items used for record types.
--  See the package Items for more information on all the private subprograms.

package GVD.Variables.Types.Records is

   -------------
   -- Records --
   -------------

   type GVD_Record_Type (<>) is new GVD_Generic_Type with private;
   type GVD_Record_Type_Access is access all GVD_Record_Type'Class;
   --  A record type (or struct in C).

   function New_Record_Type (Num_Fields : Natural) return GVD_Type_Holder;
   --  Create a new record type with a specific number of fields.
   --  Num_Fields can be null for a 'null record'.

   function Num_Fields (Self : not null access GVD_Record_Type) return Natural;
   --  Return the number of fields in the record, or 0 for a null record.

   procedure Set_Field_Name
     (Self          : not null access GVD_Record_Type;
      Index         : Positive;
      Name          : String;
      Variant_Parts : Natural := 0);
   --  Set the name of the Index-nth field in the record.
   --  If Variant_Parts is not 0, then the field in the record is considered
   --  as a field with a variant_part (ie whose value depends on another field
   --  in the record (Name)).

   function Get_Variant_Parts
     (Self  : not null access GVD_Record_Type;
      Field : Positive)
      return Natural;
   --  Get the number of variant parts for a specific field in the record.

   function Get_Field_Name
     (Self  : not null access GVD_Record_Type;
      Index : Positive)
      return String;
   --  Return the name of the Index-th field in Item.

   function Find_Variant_Part
     (Self     : not null access GVD_Record_Type;
      Field    : Positive;
      Contains : String)
      return GVD_Type_Holder;
   --  Return the variant part of the field-th of Item, whose first field is
   --  Contains.
   --  null is returned if no such part is found.
   --  All variant parts become invalid, except for the one that is returned.
   --  If Contains is the empty string, this returns the first variant part
   --  that has a "null" component

   procedure Set_Variant_Field
     (Self          : not null access GVD_Record_Type;
      Index         : Positive;
      Variant_Index : Positive;
      Value         : GVD_Type_Holder);
   --  Set the Variant_Index-nth part of the Index-nth element in the array.
   --  Nothing is done if the Index-nth field in Item does not have any
   --  variant part.

   procedure Set_Value
     (Self  : not null access GVD_Record_Type;
      Value : GVD_Type_Holder;
      Field : String);
   --  Set the value of a specific field in the record.
   --  Value is not duplicated, we simply keep a pointer to it.

   procedure Set_Value
     (Self  : not null access GVD_Record_Type;
      Value : GVD_Type_Holder;
      Field : Positive);
   --  Same as above, for a specific field index.

   function Get_Value
     (Self  : not null access GVD_Record_Type;
      Field : String)
      return GVD_Type_Holder;
   --  Get the value of a specific field.

   function Get_Value
     (Self  : not null access GVD_Record_Type;
      Field : Positive)
      return GVD_Type_Holder;
   --  Same as above, but for a specific field index.

   procedure Draw_Border
     (Self : not null access GVD_Record_Type;
      Draw : Boolean := True);
   --  If Draw is True (the default for new items), a border is drawn around
   --  the item when it is displayed on the screen.

   ------------
   -- Unions --
   ------------

   type GVD_Union_Type (<>) is new GVD_Record_Type with private;
   type GVD_Union_Type_Access is access all GVD_Union_Type'Class;
   --  A union type, ie a set of fields that are stored at the same address in
   --  memory.

   function New_Union_Type (Num_Fields : Positive) return GVD_Type_Holder;
   --  Create a new union type with a specific number of fields.

private

   type Record_Type_Array;
   type Record_Type_Array_Access is access Record_Type_Array;

   type Record_Field is record
      Name         : Unbounded_String := Null_Unbounded_String;
      Value        : GVD_Type_Holder  := Empty_GVD_Type_Holder;
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

   type GVD_Record_Type (Num_Fields : Natural) is
     new GVD_Generic_Type with record
      Gui_Fields_Width : Glib.Gint := 0;
      --  Width allocated for the field names column when drawing the item
      --  on a pixmap. This is calculated once when Size_Request is called.

      Type_Height      : Glib.Gint := 0;
      --  Height of the first line used to display the type of the item.

      Border_Spacing   : Glib.Gint := Types.Border_Spacing;
      --  Size to leave on each size between the border and the actual
      --  display of the item. If this is set to 0, then no border is drawn.

      Fields           : Record_Field_Array (1 .. Num_Fields);
   end record;
   --  Num_Fields can be 0 in case of a 'null record'. Thus, it has to be
   --  a Natural.

   type Record_Type_Array is array (Positive range <>) of GVD_Type_Holder;

   overriding procedure Clear (Self : not null access GVD_Record_Type);

   procedure Free (Value : in out Record_Type_Array_Access);

   overriding function Get_Type_Descr
     (Self : not null access GVD_Record_Type) return String is ("Record");

   overriding procedure Free
     (Self : not null access GVD_Record_Type);

   overriding procedure Clone
     (Self : not null access GVD_Record_Type;
      Item : not null GVD_Generic_Type_Access);

   overriding function Replace
     (Self         : not null access GVD_Record_Type;
      Current      : GVD_Type_Holder'Class;
      Replace_With : GVD_Type_Holder'Class) return GVD_Type_Holder'Class;

   overriding function Structurally_Equivalent
     (Self : not null access GVD_Record_Type;
      Item : GVD_Type_Holder'Class)
      return Boolean;

   overriding function Get_Simple_Value
     (Self : not null access GVD_Record_Type) return String;

   overriding function Start
     (Self : not null access GVD_Record_Type)
      return Generic_Iterator'Class;

   type GVD_Union_Type (Num_Fields : Natural) is
     new GVD_Record_Type (Num_Fields) with null record;

   overriding function Get_Type_Descr
     (Self : not null access GVD_Union_Type) return String is ("Union");
   --  Free is inherited from Record_Type.

end GVD.Variables.Types.Records;
