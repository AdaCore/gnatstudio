with System;

package Generic_Values is

   --  Note: the types in this package are currently public, since the parser
   --  function needs to be able to create them.
   --  However, it would be cleaner to provide a set of subprograms to create
   --  and access them.


   type String_Access is access all String;

   ------------------
   -- Generic_Type --
   ------------------

   type Generic_Type is abstract tagged record
      Address : System.Address;  --  Address of the variable or field
   end record;
   type Generic_Type_Access is access all Generic_Type'Class;

   procedure Print (Value : Generic_Type) is abstract;
   --  Print Value on Standard_Output.

   -----------------
   -- Simple_Type --
   -----------------
   --  For simple values, like integers, floats and strings, we simply store
   --  the string as displayed by the debugger.

   type Simple_Type is new Generic_Type with record
      Value : String_Access; --  The value, as displayed by the debugger
   end record;

   procedure Print (Value : Simple_Type);
   --  Print Value on Standard_Output.

   ------------------
   -- Access Types --
   ------------------
   --  Access types are similar to Simple_Types, but since they can be
   --  dereferenced we create a new type. Note that the value is different
   --  from the Address field in Generic_Type which is the address of the
   --  variable itself.

   type Access_Type is new Generic_Type with record
      Value : String_Access;
   end record;

   procedure Print (Value : Access_Type);
   --  Print Value on Standard_Output.

   ----------------
   -- Enum Types --
   ----------------

   type Enum_Type is new Generic_Type with record
      Value : String_Access;
   end record;

   procedure Print (Value : Enum_Type);

   ----------------
   -- Array_Type --
   ----------------
   --  For array types.
   --  We store the dimensions of the array and the type of the items the first
   --  type we parse the type, so what parsing the value can be made faster
   --  afterwards.
   --  We also store the values in a sparse array (equal adjacent values are
   --  stores as a Repeat_Type (for instance with '<repeat .. times>' in gdb).
   --  For unidimensional arrays of characters, we store their value as
   --  a single item, a string.

   type Dimension is record
      First, Last : Integer;
   end record;
   type Dimension_Array is array (Positive range <>) of Dimension;

   type Integer_Array is array (Positive range <>) of Integer;

   type Array_Item is record
      Index : Integer;
      --  Calculated with Compute_Index.

      Value : Generic_Type_Access;
   end record;
   type Array_Item_Array is array (Positive range <>) of Array_Item;

   type Array_Item_Array_Access is access Array_Item_Array;

   type Array_Orientation is (Vertical, Horizontal);

   type Array_Type (Num_Dimensions : Positive) Is new Generic_Type with record
      Dimensions  : Dimension_Array (1 .. Num_Dimensions);
      Values      : Array_Item_Array_Access;
      Item_Type   : Generic_Type_Access;

      Orientation : Array_Orientation := Horizontal;
      --  Set by default by the language (FORTRAN vs C), but can be
      --  overriden by the user.
   end record;
   type Array_Type_Access is access all Array_Type'Class;

   procedure Print (Value : Array_Type);
   --  Print Value on Standard_Output.

   -----------------------
   -- Computing indexes --
   -----------------------

   generic
      For_Array : Array_Type;
   package Array_Indexes is
      type Indexes is array (1 .. For_Array.Num_Dimensions) of Integer;
      function Compute_Indexes (I : Indexes) return Integer;
      --  Return the index corresponding to a given position in the array.
      --  This result can be used in the record Array_Item
   end Array_Indexes;

   ------------
   -- Repeat --
   ------------
   --  To handle the '0 <repeats .. times>' case.

   type Repeat_Type is new Generic_Type with record
      Repeat_Num : Integer;
      Value      : Generic_Type_Access;
   end record;

   procedure Print (Value : Repeat_Type);
   --  Print Value on Standard_Output.

   -------------
   -- Records --
   -------------

   type Record_Type_Array;
   type Record_Type_Array_Access is access Record_Type_Array;

   type Record_Field is record
      Name  : String_Access;
      Value : Generic_Type_Access;
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
      Fields     : Record_Field_Array (1 .. Num_Fields);
   end record;
   type Record_Type_Access is access all Record_Type'Class;
   --  Num_Fields can be 0 in case of a 'null record'. Thus, it has to be
   --  a Natural.

   type Record_Type_Array is array (Positive range <>) of Record_Type_Access;

   procedure Print (Value : Record_Type);
   --  Print Value on Standard_Output.


--  enum DispValueType {
--    UnknownType = 0,    // Unknown type
--    Simple = 1,         // Ordinary or other value
--    Pointer,            // Pointer value
--    Array,              // Array
--    Struct,             // Struct, Class, Record, Object
--    Reference,          // C++ Reference.  2 children.
--    Sequence,           // Sequence of values.
--    List,               // List of values.  Last member may be text.
--    Text                // Multi-line text.  Last member of a list.
--  };


end Generic_Values;
