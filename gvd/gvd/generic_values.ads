with System;
with Unchecked_Deallocation;

package Generic_Values is

   --  Note: the types in this package are currently public, since the parser
   --  function needs to be able to create them.
   --  However, it would be cleaner to provide a set of subprograms to create
   --  and access them.


   type String_Access is access all String;
   procedure Free is new Unchecked_Deallocation (String, String_Access);

   ------------------
   -- Generic_Type --
   ------------------

   type Generic_Type is abstract tagged record
      Address : System.Address;  --  Address of the variable or field
   end record;
   type Generic_Type_Access is access all Generic_Type'Class;

   procedure Print (Value : Generic_Type) is abstract;
   --  Print Value on Standard_Output.

   procedure Clear_Value (Value : in out Generic_Type) is abstract;
   --  Clear the value, and free the memory that was allocated for it.
   --  This also clears all the children or components of Value.

   function Clone (Value : Generic_Type) return Generic_Type_Access
      is abstract;
   --  Clone Value, and return a new allocated record.
   --  Only the type-related fields are cloned, the value fields are reset to
   --  Null.

   procedure Free is new Unchecked_Deallocation
     (Generic_Type'Class, Generic_Type_Access);

   -----------------
   -- Simple_Type --
   -----------------
   --  For simple values, like integers, floats and strings, we simply store
   --  the string as displayed by the debugger.

   type Simple_Type is new Generic_Type with record
      Value : String_Access := null;
      --  The value, as displayed by the debugger
   end record;
   type Simple_Type_Access is access all Simple_Type'Class;

   procedure Print (Value : Simple_Type);
   --  Print Value on Standard_Output.

   procedure Clear_Value (Value : in out Simple_Type);

   function Clone (Value : Simple_Type) return Generic_Type_Access;

   -----------------
   -- Range Types --
   -----------------
   --  A range value, as in Ada 'range 0 .. 10', ie a number between the min
   --  and max value.

   type Range_Type is new Simple_Type with record
      Min, Max : Long_Integer;
   end record;
   type Range_Type_Access is access all Range_Type'Class;

   procedure Print (Value : Range_Type);
   function Clone (Value : Range_Type) return Generic_Type_Access;

   ---------------
   -- Mod Types --
   ---------------
   --  These are modular types, between 0 and a maximal value.

   type Mod_Type is new Simple_Type with record
      Max : Long_Integer;
   end record;
   type Mod_Type_Access is access all Mod_Type'Class;

   procedure Print (Value : Mod_Type);
   function Clone (Value : Mod_Type) return Generic_Type_Access;

   ------------------
   -- Access Types --
   ------------------
   --  Access types are similar to Simple_Types, but since they can be
   --  dereferenced we create a new type. Note that the value is different
   --  from the Address field in Generic_Type which is the address of the
   --  variable itself.

   type Access_Type is new Simple_Type with null record;
   procedure Print (Value : Access_Type);
   function Clone (Value : Access_Type) return Generic_Type_Access;

   ----------------
   -- Enum Types --
   ----------------

   type Enum_Type is new Simple_Type with null record;

   procedure Print (Value : Enum_Type);
   function Clone (Value : Enum_Type) return Generic_Type_Access;

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
      First, Last : Long_Integer;
   end record;
   type Dimension_Array is array (Positive range <>) of Dimension;

   type Integer_Array is array (Positive range <>) of Integer;

   type Array_Item is record
      Index : Long_Integer;
      --  Calculated with Compute_Index.

      Value : Generic_Type_Access := null;
   end record;
   type Array_Item_Array is array (Positive range <>) of Array_Item;

   type Array_Item_Array_Access is access Array_Item_Array;
   procedure Free is new Unchecked_Deallocation
     (Array_Item_Array, Array_Item_Array_Access);

   type Array_Orientation is (Vertical, Horizontal);

   type Array_Type (Num_Dimensions : Positive) Is new Generic_Type with record
      Dimensions  : Dimension_Array (1 .. Num_Dimensions);
      Values      : Array_Item_Array_Access := null;
      Item_Type   : Generic_Type_Access := null;
   end record;
   type Array_Type_Access is access all Array_Type'Class;

   procedure Print (Value : Array_Type);
   procedure Clear_Value (Value : in out Array_Type);
   function Clone (Value : Array_Type) return Generic_Type_Access;

   -----------------------
   -- Computing indexes --
   -----------------------

   generic
      For_Array : Array_Type;
   package Array_Indexes is
      type Indexes is array (1 .. For_Array.Num_Dimensions) of Long_Integer;
      function Compute_Index (I : Indexes) return Long_Integer;
      --  Return the index corresponding to a given position in the array.
      --  This result can be used in the record Array_Item
   end Array_Indexes;

   ------------
   -- Repeat --
   ------------
   --  To handle the '0 <repeats .. times>' case.

   type Repeat_Type is new Generic_Type with record
      Repeat_Num : Integer;
      Value      : Generic_Type_Access := null;
   end record;
   type Repeat_Type_Access is access all Repeat_Type'Class;

   procedure Print (Value : Repeat_Type);
   procedure Clear_Value (Value : in out Repeat_Type);
   function Clone (Value : Repeat_Type) return Generic_Type_Access;

   -------------
   -- Records --
   -------------

   type Record_Type_Array;
   type Record_Type_Array_Access is access Record_Type_Array;

   type Record_Field is record
      Name  : String_Access := null;
      Value : Generic_Type_Access := null;
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
   procedure Clear_Value (Value : in out Record_Type);
   function Clone (Value : Record_Type) return Generic_Type_Access;

   ------------
   -- Unions --
   ------------

   type Union_Type (Num_Fields : Natural) is new Record_Type (Num_Fields) with
      record
         null;
      end record;
   type Union_Type_Access is access all Union_Type'Class;

   procedure Print (Value : Union_Type);
   function Clone (Value : Union_Type) return Generic_Type_Access;

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
