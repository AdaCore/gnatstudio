with Ada.Unchecked_Deallocation;

generic

   type Data_Type is private;
   --  The type of data inside the array.

   Null_Element : Data_Type;
   --  The default value of data

   Free_Buffer : Positive := 10;
   --  This is the size that the array uses to grow when the memory is
   --  too small.

   with procedure Free (Data : in out Data_Type) is <>;

package Dynamic_Arrays is

   type Dynamic_Array is private;
   --  This object is a dynamic Array with a virtual infinite size.
   --  By default, each element is initialized with the value Null_Element.

   Null_Array : constant Dynamic_Array;
   --  This is an emty array.

   procedure Set_Element
     (This     : in out Dynamic_Array;
      Element  : Data_Type;
      Position : Positive);
   --  Use to put an element inside the array.

   function Get_Element
     (This     : Dynamic_Array;
      Position : Positive)
     return Data_Type;
   --  Use to get an element from the array. If there is nothing inside,
   --  Null_Element is returned.

   procedure Delete_Element (This : in out Dynamic_Array; Position : Positive);
   --  Initialize a position with Null_Element.

   procedure Resize (This : in out Dynamic_Array);
   --  When called, optimise the size taken by the array by using the
   --  Free_Buffer value.

   procedure Free (This : in out Dynamic_Array);
   --  Free the memory used by a dynamic array.


   function Clone (This : Dynamic_Array) return Dynamic_Array;
   --  Copy all the values of an arry into an other.


private

   type Static_Array is array (Positive range <>) of Data_Type;
   type Dynamic_Array is access Static_Array;

   procedure Resize (This : in out Dynamic_Array; New_Size : Positive);
   function Get_Using_Size (This : Dynamic_Array) return Natural;
   procedure Delete is
      new Ada.Unchecked_Deallocation (Static_Array, Dynamic_Array);

   Null_Array : constant Dynamic_Array := null;

end Dynamic_Arrays;
