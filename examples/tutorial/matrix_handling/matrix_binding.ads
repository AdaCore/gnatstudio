with System;
with Interfaces.C; use Interfaces.C;

package Matrix_Binding is

   type Matrix_Type is new System.Address;

   function Alloc (Rows, Columns : Integer) return Matrix_Type;
   --  Create a null matrix of dimensions (Rows, Columns).

   procedure Set
     (M      : Matrix_Type;
      Row    : Integer;
      Column : Integer;
      Value  : Integer);
   --  Set the value of one cell.

   function Get
     (M      : Matrix_Type;
      Row    : Integer;
      Column : Integer) return Integer;
   --  Get the value of one cell.

   function Print (M : Matrix_Type) return String;
   --  Display the contents of M.

   function Copy (M : Matrix_Type) return Matrix_Type;
   --  Return a copy of M.

   function "+" (Left, Right : Matrix_Type) return Matrix_Type;
   function "*" (Left, Right : Matrix_Type) return Matrix_Type;
   --  Basic operations.

end Matrix_Binding;
