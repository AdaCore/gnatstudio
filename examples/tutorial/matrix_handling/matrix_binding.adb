with Interfaces.C.Strings;

package body Matrix_Binding is

   -----------
   -- Alloc --
   -----------

   function Alloc (Rows, Columns : Integer) return Matrix_Type is
      function C_Alloc (Rows, Columns : short) return Matrix_Type;

      pragma Import (C, C_Alloc, "matrixAlloc");
   begin
      return C_Alloc (short (Rows), short (Columns));
   end Alloc;

   -----------
   -- Print --
   -----------

   function Print (M : Matrix_Type) return String is
      function C_Print (M : Matrix_Type) return Interfaces.C.Strings.chars_ptr;

      pragma Import (C, C_Print, "matrixPrint");
   begin
      return Interfaces.C.Strings.Value (C_Print (M));
   end Print;

   ---------
   -- Set --
   ---------

   procedure Set
     (M      : Matrix_Type;
      Row    : Integer;
      Column : Integer;
      Value  : Integer)
   is
      procedure C_Set
        (M      : Matrix_Type;
         Row    : Integer;
         Column : Integer;
         Value  : Integer);

      pragma Import (C, C_Set, "matrixSet");
   begin
      C_Set (M, Row, Column, Value);
   end Set;

   ---------
   -- Get --
   ---------

   function Get
     (M      : Matrix_Type;
      Row    : Integer;
      Column : Integer) return Integer
   is
      function C_Get
        (M      : Matrix_Type;
         Row    : Integer;
         Column : Integer) return Integer;

      pragma Import (C, C_Get, "matrixGet");
   begin
      return C_Get (M, Row, Column);
   end Get;

   ----------
   -- Copy --
   ----------

   function Copy (M : Matrix_Type) return Matrix_Type is
      function C_Copy (M : Matrix_Type) return Matrix_Type;

      pragma Import (C, C_Copy, "matrixCopy");
   begin
      return C_Copy (M);
   end Copy;

   ---------
   -- "+" --
   ---------

   function "+" (Left, Right : Matrix_Type) return Matrix_Type
   is
      function C_Add (M, N : Matrix_Type) return Matrix_Type;
      pragma Import (C, C_Add, "matrixAdd");

   begin
      return C_Add (Left, Right);
   end "+";

   ---------
   -- "*" --
   ---------

   function "*" (Left, Right : Matrix_Type) return Matrix_Type
   is
      function C_Mul (M, N : Matrix_Type) return Matrix_Type;

      pragma Import (C, C_Mul, "matrixMul");
   begin
      return C_Mul (Left, Right);
   end "*";

end Matrix_Binding;
