package Matrix_Math is

   type Matrix is array (Natural range <>, Natural range <>) of Float;

   function "*" (Left, Right : Matrix) return Matrix with
      Pre => Left'Length(2) = Right'Length(1);

end Matrix_Math;
