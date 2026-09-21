package body Pkg is

   function Check (A, B : Boolean) return Boolean is
   begin
      return A and then B;
   end Check;

   function Unused (A, B : Boolean) return Boolean is
   begin
      return A or else B;
   end Unused;

end Pkg;
