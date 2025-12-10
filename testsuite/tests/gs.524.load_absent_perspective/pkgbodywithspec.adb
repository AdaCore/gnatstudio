package body PkgBodyWithSpec is
   Global1 : Integer := 0;

   procedure Proc1 (Arg1, Arg2 : Integer) is
      Y : Integer := Arg1 + Arg2 + Global1;
   begin
      null;
   end Proc1;

   function Expr_Fct (Arg1, Arg2 : Integer) return Integer
   is (Arg1 + Arg2 + Global1);

   procedure Proc2 (Arg1, Arg2 : Integer) is
      function Nested_Expr_Fct (Arg3, Arg4 : Integer) return Integer
      is (Arg1 + Arg2 + Arg3 + Arg4 + Global1);
   begin
      null;
   end Proc2;

end PkgBodyWithSpec;
