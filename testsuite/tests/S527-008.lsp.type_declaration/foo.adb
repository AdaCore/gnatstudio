package body Foo is

   function Get_My_Type return My_Type is
      A : My_Type := 10;
   begin
      return A;
   end Get_My_Type;

end Foo;
