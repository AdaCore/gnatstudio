procedure T is
   function F1 (A : Boolean; B : Boolean) return Boolean is (True);

   function F2 (A : Boolean; B : Boolean) return Boolean is (False);

   function F3 return Boolean is (F (A => True, B => False));

   function F4 return Boolean is (F2 (A => True, B => False));
begin
   null;
end T;
