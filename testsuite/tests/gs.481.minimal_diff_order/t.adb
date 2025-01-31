procedure t is
   function F (A : Boolean; B : Boolean) return Boolean is (True);

   function F2 return Boolean is
       (F (A => True,
               B => False));

begin
   null;
end;
