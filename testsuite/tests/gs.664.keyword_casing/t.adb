procedure t is
   function Bye_Bye (X_A : Boolean; X_B : Boolean) return Boolean is (True);

   function Hello_World reTurn Boolean
   IS (Bye_Bye (X_A => True, X_B => False));

begin
   null;
end;
