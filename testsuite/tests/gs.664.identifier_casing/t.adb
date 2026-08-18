procedure t is
   function Bye_Bye (X_A : Boolean; X_B : Boolean) return Boolean is (True);

   function Hello_World return Boolean
   is (BYE_bye (X_A => True, x_b => False));

begin
   null;
end;
