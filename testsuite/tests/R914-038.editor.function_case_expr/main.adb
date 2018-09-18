procedure Main is
   Bool : Boolean;
   function Hello return String is
     (case Bool is
         when False => "Hi",
         when True => "Hello");

   procedure Bbb is null;
begin
   null;
end Main;
