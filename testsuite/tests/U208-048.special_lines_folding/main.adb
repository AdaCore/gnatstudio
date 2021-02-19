procedure Main is

   Even_Number_Count, Odd_Number_Count : Natural := Natural'First;

begin

   for I in 1 .. 100 loop

      if I mod 2 = 0 then

         Even_Number_Count := Even_Number_Count + 1;

      else

         Odd_Number_Count := Odd_Number_Count + 1;

      end if;

   end loop;

end Main;
