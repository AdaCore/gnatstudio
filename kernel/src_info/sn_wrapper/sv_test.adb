with Simple_Vector;

procedure SV_Test is
   --  Simple test for Simple_Vector package.
   --  Test is PASSED if test executable is finished with exit code 0
   --  and without any messages on stderr/stdout.
   --  Otherwise test is FAILED.
   subtype T is Integer;

   package IntVec is new Simple_Vector (T);
   use IntVec;

   type List is new Node_Access;

   L : List := null;

begin

   for I in 1 .. 10 loop
      Append (L, I);
      Append (I, L);
   end loop;

   pragma Assert (Size (L) = 20,
     "Incorrect Simple_Vector behaviour for 'Append' operation");

   for I in 1 .. 10 loop
      pragma Assert
        (Get_Element_At (L, I) = Integer (10 - I + 1),
         "Incorrect Simple_Vector behaviour for 'Get_Element_At' operation");
      null;
   end loop;

   Release_Vector (L);

   pragma Assert (Size (L) = 0,
     "Incorrect Simple_Vector behaviour for 'Release' operation");

end SV_Test;
