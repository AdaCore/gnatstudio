package Bar is

   type My_Int is tagged record
      A : Integer;
   end record;

   procedure Do_Nothing
     (Obj                                : My_Int;
      A1, A2, A3, A4, A5, A6, A7, A8, A9 : Integer;
      B1, B2, B3, B4, B5, B6, B7, B8, B9 : Float);

end Bar;
