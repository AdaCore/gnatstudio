package Bar is

   type My_Int is tagged record
      A : Integer;
   end record;

   procedure Do_Nothing (Obj : My_Int; A :Integer; B : Integer) is null;

end Bar;
