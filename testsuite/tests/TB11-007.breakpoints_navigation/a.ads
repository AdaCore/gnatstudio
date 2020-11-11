package A is

   type Object_Type_A is record
      Num : Natural;
   end record;

   procedure Print (Self : Object_Type_A);
   procedure Clear (Self : Object_Type_A);
end A;
