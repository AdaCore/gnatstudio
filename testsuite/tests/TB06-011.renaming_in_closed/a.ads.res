package A is

   type Object_Type_Aa is record
      Num : Natural;
   end record;

   procedure Print (Self : Object_Type_Aa);
   procedure Clear (Self : Object_Type_Aa);
end A;
