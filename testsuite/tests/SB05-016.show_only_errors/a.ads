package A is

   procedure Print;

   procedure Print (Value : String);

   type Print_Access is access procedure (Value : String);

   type Object_Type_A is record
      Num : Natural;
   end record;

   procedure Print (Self : Object_Type_A);
   procedure Clear (Self : Object_Type_A);
end A;
