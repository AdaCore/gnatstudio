with Ada.Finalization;

package Parse_Controlled is

   type T1 is new Ada.Finalization.Controlled with record null; end record;
   type M2 is array (1 .. 2, 1.. 1) of T1;
   type R2 is record X : M2; end record;
   X1 : R2;

   My_Exception : exception;

   --  From 8511-011
   type Variant_Kind is (VK_Null, VK_Num, VK_String);
   type Null_Variant_Record (Kind : Variant_Kind := VK_Null) is record
      case Kind is
         when VK_Null =>
            null;
         when VK_Num =>
            Num_Value : Long_Float;
         when VK_String =>
            String_Value : Natural;
      end case;
   end record;
   type Null_Variant is new Ada.Finalization.Controlled with record
      V : Null_Variant_Record;
   end record;

end Parse_Controlled;
