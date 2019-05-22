
with Ada.Text_IO;

procedure A
is
   type My_Bool is record
      Value : Boolean := True;
   end record;

   function "and" (V1, V2 : My_Bool) return Boolean;

   B1, B2 : My_Bool;

   function "and" (V1, V2 : My_Bool) return Boolean is
   begin
      return V1.Value and V2.Value;
   end "and";

begin
   if B1 and B2 then
      Ada.Text_IO.Put_Line ("Both");
   end if;
end A;
