
with Ada.Text_IO;

procedure Main is
   function "&" (L,R : Integer) return Integer;

   function "&" (L,R : Integer) return Integer is
   begin
      return L + R;
   end "&";

begin
   Ada.Text_IO.Put_Line (Integer'Image (3 & 4));
end Main;
