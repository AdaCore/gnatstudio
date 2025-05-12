with Ada.Text_IO;
with Ada.Unchecked_Conversion;
with System;
with System.Address_To_Access_Conversions;

procedure Main is

   type Integer_Access is access all Integer;
   package To_Access is
     new System.Address_To_Access_Conversions (Integer);
   function To_Address is
     new Ada.Unchecked_Conversion (Integer, System.Address);

   I : Integer_Access;

begin
   I := Integer_Access (To_Access.To_Pointer (To_Address (16#8#)));
   Ada.Text_IO.Put_Line (I.all'Img);
end Main;
