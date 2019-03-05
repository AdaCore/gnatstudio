with Ada.Text_IO;

procedure Main is
   Result : Integer := 1;

   procedure Print_Result is
   begin
      Ada.Text_IO.Put_Line ("Result is " & Integer'Image (Result));
   end Print_Result;
begin
   Print_Result;
   Print_Result;
   Print_Result;
end Main;
