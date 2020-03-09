with Ada.Text_IO;

procedure Main is
   Result : Integer;

   procedure Print_Result is
   begin
      Ada.Text_IO.Put_Line ("Result is " & Integer'Image (Result));
   end Print_Result;
begin
   Result := 2 + 2;
   Print_Result;
end Main;
