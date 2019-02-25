with Ada.Text_IO; use Ada.Text_IO;

procedure Hello is
   procedure Nested is
   begin
      null;
   end;
begin
   Put ("Hello World");
end Hello;
