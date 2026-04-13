with Ada.Text_IO;

procedure Main is

   procedure Hello is
   begin
      Ada.Text_IO.Put_Line ("Hello");
   end Hello;

   procedure World is
   begin
      Ada.Text_IO.Put_Line ("World");
   end World;

begin
   Hello;
   World;
end Main;
