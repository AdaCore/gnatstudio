with Ada.Text_IO;

package body Bar is
   
   -----------
   -- Hello --
   -----------

   procedure Hello is
   begin
	Ada.Text_IO.Put_Line ("Hello");
   end Hello;

   ---------
   -- Foo --
   ---------
   
   procedure Foo is
   begin
	Ada.Text_IO.Put_Line ("Foo");
   end Foo;
   
end Bar;
