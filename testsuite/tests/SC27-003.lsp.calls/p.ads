with Ada.Text_IO; use Ada.Text_IO;
package P is

   procedure Foo is
      procedure Bla is null;
   begin
      Foo;
      Bla;
      Put_Line ("hello");
   end Foo;

end P;
