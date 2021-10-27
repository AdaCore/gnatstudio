with Ada.Text_IO;

package body Bar is

   -----------
   -- Hello --
   -----------

   procedure Hello 
   is
      X : constant Integer := 1;
   begin
      Ada.Text_IO.Put_Line ("Hello");
      if X > 2 then
         Ada.Text_IO.Put ("!");
      end if;
   end Hello;

end Bar;
