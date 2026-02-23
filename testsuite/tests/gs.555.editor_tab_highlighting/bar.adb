with Ada.Text_IO;

package body Bar is
   
   -----------
   -- Hello --
   -----------

   procedure Hello is
     S : String := "Hello" with Unreferenced;
   begin
	  Ada.Text_IO.Put_Line (S);
   end Hello;

end Bar;
