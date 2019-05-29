with Ada.Text_IO;

package body B is

   procedure Put (Value : String) is
   begin
      Ada.Text_IO.Put_Line (Value);
   end Put;

end B;
