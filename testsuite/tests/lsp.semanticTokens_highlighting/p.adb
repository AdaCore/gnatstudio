with Ada.Text_IO;

 package body P is

   H : constant String := "Hello";

   procedure Hello is
   begin
      Ada.Text_IO.Put_Line (H);
   end Hello;
 end P;
