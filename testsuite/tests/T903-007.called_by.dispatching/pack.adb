with Ada.Text_IO;

package body Pack is

   -----------
   -- Print --
   -----------

   procedure Hello (X : Abstract_Record) is
   begin
      Ada.Text_IO.Put_Line ("Hello");
   end Hello;

   -------------
   -- Print_I --
   -------------

   procedure Print_I (X : My_Record) is
   begin
      Ada.Text_IO.Put_Line (Integer'Image (X.I));
   end Print_I;

   -----------
   -- Print --
   -----------

   overriding procedure Hello (X : My_Record) is
   begin
      Ada.Text_IO.Put_Line ("Hello!");
   end Hello;

end Pack;
