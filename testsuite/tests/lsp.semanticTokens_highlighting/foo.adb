with Ada.Text_IO;

procedure Foo is
begin
   AAA :  declare
   begin
      null;
   end AAA;

   BBB: declare
   begin
      null;
   end BBB;

   CCC:declare
   begin
      null;
   end CCC;

   XXX : begin
      null;
   end XXX;

   YYY : declare
   begin
      null;
   end YYY;

   ZZZ :
   declare
   begin
      Ada.Text_IO.Put_Line ("Hello");
   end ZZZ;
end Foo;
