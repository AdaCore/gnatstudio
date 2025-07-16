with Ada.Text_IO;

procedure Foo is
   pragma SPARK_Mode (On);

   type Integer_Access is access Integer;

   procedure Bar (X : not null Integer_Access);

   ---------
   -- Bar --
   ---------

   procedure Bar (X : not null Integer_Access) is
   begin
      if X = null then
         Ada.Text_IO.Put_Line ("Not possible");
      end if;
   end Bar;

   X : Integer_Access := new Integer'(5);
begin
   Bar (X);
end Foo;
