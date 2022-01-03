with Ada.Text_IO;

procedure main is

   type AN_INDEX is mod 10;

   type A_LOG is record
      Int1 : Integer;
   end record;

   type A_LOG_ARRAY is array (AN_INDEX) of A_LOG;

   Array_Of_Integers : A_LOG_ARRAY;

begin

   for Index in AN_INDEX'Range loop
      Array_Of_Integers(Index) := (Int1 => Integer(Index));
   end loop;
   loop
      null;
   end loop;
end main;
