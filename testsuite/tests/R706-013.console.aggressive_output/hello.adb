--  This program outputs an uninterrupted stream of 'a' for 3 minutes
with Ada.Calendar; use Ada.Calendar;
with Ada.Text_IO;  use Ada.Text_IO;

procedure Hello is
   Start : Time := Clock;
begin
   while True loop
      for J in 1 .. 10_000 loop
         Put ('a');
      end loop;
      New_Line;
      exit when Clock - Start > 180.0;
   end loop;
   Put_Line ("the end");
end Hello;
