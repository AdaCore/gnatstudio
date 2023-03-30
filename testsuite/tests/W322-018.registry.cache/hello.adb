with Ada.Text_IO; use Ada.Text_IO;
procedure hello is
begin
    for J in 1 .. 10_000 loop
      Put_Line ("hello.adb:1:1: problem");
   end loop;
end;
