with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO;
procedure hello is
   X : Unbounded_String;

begin
   Append (X, "hello");
end;
