procedure T is
X : Integer := 1;
YYY : Natural := 2;
begin
if X = YYY
or else X + X > YYY
and then (X < YYY
and then Y > 2)
then
null;
end if;

for I in 1 .. X loop
if X > 2 then
Y := Y + 1;
end if;
end loop;

case X is
when X > 10 =>
null;
when X > 20 =>
null;
when X > 30
| Y > 10 =>
null;
when others =>
null;
end case;

this_statement_is_missing_a_semicolon
end T;
