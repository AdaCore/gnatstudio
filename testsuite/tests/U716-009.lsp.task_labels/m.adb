with P;
with GNATCOLL.Utils;


procedure M is
   S : constant String := GNATCOLL.Utils.Replace ("Hello", "a", "b");
begin
   P.S;
end M;
