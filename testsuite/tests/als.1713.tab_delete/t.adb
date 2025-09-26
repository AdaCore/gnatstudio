procedure T is
	function Foo (I : Integer)
	return Boolean is (True);

   XXXXXXXXXXXXXX : Integer;
   YYYYYYYYYYYYYY : Integer;
begin
	if Foo (XXXXXXXXXXXXXX + XXXXXXXXXXXXXX * YYYYYYYYYYYYYY = YYYYYYYYYYYYYY + YYYYYYYYYYYYYY) then
      null;
   end if;
end T;
