with Pkg;

procedure Main is
   Dummy : Boolean;
begin
   Dummy := Pkg.Check (True, True);
   Dummy := Pkg.Check (True, False);
end Main;
