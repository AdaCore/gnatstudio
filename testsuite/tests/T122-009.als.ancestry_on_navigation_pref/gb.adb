
with Class_Definition.Subclass;
with User;

procedure Gb is

   V1 : constant Class_Definition.A'Class := Class_Definition.Subclass.Gen;
   V2 : constant Class_Definition.A'Class := User.Gen;

begin
   Class_Definition.P1 (V1);
   Class_Definition.P1 (V2);
end Gb;
