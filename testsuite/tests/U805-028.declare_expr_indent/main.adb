procedure Main is

   function ComputeSquarePlusCube(x : Integer) return Integer is
     (declare t : constant Integer := Integer(x); begin
      t * t * (1 + t));

        function Foo(x : Integer) return Integer is (x);
begin
   null;
end Main;
