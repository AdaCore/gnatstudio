procedure Main
is
   procedure Do_Something (A : Integer; B : Integer);
   procedure Do_Something (A : Integer);
   procedure Do_Something (F : Float);

   ------------------
   -- Do_Something --
   ------------------

   procedure Do_Something (A : Integer; B : Integer) is
   begin
       null;
   end Do_Something;

   ------------------
   -- Do_Something --
   ------------------

   procedure Do_Something (A : Integer) is
   begin
      null;
   end Do_Something;

   ------------------
   -- Do_Something --
   ------------------

   procedure Do_Something (F : Float) is
   begin
      null;
   end Do_Something;
begin
   Do_Something
end Main;
