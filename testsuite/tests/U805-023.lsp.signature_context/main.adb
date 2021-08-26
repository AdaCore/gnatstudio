procedure Main
is
   procedure Do_Something (I : Integer);
   procedure Do_Something (I, J : Integer);

   ------------------
   -- Do_Something --
   ------------------

   procedure Do_Something (I : Integer) is
   begin
      null;
   end Do_Something;

   ------------------
   -- Do_Something --
   ------------------

   procedure Do_Something (I, J : Integer) is
   begin
      null;
   end Do_Something;

begin
   Do_Something
end Main;
