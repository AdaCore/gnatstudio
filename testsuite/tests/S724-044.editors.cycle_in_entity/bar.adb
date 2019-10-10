procedure Bar is
   task A is
      entry Hello (Name : String);
   end A;

   task body A is
   begin
      accept Hello (Name : String) do
         Ada.Text_IO.Put_Line (Name);
      end Hello;
   end A;

   protected type Shield is
      entry Set (J : Integer);
   private
      I : Integer := 0;
   end Shield;

   protected body Shield is
      entry Set (J : Integer)
        when I = 10 is
      begin
         I := J;
      end Set;
   end Shield;

begin
   null;
end Bar;
