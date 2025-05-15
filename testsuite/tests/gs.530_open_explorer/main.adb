with Ada.Text_IO;

procedure Main is

   procedure P1;
   procedure P2;

   procedure P1 is
      Var : Boolean := False;
   begin
      P2;
   end P1;

   procedure P2 is
      Var : Boolean := True;
   begin
      Ada.Text_IO.Put_Line ("Hello");
   end P2;


begin
   P1;
end Main;
