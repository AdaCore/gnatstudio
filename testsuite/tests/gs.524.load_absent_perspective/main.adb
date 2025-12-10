with Ada.Text_IO;
with PkgBodyWithSpec;

procedure Main is

   Is_Bool : Boolean := False;

   procedure Test is
      Local : Integer;
   begin
      Local := 1;
   end Test;

   procedure P is
      Var : Integer := 0;
   begin
      pragma Assert (False);

      if Var <= 0 then
         null;
      else
         Is_Bool := True;
      end if;
   end P;

begin
   Ada.Text_IO.Put_Line ("""([A-Z]|[0-9]|[, !""&#$%'\(\)\*\+\-\./:;<=>@\[\\\]\^_\|\{\}""""])*""");
end Main;
--  " - " & '"' & "
--  & "" & = &
