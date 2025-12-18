with Ada.Text_IO;

procedure Main is
begin
   Ada.Text_IO.Put_Line ("""([A-Z]|[0-9]|[, !""&#$%'\(\)\*\+\-\./:;<=>@\[\\\]\^_\|\{\}""""])*""");
end Main;
