with ada.text_io;

procedure hello is
   V : String := "V";
   T : String := "literal";
begin
   if True then
      --  Comment
      Ada.Text_IO.Put_Line ("hello");
   end if;
end;
