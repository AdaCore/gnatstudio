with Ada.Text_IO;

procedure Main is
begin
   Ada.Text_IO.Put_Line
     ("Nunc nec tellus in libero maximus "
      & ASCII.LF
      & "feugiat vitae porta quam. Sed pharetra risus mauris. Integer "
      & ASCII.LF
      & "pellentesque ut libero vitae pharetra. Fusce facilisis neque id "
      & ASCII.LF
      & "sapien fringilla varius. Pellentesque in justo maximus, sodales "
      & ASCII.LF
      & "risus nec, scelerisque ante.");

    for I in 1 .. 10 loop
      Ada.Text_IO.Put_Line ("garbage");
      delay (0.5);
    end loop;
end Main;
