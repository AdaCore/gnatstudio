with Ada.Text_IO;

procedure Main is

   procedure One is
      procedure Two is
         procedure Three is
            procedure Four is
            begin
               Ada.Text_IO.Put_Line ("Let's update the Call Stack!");
            end Four;
         begin
            Four;
         end Three;
      begin
         Three;
      end Two;

   begin
      Two;
   end One;

begin
   One;
end Main;
