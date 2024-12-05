with Ada.Text_IO.Float_IO;

procedure Hello is
   type Lat_Long_Degrees_Float_Type is digits 10;

   package Lat_Long_Degrees_Text_IO is
     new Ada.Text_IO.Float_IO (Lat_Long_Degrees_Float_Type);
begin
   Lat_Long_Degrees_Text_IO.Put
end Hello;
