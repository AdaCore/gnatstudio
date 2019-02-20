with Ada.Text_IO;

procedure Main
is
   package P_MC2_CMS_Sub_Sub_Step_V is new Generic_Heap_Vector(MC2_CMS_Sub_Sub_Step, MC2_CMS_Sub_Sub_Step_V);
begin
   delay (1.0);
   Ada.Text_IO.Put_Line ("Hello");
end Main;
