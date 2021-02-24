with Ada.Real_Time;
with Ada.Text_IO;
with Ada.Calendar.Formatting;

with A; use A;

procedure Main is

   type Object_Type_1 is record
      Num : Natural;
   end record;

   type Object_Type_2 is record
      Num : Positive;
   end record;

   Object_2 : Object_Type_1;
   Object_1 : Object_Type_2;
   for Object_1 use at Object_2'Address;

   ObjA : Object_Type_A;
begin
   #if FP_Initialize_Required then
      Ada.Text_IO.Put_Line ("Here");
   #end if;

   loop
      exit when Ada.Text_IO.Get_Line = "q";
   end loop;

   Print (ObjA);
end Main;
