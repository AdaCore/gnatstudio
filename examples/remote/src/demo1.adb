with Instr; use Instr;
with Gen_List;
with Ada.Text_IO; use Ada.Text_IO;

procedure Demo1 is

   type Acc is access all Instrument'Class;

   package Dash_Board is new Gen_List (Acc);
   use Dash_Board;

   procedure Print_Dash_Board (L : List);

   procedure Print_Dash_Board (L : List) is
      L1 : List := L;
      A : Acc;
   begin
      while L1 /= Nil loop
         A := Element (L1);
         Display_Value (A.all);
         L1 := Tail (L1);
      end loop;

      New_Line;
   end Print_Dash_Board;


   Speed      : aliased Speedometer;
   Fuel       : aliased Gauge;
   Oil, Water : aliased Graphic_Gauge;
   Time       : aliased Clock;
   Chrono     : aliased Chronometer;

   DB : List := Nil;

begin
   Set_Name (Speed, "Speed");
   Set_Name (Fuel, "Fuel");
   Set_Name (Water, "Water");
   Set_Name (Oil, "Oil");
   Set_Name (Time, "Time");
   Set_Name (Chrono, "Chronometer");

   Speed.Value := 45; --  mph
   Fuel.Value := 60; --  %
   Water.Value := 80; -- %
   Oil.Value := 30; --  %
   Init (Time, 12, 15, 00);
   Init (Chrono, 22, 12, 56);

   DB := Append
     (Speed'Access,
      Append
        (Fuel'Access,
         Append
           (Water'Access,
            Append
              (Oil'Access,
               Append (Time'Access, Chrono'Access)))));

   Print_Dash_Board (DB);
end Demo1;
