package Instr is

   --   Instruments for a Dashboard
   --
   --   Instrument ---- Speedometer
   --              |
   --              ---- Gauge ---- Graphic_Gauge
   --              |
   --              ---- Clock ---- Chronometer


   -----------------
   --  Root Type  --
   -----------------

   type Instrument is tagged record
      Name : String (1 .. 14) := "              ";
   end record;

   procedure Set_Name (I : in out Instrument; S : String);
   procedure Display_Value (I : Instrument);

   -------------------
   --  Speedometer  --
   -------------------

   subtype Speed is Integer range 0 .. 85; -- mph

   type Speedometer is new Instrument with record
      Value : Speed;
   end record;

   procedure Display_Value (S : Speedometer);

   -----------------
   --   Gauges    --
   -----------------

   subtype Percent is Integer range 0 .. 100;

   type Gauge is new Instrument with record
      Value : Percent;
   end record;

   procedure Display_Value (G : Gauge);

   type Graphic_Gauge is new Gauge with record
      Size  : Integer := 20;
      Fill  : Character := '*';
      Empty : Character := '.';
   end record;

   procedure Display_Value (G : Graphic_Gauge);

   -----------------
   --   Clocks    --
   -----------------

   subtype Sixty is Integer range 0 .. 60;
   subtype Twenty_Four is Integer range 0 .. 24;

   type Clock is new Instrument with record
      Seconds : Sixty := 0;
      Minutes : Sixty := 0;
      Hours   : Twenty_Four := 0;
   end record;

   procedure Display_Value (C : Clock);
   procedure Init (C : in out Clock;
                   H : Twenty_Four := 0;
                   M, S : Sixty := 0);

   procedure Increment (C : in out Clock; Inc : Integer := 1);

   type Chronometer is new Clock with null record;

   procedure Display_Value (C : Chronometer);

end Instr;

