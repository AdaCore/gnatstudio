with GNAT.IO; use GNAT.IO;

package body Instr is

   procedure Set_Name (I : in out Instrument; S : String) is
   begin
      for J in S'Range loop
         I.Name (J) := S (J);
         exit when J >= I.Name'Last;
      end loop;
   end Set_Name;

   procedure Display_Value (I : Instrument) is
   begin
      New_Line;
      Put (I.Name);
      Put (" : ");
   end Display_Value;


   procedure Display_Value (S : Speedometer) is
   begin
      Display_Value (Instrument (S));
      Put (S.Value);
      Put (" Miles per Hour");
   end Display_Value;

   procedure Display_Value (G : Gauge) is
   begin
      Display_Value (Instrument (G));
      Put (G.Value);
      Put (" %");
   end Display_Value;

   procedure Display_Value (G : Graphic_Gauge) is
      Lg : constant Integer := G.Size * G.Value / 100;
      S1 : constant String (1 .. Lg) := (others => G.Fill);
      S2 : constant String (Lg + 1 .. G.Size) := (others => G.Empty);
   begin
      Display_Value (Instrument (G));
      Put ('<');
      Put (S1);
      Put (S2);
      Put ('>');
   end Display_Value;

   procedure Display_Value (C : Clock) is
   begin
      Display_Value (Instrument (C));
      Put (Character'Val (Character'Pos ('0') + C.Hours / 10));
      Put (Character'Val (Character'Pos ('0') + C.Hours mod 10));
      Put (":");
      Put (Character'Val (Character'Pos ('0') + C.Minutes / 10));
      Put (Character'Val (Character'Pos ('0') + C.Minutes mod 10));
      Put (":");
      Put (Character'Val (Character'Pos ('0') + C.Seconds / 10));
      Put (Character'Val (Character'Pos ('0') + C.Seconds mod 10));
   end Display_Value;

   procedure Increment (C : in out Clock; Inc : Integer := 1) is
      nInc : Integer;

   begin
      C.Seconds := (C.Seconds + Inc) mod 60;
      nInc := (C.Seconds + Inc) / 60;
      C.Minutes := (C.Minutes + nInc) mod 60;
      nInc := (C.Minutes + nInc) / 60;
      C.Hours := (C.Hours + nInc) mod 24;
   end Increment;

   procedure Init
     (C    : in out Clock;
      H    : Twenty_Four := 0;
      M, S : Sixty := 0) is
   begin
      C.Seconds := S;
      C.Minutes := M;
      C.Hours := H;
   end Init;

   procedure Display_Value (C : Chronometer) is
      V : Integer;
   begin
      Display_Value (Instrument (C));

      V :=  C.Seconds + C.Minutes * 60 + C.Hours   * 3600;

      Put ("<<");
      Put (Character'Val (Character'Pos ('0') + (V / 1000) mod 10));
      Put (Character'Val (Character'Pos ('0') + (V / 100) mod 10));
      Put (Character'Val (Character'Pos ('0') + (V / 10) mod 10));
      Put (Character'Val (Character'Pos ('0') + V mod 10));
      Put (">>");
   end Display_Value;

end Instr;
