package RAD.Debug is

   procedure Message (Str : String; Level : Positive := 1);
   --  Output Str if level is lower than the current level of traces

   procedure Set_Level (Level : Positive);
   --  Set the current level of debugging

end RAD.Debug;
