package Pack is

   procedure Obs (I, J, K, L : Integer);
   pragma Obsolescent (Obs, "Use Real");

   procedure Real (I, J, K : Integer);

end Pack;
