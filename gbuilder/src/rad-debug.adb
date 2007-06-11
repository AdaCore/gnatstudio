with GNAT.IO; use GNAT.IO;

package body RAD.Debug is

   Current_Level : Positive := 1;
   --  Current level of output for debugging
   --  1 is the lowest level of debugging

   -------------
   -- Message --
   -------------

   procedure Message (Str : String; Level : Positive := 1) is
   begin
      if Level <= Current_Level then
         Put_Line (Str);
      end if;
   end Message;

   ---------------
   -- Set_Level --
   ---------------

   procedure Set_Level (Level : Positive) is
   begin
      Current_Level := Level;
   end Set_Level;

end RAD.Debug;

