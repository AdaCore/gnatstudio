
with AWS.Utils;

with WBlocks.Widget_Counter;

package body @_Project_Name_@.Ajax is

   use AWS;
   use AWS.Services;

   ------------------
   -- Onclick_Incr --
   ------------------

   procedure Onclick_Incr
     (Request      : in              Status.Data;
      Context      : not null access Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set)
   is
      N : Natural := 0;
   begin
      if Context.Exist ("N") then
         N := Natural'Value (Context.Get_Value ("N"));
      end if;

      N := N + 1;

      Context.Set_Value ("N", Utils.Image (N));

      Templates.Insert
        (Translations, Templates.Assoc (WBlocks.Widget_Counter.COUNTER, N));
   end Onclick_Incr;

end @_Project_Name_@.Ajax;
