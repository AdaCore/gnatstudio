
with WBlocks.Widget_Counter;

package body @_Project_Name_@.Web_Blocks is

   --------------------
   -- Widget_Counter --
   --------------------

   procedure Widget_Counter
     (Request      : in              Status.Data;
      Context      : not null access Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set)
   is
      N : Natural := 0;
   begin
      if Context.Exist ("N") then
         N := Natural'Value (Context.Get_Value ("N"));
      end if;

      Templates.Insert
        (Translations, Templates.Assoc (WBlocks.Widget_Counter.COUNTER, N));
   end Widget_Counter;

end @_Project_Name_@.Web_Blocks;
