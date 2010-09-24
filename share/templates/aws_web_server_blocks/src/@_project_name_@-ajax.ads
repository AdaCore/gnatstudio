
with AWS.Status;
with AWS.Templates;
with AWS.Services.Web_Block.Context;

package @_Project_Name_@.Ajax is

   use AWS;
   use AWS.Services;

   procedure Onclick_Incr
     (Request      : in              Status.Data;
      Context      : not null access Web_Block.Context.Object;
      Translations : in out          Templates.Translate_Set);

end @_Project_Name_@.Ajax;
