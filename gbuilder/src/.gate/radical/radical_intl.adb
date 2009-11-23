with Gtkada.Intl; use Gtkada.Intl;

package body Radical_Intl is

   function "-" (Msg : String) return String is
   begin
      return Dgettext ("Radical", Msg);
   end "-";

end Radical_Intl;
