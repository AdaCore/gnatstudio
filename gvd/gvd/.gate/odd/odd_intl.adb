with Gtkada.Intl; use Gtkada.Intl;

package body Odd_Intl is

   function "-" (Msg : String) return String is
   begin
      return Dgettext ("Odd", Msg);
   end "-";

end Odd_Intl;
