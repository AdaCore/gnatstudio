with Gtkada.Intl; use Gtkada.Intl;

package body Vdiff_Intl is

   function "-" (Msg : String) return String is
   begin
      return Dgettext ("Vdiff", Msg);
   end "-";

end Vdiff_Intl;
