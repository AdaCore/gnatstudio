with Gtkada.Intl; use Gtkada.Intl;

package body Glide_Intl is

   function "-" (Msg : String) return String is
   begin
      return Dgettext ("Glide", Msg);
   end "-";

end Glide_Intl;
