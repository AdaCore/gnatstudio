with Gtkada.Intl; use Gtkada.Intl;

package body Codefix_Interface_Intl is

   function "-" (Msg : String) return String is
   begin
      return Dgettext ("Codefix_Interface", Msg);
   end "-";

end Codefix_Interface_Intl;
