with Gtkada.Intl; use Gtkada.Intl;

package body Files_Extra_Intl is

   function "-" (Msg : String) return String is
   begin
      return Dgettext ("Files_Extra", Msg);
   end "-";

end Files_Extra_Intl;
