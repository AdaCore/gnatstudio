with Gtkada.Intl; use Gtkada.Intl;

package body Prj_Edit_Intl is

   function "-" (Msg : String) return String is
   begin
      return Dgettext ("Prj_Edit", Msg);
   end "-";

end Prj_Edit_Intl;
