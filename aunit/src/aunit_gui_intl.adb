with Gtkada.Intl; use Gtkada.Intl;

package body Aunit_Gui_Intl is

   function "-" (Msg : String) return String is
   begin
      return Dgettext ("Aunit_Gui", Msg);
   end "-";

end Aunit_Gui_Intl;
