with Gtkada.Intl; use Gtkada.Intl;

package body Switches_Glade_Intl is

   function "-" (Msg : String) return String is
   begin
      return Dgettext ("Switches_Glade", Msg);
   end "-";

end Switches_Glade_Intl;
