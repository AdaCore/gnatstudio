with Gtkada.Intl; use Gtkada.Intl;

package body Aunit_Make_Suite_Intl is

   function "-" (Msg : String) return String is
   begin
      return Dgettext ("Aunit_Make_Suite", Msg);
   end "-";

end Aunit_Make_Suite_Intl;
