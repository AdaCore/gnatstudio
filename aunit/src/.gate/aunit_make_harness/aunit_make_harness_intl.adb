with Gtkada.Intl; use Gtkada.Intl;

package body Aunit_Make_Harness_Intl is

   function "-" (Msg : String) return String is
   begin
      return Dgettext ("Aunit_Make_Harness", Msg);
   end "-";

end Aunit_Make_Harness_Intl;
