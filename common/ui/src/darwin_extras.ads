with Gdk.Types; use Gdk.Types;

--  Patch adapatation for macOS (at least 12.0)

package Darwin_Extras is

   function Get_Default_Mod_Mask return Gdk_Modifier_Type;

end Darwin_Extras;
