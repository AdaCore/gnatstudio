with Config;
with Gtk.Accel_Group;

package body Darwin_Extras is

   --------------------------
   -- Get_Default_Mod_Mask --
   --------------------------

   function Get_Default_Mod_Mask return Gdk_Modifier_Type is
   --  macOS (at least 12.0) fires MOD2 and META modifiers
   --  as we want only META we suppress MOD2 to the default mask
   begin
      if Config.Darwin_Target then
         return Gtk.Accel_Group.Get_Default_Mod_Mask and not Mod2_Mask;
      else
         return Gtk.Accel_Group.Get_Default_Mod_Mask;
      end if;
   end Get_Default_Mod_Mask;

end Darwin_Extras;
