-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2003                            --
--                            ACT-Europe                             --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Glide_Kernel;

package Aliases_Module is

   procedure Register_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Register the module into the list

   Invalid_Expansion : constant String := "!@#$%";

   type Alias_Expansion_Function is access
     function (Kernel    : access Glide_Kernel.Kernel_Handle_Record'Class;
               Expansion : String;
               Special   : Character) return String;
   --  Function called to test alias expansion.
   --  The context in which the expansion takes place can be found by calling
   --  Glide_Kernel.Get_Current_Focus_Widget.
   --  Special is the special entity seen in the alias expansion, without the
   --  leading '%'.
   --  The expansion of the alias, as computed up to the encounter with the
   --  special character, is given in Expansion. For instance, if the alias is
   --  defined as "foo%o bar", Expansion will be set to "foo" when the
   --  expansion function for 'o' is called.
   --  This function should return the new expansion of the alias (ie Expansion
   --  & the expansion for Special). If Special needs to replace some text in
   --  the alias, it doesn't need to return the whole of Expansion.
   --  If Invalid_Expansion is returned, then this special character will
   --  simply be ignored.

   procedure Register_Special_Alias_Entity
     (Kernel      : access Glide_Kernel.Kernel_Handle_Record'Class;
      Description : String;
      Entity      : Character;
      Func        : Alias_Expansion_Function);
   --  Register a new special entity that can be used in aliases expansion.
   --  Special is the text for that special entity, without the leading '%'.
   --  Func is the function called to get the replacement text for Special.
   --  Description is used in the contextual menu in the aliases editor to
   --  describe this special character.

end Aliases_Module;

