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
     function (Data    : Glide_Kernel.Event_Data;
               Special : Character) return String;
   --  Function called to test alias expansion.
   --  It is passed the current context in which the expansion takes module,
   --  and modules are encouraged to do nothing if the context is not thr right
   --  one. For instance, the "current file" special entity only makes sense in
   --  the source editor, and the src_editor module should do nothing if it is
   --  called but Get_Widget (Data) is not a source editor.
   --  Special is the special entity seen in the alias expansion, without the
   --  leading '%'.
   --  This function should return the replacement text for Special, and should
   --  return the Invalid_Expansion if it couldn't expand Special because the
   --  context is incorrect.

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

