------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2003-2016, AdaCore                     --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

with Ada.Strings.Unbounded;

with Glib;                  use Glib;

with GPS.Kernel;

package Aliases_Module is
   package SU renames Ada.Strings.Unbounded;

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register the module into the list

   Invalid_Expansion : constant String := "!@#$%";

   type Alias_Expansion_Function is access
     function (Kernel    : access GPS.Kernel.Kernel_Handle_Record'Class;
               Expansion : String;
               Special   : Character) return String;
   --  Function called to test alias expansion.
   --  The context in which the expansion takes place can be found by calling
   --  GPS.Kernel.Get_Current_Focus_Widget.
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
     (Kernel      : access GPS.Kernel.Kernel_Handle_Record'Class;
      Description : String;
      Entity      : Character;
      Func        : Alias_Expansion_Function);
   --  Register a new special entity that can be used in aliases expansion.
   --  Special is the text for that special entity, without the leading '%'.
   --  Func is the function called to get the replacement text for Special.
   --  Description is used in the contextual menu in the aliases editor to
   --  describe this special character.

   function Expand_Alias
     (Kernel        : access GPS.Kernel.Kernel_Handle_Record'Class;
      Name          : String;
      Cursor        : access Integer;
      Must_Reindent : access Boolean;
      Offset_Column : Gint)
      return String;
   --  Return the expanded version of Name.
   --  Cursor is the index in the returned string for the cursor position.
   --  The empty string is returned if there is no such alias.
   --  Must_Reindent is set to True if the editor should be reindented after
   --  insertion.

   type Alias_Info is record
      Name : SU.Unbounded_String;
      Expansion : SU.Unbounded_String;
   end record;
   No_Alias_Info : constant Alias_Info := (SU.Null_Unbounded_String,
                                           SU.Null_Unbounded_String);

   type Alias_Info_List is array (Positive range <>) of Alias_Info;

   function Get_Aliases_List return Alias_Info_List;
   function Get_Alias (Name : SU.Unbounded_String) return Alias_Info;

end Aliases_Module;
