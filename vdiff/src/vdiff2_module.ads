------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2019, AdaCore                     --
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

--  New Visual Diff module

with Default_Preferences;
with Diff_Utils2;              use Diff_Utils2;
with GPS.Kernel.Modules;       use GPS.Kernel;

package Vdiff2_Module is

   type VDiff2_Module is private;

   Vdiff_Module_ID        : GPS.Kernel.Modules.Module_ID;
   Vdiff_Module_Name      : constant String := "Visual_Diff2";

   Diff3_Cmd              : Default_Preferences.String_Preference;
   Diff_Default_Color     : Default_Preferences.Color_Preference;
   Diff_Append_Color      : Default_Preferences.Color_Preference;
   Diff_Remove_Color      : Default_Preferences.Color_Preference;
   Diff_Change_Color      : Default_Preferences.Color_Preference;
   Diff_Fine_Change_Color : Default_Preferences.Color_Preference;

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register the module in the list

private

   type VDiff2_Module_Record is new GPS.Kernel.Modules.Module_ID_Record with
      record
         Number_active    : Natural := 0;
         List_Diff        : Diff_Head_List_Access;
         Enable_Fine_Diff : Boolean := True;
      end record;

   type VDiff2_Module is access all VDiff2_Module_Record'Class;
   overriding procedure Destroy (Id : in out VDiff2_Module_Record);

end Vdiff2_Module;
