-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2008, AdaCore              --
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
   Diff_Old_Color         : Default_Preferences.Color_Preference;
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
   procedure Destroy (Id : in out VDiff2_Module_Record);

end Vdiff2_Module;
