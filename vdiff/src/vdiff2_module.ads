-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2003                       --
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

--  New Visual Diff module.

with Glide_Kernel;             use Glide_Kernel;
with Glib.Properties.Creation; use Glib.Properties.Creation;
with Default_Preferences;      use Default_Preferences;
with Diff_Utils2;              use Diff_Utils2;
with Vdiff2_Command;           use Vdiff2_Command;
with Gtk.Handlers;             use Gtk.Handlers;
with Glib;                     use Glib;

package Vdiff2_Module is


   type VDiff2_Module_Record is private;
   Vdiff_Module_ID        : Glide_Kernel.Module_ID;
   Vdiff_Module_Name  : constant String := "Visual_Diff2";
   Diff3_Cmd                : Param_Spec_String;

   Diff_Default_Color : Param_Spec_Color;
   Diff_Old_Color     : Param_Spec_Color;
   Diff_Append_Color  : Param_Spec_Color;
   Diff_Remove_Color  : Param_Spec_Color;
   Diff_Change_Color  : Param_Spec_Color;

   procedure Register_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Register the module in the list

private

   No_Handler : constant Handler_Id := (Null_Signal_Id, null);

   type VDiff2_Module_Record is new Module_ID_Record with record
      Kernel              : Kernel_Handle;
      Is_Active           : Boolean := False;
      Number_active       : Natural := 0;
      List_Diff           : Diff_Head_List_Access;
      Command_Prev        : Diff_Command_Access;
      Command_Next        : Diff_Command_Access;
      Command_First       : Diff_Command_Access;
      Command_Last        : Diff_Command_Access;
      File_Closed_Id      : Handler_Id := No_Handler;
   end record;
   type VDiff2_Module is access all VDiff2_Module_Record'Class;
   procedure Destroy (Id : in out VDiff2_Module_Record);

end Vdiff2_Module;
