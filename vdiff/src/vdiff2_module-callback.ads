-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2001-2003                    --
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
--  Provide all callback for menu

with Glib;                 use Glib;
with Glib.Object;          use Glib.Object;

with Glide_Kernel;         use Glide_Kernel;
with Glide_Kernel.Scripts; use Glide_Kernel.Scripts;
with Glide_Kernel.Hooks;   use Glide_Kernel.Hooks;

package Vdiff2_Module.Callback is

   function Diff_Hook
     (Kernel : access Kernel_Handle_Record'Class; Data : Hooks_Data'Class)
      return Boolean;
   --  Process, if possible, the data sent by the kernel

   procedure On_Compare_Three_Files
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Callback for Tools->VDiff->Compare Two Files...

   procedure On_Compare_Two_Files
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Callback for Tools->VDiff->Compare Tree Files...

   procedure On_Merge_Three_Files
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Callback for Tools->VDiff->Merge Two Files...

   procedure On_Merge_Two_Files
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Callback for Tools->VDiff->Merge Tree Files...

   procedure File_Closed_Cb
     (Kernel  : access Glide_Kernel.Kernel_Handle_Record'Class;
      Data    : Glide_Kernel.Hooks.Hooks_Data'Class);
   --  Callback for the "file_closed" signal.

   procedure Diff_Command_Handler
     (Data    : in out Callback_Data'Class; Command : String);
   --  Interactive command handler for the Visual_Diff module.

   procedure On_Preferences_Changed
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Called when the preferences have changed

   procedure On_Ref_Change
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);
   --  Change the Ref File for the current diff and reload hightlighting

   procedure On_Hide_Differences
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);
   --  Hide the highlighting and free the difference list

   procedure On_Recalculate
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);
   --  Hide difference ,recalculate the difference
   --  and show the new difference list

   procedure On_Close_Difference
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);
   --  Close all file for current diff

end Vdiff2_Module.Callback;
