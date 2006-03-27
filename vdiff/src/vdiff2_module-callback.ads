-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2001-2006                    --
--                                AdaCore                            --
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

with Glib.Object;          use Glib.Object;

with Commands.Interactive; use Commands, Commands.Interactive;
with GPS.Kernel.Hooks;     use GPS.Kernel.Hooks;

package Vdiff2_Module.Callback is

   function Diff_Hook
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class)
      return Boolean;
   --  Process, if possible, the data sent by the kernel

   procedure On_Compare_Three_Files
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Callback for Tools->VDiff->Compare Three Files...

   procedure On_Compare_Two_Files
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Callback for Tools->VDiff->Compare Two Files...

   procedure On_Merge_Three_Files
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Callback for Tools->VDiff->Merge Three Files...

   procedure On_Merge_Two_Files
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Callback for Tools->VDiff->Merge Two Files...

   procedure File_Closed_Cb
     (Kernel  : access GPS.Kernel.Kernel_Handle_Record'Class;
      Data    : access GPS.Kernel.Hooks.Hooks_Data'Class);
   --  Callback for the "file_closed" signal

   procedure On_Preferences_Changed
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Called when the preferences have changed

   type Change_Ref_File_Command is new Interactive_Command with null record;
   function Execute
     (Command : access Change_Ref_File_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Change the Ref File for the current diff and reload hightlighting

   type Hide_Difference_Command is new Interactive_Command with null record;
   function Execute
     (Command : access Hide_Difference_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Hide the highlighting and free the difference list

   type Recompute_Diff_Command is new Interactive_Command with null record;
   function Execute
     (Command : access Recompute_Diff_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Hide difference ,recalculate the difference
   --  and show the new difference list

   type Close_Difference_Command is new Interactive_Command with null record;
   function Execute
     (Command : access Close_Difference_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Close all file for current diff

end Vdiff2_Module.Callback;
