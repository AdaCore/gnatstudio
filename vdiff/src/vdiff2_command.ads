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
with Glide_Kernel; use Glide_Kernel;
with Commands;     use Commands;
with Diff_Utils;   use Diff_Utils;


package Vdiff2_Command is

   type Handler_Action is access procedure
     (Kernel : Kernel_Handle;
      Diff : in out Diff_Head_Access);

   type Diff_Command is new Root_Command with record
      Kernel    : Kernel_Handle;
      List_Diff : Diff_Occurrence_List_Access;
      Action    : Handler_Action;
   end record;
   type Diff_Command_Access is access all Diff_Command;

   procedure Create
     (Item      : out Diff_Command_Access;
      Kernel    : Kernel_Handle;
      List_Diff : Diff_Occurrence_List_Access;
      Action    : Handler_Action);

   function Execute (Command : access Diff_Command)
                     return Command_Return_Type;

   procedure Next_Difference (Kernel : Kernel_Handle;
                             Diff   : in out  Diff_Head_Access);

   procedure Prev_Difference (Kernel : Kernel_Handle;
                             Diff   : in out Diff_Head_Access);

   procedure First_Difference (Kernel : Kernel_Handle;
                              Diff   : in out Diff_Head_Access);

   procedure Last_Difference (Kernel : Kernel_Handle;
                              Diff   : in out Diff_Head_Access);
   procedure Reload_Difference (Kernel : Kernel_Handle;
                              Diff   : in out Diff_Head_Access);
   procedure Close_Difference (Kernel : Kernel_Handle;
                               Diff   : in out Diff_Head_Access);
   procedure Unhighlight_Difference (Kernel : Kernel_Handle;
                              Diff   : in out Diff_Head_Access);
end Vdiff2_Command;
