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

--  This Package provide the structure for all command for Vdiff2

with Glide_Kernel;         use Glide_Kernel;
with Commands;             use Commands;
with Commands.Interactive; use Commands.Interactive;

with Diff_Utils2;          use Diff_Utils2;
with Gdk.Event;

package Vdiff2_Command is

   type Handler_Action is access procedure
     (Kernel : Kernel_Handle;
      Diff   : in out Diff_Head);
      --  Is an access for the action executed by an Diff_Command

   type Diff_Command is new Interactive_Command with record
      Kernel    : Kernel_Handle;
      List_Diff : Diff_Head_List_Access;
      Action    : Handler_Action;
   end record;

   type Diff_Command_Access is access all Diff_Command;

   procedure Create
     (Item      : out Diff_Command_Access;
      Kernel    : Kernel_Handle;
      List_Diff : Diff_Head_List_Access;
      Action    : Handler_Action);
   --  Create a command with all information needed by this

   procedure Unchecked_Execute
     (Command : access Diff_Command;
      Diff    : in out Diff_Head_List.List_Node);
   --  Execute the command Command
   --  without control of existence in diff list

   function Execute
     (Command : access Diff_Command;
      Event   : Gdk.Event.Gdk_Event) return Command_Return_Type;

   function Execute
     (Command : access Diff_Command) return Command_Return_Type;
   --  Execute the command Command
   --  Search in the global List of Diff the current diff end apply Action on
   --  this

   procedure Next_Difference
     (Kernel : Kernel_Handle;
      Diff   : in out Diff_Head);
   --  Goto Next difference

   procedure Prev_Difference
     (Kernel : Kernel_Handle;
      Diff   : in out Diff_Head);
   --  Goto prev difference

   procedure First_Difference
     (Kernel : Kernel_Handle;
      Diff   : in out Diff_Head);
   --  Goto First difference

   procedure Last_Difference
     (Kernel : Kernel_Handle;
      Diff   : in out Diff_Head);
   --  Goto Last difference

   procedure Reload_Difference
     (Kernel : Kernel_Handle;
      Item   : in out Diff_Head);
   --  Remove the highlighting, recalculate differences and show difference

   procedure Remove_Difference
     (Kernel : Kernel_Handle;
      Diff   : in out Diff_Head);
   --  Remove the highlighting, and free the memory associated with Diff_list

   procedure Close_Difference
     (Kernel : Kernel_Handle;
      Diff   : in out Diff_Head);
   --  Close the current difference

   procedure Unhighlight_Difference
     (Kernel : Kernel_Handle;
      Diff   : in out Diff_Head);
   --  Remove the highlighting from all file of diff

   procedure Change_Ref_File
     (Kernel : Kernel_Handle;
      Diff   : in out Diff_Head);

end Vdiff2_Command;


