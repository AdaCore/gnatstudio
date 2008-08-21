-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                    Copyright (C) 2001-2008, AdaCore               --
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

--  This Package provide the structure for all the Vdiff2 commands

with Commands;             use Commands;
with Commands.Interactive; use Commands.Interactive;
with Diff_Utils2;          use Diff_Utils2;
with GPS.Kernel;           use GPS.Kernel;
with Vdiff2_Command;       use Vdiff2_Command;

package Vdiff2_Command_Block is

   type Handler_Action is access procedure
     (Kernel : Kernel_Handle;
      Diff   : access Diff_Head);
   --  Is an access for the action executed by an Diff_Command_block

   type Diff_Command_Block is new Diff_Command with record
      Action           : Handler_Action;
      Last_Active_Diff : Diff_Head_Access := null;
   end record;
   type Diff_Command_Access is access all Diff_Command_Block'Class;

   procedure Create
     (Item      : out Diff_Command_Access;
      Kernel    : Kernel_Handle;
      List_Diff : Diff_Head_List_Access;
      Action    : Handler_Action);
   --  Create a command with all required information

   procedure Unchecked_Execute
     (Command : access Diff_Command_Block;
      Diff    : access Diff_Head);
   --  Execute the command Command without control of existence in diff list

   overriding function Execute
     (Command : access Diff_Command_Block;
      Context : Interactive_Command_Context) return Command_Return_Type;

   overriding function Execute
     (Command : access Diff_Command_Block) return Command_Return_Type;
   --  Execute the command Command
   --  Search in the global List of Diff the current diff and apply Action on
   --  it

   procedure Reload_Difference
     (Kernel : Kernel_Handle;
      Item   : access Diff_Head);
   --  Remove the highlighting, recalculate differences and show difference

   procedure Remove_Difference
     (Kernel : Kernel_Handle;
      Diff   : access Diff_Head);
   --  Remove the highlighting, and free the memory associated with Diff_list

   procedure Close_Difference
     (Kernel : Kernel_Handle;
      Diff   : access Diff_Head);
   --  Close the current difference

   procedure Unhighlight_Difference
     (Kernel : Kernel_Handle;
      Diff   : access Diff_Head);
   --  Remove the highlighting from all file of diff

   procedure Change_Ref_File
     (Kernel : Kernel_Handle;
      Diff   : access Diff_Head);

end Vdiff2_Command_Block;
