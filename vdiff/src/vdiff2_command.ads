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

--  This Package provide the base structure for all command for Vdiff2

with Glide_Kernel;         use Glide_Kernel;
with Commands;             use Commands;
with Commands.Interactive; use Commands.Interactive;
with Diff_Utils2;          use Diff_Utils2;

package Vdiff2_Command is

   type Diff_Command is abstract new Interactive_Command with record
      Kernel           : Kernel_Handle;
      List_Diff        : Diff_Head_List_Access;
   end record;

   type Diff_Command_Access is access all Diff_Command'Class;

end Vdiff2_Command;
