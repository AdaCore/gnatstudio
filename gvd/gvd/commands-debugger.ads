------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2012, AdaCore                     --
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

--  This package contains commands related to the debugger.

with GPS.Kernel; use GPS.Kernel;
with GVD.Process;  use GVD.Process;
with GNATCOLL.VFS;          use GNATCOLL.VFS;

package Commands.Debugger is

   Breakpoints_Column_Id : constant String := "Debugger/Breakpoints";

   type Set_Breakpoint_Command is new Root_Command with private;
   type Set_Breakpoint_Command_Access is access all Set_Breakpoint_Command;

   type Breakpoint_Command_Mode is (Set, Unset);

   procedure Create
     (Item           : out Set_Breakpoint_Command_Access;
      Kernel         : Kernel_Handle;
      Debugger       : Visual_Debugger;
      Mode           : Breakpoint_Command_Mode;
      File           : GNATCOLL.VFS.Virtual_File;
      Line           : Positive);

   overriding function Execute
     (Command : access Set_Breakpoint_Command) return Command_Return_Type;

private

   type Set_Breakpoint_Command is new Root_Command with record
      File     : GNATCOLL.VFS.Virtual_File;
      Kernel   : Kernel_Handle;
      Line     : Positive;
      BMode    : Breakpoint_Command_Mode;
      Debugger : Visual_Debugger;
   end record;

end Commands.Debugger;
