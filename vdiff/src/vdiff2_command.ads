------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2018, AdaCore                     --
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

--  This Package provide the base structure for all command for Vdiff2

with Commands;             use Commands;
with Commands.Interactive; use Commands.Interactive;
with Diff_Utils2;          use Diff_Utils2;
with GPS.Kernel;           use GPS.Kernel;

package Vdiff2_Command is

   type Diff_Command is abstract new Interactive_Command with record
      Kernel    : Kernel_Handle;
      List_Diff : Diff_Head_List_Access;
   end record;

   type Diff_Command_Access is access all Diff_Command'Class;

end Vdiff2_Command;
