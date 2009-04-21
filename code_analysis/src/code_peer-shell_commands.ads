-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2009, AdaCore                   --
--                                                                   --
-- GPS is Free  software;  you can redistribute it and/or modify  it --
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
with GNATCOLL.VFS;
with GPS.Kernel;

package Code_Peer.Shell_Commands is

   function Build_Target
     (Kernel : GPS.Kernel.Kernel_Handle;
      Name   : String)
      return String;
   --  Creates BuildTarget and returns it.

   procedure Build_Target_Execute
     (Kernel      : GPS.Kernel.Kernel_Handle;
      Object      : String;
      Main_Name   : String                    := "";
      File        : GNATCOLL.VFS.Virtual_File := GNATCOLL.VFS.No_File;
      Force       : Boolean                   := False;
      Extra_Args  : String                    := "";
      Build_Mode  : String                    := "";
      Synchronous : Boolean                   := True);
   --  Executes BuildTarget.execute function.

   function Get_Build_Mode (Kernel : GPS.Kernel.Kernel_Handle) return String;
   --  Executes get_build_mode function.

   procedure Set_Build_Mode
     (Kernel : GPS.Kernel.Kernel_Handle;
      Mode   : String);
   --  Executes set_build_mode function.

end Code_Peer.Shell_Commands;
