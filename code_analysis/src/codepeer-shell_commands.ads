------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2009-2019, AdaCore                     --
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

with GNATCOLL.VFS;
with GPS.Kernel;

package CodePeer.Shell_Commands is

   function Build_Target
     (Kernel : GPS.Kernel.Kernel_Handle;
      Name   : String)
      return String;
   --  Creates BuildTarget and returns it.

   procedure Build_Target_Execute
     (Kernel      : GPS.Kernel.Kernel_Handle;
      Target_ID   : String;
      Main_Name   : String                    := "";
      File        : GNATCOLL.VFS.Virtual_File := GNATCOLL.VFS.No_File;
      Force       : Boolean                   := False;
      Extra_Args  : String                    := "";
      Build_Mode  : String                    := "";
      Synchronous : Boolean                   := True;
      Dir         : GNATCOLL.VFS.Virtual_File := GNATCOLL.VFS.No_File);
   --  Executes BuildTarget.execute function.

end CodePeer.Shell_Commands;
