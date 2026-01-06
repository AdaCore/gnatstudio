------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2022-2026, AdaCore                  --
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

--  Concrete implementation of the DAP 'attach' request

with VSS.Strings;
with GNATCOLL.VFS;

package DAP.Clients.Attach is

   procedure Send_Attach_Request
     (Client     : in out DAP.Clients.DAP_Client'Class;
      PID        : Integer := -1;
      Executable : GNATCOLL.VFS.Virtual_File := GNATCOLL.VFS.No_File;
      Target     : VSS.Strings.Virtual_String :=
        VSS.Strings.Empty_Virtual_String);
   --  Send a DAP 'attach' request.
   --  PID refers to the process we want to attach to.
   --  Executable refers to the debuggee that should be loaded by the
   --  debugger, when it can't guess which program is being debugged after
   --  attaching. This should be specified for remote debugging for instance.
   --  Target refers to the remote target we want to connect.
   --  Note that PID and Target are mutually exclusive: specifying one
   --  parameter will make the underlying DAP adapter ignore the other.

end DAP.Clients.Attach;
