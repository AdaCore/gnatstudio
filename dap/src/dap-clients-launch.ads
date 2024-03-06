------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2023, AdaCore                       --
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

--  Concrete implementation of the DAP 'launch' request

with GNATCOLL.VFS;

package DAP.Clients.Launch is

   procedure Send_Launch_Request
     (Client            : in out DAP.Clients.DAP_Client'Class;
      Executable        : GNATCOLL.VFS.Virtual_File;
      Executable_Args   : VSS.String_Vectors.Virtual_String_Vector;
      Stop_At_Beginning : Boolean := False);
   --  Send the DAP 'launch' request.
   --  Executable refers to the debuggee that should be launched by the
   --  debugger.
   --  Executable_Args are the arguments passed to the launched debuggee.
   --  When Stop_At_Beginning is True, the program will be stopped at the
   --  beginning of the main.

end DAP.Clients.Launch;
