------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2022-2023, AdaCore                  --
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

with GPS.Kernel;                 use GPS.Kernel;
with DAP.Clients;                use DAP.Clients;
with Interactive_Consoles;       use Interactive_Consoles;

package DAP.Consoles is

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register menus and other functions to support the consoles

   procedure Attach_To_Debugger_Console
     (Client              : access DAP_Client'Class;
      Kernel              : not null access Kernel_Handle_Record'Class;
      Create_If_Necessary : Boolean);
   --  Attach debugger to a console
   --  If an unattached console exists in the desktop, it is reused.
   --  If none exists, one is created if Create_If_Necessary is true.
   --  Nothing is done when Debugger is already attached to a console.

   function Get_Debugger_Interactive_Console
     (Client : DAP_Client)
      return access Interactive_Console_Record'Class;
   --  Return the interactive console associated with the given debugger.
   --  If no interactive console is associated with this debugger, return null.

end DAP.Consoles;
