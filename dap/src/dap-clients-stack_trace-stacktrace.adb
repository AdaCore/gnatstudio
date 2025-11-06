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

with GPS.Kernel;

package body DAP.Clients.Stack_Trace.StackTrace is

   -------------
   -- Create --
   -------------

   function Create
     (Client : access DAP.Clients.DAP_Client'Class;
      From   : Integer := 0;
      Limit  : Integer := 0)
      return StackTrace_Request_Access
   is
      Self : constant StackTrace_Request_Access :=
        new StackTrace_Request
          (Kernel    => GPS.Kernel.Kernel_Handle (Client.Kernel),
           Callbacks => Client.Callbacks);
   begin
      Self.Parameters.arguments.threadId := Client.Get_Current_Thread;

      if Limit /= 0 then
         Self.Parameters.arguments.startFrame := (True, From);
         Self.Parameters.arguments.levels     := (True, Limit);
      end if;

      return Self;
   end Create;

end DAP.Clients.Stack_Trace.StackTrace;
