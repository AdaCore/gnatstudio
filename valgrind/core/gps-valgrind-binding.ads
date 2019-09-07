------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                       Copyright (C) 2017-2019, AdaCore                   --
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
--  This is Ada implementation of Valgrind client requests

private with Interfaces.C;

package GPS.Valgrind.Binding is

   type Client_Request_Kinds is
     (Callgrind_Dump_Stats,
      Callgrind_Zero_Stats,
      Callgrind_Toggle_Collect,
      Callgrind_Start_Instrumentation,
      Callgrind_Stop_Instrumentation);

   procedure Do_Client_Request (Kind : Client_Request_Kinds);
   --  Send a client request to valgrind

private

   type Word is new Interfaces.C.unsigned with Volatile => True;

   Map : constant array (Client_Request_Kinds) of Word :=
     (Callgrind_Dump_Stats => 16#43540000#,
      Callgrind_Zero_Stats => 16#43540001#,
      Callgrind_Toggle_Collect => 16#43540002#,
      --  Callgrind_Dump_Stats_At => 16#43540003#,
      Callgrind_Start_Instrumentation => 16#43540004#,
      Callgrind_Stop_Instrumentation => 16#43540005#);

end GPS.Valgrind.Binding;
