------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2025, AdaCore                       --
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

package body GPS.LSP_Client.Callbacks is

   ---------------------
   -- Schedule_Timer --
   ---------------------

   overriding procedure Schedule_Timer
     (Self     : Null_Callback;
      Interval : Natural;
      Callback : Timer_Callback;
      Timer    : out Timer_Id)
   is
      pragma Unreferenced (Self, Interval, Callback);
   begin
      Timer := No_Timer;
      --  Null implementation: timers are disabled
      --  Server auto-restart will not work, but manual restart is fine
   end Schedule_Timer;

   -------------------
   -- Cancel_Timer --
   -------------------

   overriding procedure Cancel_Timer
     (Self  : Null_Callback;
      Timer : in out Timer_Id)
   is
      pragma Unreferenced (Self);
   begin
      Timer := No_Timer;
      --  Null implementation: nothing to cancel
   end Cancel_Timer;

end GPS.LSP_Client.Callbacks;
