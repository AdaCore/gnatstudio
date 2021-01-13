------------------------------------------------------------------------------
--                     Copyright (C) 2018-2021, AdaCore                     --
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

with System;

package body Generic_Timers is

   use Ada;
   use type Ada.Real_Time.Time;

   protected Events is
      pragma Priority (System.Any_Priority'Last);

      procedure Handler (Event : in out Real_Time.Timing_Events.Timing_Event);
   end Events;

   ------------
   -- Events --
   ------------

   protected body Events is

      -------------
      -- Handler --
      -------------

      procedure Handler
        (Event : in out Real_Time.Timing_Events.Timing_Event) is
      begin
         Action;
         if not One_Shot then
            Start;  -- periodic timer continues
         end if;
      end Handler;
   end Events;

   -----------
   -- Start --
   -----------

   procedure Start is
      use type Ada.Real_Time.Timing_Events.Timing_Event_Handler;
   begin
      if Real_Time.Timing_Events.Current_Handler (The_Event) = null then
         Real_Time.Timing_Events.Set_Handler
           (The_Event, Ada.Real_Time.Clock + Period, Events.Handler'Access);
      else
         raise Timer_Error with Timer_Name & " started already";
      end if;
   end Start;

   ----------
   -- Stop --
   ----------

   procedure Stop is
      Success : Boolean := False;
      use type Ada.Real_Time.Timing_Events.Timing_Event_Handler;
   begin
      if Real_Time.Timing_Events.Current_Handler (The_Event) /= null then
         Real_Time.Timing_Events.Cancel_Handler (The_Event, Success);
         if not Success then
            raise Timer_Error with "fails to cancel " & Timer_Name;
         end if;
      end if;
   end Stop;

   ------------
   -- Cancel --
   ------------

   procedure Cancel renames Stop;

end Generic_Timers;
