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

--  This is an implementation of generic timer based on
--  Ada.Real_Time.Timing_Events.
--
--  See https://www.adacore.com/gems/ada-gem-15 for more info.

with Ada.Real_Time.Timing_Events;

generic
   One_Shot   : Boolean := True;
   Timer_Name : String := "Generic_Timers";
   Period     : in Ada.Real_Time.Time_Span;
   with procedure Action is <>;

package Generic_Timers is

   Timer_Error : exception;

   procedure Start;
   procedure Stop;
   procedure Cancel;

private

   The_Event : Ada.Real_Time.Timing_Events.Timing_Event;

end Generic_Timers;
