------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2012, AdaCore                          --
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

with Ada.Calendar.Time_Zones;  use Ada.Calendar.Time_Zones;

package body Time_Utils is

   TZ : constant Time_Offset := UTC_Time_Offset;
   --  Time zone cache, assuming that the OS will not change time zones while
   --  this partition is running.

   -----------------
   -- Local_Split --
   -----------------

   procedure Local_Split
     (Date    : Time;
      Year    : out Year_Number;
      Month   : out Month_Number;
      Day     : out Day_Number;
      Seconds : out Day_Duration)
   is
      Ls : Boolean;
   begin
      Ada.Calendar.Formatting.Split (Date        => Date,
                                     Year        => Year,
                                     Month       => Month,
                                     Day         => Day,
                                     Seconds     => Seconds,
                                     Leap_Second => Ls,
                                     Time_Zone   => TZ);
   end Local_Split;

   procedure Local_Split
     (Date       : Time;
      Year       : out Year_Number;
      Month      : out Month_Number;
      Day        : out Day_Number;
      Hour       : out Hour_Number;
      Minute     : out Minute_Number;
      Second     : out Second_Number;
      Sub_Second : out Second_Duration)
   is
      Ls : Boolean;
   begin
      Ada.Calendar.Formatting.Split (Date        => Date,
                                     Year        => Year,
                                     Month       => Month,
                                     Day         => Day,
                                     Hour        => Hour,
                                     Minute      => Minute,
                                     Second      => Second,
                                     Sub_Second  => Sub_Second,
                                     Leap_Second => Ls,
                                     Time_Zone   => TZ);
   end Local_Split;

end Time_Utils;
