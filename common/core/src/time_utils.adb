------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2012-2018, AdaCore                     --
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
with GNATCOLL.Utils;           use GNATCOLL.Utils;
with GNAT.Calendar.Time_IO;    use GNAT.Calendar.Time_IO;

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

   ---------------------------
   -- Local_Timestamp_Image --
   ---------------------------

   function Local_Timestamp_Image (T : Ada.Calendar.Time) return String is
      Y : Year_Number;
      M : Month_Number;
      D : Day_Number;
      H : Hour_Number;
      Mi : Minute_Number;
      S : Second_Number;
      Ss : Second_Duration;
   begin
      Local_Split (T, Y, M, D, H, Mi, S, Ss);
      return Image (Y, 4) & Image (M, 2) & Image (D, 2) & "-" &
        Image (H, 2) & ":" & Image (Mi, 2) & ":" & Image (S, 2);
   end Local_Timestamp_Image;

   -------------
   -- Elapsed --
   -------------

   function Elapsed
     (Start_Time : Ada.Calendar.Time;
      End_Time   : Ada.Calendar.Time) return String
   is
      In_Day : Duration := End_Time - Start_Time;
      Days   : Integer := 0;
   begin
      if In_Day >= 86_400.0 then
         Days := Integer (Float'Floor (Float (In_Day / 86_400.0)));
         In_Day := End_Time - Start_Time - 86_400.0 * Days;
      end if;

      declare
         Elapsed : constant String := Ada.Calendar.Formatting.Image
           (In_Day, Include_Time_Fraction => True);
         Elapsed_Start : Natural := Elapsed'First;
      begin
         --  Do not show hours and minutes if they are 0. The output is
         --  thus similar to the one of the Unix command time

         if Elapsed (Elapsed_Start .. Elapsed_Start + 1) = "00" then
            Elapsed_Start := Elapsed_Start + 3;
         end if;

         if Elapsed (Elapsed_Start .. Elapsed_Start + 1) = "00" then
            Elapsed_Start := Elapsed_Start + 3;
         end if;

         return (if Days > 0 then Image (Days, 1) & " days " else "")
           & Elapsed (Elapsed_Start .. Elapsed'Last);
      end;
   end Elapsed;

   ---------------
   -- Timestamp --
   ---------------

   function Timestamp (T : Ada.Calendar.Time) return String is
   begin
      return "[" & Image (T, ISO_Date & " %T") & "] ";
   end Timestamp;

end Time_Utils;
