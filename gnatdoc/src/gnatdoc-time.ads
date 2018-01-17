------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2007-2018, AdaCore                     --
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

--  This package provides internal support to evaluate the time consumed by the
--  components of GNATdoc. Used to evaluate the performance of GNATdoc and
--  help identifying what components must be optimized.

with Ada.Calendar;

private package GNATdoc.Time is

   --  Frontend time
   Build_Tree_Time            : Duration;
   GetDoc_Time                : Duration;
   Build_Comments_Time        : Duration;
   Frontend_Time              : Duration;

   --  Backend time
   Generate_Doc_Time          : Duration;
   Generate_Global_Index_Time : Duration;

   procedure Reset;
   --  Reset all the accumulated timers

   ----------------
   -- Delay_Time --
   ----------------

   type Delay_Time is private;

   procedure Start (D : in out Delay_Time);
   procedure Stop  (D : in out Delay_Time; Accum : in out Duration);
   --  Compute the number of seconds since the call to Start and accummulates
   --  the results in Accum.

   procedure Print_Time (Context : access constant Docgen_Context);
   --  Print a report in the directory where the documentation is generated.

private

   type Delay_Time is record
      T1 : Ada.Calendar.Time;
      T2 : Ada.Calendar.Time;
   end record;

end GNATdoc.Time;
