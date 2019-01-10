------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2000-2019, AdaCore                     --
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

with GPS.Kernel; use GPS.Kernel;
with GVD.Process;  use GVD.Process;

package GVD.Trace is

   procedure Output_Error
     (Kernel : access Kernel_Handle_Record'Class;
      Str    : String);
   --  Output an error through GPS.Kernel.Console

   procedure Set_Input_Output_Filter
     (Process : not null access Visual_Debugger_Record'Class);
   --  Set filters to log input and output of the debugger

end GVD.Trace;
