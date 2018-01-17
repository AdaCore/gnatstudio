------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2005-2018, AdaCore                     --
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

package Completion_Module is

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register this module.
   --  Note this isn't a real module, and therefore shouldn't be register by
   --  gps-main.adb, only by the source editor itself. There are just too
   --  many links with the rest of the source editor.

   procedure Reset_Completion_Data;
   --  Reset the current completion data. It should be called only when
   --  the user performs a completion operation.

   procedure Remove_Completion;
   --  Remove the completion window

   function In_Smart_Completion return Boolean;
   --  Return True if we are currently showing a smart completion window

end Completion_Module;
