------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2003-2011, AdaCore                     --
--                                                                          --
-- GPS is free software;  you can  redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT; see file COPYING3.  If not, go to --
-- http://www.gnu.org/licenses for a complete copy of the license.          --
--                                                                          --
------------------------------------------------------------------------------

--  Dummy (unix) implementation

package body DDE is

   -------------------------
   -- Register_DDE_Server --
   -------------------------

   procedure Register_DDE_Server (Kernel : GPS.Kernel.Kernel_Handle) is
      pragma Unreferenced (Kernel);
   begin
      null;
   end Register_DDE_Server;

   ---------------------------
   -- Unregister_DDE_Server --
   ---------------------------

   procedure Unregister_DDE_Server is
   begin
      null;
   end Unregister_DDE_Server;

end DDE;
