-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2003                         --
--                            ACT-Europe                             --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

--  Dummy (unix) implementation

package body DDE is

   -------------------------
   -- Register_DDE_Server --
   -------------------------

   procedure Register_DDE_Server (Kernel : Glide_Kernel.Kernel_Handle) is
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
