------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2004-2012, AdaCore                     --
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

with GPS.Kernel.Contexts; use GPS.Kernel.Contexts;
with GPS.Kernel.MDI;      use GPS.Kernel.MDI;

package body Aunit_Utils is

   ---------------------------
   -- Get_Context_Directory --
   ---------------------------

   function Get_Context_Directory
     (Kernel : Kernel_Handle) return Virtual_File
   is
      Context : constant Selection_Context := Get_Current_Context (Kernel);
   begin
      if Has_Directory_Information (Context) then
         return Directory_Information (Context);
      else
         return No_File;
      end if;
   end Get_Context_Directory;

end Aunit_Utils;
