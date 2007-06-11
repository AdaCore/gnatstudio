-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                         Copyright (C) 2004-2006                   --
--                               AdaCore                             --
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

with GPS.Kernel.Contexts; use GPS.Kernel.Contexts;

package body Aunit_Utils is

   ---------------------------
   -- Get_Context_Directory --
   ---------------------------

   function Get_Context_Directory (Kernel : Kernel_Handle) return String is
      Context : constant Selection_Context :=
                  Get_Current_Context (Kernel);
   begin
      if Has_Directory_Information (Context) then
         return Directory_Information (Context);
      else
         return "";
      end if;
   end Get_Context_Directory;

end Aunit_Utils;
