-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                         Copyright (C) 2004                        --
--                             ACT-Europe                            --
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

with Glide_Kernel.Contexts; use Glide_Kernel.Contexts;

package body Aunit_Utils is

   ---------------------------
   -- Get_Context_Directory --
   ---------------------------

   function Get_Context_Directory (Kernel : Kernel_Handle) return String is
      Context : constant Selection_Context_Access :=
                  Get_Current_Context (Kernel);
      File_Context : File_Selection_Context_Access;
   begin
      if Context /= null
        and then Context.all in File_Selection_Context'Class
      then
         File_Context := File_Selection_Context_Access (Context);
      end if;

      if File_Context /= null
        and then Has_Directory_Information (File_Context)
      then
         return Directory_Information (File_Context);
      else
         return "";
      end if;
   end Get_Context_Directory;

end Aunit_Utils;
