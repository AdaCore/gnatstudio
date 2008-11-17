-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                 Copyright (C) 2008, AdaCore                       --
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

with Output; use Output;

package body Prj_Output is

   Default_Output_Proc : Output.Output_Proc := null;

   --------------------------------
   -- Set_Default_Output_Handler --
   --------------------------------

   procedure Set_Default_Output_Handler
     (P : Output.Output_Proc)
   is
   begin
      Default_Output_Proc := P;
      Output.Set_Special_Output (P);
   end Set_Default_Output_Handler;

   ------------------------
   -- Set_Special_Output --
   ------------------------

   procedure Set_Special_Output
     (P : Output.Output_Proc)
   is
   begin
      if P = null then
         Output.Set_Special_Output (Default_Output_Proc);
      else
         Output.Set_Special_Output (P);
      end if;
   end Set_Special_Output;

   ---------------------------
   -- Cancel_Special_Output --
   ---------------------------

   procedure Cancel_Special_Output is
   begin
      Output.Set_Special_Output (Default_Output_Proc);
   end Cancel_Special_Output;

end Prj_Output;
