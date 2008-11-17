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

--  This package controls the Output procedures handling, used by Prj.*
--  packages when displaying errors or information while loading/updating a
--  project.

with Output;

package Prj_Output is

   procedure Set_Default_Output_Handler
     (P : Output.Output_Proc);
   --  Set P as the default output handler. If P is null, this removes the
   --  default handler.

   procedure Set_Special_Output
     (P : Output.Output_Proc);
   --  Set P as a temporary output handler. Setting P to null is equivalent to
   --  calling Cancel_Special_Output.

   procedure Cancel_Special_Output;
   --  Removes the temporary output handler, and restores the default one, as
   --  previously set by Set_Default_Output_Handler.

end Prj_Output;
