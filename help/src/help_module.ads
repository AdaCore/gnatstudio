-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2005                       --
--                              AdaCore                              --
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

--  This package provides generic Help facilities.

with Glide_Kernel;
with GNAT.OS_Lib;

package Help_Module is

   procedure Register_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Register the module in the list

   ------------------
   -- URL contexts --
   ------------------

   type URL_Context is new Glide_Kernel.Selection_Context with private;
   type URL_Context_Access is access all URL_Context'Class;

   procedure Set_URL_Information
     (Context : access URL_Context;
      URL     : String := "");
   --  Set the information in this context.

   procedure Destroy (Context : in out URL_Context);
   --  Free the memory associated with the context

private

   type URL_Context is new Glide_Kernel.Selection_Context with record
      URL : GNAT.OS_Lib.String_Access := null;
   end record;

end Help_Module;
