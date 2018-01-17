------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2017-2018, AdaCore                     --
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

with Memory_Usage_Views.Providers;
with Memory_Usage_Views.Scripts;

package body Memory_Usage_Views.Module is

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class) is
   begin
      Memory_Usage_Views.Memory_Usage_MDI_Views.Register_Module (Kernel);
      Memory_Usage_Views.Register_Module (Kernel);
      Memory_Usage_Views.Providers.Register_Module (Kernel);
      Memory_Usage_Views.Scripts.Register_Scripts (Kernel);
   end Register_Module;

end Memory_Usage_Views.Module;
