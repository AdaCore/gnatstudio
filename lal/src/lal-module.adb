------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                       Copyright (C) 2017, AdaCore                        --
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

package body LAL.Module is

   Module : LAL_Module_Id;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Core_Kernels.Core_Kernel_Record'Class)
   is
   begin
      Module         := new LAL_Module_Id_Record;
      Module.Kernel  := GPS.Core_Kernels.Core_Kernel (Kernel);
      Module.Context := Libadalang.Analysis.Create
        (Unit_File_Provider => Module.Unit_Provider'Access);

      Module.Unit_Provider.Initialize (GPS.Core_Kernels.Core_Kernel (Kernel));

      Kernel.Register_Module (GPS.Core_Kernels.Abstract_Module (Module));
   end Register_Module;

end LAL.Module;
