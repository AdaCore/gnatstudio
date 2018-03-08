------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                       Copyright (C) 2017-2018, AdaCore                   --
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
--  Main entry point for Valgrind integration module

with GPS.Core_Kernels;

package GPS.Valgrind is

   type Valgrind_Module_Id_Record is
     new GPS.Core_Kernels.Abstract_Module_Record with record
      Kernel        : GPS.Core_Kernels.Core_Kernel;
   end record;

   type Valgrind_Module_Id is access all Valgrind_Module_Id_Record'Class;

   procedure Register_Module
     (Kernel : access GPS.Core_Kernels.Core_Kernel_Record'Class);
   --  Register module

end GPS.Valgrind;
