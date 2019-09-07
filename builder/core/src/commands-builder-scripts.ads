------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2008-2019, AdaCore                     --
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

--  This package defines a script interface (Python and Shell) to the builder
--  facility module. GUI independent commands only.
--  See Builder_Facility_Module for a global description.

with GPS.Core_Kernels;           use GPS.Core_Kernels;

package Commands.Builder.Scripts is

   function Get_Target_Name (Inst : Class_Instance) return String;
   --  Convenience function to get the target stored in Inst

   function Get_Target_Class
     (Kernel : access Core_Kernel_Record'Class) return Class_Type;
   --  Convenience function to get the target class

   procedure Register_Commands (Kernel : access Core_Kernel_Record'Class);
   --  Register the script classes and commands.

end Commands.Builder.Scripts;
