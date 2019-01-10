------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                        Copyright (C) 2013-2019, AdaCore                  --
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

--  Implementation of Entity class

with GPS.Core_Kernels;
with Xref;                   use Xref;

package GPS.Scripts.Entities is

   procedure Register_Commands
     (Kernel : access GPS.Core_Kernels.Core_Kernel_Record'Class);
   --  Add script commands for Entity class.

   function Get_Entity_Class
     (Kernel : access GPS.Core_Kernels.Core_Kernel_Record'Class)
      return Class_Type;
   --  Return the class to use for entities. This encapsulates an
   --  Entity_Information.

   procedure Set_Data
     (Instance : Class_Instance; Entity : Xref.Root_Entity'Class);
   function Get_Data
     (Data : Callback_Data'Class; N : Positive)
      return Xref.Root_Entity'Class;
   --  The Entity class stores some Entity_Information data in Instance
   --  You should destroy the entity passed to Set_Data, but not the value
   --  returned by Get_Data

   function Create_Entity
     (Script : access Scripting_Language_Record'Class;
      Entity : Xref.Root_Entity'Class) return Class_Instance;
   --  Return a new entity. Entity parameter should be freed by the caller

end GPS.Scripts.Entities;
