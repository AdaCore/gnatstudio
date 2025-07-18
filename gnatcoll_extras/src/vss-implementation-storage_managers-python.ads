------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                       Copyright (C) 2025, AdaCore                        --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

--  Storage manager to wrap Python's bytes object.

with GNATCOLL.Python;

package VSS.Implementation.Storage_Managers.Python is

   type Python_Storage_Manager is
     new Abstract_Storage_Manager with null record;

   procedure Initialize
     (Self            : in out Python_Storage_Manager'Class;
      Storage_Address : out System.Address;
      Bytes           : GNATCOLL.Python.PyObject);

   overriding function Capacity
     (Self : in out Python_Storage_Manager)
      return VSS.Unicode.UTF8_Code_Unit_Count;

   overriding procedure Reference (Self : in out Python_Storage_Manager);

   overriding procedure Unreference (Self : in out Python_Storage_Manager);

   overriding procedure Mutate
     (Self            : in out Python_Storage_Manager;
      Storage_Address : in out System.Address;
      Capacity        : VSS.Unicode.UTF8_Code_Unit_Count);

end VSS.Implementation.Storage_Managers.Python;
