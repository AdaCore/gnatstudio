------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2003-2018, AdaCore                     --
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

--  This package provides scripting support for hooks, so that they can be
--  used from python.

with GNATCOLL.Scripts; use GNATCOLL.Scripts;

package GPS.Kernel.Scripts.Hooks is

   Hook_Class_Name : constant String := "Hook";
   --  The python class name for hooks

   type Python_Hook_Function is new Hook_Function with record
      Func : Subprogram_Type;
   end record;
   --  A special function that encapsulates a python subprogram.
   --  We do not check the number of parameters or their types, since
   --  python does not support this in any case. So this wrapper can be
   --  used for any type of hook.

   function Get_Hook_Class
      (Kernel : not null access Kernel_Handle_Record'Class)
      return Class_Type;
   --  Return the "GPS.Hook" class

   function Get_Hook
      (Data : Callback_Data'Class; Param : Positive)
      return access Hook_Types'Class;
   --  Return the hook associated with the class instance as the
   --  Param-th argument in data.

   procedure Register_Module
      (Kernel : not null access Kernel_Handle_Record'Class);
   --  Add python support for the Hook class.

   function Get_Hook
      (Kernel : not null access Kernel_Handle_Record'Class;
       Name   : String) return access Hook_Types'Class;
   --  Fetch a hook by name

end GPS.Kernel.Scripts.Hooks;
