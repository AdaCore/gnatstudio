------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                        Copyright (C) 2013-2015, AdaCore                  --
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

with GNATCOLL.Scripts;                 use GNATCOLL.Scripts;

package body GPS.Scripts is

   ------------
   -- Create --
   ------------

   function Create
     (Kernel : GPS.Core_Kernels.Core_Kernel)
      return Kernel_Scripts_Repository is
   begin
      return (Scripts_Repository_Record with Kernel => Kernel);
   end Create;

   ----------------
   -- Get_Kernel --
   ----------------

   function Get_Kernel
     (Data : GNATCOLL.Scripts.Callback_Data'Class)
      return GPS.Core_Kernels.Core_Kernel is
   begin
      return Kernel_Scripts_Repository (Get_Repository (Data).all).Kernel;
   end Get_Kernel;

   ----------------
   -- Get_Kernel --
   ----------------

   function Get_Kernel
     (Script : access GNATCOLL.Scripts.Scripting_Language_Record'Class)
      return GPS.Core_Kernels.Core_Kernel is
   begin
      return Kernel_Scripts_Repository (Get_Repository (Script).all).Kernel;
   end Get_Kernel;

end GPS.Scripts;
