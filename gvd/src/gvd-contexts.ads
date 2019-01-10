------------------------------------------------------------------------------
--                      GVD - The GNU Visual Debugger                       --
--                                                                          --
--                     Copyright (C) 2016-2019, AdaCore                     --
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

--  Utilities to support selection contexts in the contxt of the debugger

with GPS.Kernel;           use GPS.Kernel;
with GVD.Variables.Items;  use GVD.Variables.Items;
package GVD.Contexts is

   function Get_Variable_Name
     (Context     : GPS.Kernel.Selection_Context;
      Dereference : Boolean) return String;
   --  If Context contains an entity, get the entity name.
   --  Dereference the entity if Dereference is True.
   --  Return "" if entity name could not be found in Context.

   procedure Set_Variable
     (Context   : in out GPS.Kernel.Selection_Context;
      Full_Name : String;
      Info      : Item_Info);
   --  Set the debugging variable into the Context.

   function Get_Variable
     (Context : GPS.Kernel.Selection_Context)
      return Item_Info;
   --  Retrieve the debugging variable from the Context.

end GVD.Contexts;
