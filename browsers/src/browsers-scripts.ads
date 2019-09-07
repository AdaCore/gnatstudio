------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2014-2019, AdaCore                     --
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

private with Gtkada.Canvas_View;
with GPS.Kernel;
with GPS.Scripts;        use GPS.Scripts;

package Browsers.Scripts is

   procedure Register_Module
     (Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register the script commands for custom browsers

private
   type Item_Proxy is new Script_Proxy with null record;
   overriding function Class_Name (Self : Item_Proxy) return String
      is ("Browser.Items");
   package Item_Proxies is new Script_Proxies
      (Gtkada.Canvas_View.Abstract_Item, Item_Proxy);
   --  Implements link between browser items and python instances. This
   --  package cannot be used to create new class instances, though, since
   --  the name of the class is too generic.

   type Python_Item is interface;
   type Python_Item_Access is access all Python_Item'Class;
   function Inst_List
     (Self : not null access Python_Item)
     return access Item_Proxy'Class is abstract;

end Browsers.Scripts;
