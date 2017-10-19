------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2000-2017, AdaCore                     --
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

with Gtkada.Canvas_View;    use Gtkada.Canvas_View;
with GPS.Kernel;            use GPS.Kernel;
with Items;                 use Items;

package GVD.Canvas.View is

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register menus and other functions to support the data windows

   procedure Dereference_Item
     (Component : not null access Component_Item_Record'Class);
   --  Dereference a component of Item ("graph display" on it with a link from
   --  the item).

   procedure Change_Visibility
     (Item      : not null access Gtkada.Canvas_View.Canvas_Item_Record'Class;
      Component : not null access Generic_Type'Class);
   --  Change the visibility status of a specific component in the item

end GVD.Canvas.View;
