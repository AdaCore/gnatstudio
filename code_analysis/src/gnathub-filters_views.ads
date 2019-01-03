------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                       Copyright (C) 2016-2019, AdaCore                   --
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

--  This package contains a view which allow to select criterias for
--  filtering GNATHub messages.

with GNAThub.Module;

package GNAThub.Filters_Views is

   procedure Open_View
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Module : not null access GNAThub.Module.GNAThub_Module_Id_Record'Class);
   --  Open the filters view

   procedure Close_View
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Close the filters view

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Registers module.

   procedure Set_Tool_Selection
     (Kernel   : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      Tool     : not null Tool_Access;
      Selected :  Boolean);
   --  Select or unselect the specified tool in the Filters view.

end GNAThub.Filters_Views;
