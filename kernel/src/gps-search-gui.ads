------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2013, AdaCore                          --
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

--  This package provides the GUI support for the global search entry
--  in the GPS toolbar.

with GPS.Kernel;   use GPS.Kernel;
with GPS.Kernel.Search;

package GPS.Search.GUI is

   procedure Register_Module
      (Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Creates the global search entry, and all GPS actions to access it.

   procedure Register_Provider_And_Action
      (Kernel   : not null access GPS.Kernel.Kernel_Handle_Record'Class;
       Provider :
          not null access GPS.Kernel.Search.Kernel_Search_Provider'Class;
       Name     : String);
   --  Register the provider (and sets its Kernel field).
   --  Creates an action for it so that users can do key bindings.

end GPS.Search.GUI;
