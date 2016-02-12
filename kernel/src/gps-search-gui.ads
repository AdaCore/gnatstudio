------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2013-2016, AdaCore                     --
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

with Commands.Interactive;
with GPS.Kernel;   use GPS.Kernel;
with Histories;

package GPS.Search.GUI is

   procedure Register_Module
      (Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Creates the global search entry, and all GPS actions to access it.

   --------------
   -- Commands --
   --------------

   type Global_Search_Command is new Commands.Interactive.Interactive_Command
   with record
      Provider : GPS.Search.Search_Provider_Access;
      History  : access Histories.History_Key;
   end record;
   type Global_Search_Command_Access
      is access all Global_Search_Command'Class;
   overriding function Execute
      (Self    : access Global_Search_Command;
       Context : Commands.Interactive.Interactive_Command_Context)
      return Commands.Command_Return_Type;
   --  Activate the global search field

end GPS.Search.GUI;
