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

--  This package presents a very bare bones scripting interface to the
--  aliases module.
--  The only functionality provided on the ada side is to be able to retrieve
--  an alias instance by name. Every instance has two properties, name and
--  expansion. Everything else is done python-side

with GPS.Kernel;

package Aliases_Module.Scripts is

   procedure Register_Commands (Kernel : GPS.Kernel.Kernel_Handle);
      --  Register the script classes and commands.

end Aliases_Module.Scripts;
