------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2003-2012, AdaCore                     --
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

--  This package provides support for DDE queries.
--  DDE, or Dynamic Data Exchange, allows running applications to request
--  services from each other. In the context of GPS, this allows Windows
--  Explorer to notify a running instance of GPS of the File Open request,
--  enabling GPS to open the file in the same instance.
--  It is only supported on Windows platforms, and provides no op on other
--  platforms.

with GPS.Kernel;

package DDE is

   procedure Register_DDE_Server (Kernel : GPS.Kernel.Kernel_Handle);
   --  Register the application as a DDE server, if supported on the
   --  platform. Do nothing otherwise.

   procedure Unregister_DDE_Server;
   --  Unregister the application as a DDE server.

end DDE;
