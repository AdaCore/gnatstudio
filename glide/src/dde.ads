-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2003                         --
--                            ACT-Europe                             --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

--  This package provides support for DDE queries.
--  DDE, or Dynamic Data Exchange, allows running applications to request
--  services from each other. In the context of GPS, this allows Windows
--  Explorer to notify a running instance of GPS of the File Open request,
--  enabling GPS to open the file in the same instance.
--  It is only supported on Windows platforms, and provides no op on other
--  platforms.

with Glide_Kernel;

package DDE is

   procedure Register_DDE_Server (Kernel : Glide_Kernel.Kernel_Handle);
   --  Register the application as a DDE server, if supported on the
   --  platform. Do nothing otherwise.

   procedure Unregister_DDE_Server;
   --  Unregister the application as a DDE server.

end DDE;
