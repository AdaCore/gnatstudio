------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2006-2023, AdaCore                     --
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

--  This package provides a view that allows the remote servers configuration

with GNATCOLL.Traces; use GNATCOLL.Traces;
with GPS.Kernel;
with Remote.Db;

package Remote_Module is

   Remote_Module_Trace : constant Trace_Handle :=
     Create ("GPS.INTERNAL.MODULE_REMOTE", GNATCOLL.Traces.Off);
   --  Trace used to disable the remote mode module.

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Initialize the module

   function Get_Database return access Remote.Db.Remote_Db_Type;

   procedure Load_Remote_Config
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);

   procedure Save_Remote_Config
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);

end Remote_Module;
