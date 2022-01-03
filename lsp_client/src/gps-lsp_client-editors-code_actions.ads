------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2021-2022, AdaCore                  --
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

--  This package implements supports for code action requests in the editors

with GNATCOLL.VFS;  use GNATCOLL.VFS;

with GPS.Kernel;    use GPS.Kernel;

package GPS.LSP_Client.Editors.Code_Actions is

   procedure Request_Code_Action
     (Kernel : not null access Kernel_Handle_Record'Class;
      File   : Virtual_File);
   --  Initiate a code action request at the location marked by the current
   --  context.

end GPS.LSP_Client.Editors.Code_Actions;
