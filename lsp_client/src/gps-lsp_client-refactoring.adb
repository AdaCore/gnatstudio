------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                       Copyright (C) 2020-2023, AdaCore                   --
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

with GPS.LSP_Client.Refactoring.Rename;
with GPS.LSP_Client.Refactoring.Name_Parameters;
with GPS.LSP_Module;

package body GPS.LSP_Client.Refactoring is

   procedure Register
     (Kernel : Kernel_Handle;
      Id     : GPS.Kernel.Modules.Module_ID) is
   begin
      GPS.LSP_Client.Refactoring.Rename.Register (Kernel, Id);
      if GPS.LSP_Module.LSP_Ada_Support_Is_Active then
         GPS.LSP_Client.Refactoring.Name_Parameters.Register (Kernel, Id);
      end if;
   end Register;

end GPS.LSP_Client.Refactoring;
