------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                       Copyright (C) 2012-2019, AdaCore                   --
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

--  Win32 version of this file.

with GPS.Kernel.Preferences;
with Src_Printing.Win32_Printer;
with Src_Printing.Command_Printer;

package body Src_Printing.Fabric is

   function Create return Abstract_Printer'Class is
      Print_Helper     : constant String :=
        GPS.Kernel.Preferences.Print_Command.Get_Pref;
   begin
      if Print_Helper = "" then
         return Src_Printing.Win32_Printer.Create;
      else
         return Src_Printing.Command_Printer.Create (Print_Helper);
      end if;
   end Create;

end Src_Printing.Fabric;
