------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                        Copyright (C) 2013-2018, AdaCore                  --
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

with GNATCOLL.Scripts;
with GNATCOLL.Scripts.Python;          use GNATCOLL.Scripts.Python;
with GNATCOLL.Utils;                   use GNATCOLL.Utils;

with GNAT.OS_Lib;                      use GNAT.OS_Lib;
--  with GNAT.Strings;                     use GNAT.Strings;

package body GPS.Python_Core is

   ---------------------
   -- Register_Python --
   ---------------------

   procedure Register_Python
     (Kernel : access GPS.Core_Kernels.Core_Kernel_Record'Class)
   is
      Python_Home : String_Access := Getenv ("GPS_PYTHONHOME");
   begin
      if Python_Home.all = "" then
         Free (Python_Home);
         Python_Home := new String'(Executable_Location);
      end if;

      Register_Python_Scripting
        (Kernel.Scripts,
         Module      => "GPS",
         Python_Home => Python_Home.all);

      Free (Python_Home);
   end Register_Python;

end GPS.Python_Core;
