------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                       Copyright (C) 2019, AdaCore                        --
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

package body GPS.LSP_Client.Configurations is

   ----------------------------
   -- Configuration_Settings --
   ----------------------------

   function Configuration_Settings
     (Self : Server_Configuration) return GNATCOLL.JSON.JSON_Value
   is
      pragma Unreferenced (Self);

   begin
      return GNATCOLL.JSON.Create_Object;
   end Configuration_Settings;

   ------------------
   -- Is_Available --
   ------------------

   function Is_Available (Self : Server_Configuration) return Boolean is
   begin
      return Self.Server_Program.Is_Regular_File;
   end Is_Available;

end GPS.LSP_Client.Configurations;
