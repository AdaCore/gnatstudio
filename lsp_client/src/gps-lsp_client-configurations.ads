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
--  Configuration object to separate language server configuration and
--  variation points from common code to handle all language server.

with Ada.Strings.Unbounded;

with GNATCOLL.JSON;
with Spawn.String_Vectors;

package GPS.LSP_Client.Configurations is

   type Server_Configuration is tagged limited record
      Server_Executable : Ada.Strings.Unbounded.Unbounded_String;
      Server_Arguments  : Spawn.String_Vectors.UTF_8_String_Vector;
   end record;

   type Server_Configuration_Access is access all Server_Configuration'Class;

   function Configuration_Settings
     (Self : Server_Configuration) return GNATCOLL.JSON.JSON_Value;
   --  Return JSON object with configuration description necessary for
   --  particular langauge server.

end GPS.LSP_Client.Configurations;
