------------------------------------------------------------------------------
--                               GNAT Studio                                --
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
with GPS.Kernel;

package GPS.LSP_Client.Configurations.ALS is

   type ALS_Configuration
     (Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class) is
     new Server_Configuration with private;

   overriding function Configuration_Settings
     (Self : ALS_Configuration) return GNATCOLL.JSON.JSON_Value;
   --  Return JSON object with configuration description necessary for
   --  particular language server.

   overriding function Is_Configuration_Supported
     (Self    : ALS_Configuration;
      Setting : Setting_Kind)
      return Boolean;

   overriding function Set_Configuration_Option
     (Self    : in out ALS_Configuration;
      Setting : Setting_Kind;
      Value   : Configuration_Value) return GNATCOLL.JSON.JSON_Value;

private

   type Settings_Values is array (Setting_Kind) of Configuration_Value;

   type ALS_Configuration
     (Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class) is
     new Server_Configuration
   with record
      Settings : Settings_Values;
   end record;

end GPS.LSP_Client.Configurations.ALS;
