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

   --------------------------------
   -- Is_Configuration_Supported --
   --------------------------------

   function Is_Configuration_Supported
     (Self    : Server_Configuration;
      Setting : Setting_Kind)
      return Boolean
   is
      pragma Unreferenced (Self, Setting);
   begin
      return False;
   end Is_Configuration_Supported;

   ------------------------------
   -- Set_Configuration_Option --
   ------------------------------

   function Set_Configuration_Option
     (Self    : in out Server_Configuration;
      Setting : Setting_Kind;
      Value   : Configuration_Value) return GNATCOLL.JSON.JSON_Value
   is
      pragma Unreferenced (Self, Setting, Value);

   begin
      return GNATCOLL.JSON.JSON_Null;
   end Set_Configuration_Option;

end GPS.LSP_Client.Configurations;
