------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2019-2024, AdaCore                     --
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

with GNATCOLL.VFS; use GNATCOLL.VFS;
with GNATCOLL.JSON;
with Spawn.String_Vectors;

with GPS.Kernel;

package GPS.LSP_Client.Configurations is

   type Server_Configuration
     (Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class)
   is tagged limited record
      Server_Program    : Virtual_File;
      --  The executable to launch for this server

      Server_Arguments  : Spawn.String_Vectors.UTF_8_String_Vector;
   end record;

   type Server_Configuration_Access is access all Server_Configuration'Class;

   procedure Prepare_Configuration_Settings
     (Self : in out Server_Configuration) is null;
   --  Prepare configuration settings.

   function Configuration_Settings
     (Self : Server_Configuration) return GNATCOLL.JSON.JSON_Value;
   --  Return JSON object with configuration description necessary for
   --  particular language server.

   function Is_Available (Self : Server_Configuration) return Boolean;
   --  Return True when language server is available for use. It checks that
   --  executable is available. Derived types can implement more checks if
   --  necessary.

   -- Settings --

   type Setting_Kind is
     (
      --  Refactoring  --
      --  Refactoring/Renaming  --
      Rename_In_Comments,

      -- Folding --
      Fold_Comments);

   type Configuration_Value_Type is (None_Type, Boolean_Type);

   type Configuration_Value
     (Kind : Configuration_Value_Type := None_Type)
   is record
      case Kind is
         when Boolean_Type =>
            vBoolean : Boolean;

         when None_Type =>
            null;
      end case;
   end record;

   function Is_Configuration_Supported
     (Self    : Server_Configuration;
      Setting : Setting_Kind)
      return Boolean;
   --  Return True if server supports the setting.

   function Set_Configuration_Option
     (Self    : in out Server_Configuration;
      Setting : Setting_Kind;
      Value   : Configuration_Value) return GNATCOLL.JSON.JSON_Value;
   --  Return JSON object with configuration value or JSON_Null

end GPS.LSP_Client.Configurations;
