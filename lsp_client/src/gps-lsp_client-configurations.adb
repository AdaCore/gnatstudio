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

with GNATCOLL.VFS;

with Spawn.Environments;

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

   ---------------------------------
   -- Full_Server_Executable_Path --
   ---------------------------------

   function Full_Server_Executable_Path
     (Self : Server_Configuration) return String
   is
      Executable : constant String :=
                     Ada.Strings.Unbounded.To_String (Self.Server_Executable);
      Aux        : constant GNATCOLL.VFS.Virtual_File :=
                     GNATCOLL.VFS.Create_From_UTF8 (Executable);

   begin
      if Aux.Is_Absolute_Path then
         return Executable;

      else
         return
           Spawn.Environments.System_Environment.Search_Path (Executable);
      end if;
   end Full_Server_Executable_Path;

   ------------------
   -- Is_Available --
   ------------------

   function Is_Available (Self : Server_Configuration) return Boolean is
   begin
      return
        GNATCOLL.VFS.Create_From_UTF8
          (Self.Full_Server_Executable_Path).Is_Regular_File;
   end Is_Available;

end GPS.LSP_Client.Configurations;
