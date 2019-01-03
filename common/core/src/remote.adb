------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2006-2019, AdaCore                     --
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

with GNAT.Strings; use GNAT.Strings;
with Toolchains_Old;

package body Remote is

   type Server_Config is record
      Is_Local : Boolean := True;
      --  Is_Local Tells if the server is the local machine or not
      Nickname : String_Access;
      --  Identifier of the server
   end record;

   type Servers_Config is array (Distant_Server_Type) of Server_Config;

   Servers : Servers_Config :=
     (others =>
        (Is_Local => True,
         Nickname => new String'(Local_Nickname)));
   --  Servers currently used. Default is the localhost.
   --  ??? Global variable, should get rid of it

   ------------
   -- Assign --
   ------------

   procedure Assign
     (Server   : Distant_Server_Type;
      Nickname : String)
   is
   begin
      Free (Servers (Server).Nickname);
      Servers (Server) := (Is_Local => Nickname = Local_Nickname,
                           Nickname => new String'(Nickname));
   end Assign;

   --------------
   -- Is_Local --
   --------------

   function Is_Local (Server : Server_Type) return Boolean is
   begin
      if Server in Distant_Server_Type then
         return Servers (Server).Is_Local;
      elsif Server = Tools_Server then
         if Toolchains_Old.Is_Toolchains_Active then
            return True;
         else
            return Servers (Build_Server).Is_Local;
         end if;
      else
         return True;
      end if;
   end Is_Local;

   ------------------
   -- Get_Nickname --
   ------------------

   function Get_Nickname (Server : Server_Type) return String is
   begin
      if Is_Local (Server) then
         return Local_Nickname;
      elsif Server = Tools_Server then
         return Servers (Build_Server).Nickname.all;
      else
         return Servers (Server).Nickname.all;
      end if;
   end Get_Nickname;

   ----------------------------
   -- Get_Printable_Nickname --
   ----------------------------

   function Get_Printable_Nickname (Server : Server_Type) return String is
   begin
      if Is_Local (Server) then
         return Display_Local_Nickname;
      elsif Server = Tools_Server then
         return Servers (Build_Server).Nickname.all;
      else
         return Servers (Server).Nickname.all;
      end if;
   end Get_Printable_Nickname;

   ---------------------------
   -- Multi_Unit_Index_Char --
   ---------------------------

   function Multi_Unit_Index_Char (Server : Server_Type) return Character is
      pragma Unreferenced (Server);
   begin
      --  ??? If we start supporting VMS, we will need to retrieve the
      --  FS Type of the server beforehand.
      return '~';
   end Multi_Unit_Index_Char;

end Remote;
