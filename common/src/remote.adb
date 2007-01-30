-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2006-2007                       --
--                             AdaCore                               --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with GNAT.Strings; use GNAT.Strings;

package body Remote is

   type Server_Config is record
      Is_Local : Boolean := True;
      --  Is_Local Tells if the server is the local machine or not
      Nickname : String_Access;
      --  Identifier of the server
   end record;

   type Servers_Config is array (Server_Type) of Server_Config;

   Servers : Servers_Config :=
               (others => (Is_Local => True, Nickname => new String'("")));
   --  Servers currently used. Default is the localhost.
   --  ??? Global variable, should get rid of it

   ------------
   -- Assign --
   ------------

   procedure Assign
     (Server   : Server_Type;
      Nickname : String)
   is
   begin
      Free (Servers (Server).Nickname);
      if Nickname /= "" and then Nickname /= Local_Nickname then
         Servers (Server) := (Is_Local => False,
                              Nickname => new String'(Nickname));
      else
         Servers (Server) := (Is_Local => True,
                              Nickname => new String'(""));
      end if;
   end Assign;

   --------------
   -- Is_Local --
   --------------

   function Is_Local (Server : Server_Type) return Boolean is
   begin
      return Servers (Server).Is_Local;
   end Is_Local;

   ------------------
   -- Get_Nickname --
   ------------------

   function Get_Nickname (Server : Server_Type) return String is
   begin
      if Is_Local (Server) then
         return "";
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
         return Local_Nickname;
      else
         return Servers (Server).Nickname.all;
      end if;
   end Get_Printable_Nickname;

end Remote;
