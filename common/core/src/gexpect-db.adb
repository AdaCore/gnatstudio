------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2009-2019, AdaCore                     --
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

package body Gexpect.Db is

   Global_DB : access Machine_Db_Interface'Class := null;

   -----------------------
   -- Define_Machine_Db --
   -----------------------

   procedure Define_Machine_Db
     (Db : access Machine_Db_Interface'Class)
   is
   begin
      Global_DB := Db;
   end Define_Machine_Db;

   -------------------
   -- Is_Configured --
   -------------------

   function Is_Configured (Nickname : String) return Boolean is
   begin
      if Nickname = "" then
         return True;
      end if;

      if Global_DB = null then
         raise Invalid_Machine_Configuration;
      end if;

      return Global_DB.Is_Configured (Nickname);
   end Is_Configured;

   ----------------
   -- Get_Server --
   ----------------

   function Get_Server (Nickname : String) return Machine_Access is
   begin
      if Global_DB = null then
         raise Invalid_Machine_Configuration;
      end if;

      return Global_DB.Get_Server (Nickname);
   end Get_Server;

   -----------------
   -- Get_Servers --
   -----------------

   function Get_Servers return String_List is
   begin
      if Global_DB = null then
         raise Invalid_Machine_Configuration;
      end if;

      return Global_DB.Get_Servers;
   end Get_Servers;

end Gexpect.Db;
