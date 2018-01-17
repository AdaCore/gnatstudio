------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2009-2018, AdaCore                     --
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

--  This package defines the required interface for g-exttre operations.
--  This is used for uncoupling the GPS remote mode configuration (in the
--  remote project) and GNAT.Expect.TTY.Remote (in the common project).

package Gexpect.Db is

   Invalid_Machine_Configuration : exception;

   type Machine_Array is array (Natural range <>) of access Machine_Type'Class;

   -----------------------------------------
   -- Placeholder for machines definition --
   -----------------------------------------

   type Machine_Db_Interface is interface;

   function Is_Configured
     (Db       : Machine_Db_Interface;
      Nickname : String) return Boolean is abstract;
   --  Tell if Machine is configured.

   function Get_Servers
     (Db       : Machine_Db_Interface) return String_List is abstract;
   --  Get the list of all configured machines.
   --  DO NOT FREE THE STRINGS IN THIS LIST.

   function Get_Server
     (Db       : Machine_Db_Interface;
      Nickname : String) return Machine_Access is abstract;
   --  Get the Machine according to its nickname.

   ----------------------
   -- DB configuration --
   ----------------------

   procedure Define_Machine_Db
     (Db : access Machine_Db_Interface'Class);
   --  Defines Db as the main repository for machines. This Db will be used
   --  by GNAT.Expect.TTY.Remote to access the machines configuration.

   function Is_Configured (Nickname : String) return Boolean;
   --  Tell if Machine is configured.
   --  Raises Invalid_Machine_Configuration if no DB has been defined.

   function Get_Servers return String_List;
   --  Get the list of all configured machines.
   --  DO NOT FREE THE STRINGS IN THIS LIST.
   --  Raises Invalid_Machine_Configuration if no DB has been defined.

   function Get_Server (Nickname : String) return Machine_Access;
   --  Get the Machine according to its nickname.
   --  Raises Invalid_Machine_Configuration if no DB has been defined.

end Gexpect.Db;
