-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2009, AdaCore                   --
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
