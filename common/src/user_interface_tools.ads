-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2006                         --
--                              AdaCore                              --
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

--  This package handles generic user interface functions

package User_Interface_Tools is

   User_Interface_Not_Set : exception;
   --  exception raised when calling a method necessiting a user interface, and
   --  this user interface has not been set.

   type User_Interface is abstract tagged null record;
   --  Object representing the User Interface used to ask questions to users.
   type User_Interface_Ptr is access all User_Interface'Class;

   function Query_User
     (UI            : User_Interface;
      Prompt        : String;
      Password_Mode : Boolean) return String is abstract;
   --  Open a new Dialog to query a response to the user.
   --  If Password_Mode is set, then the query will print * instead of
   --   the entered characters.
   --  Return "" if the user hasn't entered anything

   procedure Set_User_Interface (UI : User_Interface_Ptr);
   --  Set the User Interface to use. It shall be set before any call to
   --  Get_Password, Get_Passphrase, Get_Tool_Password

   function Get_User_Interface return User_Interface'Class;
   --  Get the User Interface set by Set_User_Interface.

   function Query_User
     (Prompt        : String;
      Password_Mode : Boolean) return String;
   --  Open a new Dialog to query a response to the user.
   --  If Password_Mode is set, then the query will print * instead of
   --   the entered characters.
   --  Raises User_Interface_Not_Set if UI was not set previously

end User_Interface_Tools;
