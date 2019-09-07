------------------------------------------------------------------------------
--                               GNAT Studio                                --
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
