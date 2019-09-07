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

--  This package handles all passwords used by GPS.

with GNAT.Regpat; use GNAT.Regpat;

package Password_Manager is

   function Get_Default_Password_Regexp return Pattern_Matcher;
   --  Get the default password regexp

   function Get_Default_Passphrase_Regexp return Pattern_Matcher;
   --  Get the default passphrase regexp

   function Get_Password
     (Network_Name : String;
      User_Name    : String := "";
      Force_Asking : Boolean := False) return String;
   --  Retrieves a password for specified machine and user
   --  If Parent is not null, the dialog asking for the password will be
   --  child of this window.
   --  If Force_Asking is set, the user will be asked for the password. Else
   --  the password is reused if it was already set.
   --  Raises User_Interface_Not_Set if UI was not set previously (see package
   --  User_Interface_Tools)

   function Get_Passphrase
     (Key_Id       : String;
      Force_Asking : Boolean := False) return String;
   --  Same as above, for a passphrase.
   --  Raises User_Interface_Not_Set if UI was not set previously (see package
   --  User_Interface_Tools)

   function Get_Tool_Password
     (Tool         : String;
      Force_Asking : Boolean := False) return String;
   --  Same as above, for the specified tool
   --  Raises User_Interface_Not_Set if UI was not set previously (see package
   --  User_Interface_Tools)

end Password_Manager;
