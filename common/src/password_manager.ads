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

--  This package handles all passwords used by GPS.

with GNAT.Regpat; use GNAT.Regpat;

package Password_Manager is

   UI_Not_Set : exception;

   type UI is abstract tagged null record;
   type UI_Ptr is access all UI'Class;

   function Query_User
     (User_Interface : UI;
      Prompt         : String;
      Password_Mode  : Boolean) return String is abstract;
   --  Open a new Dialog to query a response to the user.
   --  If Password_Mode is set, then the query will print * instead of
   --   the entered characters.
   --  Return "" if the user hasn't entered anything

   procedure Set_UI (User_Interface : UI_Ptr);
   --  Set the User Interface to use. It shall be set before any call to
   --  Get_Password, Get_Passphrase, Get_Tool_Password

   function Get_UI return UI_Ptr;
   --  Get the User Interface set by Set_UI.

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
   --  Raises UI_Not_Set if UI was not set previously

   function Get_Passphrase
     (Key_Id       : String;
      Force_Asking : Boolean := False) return String;
   --  Same as above, for a passphrase.
   --  Raises UI_Not_Set if UI was not set previously

   function Get_Tool_Password
     (Tool         : String;
      Force_Asking : Boolean := False) return String;
   --  Same as above, for the specified tool
   --  Raises UI_Not_Set if UI was not set previously

end Password_Manager;
