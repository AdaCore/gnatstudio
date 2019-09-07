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

package body User_Interface_Tools is

   The_UI : User_Interface_Ptr := null;

   ------------------------
   -- Set_User_Interface --
   ------------------------

   procedure Set_User_Interface (UI : User_Interface_Ptr) is
   begin
      The_UI := UI;
   end Set_User_Interface;

   ------------------------
   -- Get_User_Interface --
   ------------------------

   function Get_User_Interface return User_Interface'Class is
   begin
      if The_UI = null then
         raise User_Interface_Not_Set;
      end if;

      return The_UI.all;
   end Get_User_Interface;

   ----------------
   -- Query_User --
   ----------------

   function Query_User
     (Prompt        : String;
      Password_Mode : Boolean) return String is
   begin
      return Query_User (Get_User_Interface, Prompt, Password_Mode);
   end Query_User;

end User_Interface_Tools;
