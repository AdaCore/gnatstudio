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
