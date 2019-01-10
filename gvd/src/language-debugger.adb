------------------------------------------------------------------------------
--                                   GPS                                    --
--                                                                          --
--                     Copyright (C) 2000-2019, AdaCore                     --
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

package body Language.Debugger is

   --------------
   -- Keywords --
   --------------

   overriding function Keywords
     (Lang : access Language_Debugger) return Strings.String_Access is
      pragma Unreferenced (Lang);
   begin
      return null;
   end Keywords;

   ------------------
   -- Set_Debugger --
   ------------------

   procedure Set_Debugger
     (The_Language : access Language_Debugger;
      The_Debugger : Debugger_Access) is
   begin
      The_Language.The_Debugger := The_Debugger;
   end Set_Debugger;

   ------------------
   -- Get_Debugger --
   ------------------

   function Get_Debugger
     (The_Language : access Language_Debugger) return Debugger_Access is
   begin
      return The_Language.The_Debugger;
   end Get_Debugger;

end Language.Debugger;
