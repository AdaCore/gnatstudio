------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2010-2019, AdaCore                     --
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

with CodePeer.Module;

package body CodePeer.Listeners is

   ------------------------------
   -- Message_Can_Be_Destroyed --
   ------------------------------

   overriding function Message_Can_Be_Destroyed
     (Self    : not null access Listener;
      Message : not null access GPS.Kernel.Messages.Abstract_Message'Class)
      return Boolean
   is
      Category : constant String := Message.Get_Category;

   begin
      if Category = CodePeer.Module.CodePeer_Category_Name then
         return Self.Cleanup;

      else
         return True;
      end if;
   end Message_Can_Be_Destroyed;

   ----------------------
   -- Set_Cleanup_Mode --
   ----------------------

   procedure Set_Cleanup_Mode
     (Self    : not null access Listener;
      Enabled : Boolean) is
   begin
      Self.Cleanup := Enabled;
   end Set_Cleanup_Mode;

end CodePeer.Listeners;
