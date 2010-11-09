-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                    Copyright (C) 2010, AdaCore                    --
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

with Code_Peer.Module;

package body Code_Peer.Listeners is

   ------------------------------
   -- Message_Can_Be_Destroyed --
   ------------------------------

   overriding function Message_Can_Be_Destroyed
     (Self    : not null access Listener;
      Message : not null access GPS.Kernel.Messages.Abstract_Message'Class)
      return Boolean is
   begin
      if Message.Get_Category = Code_Peer.Module.Code_Peer_Category_Name then
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

end Code_Peer.Listeners;
