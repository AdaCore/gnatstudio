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
--  This package provides listener to prevent destruction of CodePeer's
--  messages when they are removed from the locations view.

with GPS.Kernel.Messages;

private package Code_Peer.Listeners is

   type Listener is new GPS.Kernel.Messages.Abstract_Listener with private;

   type Listener_Access is access all Listener'Class;

   overriding function Message_Can_Be_Destroyed
     (Self    : not null access Listener;
      Message : not null access GPS.Kernel.Messages.Abstract_Message'Class)
      return Boolean;

   procedure Set_Cleanup_Mode
     (Self    : not null access Listener;
      Enabled : Boolean);

private

   type Listener is new GPS.Kernel.Messages.Abstract_Listener with record
      Cleanup : Boolean := False;
   end record;

end Code_Peer.Listeners;
