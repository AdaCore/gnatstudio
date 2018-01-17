------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2010-2018, AdaCore                     --
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
--  This package provides listener to prevent destruction of CodePeer's
--  messages when they are removed from the locations view.

with GPS.Kernel.Messages;

private package CodePeer.Listeners is

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

end CodePeer.Listeners;
