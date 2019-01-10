------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                       Copyright (C) 2016-2019, AdaCore                   --
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
--  Abstract interface of message review dialogs. Dialog can return list of
--  reviewed messages and can emit 'ok_activated' signal.

with Glib;

with GPS.Kernel.MDI;    use GPS.Kernel.MDI;
with GPS.Dialogs;       use GPS.Dialogs;

package CodePeer.Message_Review_Dialogs is

   type Message_Review_Dialog_Record is
      abstract new GPS_Dialog_Record with null record;

   not overriding function Get_Messages
     (Self : not null access constant Message_Review_Dialog_Record)
      return CodePeer.Message_Vectors.Vector is abstract;
   --  Returns set of reviewed messages.

   Signal_Ok_Activated : constant Glib.Signal_Name;

private

   Signal_Ok_Activated : constant Glib.Signal_Name := "ok_activated";

end CodePeer.Message_Review_Dialogs;
