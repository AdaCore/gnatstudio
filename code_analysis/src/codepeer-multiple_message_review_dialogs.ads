------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2009-2018, AdaCore                     --
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

private with Gtk.Combo_Box;
private with Gtk.GEntry;
private with Gtk.Text_Buffer;

with GPS.Kernel; use GPS.Kernel;
with CodePeer.Message_Review_Dialogs;

package CodePeer.Multiple_Message_Review_Dialogs is

   type Message_Review_Dialog_Record is
     new CodePeer.Message_Review_Dialogs.Message_Review_Dialog_Record
     with private;

   type Message_Review_Dialog is
     access all Message_Review_Dialog_Record'Class;

   procedure Gtk_New
     (Dialog   : out Message_Review_Dialog;
      Kernel   : not null access Kernel_Handle_Record'Class;
      Messages : CodePeer.Message_Vectors.Vector);
   procedure Initialize
     (Self     : not null access Message_Review_Dialog_Record'Class;
      Kernel   : not null access Kernel_Handle_Record'Class;
      Messages : CodePeer.Message_Vectors.Vector);

private

   type Message_Review_Dialog_Record is
     new CodePeer.Message_Review_Dialogs.Message_Review_Dialog_Record with
   record
      Messages       : CodePeer.Message_Vectors.Vector;
      New_Status     : Gtk.Combo_Box.Gtk_Combo_Box;
      Comment_Buffer : Gtk.Text_Buffer.Gtk_Text_Buffer;
      Approved_Entry : Gtk.GEntry.Gtk_Entry;
   end record;

   overriding function Get_Messages
     (Self : not null access constant Message_Review_Dialog_Record)
      return CodePeer.Message_Vectors.Vector;
   --  Returns set of reviewed messages.

end CodePeer.Multiple_Message_Review_Dialogs;
