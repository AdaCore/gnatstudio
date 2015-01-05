------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2009-2015, AdaCore                     --
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

with Glib;
with Gtk.Combo_Box;
with Gtk.Dialog;
with Gtk.GEntry;
with Gtk.Text_Buffer;

package CodePeer.Message_Review_Dialogs_V3 is

   type Message_Review_Dialog_Record is
     new Gtk.Dialog.Gtk_Dialog_Record with private;

   type Message_Review_Dialog is access all Message_Review_Dialog_Record'Class;

   procedure Gtk_New
     (Dialog  : in out Message_Review_Dialog;
      Message : CodePeer.Message_Access);

   procedure Initialize
     (Self    : not null access Message_Review_Dialog_Record'Class;
      Message : CodePeer.Message_Access);

   Signal_Ok_Activated : constant Glib.Signal_Name;

private

   type Message_Review_Dialog_Record is
     new Gtk.Dialog.Gtk_Dialog_Record with record
      Message        : CodePeer.Message_Access;
      New_Status     : Gtk.Combo_Box.Gtk_Combo_Box;
      Comment_Buffer : Gtk.Text_Buffer.Gtk_Text_Buffer;
      Approved_Entry : Gtk.GEntry.Gtk_Entry;
   end record;

   Signal_Ok_Activated : constant Glib.Signal_Name := "ok_activated";

end CodePeer.Message_Review_Dialogs_V3;
