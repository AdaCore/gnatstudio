-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2009, AdaCore                   --
--                                                                   --
-- GPS is Free  software;  you can redistribute it and/or modify  it --
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

with Gtk.Combo_Box;
with Gtk.Dialog;
with Gtk.Text_Buffer;

package Code_Peer.Message_Review_Dialogs is

   type Message_Review_Dialog_Record is
     new Gtk.Dialog.Gtk_Dialog_Record with private;

   type Message_Review_Dialog is access all Message_Review_Dialog_Record'Class;

   procedure Gtk_New
     (Dialog : in out Message_Review_Dialog;
      Audit  : Code_Peer.Audit_Trail);

   procedure Initialize
     (Self  : not null access Message_Review_Dialog_Record'Class;
      Audit : Code_Peer.Audit_Trail);

private

   type Message_Review_Dialog_Record is
     new Gtk.Dialog.Gtk_Dialog_Record with record
      New_Probability : Gtk.Combo_Box.Gtk_Combo_Box;
      Comment_Buffer  : Gtk.Text_Buffer.Gtk_Text_Buffer;
   end record;

end Code_Peer.Message_Review_Dialogs;
