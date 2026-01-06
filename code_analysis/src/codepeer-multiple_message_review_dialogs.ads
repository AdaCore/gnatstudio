------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2009-2026, AdaCore                     --
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
private with Gtk.Tree_View;
private with Gtk.Text_Buffer;
private with Gtk.Tree_Store;
private with Gtk.Widget;

with GPS.Kernel; use GPS.Kernel;
with CodePeer.Message_Review_Dialogs;
with CodePeer.Module;

package CodePeer.Multiple_Message_Review_Dialogs is

   type Message_Review_Dialog_Record is
     new CodePeer.Message_Review_Dialogs.Message_Review_Dialog_Record
     with private;

   type Message_Review_Dialog is
     access all Message_Review_Dialog_Record'Class;

   procedure Gtk_New
     (Dialog   : out Message_Review_Dialog;
      Kernel   : not null access Kernel_Handle_Record'Class;
      Module   : access CodePeer.Module.Module_Id_Record'Class;
      Messages : CodePeer.Message_Vectors.Vector);
   procedure Initialize
     (Self     : not null access Message_Review_Dialog_Record'Class;
      Kernel   : not null access Kernel_Handle_Record'Class;
      Module   : access CodePeer.Module.Module_Id_Record'Class;
      Messages : CodePeer.Message_Vectors.Vector);

private

   type Message_Review_Dialog_Record is
     new CodePeer.Message_Review_Dialogs.Message_Review_Dialog_Record with
   record
      Module                : access CodePeer.Module.Module_Id_Record'Class;

      --  All messages list
      All_Messages_Store    : Gtk.Tree_Store.Gtk_Tree_Store;
      All_Messages_View     : Gtk.Tree_View.Gtk_Tree_View;
      Set_Selection         : Boolean := False;

      --  Reviewing messages
      Messages              : CodePeer.Message_Vectors.Vector;
      Review_Messages_Store : Gtk.Tree_Store.Gtk_Tree_Store;
      Audit_Store           : Gtk.Tree_Store.Gtk_Tree_Store;

      --  GUI
      New_Status            : Gtk.Combo_Box.Gtk_Combo_Box;
      Comment_Buffer        : Gtk.Text_Buffer.Gtk_Text_Buffer;
      Approved_Entry        : Gtk.GEntry.Gtk_Entry;
      Next                  : Gtk.Widget.Gtk_Widget;
      Previous              : Gtk.Widget.Gtk_Widget;
   end record;

   overriding function Get_Messages
     (Self : not null access constant Message_Review_Dialog_Record)
      return CodePeer.Message_Vectors.Vector;
   --  Returns set of reviewed messages.

   procedure Fill_Messages
     (Self : not null access Message_Review_Dialog_Record'Class);
   --  Fill reviewing messages model

   procedure Fill_All_Messages
     (Self : not null access Message_Review_Dialog_Record'Class);
   --  Fill all messages model

   procedure Fill_Audit
     (Self : not null access Message_Review_Dialog_Record'Class);

end CodePeer.Multiple_Message_Review_Dialogs;
