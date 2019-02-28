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

with Ada.Calendar.Formatting;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces.C.Strings;
with System;

with Glib.Object;
with Gtk.Button;
with Gtk.Cell_Renderer_Text;
with Gtk.Dialog;               use Gtk.Dialog;
with Gtk.Enums;
with Gtk.Handlers;
with Gtk.Label;
with Gtk.Scrolled_Window;
with Gtk.Stock;
with Gtk.Table;
with Gtk.Text_Iter;
with Gtk.Text_View;
with Gtk.Tree_Model;
with Gtk.Tree_Store;           use Gtk.Tree_Store;
with Gtk.Tree_View;
with Gtk.Tree_View_Column;
with Gtk.Widget;

with GNATCOLL.Xref;
with GPS.Intl; use GPS.Intl;
with GPS.Kernel.MDI;
with GPS.Dialogs;
with GPS.Main_Window;

with Glib_Values_Utils;        use Glib_Values_Utils;

with CodePeer.Message_Review_Dialogs.Utils;
use CodePeer.Message_Review_Dialogs.Utils;

package body CodePeer.Multiple_Message_Review_Dialogs is

   Messages_Model_Ranking_Column  : constant := 0;
   Messages_Model_Status_Column   : constant := 1;
   Messages_Model_Location_Column : constant := 2;
   Messages_Model_Text_Column     : constant := 3;

   Messages_Model_Types : constant Glib.GType_Array :=
     (Messages_Model_Ranking_Column  => Glib.GType_String,
      Messages_Model_Status_Column   => Glib.GType_String,
      Messages_Model_Location_Column => Glib.GType_String,
      Messages_Model_Text_Column     => Glib.GType_String);

   package Message_Review_Callbacks is
     new Gtk.Handlers.User_Callback
       (Glib.Object.GObject_Record, Message_Review_Dialog);

   procedure On_Ok
     (Object : access Glib.Object.GObject_Record'Class;
      Self   : Message_Review_Dialog);

   procedure On_Cancel
     (Object : access Glib.Object.GObject_Record'Class;
      Self   : Message_Review_Dialog);

   procedure Emit_By_Name
     (Object : System.Address;
      Name   : Glib.Signal_Name);
   pragma Import (C, Emit_By_Name, "ada_g_signal_emit_by_name");

   Class_Record : Glib.Object.Ada_GObject_Class :=
      Glib.Object.Uninitialized_Class;

   Signals : constant Interfaces.C.Strings.chars_ptr_array :=
     (1 => Interfaces.C.Strings.New_String
        (String (CodePeer.Message_Review_Dialogs.Signal_Ok_Activated)));

   Signal_Parameters : constant Glib.Object.Signal_Parameter_Types :=
     (1 => (1 => Glib.GType_None));

   ------------------
   -- Get_Messages --
   ------------------

   overriding function Get_Messages
     (Self : not null access constant Message_Review_Dialog_Record)
      return CodePeer.Message_Vectors.Vector is
   begin
      return Self.Messages;
   end Get_Messages;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Dialog   : out Message_Review_Dialog;
      Kernel   : not null access Kernel_Handle_Record'Class;
      Messages : CodePeer.Message_Vectors.Vector)
   is
   begin
      Dialog := new Message_Review_Dialog_Record;
      Initialize (Dialog, Kernel, Messages);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self     : not null access Message_Review_Dialog_Record'Class;
      Kernel   : not null access Kernel_Handle_Record'Class;
      Messages : CodePeer.Message_Vectors.Vector)
   is
      Scrolled      : Gtk.Scrolled_Window.Gtk_Scrolled_Window;
      Text_View     : Gtk.Text_View.Gtk_Text_View;
      Label         : Gtk.Label.Gtk_Label;
      Table         : Gtk.Table.Gtk_Table;
      Store         : Gtk.Tree_Store.Gtk_Tree_Store;
      Iter          : Gtk.Tree_Model.Gtk_Tree_Iter;
      Text_Renderer : Gtk.Cell_Renderer_Text.Gtk_Cell_Renderer_Text;
      Tree_View     : Gtk.Tree_View.Gtk_Tree_View;
      Column        : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
      Dummy_W       : Gtk.Widget.Gtk_Widget;
      pragma Warnings (Off, Dummy_W);
      Dummy_I       : Glib.Gint;
      pragma Warnings (Off, Dummy_I);

   begin
      Glib.Object.Initialize_Class_Record
        (Ancestor     => Gtk.Dialog.Get_Type,
         Signals      => Signals,
         Class_Record => Class_Record,
         Type_Name    => "CodePeerMultipleMessageReviewDialogV3",
         Parameters   => Signal_Parameters);

      GPS.Dialogs.Initialize
        (Self,
         Title  => -"CodePeer message review",
         Kernel => Kernel,
         Typ    => Class_Record.The_Type);
      GPS.Main_Window.Set_Default_Size_From_History
        (Self, "CodePeer message review", Kernel, 400, 400);

      --  Filter messages with non-editable audit trail.

      for Message of Messages loop
         if Message.Status_Editable then
            Self.Messages.Append (Message);
         end if;
      end loop;

      --  Messages view and underling model

      Gtk.Scrolled_Window.Gtk_New (Scrolled);
      Scrolled.Set_Policy
        (Gtk.Enums.Policy_Automatic, Gtk.Enums.Policy_Automatic);
      Self.Get_Content_Area.Pack_Start (Scrolled, False, False);

      Gtk.Tree_Store.Gtk_New (Store, Messages_Model_Types);

      Gtk.Tree_View.Gtk_New (Tree_View, Store);
      Scrolled.Add (Tree_View);

      Gtk.Tree_View_Column.Gtk_New (Column);
      Column.Set_Title (-"Ranking");
      Gtk.Cell_Renderer_Text.Gtk_New (Text_Renderer);
      Column.Pack_Start (Text_Renderer, False);
      Column.Add_Attribute
        (Text_Renderer, "text", Messages_Model_Ranking_Column);
      Dummy_I := Tree_View.Append_Column (Column);

      Gtk.Tree_View_Column.Gtk_New (Column);
      Column.Set_Title (-"Status");
      Gtk.Cell_Renderer_Text.Gtk_New (Text_Renderer);
      Column.Pack_Start (Text_Renderer, False);
      Column.Add_Attribute
        (Text_Renderer, "text", Messages_Model_Status_Column);
      Dummy_I := Tree_View.Append_Column (Column);

      Gtk.Tree_View_Column.Gtk_New (Column);
      Column.Set_Title (-"Location");
      Gtk.Cell_Renderer_Text.Gtk_New (Text_Renderer);
      Column.Pack_Start (Text_Renderer, False);
      Column.Add_Attribute
        (Text_Renderer, "text", Messages_Model_Location_Column);
      Dummy_I := Tree_View.Append_Column (Column);

      Gtk.Tree_View_Column.Gtk_New (Column);
      Column.Set_Title (-"Text");
      Gtk.Cell_Renderer_Text.Gtk_New (Text_Renderer);
      Column.Pack_Start (Text_Renderer, False);
      Column.Add_Attribute
        (Text_Renderer, "text", Messages_Model_Text_Column);
      Dummy_I := Tree_View.Append_Column (Column);

      --  Fill messages model

      for Message of Self.Messages loop
         declare
            Line_Image    : constant String :=
              Integer'Image (Message.Get_Line);
            Column_Image  : constant String :=
              GNATCOLL.Xref.Visible_Column'Image (Message.Get_Column);
            Location_Text : constant String :=
              Message.Get_File.Display_Base_Name
              & ':'
              & Line_Image (Line_Image'First + 1 .. Line_Image'Last)
              & ':'
              & Column_Image (Column_Image'First + 1 .. Column_Image'Last);

         begin
            Store.Append (Iter, Gtk.Tree_Model.Null_Iter);
            Set_All_And_Clear
              (Store, Iter,
               (Messages_Model_Ranking_Column  =>
                  As_String (Image (Message.Ranking)),
                Messages_Model_Status_Column   =>
                  As_String (Image (Message.Status)),
                Messages_Model_Location_Column =>
                  As_String (Location_Text),
                Messages_Model_Text_Column     =>
                  As_String (To_String (Message.Get_Text))));
         end;
      end loop;

      Gtk.Table.Gtk_New (Table, 2, 2, False);
      Self.Get_Content_Area.Pack_Start (Table, False, False);

      if not Self.Messages.Is_Empty then
         --  New status combobox and underling model

         Gtk.Label.Gtk_New (Label, "New status:");
         Table.Attach (Label, 0, 1, 0, 1);

         Self.New_Status := Create_Status_Combo_Box (Uncategorized);
         Table.Attach (Self.New_Status, 1, 2, 0, 1);

         --  "Approved by" entry

         Gtk.Label.Gtk_New (Label, "Approved by");
         Table.Attach (Label, 0, 1, 1, 2);

         Gtk.GEntry.Gtk_New (Self.Approved_Entry);
         Table.Attach (Self.Approved_Entry, 1, 2, 1, 2);

         --  Comment field

         Gtk.Label.Gtk_New (Label, "Comment");
         Label.Set_Alignment (0.0, 0.5);
         Self.Get_Content_Area.Pack_Start (Label, False, False);

         Gtk.Scrolled_Window.Gtk_New (Scrolled);
         Scrolled.Set_Policy
           (Gtk.Enums.Policy_Automatic, Gtk.Enums.Policy_Automatic);
         Self.Get_Content_Area.Pack_Start (Scrolled, False, False);

         Gtk.Text_View.Gtk_New (Text_View);
         Text_View.Set_Wrap_Mode (Gtk.Enums.Wrap_Word);
         Scrolled.Add (Text_View);

         Self.Comment_Buffer := Text_View.Get_Buffer;

         --  Dialog buttons

         Dummy_W :=
           Self.Add_Button (Gtk.Stock.Stock_Ok, Gtk.Dialog.Gtk_Response_OK);
         Message_Review_Callbacks.Connect
           (Dummy_W,
            Gtk.Button.Signal_Clicked,
            Message_Review_Callbacks.To_Marshaller (On_Ok'Access),
            Message_Review_Dialog (Self));
      end if;

      Dummy_W :=
        Self.Add_Button
          (Gtk.Stock.Stock_Cancel, Gtk.Dialog.Gtk_Response_Cancel);
      Message_Review_Callbacks.Connect
        (Dummy_W,
         Gtk.Button.Signal_Clicked,
         Message_Review_Callbacks.To_Marshaller (On_Cancel'Access),
         Message_Review_Dialog (Self));
   end Initialize;

   ---------------
   -- On_Cancel --
   ---------------

   procedure On_Cancel
     (Object : access Glib.Object.GObject_Record'Class;
      Self   : Message_Review_Dialog)
   is
      pragma Unreferenced (Object);

   begin
      Self.Destroy;
   end On_Cancel;

   -----------
   -- On_Ok --
   -----------

   procedure On_Ok
     (Object : access Glib.Object.GObject_Record'Class;
      Self   : Message_Review_Dialog)
   is
      pragma Unreferenced (Object);

      use type Glib.Signal_Name;

      Model      : constant Gtk.Tree_Store.Gtk_Tree_Store :=
                     -(Self.New_Status.Get_Model);
      Iter       : constant Gtk.Tree_Model.Gtk_Tree_Iter :=
                     Self.New_Status.Get_Active_Iter;
      Status     : constant CodePeer.Audit_Status_Kinds :=
                     CodePeer.Audit_Status_Kinds'Val
                       (Model.Get_Int
                          (Iter, Status_Model_Value_Column));
      Timestamp  : constant Unbounded_String :=
                     To_Unbounded_String
                       (Ada.Calendar.Formatting.Image (Ada.Calendar.Clock));
      Approved   : constant Unbounded_String :=
                     To_Unbounded_String (Self.Approved_Entry.Get_Text);
      Comment    : Unbounded_String;
      New_Record : CodePeer.Audit_Record_Access;
      Start_Iter : Gtk.Text_Iter.Gtk_Text_Iter;
      End_Iter   : Gtk.Text_Iter.Gtk_Text_Iter;

   begin
      Self.Comment_Buffer.Get_Start_Iter (Start_Iter);
      Self.Comment_Buffer.Get_End_Iter (End_Iter);
      Comment :=
        To_Unbounded_String
          (Self.Comment_Buffer.Get_Text (Start_Iter, End_Iter));

      --  Add new record and change message probability

      for Message of Self.Messages loop
         Message.Status := Status;
         New_Record :=
           new CodePeer.Audit_Record'
             (Timestamp   => Timestamp,
              Comment     => Comment,
              Approved_By => Approved,
              Status      => Status);
         Message.Audit.Prepend (New_Record);
      end loop;

      --  Emit signal

      Emit_By_Name
        (Self.Get_Object,
         CodePeer.Message_Review_Dialogs.Signal_Ok_Activated & ASCII.NUL);

      --  Hide dialog

      Self.Destroy;
   end On_Ok;

end CodePeer.Multiple_Message_Review_Dialogs;
