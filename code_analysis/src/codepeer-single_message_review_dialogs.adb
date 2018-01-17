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

with Ada.Calendar.Formatting;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces.C.Strings;
with System;

with Glib.Object;
with Glib_Values_Utils;        use Glib_Values_Utils;

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

with CodePeer.Message_Review_Dialogs.Utils;
use CodePeer.Message_Review_Dialogs.Utils;

package body CodePeer.Single_Message_Review_Dialogs is

   Messages_Model_Ranking_Column  : constant := 0;
   Messages_Model_Status_Column   : constant := 1;
   Messages_Model_Location_Column : constant := 2;
   Messages_Model_Text_Column     : constant := 3;

   Messages_Model_Types : constant Glib.GType_Array :=
     (Messages_Model_Ranking_Column  => Glib.GType_String,
      Messages_Model_Status_Column   => Glib.GType_String,
      Messages_Model_Location_Column => Glib.GType_String,
      Messages_Model_Text_Column     => Glib.GType_String);

   History_Model_Timestamp_Column : constant := 0;
   History_Model_Status_Column    : constant := 1;
   History_Model_Approved_Column  : constant := 2;
   History_Model_Comment_Column   : constant := 3;

   History_Model_Types : constant Glib.GType_Array :=
     (History_Model_Timestamp_Column => Glib.GType_String,
      History_Model_Status_Column    => Glib.GType_String,
      History_Model_Approved_Column  => Glib.GType_String,
      History_Model_Comment_Column   => Glib.GType_String);

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
      return Messages : CodePeer.Message_Vectors.Vector do
         Messages.Append (Self.Message);
      end return;
   end Get_Messages;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Dialog  : out Message_Review_Dialog;
      Kernel  : not null access Kernel_Handle_Record'Class;
      Message : CodePeer.Message_Access)
   is
   begin
      Dialog := new Message_Review_Dialog_Record;
      Initialize (Dialog, Kernel, Message);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self    : not null access Message_Review_Dialog_Record'Class;
      Kernel  : not null access Kernel_Handle_Record'Class;
      Message : CodePeer.Message_Access)
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

      procedure Process_Audit (Position : CodePeer.Audit_V3_Vectors.Cursor);
      --  Fill GtkTreeStore of history view

      -------------------
      -- Process_Audit --
      -------------------

      procedure Process_Audit (Position : CodePeer.Audit_V3_Vectors.Cursor)
      is
         Audit : constant CodePeer.Audit_Record_V3_Access :=
           CodePeer.Audit_V3_Vectors.Element (Position);
      begin
         Store.Append (Iter, Gtk.Tree_Model.Null_Iter);
         Set_All_And_Clear
           (Store, Iter,
            (0 => As_String (To_String (Audit.Timestamp)),
             1 => As_String (Image (Audit.Status)),
             2 => As_String (To_String (Audit.Approved_By)),
             3 => As_String (To_String (Audit.Comment))));
      end Process_Audit;

   begin
      Glib.Object.Initialize_Class_Record
        (Ancestor     => Gtk.Dialog.Get_Type,
         Signals      => Signals,
         Class_Record => Class_Record,
         Type_Name    => "CodePeerSingleMessageReviewDialog",
         Parameters   => Signal_Parameters);

      GPS.Dialogs.Initialize
        (Self,
         Title  => -"CodePeer message review",
         Kernel => Kernel,
         Typ    => Class_Record.The_Type);

      Self.Message := Message;

      --  Messages view and underling model

      Gtk.Scrolled_Window.Gtk_New (Scrolled);
      Scrolled.Set_Size_Request (Height => 70, Width => 700);
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

      Gtk.Table.Gtk_New (Table, 2, 4, False);
      Self.Get_Content_Area.Pack_Start (Table, False, False);

      if Message.Status_Editable then
         --  New status combobox and underling model

         Gtk.Label.Gtk_New (Label, "New status:");
         Table.Attach (Label, 0, 1, 2, 3);

         Self.New_Status := Create_Status_Combo_Box (Message.Status);
         Table.Attach (Self.New_Status, 1, 2, 2, 3);

         --  "Approved by" entry

         Gtk.Label.Gtk_New (Label, "Approved by");
         Table.Attach (Label, 0, 1, 3, 4);

         Gtk.GEntry.Gtk_New (Self.Approved_Entry);
         Table.Attach (Self.Approved_Entry, 1, 2, 3, 4);

         --  Comment field

         Gtk.Label.Gtk_New (Label, "Comment");
         Label.Set_Alignment (0.0, 0.5);
         Self.Get_Content_Area.Pack_Start (Label, False, False);

         Gtk.Scrolled_Window.Gtk_New (Scrolled);
         Scrolled.Set_Size_Request (Height => 200);
         Scrolled.Set_Policy
           (Gtk.Enums.Policy_Automatic, Gtk.Enums.Policy_Automatic);
         Self.Get_Content_Area.Pack_Start (Scrolled, False, False);

         Gtk.Text_View.Gtk_New (Text_View);
         Text_View.Set_Wrap_Mode (Gtk.Enums.Wrap_Word);
         Scrolled.Add (Text_View);

         Self.Comment_Buffer := Text_View.Get_Buffer;
      end if;

      --  History view and underling model

      Gtk.Scrolled_Window.Gtk_New (Scrolled);
      Scrolled.Set_Size_Request (Height => 300, Width => 700);
      Scrolled.Set_Policy
        (Gtk.Enums.Policy_Automatic, Gtk.Enums.Policy_Automatic);
      Self.Get_Content_Area.Pack_End (Scrolled, True, True);

      Gtk.Tree_Store.Gtk_New (Store, History_Model_Types);

      Message.Audit_V3.Iterate (Process_Audit'Access);

      Gtk.Tree_View.Gtk_New (Tree_View, Store);
      Scrolled.Add (Tree_View);

      Gtk.Tree_View_Column.Gtk_New (Column);
      Column.Set_Title (-"Timestamp");
      Gtk.Cell_Renderer_Text.Gtk_New (Text_Renderer);
      Column.Pack_Start (Text_Renderer, False);
      Column.Add_Attribute
        (Text_Renderer, "text", History_Model_Timestamp_Column);
      Dummy_I := Tree_View.Append_Column (Column);

      Gtk.Tree_View_Column.Gtk_New (Column);
      Column.Set_Title (-"Status");
      Gtk.Cell_Renderer_Text.Gtk_New (Text_Renderer);
      Column.Pack_Start (Text_Renderer, False);
      Column.Add_Attribute
        (Text_Renderer, "text", History_Model_Status_Column);
      Dummy_I := Tree_View.Append_Column (Column);

      Gtk.Tree_View_Column.Gtk_New (Column);
      Column.Set_Title (-"Approved by");
      Gtk.Cell_Renderer_Text.Gtk_New (Text_Renderer);
      Column.Pack_Start (Text_Renderer, False);
      Column.Add_Attribute
        (Text_Renderer, "text", History_Model_Approved_Column);
      Dummy_I := Tree_View.Append_Column (Column);

      Gtk.Tree_View_Column.Gtk_New (Column);
      Column.Set_Title (-"Comment");
      Column.Set_Resizable (True);
      Gtk.Cell_Renderer_Text.Gtk_New (Text_Renderer);
      Column.Pack_Start (Text_Renderer, False);
      Column.Add_Attribute
        (Text_Renderer, "text", History_Model_Comment_Column);
      Dummy_I := Tree_View.Append_Column (Column);

      --  Dialog buttons

      if Message.Status_Editable then
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
      New_Record : constant CodePeer.Audit_Record_V3_Access :=
                     new CodePeer.Audit_Record_V3;
      Start_Iter : Gtk.Text_Iter.Gtk_Text_Iter;
      End_Iter   : Gtk.Text_Iter.Gtk_Text_Iter;

   begin
      --  Add new record and change message probability

      New_Record.Status :=
        CodePeer.Audit_Status_Kinds'Val
          (Model.Get_Int (Iter, Status_Model_Value_Column));
      Self.Message.Status := New_Record.Status;

      Self.Comment_Buffer.Get_Start_Iter (Start_Iter);
      Self.Comment_Buffer.Get_End_Iter (End_Iter);
      New_Record.Comment :=
        To_Unbounded_String
          (Self.Comment_Buffer.Get_Text (Start_Iter, End_Iter));

      New_Record.Approved_By :=
        To_Unbounded_String (Self.Approved_Entry.Get_Text);

      New_Record.Timestamp :=
        To_Unbounded_String
          (Ada.Calendar.Formatting.Image (Ada.Calendar.Clock));
      Self.Message.Audit_V3.Prepend (New_Record);

      --  Emit signal

      Emit_By_Name
        (Self.Get_Object,
         CodePeer.Message_Review_Dialogs.Signal_Ok_Activated & ASCII.NUL);

      --  Hide dialog

      Self.Destroy;
   end On_Ok;

end CodePeer.Single_Message_Review_Dialogs;
