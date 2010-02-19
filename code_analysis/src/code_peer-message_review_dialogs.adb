-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2009-2010, AdaCore              --
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

with Ada.Calendar.Formatting;
with Interfaces.C.Strings;
with System;

with Glib.Object;
with Gtk.Button;
with Gtk.Cell_Layout;
with Gtk.Cell_Renderer_Text;
with Gtk.Enums;
with Gtk.GEntry;
with Gtk.Handlers;
with Gtk.Label;
with Gtk.Scrolled_Window;
with Gtk.Stock;
with Gtk.Table;
with Gtk.Text_Iter;
with Gtk.Text_View;
with Gtk.Tree_Model;
with Gtk.Tree_Store;
with Gtk.Tree_View;
with Gtk.Tree_View_Column;
with Gtk.Widget;

with GPS.Intl; use GPS.Intl;

package body Code_Peer.Message_Review_Dialogs is

   Probability_Model_Label_Column     : constant := 0;
   Probability_Model_Changed_Column : constant := 1;
   Probability_Model_New_Level_Column : constant := 2;

   Probability_Model_Types : constant Glib.GType_Array :=
     (Probability_Model_Label_Column     => Glib.GType_String,
      Probability_Model_Changed_Column => Glib.GType_Boolean,
      Probability_Model_New_Level_Column => Glib.GType_Int);

   History_Model_Timestamp_Column   : constant := 0;
   History_Model_Probability_Column : constant := 1;
   History_Model_Comment_Column     : constant := 2;

   History_Model_Types : constant Glib.GType_Array :=
     (History_Model_Timestamp_Column   => Glib.GType_String,
      History_Model_Probability_Column => Glib.GType_String,
      History_Model_Comment_Column     => Glib.GType_String);

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

   Class_Record : Glib.Object.GObject_Class := Glib.Object.Uninitialized_Class;

   Signals : constant Interfaces.C.Strings.chars_ptr_array :=
     (1 => Interfaces.C.Strings.New_String (String (Signal_Ok_Activated)));

   Signal_Parameters : constant Glib.Object.Signal_Parameter_Types :=
     (1 => (1 => Glib.GType_None));

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Dialog  : in out Message_Review_Dialog;
      Message : Code_Peer.Message_Access)
   is
   begin
      Dialog := new Message_Review_Dialog_Record;
      Initialize (Dialog, Message);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self    : not null access Message_Review_Dialog_Record'Class;
      Message : Code_Peer.Message_Access)
   is
      Scrolled      : Gtk.Scrolled_Window.Gtk_Scrolled_Window;
      Text_View     : Gtk.Text_View.Gtk_Text_View;
      Label         : Gtk.Label.Gtk_Label;
      Table         : Gtk.Table.Gtk_Table;
      Store         : Gtk.Tree_Store.Gtk_Tree_Store;
      Iter          : Gtk.Tree_Model.Gtk_Tree_Iter;
      Text_Entry    : Gtk.GEntry.Gtk_Entry;
      Text_Renderer : Gtk.Cell_Renderer_Text.Gtk_Cell_Renderer_Text;
      Tree_View     : Gtk.Tree_View.Gtk_Tree_View;
      Column        : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
      Dummy_W       : Gtk.Widget.Gtk_Widget;
      pragma Warnings (Off, Dummy_W);
      Dummy_I       : Glib.Gint;
      pragma Warnings (Off, Dummy_I);

      procedure Process_Audit (Position : Code_Peer.Audit_Vectors.Cursor);

      function Probability_Image
        (Probability : Code_Peer.Message_Probability_Level) return String;

      -----------------------
      -- Probability_Image --
      -----------------------

      function Probability_Image
        (Probability : Code_Peer.Message_Probability_Level) return String is
      begin
         case Probability is
            when Code_Peer.Informational =>
               return "Info";

            when Code_Peer.Low =>
               return "Low";

            when Code_Peer.Medium =>
               return "Medium";

            when Code_Peer.High =>
               return "High";

            when Code_Peer.Suppressed =>
               return "Suppressed";
         end case;
      end Probability_Image;

      -------------------
      -- Process_Audit --
      -------------------

      procedure Process_Audit (Position : Code_Peer.Audit_Vectors.Cursor) is
         Audit : constant Code_Peer.Audit_Record_Access :=
                   Code_Peer.Audit_Vectors.Element (Position);

      begin
         Store.Append (Iter, Gtk.Tree_Model.Null_Iter);
         Store.Set
           (Iter,
            History_Model_Timestamp_Column,
            Audit.Timestamp.all);

         if Audit.Probability_Changed then
            Store.Set
              (Iter,
               History_Model_Probability_Column,
               Probability_Image (Audit.Probability));

         else
            Store.Set
              (Iter,
               History_Model_Probability_Column,
               "");
         end if;

         Store.Set
           (Iter,
            History_Model_Comment_Column,
            Audit.Comment.all);
      end Process_Audit;

   begin
      Gtk.Dialog.Initialize (Self);
      Glib.Object.Initialize_Class_Record
        (Self,
         Signals,
         Class_Record,
         "CodePeerMessageReviewDialog",
         Signal_Parameters);
      Self.Set_Title (-"CodePeer message review");

      Self.Message := Message;

      Gtk.Table.Gtk_New (Table, 2, 3, False);
      Self.Get_Vbox.Pack_Start (Table, False, False);

      Gtk.Label.Gtk_New (Label, "Original ranking");
      Table.Attach (Label, 0, 1, 0, 1);

      Gtk.GEntry.Gtk_New (Text_Entry);
      Text_Entry.Set_Editable (False);
      Text_Entry.Set_Text (Probability_Image (Message.Computed_Probability));
      Table.Attach (Text_Entry, 1, 2, 0, 1);

      Gtk.Label.Gtk_New (Label, "Current ranking");
      Table.Attach (Label, 0, 1, 1, 2);

      Gtk.GEntry.Gtk_New (Text_Entry);
      Text_Entry.Set_Editable (False);
      Text_Entry.Set_Text (Probability_Image (Message.Current_Probability));
      Table.Attach (Text_Entry, 1, 2, 1, 2);

      --  New probability combobox and underling model

      Gtk.Label.Gtk_New (Label, "New ranking:");
      Table.Attach (Label, 0, 1, 2, 3);

      Gtk.Tree_Store.Gtk_New (Store, Probability_Model_Types);

      Gtk.Combo_Box.Gtk_New_With_Model (Self.New_Probability, Store);
      Table.Attach (Self.New_Probability, 1, 2, 2, 3);

      Gtk.Cell_Renderer_Text.Gtk_New (Text_Renderer);
      Gtk.Cell_Layout.Pack_Start
        (Gtk.Combo_Box."+" (Self.New_Probability), Text_Renderer, True);
      Gtk.Cell_Layout.Add_Attribute
        (Gtk.Combo_Box."+" (Self.New_Probability),
         Text_Renderer,
         "text",
         Probability_Model_Label_Column);

      Store.Append (Iter, Gtk.Tree_Model.Null_Iter);
      Store.Set (Iter, Probability_Model_Label_Column, -"Unchanged");
      Store.Set (Iter, Probability_Model_Changed_Column, False);
      Self.New_Probability.Set_Active_Iter (Iter);

      Store.Append (Iter, Gtk.Tree_Model.Null_Iter);
      Store.Set (Iter, Probability_Model_Label_Column, -"Low");
      Store.Set (Iter, Probability_Model_Changed_Column, True);
      Store.Set
        (Iter,
         Probability_Model_New_Level_Column,
         Message_Probability_Level'Pos (Low));

      Store.Append (Iter, Gtk.Tree_Model.Null_Iter);
      Store.Set (Iter, Probability_Model_Label_Column, -"Medium");
      Store.Set (Iter, Probability_Model_Changed_Column, True);
      Store.Set
        (Iter,
         Probability_Model_New_Level_Column,
         Message_Probability_Level'Pos (Medium));

      Store.Append (Iter, Gtk.Tree_Model.Null_Iter);
      Store.Set (Iter, Probability_Model_Label_Column, -"High");
      Store.Set (Iter, Probability_Model_Changed_Column, True);
      Store.Set
        (Iter,
         Probability_Model_New_Level_Column,
         Message_Probability_Level'Pos (High));

      Store.Append (Iter, Gtk.Tree_Model.Null_Iter);
      Store.Set (Iter, Probability_Model_Label_Column, -"Not an error");
      Store.Set (Iter, Probability_Model_Changed_Column, True);
      Store.Set
        (Iter,
         Probability_Model_New_Level_Column,
         Message_Probability_Level'Pos (Suppressed));

      --  Comment field

      Gtk.Label.Gtk_New (Label, "Comment");
      Label.Set_Alignment (0.0, 0.5);
      Self.Get_Vbox.Pack_Start (Label, False, False);

      Gtk.Scrolled_Window.Gtk_New (Scrolled);
      Scrolled.Set_Size_Request (Height => 200);
      Scrolled.Set_Policy
        (Gtk.Enums.Policy_Automatic, Gtk.Enums.Policy_Automatic);
      Self.Get_Vbox.Pack_Start (Scrolled, False, False);

      Gtk.Text_View.Gtk_New (Text_View);
      Text_View.Set_Wrap_Mode (Gtk.Enums.Wrap_Word);
      Scrolled.Add (Text_View);

      Self.Comment_Buffer := Text_View.Get_Buffer;

      --  History view and underling model

      Gtk.Scrolled_Window.Gtk_New (Scrolled);
      Scrolled.Set_Size_Request (Height => 300, Width => 700);
      Scrolled.Set_Policy
        (Gtk.Enums.Policy_Automatic, Gtk.Enums.Policy_Automatic);
      Self.Get_Vbox.Pack_End (Scrolled, True, True);

      Gtk.Tree_Store.Gtk_New (Store, History_Model_Types);

      Message.Audit.Iterate (Process_Audit'Access);

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
      Column.Set_Title (-"Ranking");
      Gtk.Cell_Renderer_Text.Gtk_New (Text_Renderer);
      Column.Pack_Start (Text_Renderer, False);
      Column.Add_Attribute
        (Text_Renderer, "text", History_Model_Probability_Column);
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

      Dummy_W :=
        Self.Add_Button (Gtk.Stock.Stock_Ok, Gtk.Dialog.Gtk_Response_OK);
      Message_Review_Callbacks.Connect
        (Dummy_W,
         Gtk.Button.Signal_Clicked,
         Message_Review_Callbacks.To_Marshaller (On_Ok'Access),
         Message_Review_Dialog (Self));

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

      Model               : constant Gtk.Tree_Store.Gtk_Tree_Store :=
                              Gtk.Tree_Store.Gtk_Tree_Store
                                (Self.New_Probability.Get_Model);
      Iter                : constant Gtk.Tree_Model.Gtk_Tree_Iter :=
                              Self.New_Probability.Get_Active_Iter;
      Probability_Changed : constant Boolean :=
                              Model.Get_Boolean
                                (Iter, Probability_Model_Changed_Column);
      New_Record          : constant Code_Peer.Audit_Record_Access :=
                              new Code_Peer.Audit_Record (Probability_Changed);
      Start_Iter          : Gtk.Text_Iter.Gtk_Text_Iter;
      End_Iter            : Gtk.Text_Iter.Gtk_Text_Iter;

   begin
      --  Add new record and change message probability

      if Probability_Changed then
         New_Record.Probability :=
           Code_Peer.Message_Probability_Level'Val
             (Model.Get_Int (Iter, Probability_Model_New_Level_Column));
         Self.Message.Current_Probability := New_Record.Probability;
      end if;

      Self.Comment_Buffer.Get_Start_Iter (Start_Iter);
      Self.Comment_Buffer.Get_End_Iter (End_Iter);

      New_Record.Comment :=
        new String'(Self.Comment_Buffer.Get_Text (Start_Iter, End_Iter));
      New_Record.Timestamp :=
        new String'(Ada.Calendar.Formatting.Image (Ada.Calendar.Clock));
      Self.Message.Audit.Prepend (New_Record);

      --  Emit signal

      Emit_By_Name (Self.Get_Object, Signal_Ok_Activated & ASCII.NUL);

      --  Hide dialog

      Self.Destroy;
   end On_Ok;

end Code_Peer.Message_Review_Dialogs;
