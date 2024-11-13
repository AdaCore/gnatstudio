------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2016-2024, AdaCore                     --
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
with Gtk.Frame;
with Interfaces.C.Strings;
with System;

with Glib.Object;
with Glib.Values;

with Gtkada.Dialogs;

with Gtk.Box;
with Gtk.Button;
with Gtk.Cell_Renderer_Text;
with Gtk.Dialog; use Gtk.Dialog;
with Gtk.Enums;
with Gtk.Handlers;
with Gtk.Label;
with Gtk.Paned;
with Gtk.Scrolled_Window;
with Gtk.Table;
with Gtk.Text_Iter;
with Gtk.Text_View;
with Gtk.Tree_Model;
with Gtk.Tree_Store; use Gtk.Tree_Store;
with Gtk.Tree_View_Column;
with Gtkada.Stock_Labels;

with GUI_Utils;

with GPS.Intl; use GPS.Intl;
with GPS.Dialogs;
with GPS.Editors;
with GPS.Main_Window;
with GPS.Kernel.MDI;

with String_Utils; use String_Utils;

with Glib_Values_Utils; use Glib_Values_Utils;

with CodePeer.Message_Review_Dialogs.Utils;
use CodePeer.Message_Review_Dialogs.Utils;

with VSS.Strings;
with VSS.Strings.Conversions;
with VSS.String_Vectors;

package body CodePeer.Multiple_Message_Review_Dialogs is

   Messages_Model_Id_Column       : constant := 0;
   Messages_Model_Ranking_Column  : constant := 1;
   Messages_Model_Status_Column   : constant := 2;
   Messages_Model_Location_Column : constant := 3;
   Messages_Model_Text_Column     : constant := 4;
   Messages_Model_File_Column     : constant := 5;
   Messages_Model_Line_Column     : constant := 6;
   Messages_Model_Col_Column      : constant := 7;

   Messages_Model_Types : constant Glib.GType_Array :=
     (Messages_Model_Id_Column       => Glib.GType_String,
      Messages_Model_Ranking_Column  => Glib.GType_String,
      Messages_Model_Status_Column   => Glib.GType_String,
      Messages_Model_Location_Column => Glib.GType_String,
      Messages_Model_Text_Column     => Glib.GType_String,
      Messages_Model_File_Column     => Glib.GType_String,
      Messages_Model_Line_Column     => Glib.GType_Int,
      Messages_Model_Col_Column      => Glib.GType_Int);

   History_Model_Id_Column        : constant := 0;
   History_Model_Timestamp_Column : constant := 1;
   History_Model_Status_Column    : constant := 2;
   History_Model_Approved_Column  : constant := 3;
   History_Model_Comment_Column   : constant := 4;

   History_Model_Types : constant Glib.GType_Array :=
     (History_Model_Id_Column        => Glib.GType_String,
      History_Model_Timestamp_Column => Glib.GType_String,
      History_Model_Status_Column    => Glib.GType_String,
      History_Model_Approved_Column  => Glib.GType_String,
      History_Model_Comment_Column   => Glib.GType_String);

   package Message_Review_Callbacks is
     new Gtk.Handlers.User_Callback
       (Glib.Object.GObject_Record, Message_Review_Dialog);

   procedure On_Close
     (Object : access Glib.Object.GObject_Record'Class;
      Self   : Message_Review_Dialog);

   procedure On_Cancel
     (Object : access Glib.Object.GObject_Record'Class;
      Self   : Message_Review_Dialog);

   procedure On_Next
     (Object : access Glib.Object.GObject_Record'Class;
      Self   : Message_Review_Dialog);
   --  Show next message

   procedure On_Previous
     (Object : access Glib.Object.GObject_Record'Class;
      Self   : Message_Review_Dialog);
   --  Show previous message

   procedure On_Apply
     (Object : access Glib.Object.GObject_Record'Class;
      Self   : Message_Review_Dialog);
   --  Called when the Apply button is pressed

   procedure Fill_Data (Self : Message_Review_Dialog);
   --  Fill data when we move to some message

   procedure Apply (Self : Message_Review_Dialog);
   --  Apply changes for the messages

   procedure On_Selection_Changed
     (View : access Glib.Object.GObject_Record'Class);
   --  Called when another messages are selected in the all-list

   function Get_Comment
     (Self : Message_Review_Dialog)
      return Unbounded_String;

   function Id_Sort_Func
     (Model : Gtk.Tree_Model.Gtk_Tree_Model;
      A     : Gtk.Tree_Model.Gtk_Tree_Iter;
      B     : Gtk.Tree_Model.Gtk_Tree_Iter) return Glib.Gint;

   function Location_Sort_Func
     (Model : Gtk.Tree_Model.Gtk_Tree_Model;
      A     : Gtk.Tree_Model.Gtk_Tree_Iter;
      B     : Gtk.Tree_Model.Gtk_Tree_Iter) return Glib.Gint;

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

   Placeholder : constant String := "Optional comment";

   -----------------
   -- Get_Comment --
   -----------------

   function Get_Comment
     (Self : Message_Review_Dialog)
      return Unbounded_String
   is
      Start_Iter : Gtk.Text_Iter.Gtk_Text_Iter;
      End_Iter   : Gtk.Text_Iter.Gtk_Text_Iter;
      Result     : Unbounded_String;

      function Mask_New_Lines (S : String) return Unbounded_String;
      --  Replace the new line ASCII.LF by "\n"

      function Mask_New_Lines (S : String) return Unbounded_String is
         Vector : constant VSS.String_Vectors.Virtual_String_Vector :=
           VSS.Strings.Conversions.To_Virtual_String (S).Split_Lines;
         First  : Boolean := True;
         Result : VSS.Strings.Virtual_String;
      begin
         for Line of Vector loop
            if not First then
               Result.Append ("\n");
            end if;

            Result.Append (Line);
            First := False;
         end loop;

         return VSS.Strings.Conversions.To_Unbounded_UTF_8_String (Result);
      end Mask_New_Lines;

   begin
      Self.Comment_Buffer.Get_Start_Iter (Start_Iter);
      Self.Comment_Buffer.Get_End_Iter (End_Iter);
      declare
         Text : constant String :=
           Self.Comment_Buffer.Get_Text
             (Start                => Start_Iter,
              The_End              => End_Iter,
              Include_Hidden_Chars => True);
      begin
         if Text /= Placeholder then
            Result := Mask_New_Lines (Text);
         end if;
      end;

      return Result;
   end Get_Comment;

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
      Module   : access CodePeer.Module.Module_Id_Record'Class;
      Messages : CodePeer.Message_Vectors.Vector)
   is
   begin
      Dialog := new Message_Review_Dialog_Record;
      Initialize (Dialog, Kernel, Module, Messages);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self     : not null access Message_Review_Dialog_Record'Class;
      Kernel   : not null access Kernel_Handle_Record'Class;
      Module   : access CodePeer.Module.Module_Id_Record'Class;
      Messages : CodePeer.Message_Vectors.Vector)
   is
      procedure Create_Column
        (Tree     : Gtk.Tree_View.Gtk_Tree_View;
         Name     : String;
         Col_Num  : Glib.Gint;
         Sortable : Boolean);
      --  Create column for the tree

      procedure Create_Columns
        (Tree     : Gtk.Tree_View.Gtk_Tree_View;
         Sortable : Boolean);
      --  Create columns for the tree

      Column        : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
      Text_Renderer : Gtk.Cell_Renderer_Text.Gtk_Cell_Renderer_Text;
      Dummy_I       : Glib.Gint;

      -------------------
      -- Create_Column --
      -------------------

      procedure Create_Column
        (Tree     : Gtk.Tree_View.Gtk_Tree_View;
         Name     : String;
         Col_Num  : Glib.Gint;
         Sortable : Boolean) is
      begin
         Gtk.Tree_View_Column.Gtk_New (Column);
         if Sortable then
            Gtk.Tree_View_Column.Set_Sort_Column_Id (Column, Col_Num);
         end if;
         Column.Set_Title (Name);
         Gtk.Cell_Renderer_Text.Gtk_New (Text_Renderer);
         Column.Pack_Start (Text_Renderer, False);
         Column.Add_Attribute (Text_Renderer, "text", Col_Num);
         Dummy_I := Tree.Append_Column (Column);
      end Create_Column;

      --------------------
      -- Create_Columns --
      --------------------

      procedure Create_Columns
        (Tree     : Gtk.Tree_View.Gtk_Tree_View;
         Sortable : Boolean) is
      begin
         Create_Column
           (Tree, "Id", Messages_Model_Id_Column, Sortable);
         Create_Column
           (Tree, "Ranking", Messages_Model_Ranking_Column, Sortable);
         Create_Column
           (Tree, "Status", Messages_Model_Status_Column, Sortable);
         Create_Column
           (Tree, "Location", Messages_Model_Location_Column, Sortable);
         Create_Column
           (Tree, "Text", Messages_Model_Text_Column, Sortable);
      end Create_Columns;

      Scrolled             : Gtk.Scrolled_Window.Gtk_Scrolled_Window;
      Text_View            : Gtk.Text_View.Gtk_Text_View;
      Label                : Gtk.Label.Gtk_Label;
      Table                : Gtk.Table.Gtk_Table;
      Review_Messages_View : Gtk.Tree_View.Gtk_Tree_View;
      Audit_View           : Gtk.Tree_View.Gtk_Tree_View;
      Dummy_W              : Gtk.Widget.Gtk_Widget;

      VPaned               : Gtk.Paned.Gtk_Paned;
      Paned_2              : Gtk.Paned.Gtk_Paned;
      Box                  : Gtk.Box.Gtk_Box;
      Frame                : Gtk.Frame.Gtk_Frame;

   begin
      Glib.Object.Initialize_Class_Record
        (Ancestor     => Gtk.Dialog.Get_Type,
         Signals      => Signals,
         Class_Record => Class_Record,
         Type_Name    => "CodePeerMultipleMessageReviewDialogV3",
         Parameters   => Signal_Parameters);

      GPS.Dialogs.Initialize
        (Self,
         Title  =>
           -(VSS.Strings.Conversions.To_UTF_8_String (CodePeer.Module_Name)
               & " message review"),
         Kernel => Kernel,
         Typ    => Class_Record.The_Type);
      GPS.Main_Window.Set_Default_Size_From_History
        (Self,
         VSS.Strings.Conversions.To_UTF_8_String (CodePeer.Module_Name)
           & " message review",
         Kernel,
         400,
         400);

      Self.Module := Module;
      Gtk.Paned.Gtk_New_Vpaned (VPaned);
      Self.Get_Content_Area.Pack_Start (VPaned);
      Gtk.Box.Gtk_New_Vbox (Box);
      VPaned.Pack1 (Box, True, True);

      --  List of all message

      Gtk.Scrolled_Window.Gtk_New (Scrolled);
      Scrolled.Set_Policy
        (Gtk.Enums.Policy_Automatic, Gtk.Enums.Policy_Automatic);
      Box.Pack_Start (Scrolled, True, True);

      Gtk.Tree_Store.Gtk_New (Self.All_Messages_Store, Messages_Model_Types);
      Gtk.Tree_View.Gtk_New (Self.All_Messages_View, Self.All_Messages_Store);
      Scrolled.Add (Self.All_Messages_View);
      Create_Columns (Self.All_Messages_View, True);
      Self.All_Messages_Store.Set_Sort_Func
        (Messages_Model_Id_Column, Id_Sort_Func'Access);
      Self.All_Messages_Store.Set_Sort_Func
        (Messages_Model_Location_Column, Location_Sort_Func'Access);

      Self.Fill_All_Messages;

      Self.All_Messages_View.Get_Selection.Set_Mode
        (Gtk.Enums.Selection_Multiple);
      Self.All_Messages_View.Get_Selection.On_Changed
        (On_Selection_Changed'Access, Self);

      --  Filter messages with non-editable audit trail.

      for Message of Messages loop
         if Message.Status_Editable then
            Self.Messages.Append (Message);
         end if;
      end loop;

      --  Reviewing messages view and underlying model

      Gtk.Frame.Gtk_New (Frame);
      Frame.Set_Label ("Reviewing:");
      Box.Pack_End (Frame);

      Gtk.Scrolled_Window.Gtk_New (Scrolled);
      Scrolled.Set_Policy
        (Gtk.Enums.Policy_Automatic, Gtk.Enums.Policy_Automatic);
      Frame.Add (Scrolled);

      Gtk.Tree_Store.Gtk_New
        (Self.Review_Messages_Store, Messages_Model_Types);
      Gtk.Tree_View.Gtk_New (Review_Messages_View, Self.Review_Messages_Store);
      Review_Messages_View.Get_Selection.Set_Mode
        (Gtk.Enums.Selection_None);
      Scrolled.Add (Review_Messages_View);
      Create_Columns (Review_Messages_View, False);

      Self.Fill_Messages;

      Gtk.Paned.Gtk_New_Vpaned (Paned_2);
      VPaned.Pack2 (Paned_2, Resize => True);
      Gtk.Box.Gtk_New_Vbox (Box);
      Paned_2.Pack1 (Box, Resize => True);
      Gtk.Table.Gtk_New (Table, 2, 4, False);
      Box.Pack_Start (Table, False, False);

      if not Self.Messages.Is_Empty then
         --  New status combobox and underling model

         Gtk.Label.Gtk_New (Label, "New status:");
         Table.Attach (Label, 0, 1, 0, 1);

         Self.New_Status := Create_Status_Combo_Box ("Uncategorized");
         Table.Attach (Self.New_Status, 1, 2, 0, 1);

         --  "Approved by" entry

         Gtk.Label.Gtk_New (Label, "Approved by");
         Table.Attach (Label, 0, 1, 1, 2);

         Gtk.GEntry.Gtk_New (Self.Approved_Entry);
         Self.Approved_Entry.Set_Name ("Codepeer_Approved_Entry");
         Table.Attach (Self.Approved_Entry, 1, 2, 1, 2);
         Self.Approved_Entry.Set_Text (CodePeer.Module.Get_Current_User);
         Self.Approved_Entry.Select_Region (0);

         --  Comment field

         Gtk.Label.Gtk_New (Label, "Comment:");
         Label.Set_Alignment (0.0, 0.5);
         Box.Pack_Start (Label, False, False);

         Gtk.Scrolled_Window.Gtk_New (Scrolled);
         Scrolled.Set_Policy
           (Gtk.Enums.Policy_Automatic, Gtk.Enums.Policy_Automatic);
         Box.Pack_Start (Scrolled, True, True);

         Gtk.Text_View.Gtk_New (Text_View);
         GUI_Utils.Set_Placeholder (Text_View, -Placeholder);
         Text_View.Set_Wrap_Mode (Gtk.Enums.Wrap_Word);
         Scrolled.Add (Text_View);

         Self.Comment_Buffer := Text_View.Get_Buffer;

         --  History view and underlying model

         Gtk.Scrolled_Window.Gtk_New (Scrolled);
         Scrolled.Set_Policy
           (Gtk.Enums.Policy_Automatic, Gtk.Enums.Policy_Automatic);
         Paned_2.Pack2 (Scrolled, Shrink => False);

         Gtk.Tree_Store.Gtk_New (Self.Audit_Store, History_Model_Types);

         Self.Fill_Audit;

         Gtk.Tree_View.Gtk_New (Audit_View, Self.Audit_Store);
         Scrolled.Add (Audit_View);

         Gtk.Tree_View_Column.Gtk_New (Column);
         Column.Set_Title (-"Id");
         Gtk.Cell_Renderer_Text.Gtk_New (Text_Renderer);
         Column.Pack_Start (Text_Renderer, False);
         Column.Add_Attribute (Text_Renderer, "text", History_Model_Id_Column);
         Dummy_I := Audit_View.Append_Column (Column);

         Gtk.Tree_View_Column.Gtk_New (Column);
         Column.Set_Title (-"Timestamp");
         Gtk.Cell_Renderer_Text.Gtk_New (Text_Renderer);
         Column.Pack_Start (Text_Renderer, False);
         Column.Add_Attribute
           (Text_Renderer, "text", History_Model_Timestamp_Column);
         Dummy_I := Audit_View.Append_Column (Column);

         Gtk.Tree_View_Column.Gtk_New (Column);
         Column.Set_Title (-"Status");
         Gtk.Cell_Renderer_Text.Gtk_New (Text_Renderer);
         Column.Pack_Start (Text_Renderer, False);
         Column.Add_Attribute
           (Text_Renderer, "text", History_Model_Status_Column);
         Dummy_I := Audit_View.Append_Column (Column);

         Gtk.Tree_View_Column.Gtk_New (Column);
         Column.Set_Title (-"Approved by");
         Gtk.Cell_Renderer_Text.Gtk_New (Text_Renderer);
         Column.Pack_Start (Text_Renderer, False);
         Column.Add_Attribute
           (Text_Renderer, "text", History_Model_Approved_Column);
         Dummy_I := Audit_View.Append_Column (Column);

         Gtk.Tree_View_Column.Gtk_New (Column);
         Column.Set_Title (-"Comment");
         Column.Set_Resizable (True);
         Gtk.Cell_Renderer_Text.Gtk_New (Text_Renderer);
         Column.Pack_Start (Text_Renderer, False);
         Column.Add_Attribute
           (Text_Renderer, "text", History_Model_Comment_Column);
         Dummy_I := Audit_View.Append_Column (Column);
      end if;

      --  Dialog buttons

      Dummy_W :=
        Self.Add_Button
          (Gtkada.Stock_Labels.Stock_Close, Gtk.Dialog.Gtk_Response_Close);
      Message_Review_Callbacks.Connect
        (Dummy_W,
         Gtk.Button.Signal_Clicked,
         Message_Review_Callbacks.To_Marshaller (On_Close'Access),
         Message_Review_Dialog (Self));

      Dummy_W :=
        Self.Add_Button
          (Gtkada.Stock_Labels.Stock_Cancel, Gtk.Dialog.Gtk_Response_Cancel);
      Message_Review_Callbacks.Connect
        (Dummy_W,
         Gtk.Button.Signal_Clicked,
         Message_Review_Callbacks.To_Marshaller (On_Cancel'Access),
         Message_Review_Dialog (Self));

      Dummy_W :=
        Self.Add_Button
          (Gtkada.Stock_Labels.Stock_Apply, Gtk.Dialog.Gtk_Response_None);
      Message_Review_Callbacks.Connect
        (Dummy_W,
         Gtk.Button.Signal_Clicked,
         Message_Review_Callbacks.To_Marshaller (On_Apply'Access),
         Message_Review_Dialog (Self));

      Self.Next :=
        Self.Add_Button
          (Gtkada.Stock_Labels.Stock_Media_Next, Gtk.Dialog.Gtk_Response_None);
      Message_Review_Callbacks.Connect
        (Self.Next,
         Gtk.Button.Signal_Clicked,
         Message_Review_Callbacks.To_Marshaller (On_Next'Access),
         Message_Review_Dialog (Self));

      Self.Previous :=
        Self.Add_Button
          (Gtkada.Stock_Labels.Stock_Media_Previous,
           Gtk.Dialog.Gtk_Response_None);
      Message_Review_Callbacks.Connect
        (Self.Previous,
         Gtk.Button.Signal_Clicked,
         Message_Review_Callbacks.To_Marshaller (On_Previous'Access),
         Message_Review_Dialog (Self));
   end Initialize;

   -----------------------
   -- Fill_All_Messages --
   -----------------------

   procedure Fill_All_Messages
     (Self : not null access Message_Review_Dialog_Record'Class)
   is
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter;
   begin
      Self.Set_Selection := True;
      Self.All_Messages_Store.Clear;

      for Message of Self.Module.Editable_Messages loop
         declare
            Location_Text : constant String :=
              Message.Get_File.Display_Base_Name
              & ':'
              & Image (Message.Get_Line)
              & ':'
              & Image (Integer (Message.Get_Column));

         begin
            Self.All_Messages_Store.Append (Iter, Gtk.Tree_Model.Null_Iter);
            Set_All_And_Clear
              (Self.All_Messages_Store, Iter,
               (Messages_Model_Id_Column       =>
                  As_String (Image (Message.Id)),
                Messages_Model_Ranking_Column  =>
                  As_String (Image (Message.Ranking)),
                Messages_Model_Status_Column   =>
                  As_String (Image (Message.Status)),
                Messages_Model_Location_Column =>
                  As_String (Location_Text),
                Messages_Model_Text_Column     =>
                  As_String (To_String (Message.Get_Text)),
                Messages_Model_File_Column      =>
                  As_String (Message.Get_File.Display_Base_Name),
                Messages_Model_Line_Column      =>
                  As_Int (Glib.Gint (Message.Get_Line)),
                Messages_Model_Col_Column       =>
                  As_Int (Glib.Gint (Message.Get_Column)))
              );
         end;
      end loop;
      Self.Set_Selection := False;
   end Fill_All_Messages;

   -------------------
   -- Fill_Messages --
   -------------------

   procedure Fill_Messages
     (Self : not null access Message_Review_Dialog_Record'Class)
   is
      use Glib.Values;
      use type Gtk.Tree_Model.Gtk_Tree_Iter;
      use type Gtk.Tree_Model.Gtk_Tree_Path;

      Iter  : Gtk.Tree_Model.Gtk_Tree_Iter;
      Value : Glib.Values.GValue;
      Path  : Gtk.Tree_Model.Gtk_Tree_Path;
   begin
      Self.Review_Messages_Store.Clear;

      for Message of Self.Messages loop
         declare
            Location_Text : constant String :=
              Message.Get_File.Display_Base_Name
              & ':'
              & Image (Message.Get_Line)
              & ':'
              & Image (Integer (Message.Get_Column));

         begin
            Self.Review_Messages_Store.Append (Iter, Gtk.Tree_Model.Null_Iter);
            Set_All_And_Clear
              (Self.Review_Messages_Store, Iter,
               (Messages_Model_Id_Column  =>
                  As_String (Image (Message.Id)),
                Messages_Model_Ranking_Column  =>
                  As_String (Image (Message.Ranking)),
                Messages_Model_Status_Column   =>
                  As_String (Image (Message.Status)),
                Messages_Model_Location_Column =>
                  As_String (Location_Text),
                Messages_Model_Text_Column     =>
                  As_String (To_String (Message.Get_Text))));
         end;
      end loop;

      --  Select reviewing messages in the list
      if not Self.Set_Selection then
         Self.Set_Selection := True;
         Self.All_Messages_View.Get_Selection.Unselect_All;

         Iter := Self.All_Messages_Store.Get_Iter_First;
         while Iter /= Gtk.Tree_Model.Null_Iter loop
            for Message of Self.Messages loop
               Self.All_Messages_Store.Get_Value
                 (Iter, Messages_Model_Id_Column, Value);

               if Message.Id = Natural'Value (Get_String (Value)) then
                  Self.All_Messages_View.Get_Selection.Select_Iter (Iter);
                  if Path = Gtk.Tree_Model.Null_Gtk_Tree_Path then
                     Path := Self.All_Messages_Store.Get_Path (Iter);
                  end if;
               end if;

               Unset (Value);
            end loop;

            Self.All_Messages_Store.Next (Iter);
         end loop;

         if Path /= Gtk.Tree_Model.Null_Gtk_Tree_Path then
            Self.All_Messages_View.Scroll_To_Cell
              (Path,
               Column    => null,
               Use_Align => True,
               Row_Align => 0.5,
               Col_Align => 0.0);
            Gtk.Tree_Model.Path_Free (Path);
         end if;

         Self.Set_Selection := False;
      end if;
   end Fill_Messages;

   ----------------
   -- Fill_Audit --
   ----------------

   procedure Fill_Audit
     (Self : not null access Message_Review_Dialog_Record'Class)
   is
      Iter       : Gtk.Tree_Model.Gtk_Tree_Iter;
      Current_Id : Integer := 0;

      procedure Process_Audit (Position : CodePeer.Audit_Vectors.Cursor);
      --  Fill GtkTreeStore of history view

      -------------------
      -- Process_Audit --
      -------------------

      procedure Process_Audit (Position : CodePeer.Audit_Vectors.Cursor) is
         Audit : constant CodePeer.Audit_Record_Access :=
           CodePeer.Audit_Vectors.Element (Position);

         function Unmask_New_Lines (From : Unbounded_String) return String;
         --  Replace the "\n" by ASCII.LF

         function Unmask_New_Lines (From : Unbounded_String) return String is
            S      : Unbounded_String := From;
            Vector : VSS.String_Vectors.Virtual_String_Vector;
            Idx    : Natural;
         begin
            loop
               Idx := S.Index ("\n");
               if Idx > 0 then
                  Vector.Append
                    (VSS.Strings.Conversions.To_Virtual_String
                       (S.Slice (1, Idx - 1)));
                  S.Delete (1, Idx + 1);
               else
                  Vector.Append
                    (VSS.Strings.Conversions.To_Virtual_String (S));
                  exit;
               end if;
            end loop;

            return VSS.Strings.Conversions.To_UTF_8_String
              (Vector.Join_Lines
                 (Terminator     => VSS.Strings.LF,
                  Terminate_Last => False));
         end Unmask_New_Lines;

      begin
         Self.Audit_Store.Append (Iter, Gtk.Tree_Model.Null_Iter);
         Set_All_And_Clear
           (Self.Audit_Store, Iter,
            (0 => As_String (Image (Current_Id)),
             1 => As_String (To_String (Audit.Timestamp)),
             2 => As_String (Image (Audit.Status)),
             3 => As_String (To_String (Audit.Approved_By)),
             4 => As_String (Unmask_New_Lines (Audit.Comment))));
      end Process_Audit;

   begin
      Self.Audit_Store.Clear;

      for Message of Self.Messages loop
         Current_Id := Message.Id;
         Message.Audit.Iterate (Process_Audit'Access);
      end loop;
   end Fill_Audit;

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

   -----------------
   -- On_Previous --
   -----------------

   procedure On_Previous
     (Object : access Glib.Object.GObject_Record'Class;
      Self   : Message_Review_Dialog)
   is
      pragma Unreferenced (Object);
      M : CodePeer.Message_Access;
   begin
      if Self.Messages.Is_Empty then
         return;
      end if;

      M := Self.Module.Previous_Editable_Message (Self.Messages.First_Element);

      if M /= null then
         Self.Messages.Clear;
         Self.Messages.Append (M);
         Self.Fill_Data;
      end if;
   end On_Previous;

   -------------
   -- On_Next --
   -------------

   procedure On_Next
     (Object : access Glib.Object.GObject_Record'Class;
      Self   : Message_Review_Dialog)
   is
      pragma Unreferenced (Object);
      M : CodePeer.Message_Access;
   begin
      if Self.Messages.Is_Empty then
         return;
      end if;

      M := Self.Module.Next_Editable_Message (Self.Messages.Last_Element);

      if M /= null then
         Self.Messages.Clear;
         Self.Messages.Append (M);
         Self.Fill_Data;
      end if;
   end On_Next;

   ---------------
   -- Fill_Data --
   ---------------

   procedure Fill_Data (Self : Message_Review_Dialog)
   is
      use GPS.Editors;
   begin
      Self.Fill_Messages;
      Self.Fill_Audit;

      if not Self.Messages.Is_Empty then
         declare
            M      : constant CodePeer.Message_Access :=
              Self.Messages.First_Element;
            Editor : constant Editor_Buffer'Class :=
              Self.Module.Kernel.Get_Buffer_Factory.Get
                (File => M.Get_File, Open_View => True);
         begin
            if Editor /= Nil_Editor_Buffer then
               Editor.Current_View.Cursor_Goto (M.Get_Editor_Mark.Location);
            end if;
         end;
      end if;
   end Fill_Data;

   --------------
   -- On_Close --
   --------------

   procedure On_Close
     (Object : access Glib.Object.GObject_Record'Class;
      Self   : Message_Review_Dialog)
   is
      pragma Unreferenced (Object);

      function Changed return Boolean;
      --  Return True if audit fields contain not default values

      Model      : constant Gtk.Tree_Store.Gtk_Tree_Store :=
        -(Self.New_Status.Get_Model);
      Iter       : constant Gtk.Tree_Model.Gtk_Tree_Iter :=
        Self.New_Status.Get_Active_Iter;
      Status     : constant CodePeer.Audit_Status_Kinds := Get_Status
        (Positive
           (Model.Get_Int (Iter, Status_Model_Value_Column)));
      Comment    : constant Unbounded_String := Self.Get_Comment;

      -------------
      -- Changed --
      -------------

      function Changed return Boolean is
      begin
         if Self.Messages.Is_Empty then
            return False;
         end if;

         if Comment.Length /= 0 then
            return True;
         end if;

         return Status.Category /= Uncategorized
           and then Self.Messages.First_Element.Status /= Status;
      end Changed;

   begin
      if Changed then
         declare
            use Gtkada.Dialogs;
            use GUI_Utils;

            Button : Message_Dialog_Buttons;

         begin
            Button := GPS_Message_Dialog
              (Msg     => -"Existing reviews were not applied yet."
               & " Do you want to apply them or exit and discard them ?",
               Buttons => Button_Yes or Button_No or Button_Cancel,
               Parent  => GPS.Kernel.MDI.Get_Current_Window (Self.Kernel));

            if Button = Button_Yes then
               Self.Apply;

            elsif Button = Button_Cancel then
               return;
            end if;
         end;
      end if;

      --  Hide dialog
      Self.Destroy;
   end On_Close;

   --------------
   -- On_Apply --
   --------------

   procedure On_Apply
     (Object : access Glib.Object.GObject_Record'Class;
      Self   : Message_Review_Dialog)
   is
      pragma Unreferenced (Object);
   begin
      Self.Apply;
      Self.Fill_All_Messages;
      Self.Fill_Data;
   end On_Apply;

   -----------
   -- Apply --
   -----------

   procedure Apply (Self : Message_Review_Dialog)
   is
      use type Glib.Signal_Name;

      Model      : constant Gtk.Tree_Store.Gtk_Tree_Store :=
                     -(Self.New_Status.Get_Model);
      Iter       : constant Gtk.Tree_Model.Gtk_Tree_Iter :=
                     Self.New_Status.Get_Active_Iter;
      Status     : constant CodePeer.Audit_Status_Kinds :=
                     Get_Status (Positive
                       (Model.Get_Int (Iter, Status_Model_Value_Column)));
      Timestamp  : constant Unbounded_String :=
                     To_Unbounded_String
                       (Ada.Calendar.Formatting.Image (Ada.Calendar.Clock));
      Approved   : constant Unbounded_String :=
                     To_Unbounded_String (Self.Approved_Entry.Get_Text);
      Comment    : constant Unbounded_String := Self.Get_Comment;
      New_Record : CodePeer.Audit_Record_Access;

   begin
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

      Self.New_Status.Set_Active (5);
      Self.Comment_Buffer.Set_Text ("");
   end Apply;

   --------------------------
   -- On_Selection_Changed --
   --------------------------

   procedure On_Selection_Changed
     (View : access Glib.Object.GObject_Record'Class)
   is
      Self  : constant Message_Review_Dialog := Message_Review_Dialog (View);
      Ids   : Natural_Sets.Set;
      Dummy : Boolean;

      procedure Selected
        (Model : Gtk.Tree_Model.Gtk_Tree_Model;
         Path  : Gtk.Tree_Model.Gtk_Tree_Path;
         Iter  : Gtk.Tree_Model.Gtk_Tree_Iter);

      --------------
      -- Selected --
      --------------

      procedure Selected
        (Model : Gtk.Tree_Model.Gtk_Tree_Model;
         Path  : Gtk.Tree_Model.Gtk_Tree_Path;
         Iter  : Gtk.Tree_Model.Gtk_Tree_Iter)
      is
         pragma Unreferenced (Path);
         use Glib.Values;

         Value : Glib.Values.GValue;
      begin
         Model.Get_Value (Iter, Messages_Model_Id_Column, Value);
         Ids.Include (Natural'Value (Get_String (Value)));
         Unset (Value);
      end Selected;

   begin
      if Self.Set_Selection then
         return;
      end if;

      Self.All_Messages_View.Get_Selection.Selected_Foreach
        (Selected'Unrestricted_Access);

      Dummy := Self.Module.Get_Messages (Ids, Self.Messages);
      Self.Set_Selection := True;
      Self.Fill_Data;
      Self.Set_Selection := False;
   end On_Selection_Changed;

   ------------------
   -- Id_Sort_Func --
   ------------------

   function Id_Sort_Func
     (Model : Gtk.Tree_Model.Gtk_Tree_Model;
      A     : Gtk.Tree_Model.Gtk_Tree_Iter;
      B     : Gtk.Tree_Model.Gtk_Tree_Iter) return Glib.Gint
   is
      use type Glib.Gint;

      S_A : constant String := Model.Get_String (A, Messages_Model_Id_Column);
      S_B : constant String := Model.Get_String (B, Messages_Model_Id_Column);
   begin
      if Natural'Value (S_A) < Natural'Value (S_B) then
         return -1;
      else
         return 1;
      end if;
   end Id_Sort_Func;

   ------------------------
   -- Location_Sort_Func --
   ------------------------

   function Location_Sort_Func
     (Model : Gtk.Tree_Model.Gtk_Tree_Model;
      A     : Gtk.Tree_Model.Gtk_Tree_Iter;
      B     : Gtk.Tree_Model.Gtk_Tree_Iter) return Glib.Gint
   is
      use type Glib.Gint;

      Name_A : constant String := Model.Get_String
        (A, Messages_Model_File_Column);
      Name_B : constant String := Model.Get_String
        (B, Messages_Model_File_Column);

      Line_A : constant Glib.Gint := Model.Get_Int
        (A, Messages_Model_Line_Column);
      Line_B : constant Glib.Gint := Model.Get_Int
        (B, Messages_Model_Line_Column);

      Col_A : constant Glib.Gint := Model.Get_Int
        (A, Messages_Model_Col_Column);
      Col_B : constant Glib.Gint := Model.Get_Int
        (B, Messages_Model_Col_Column);

   begin
      if Name_A < Name_B then
         return -1;

      elsif Name_A > Name_B then
         return 1;

      else
         if Line_A < Line_B then
            return -1;

         elsif Line_A > Line_B then
            return 1;

         else
            if Col_A < Col_B then
               return -1;

            else
               return 1;
            end if;
         end if;
      end if;
   end Location_Sort_Func;

end CodePeer.Multiple_Message_Review_Dialogs;
