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
with Interfaces.C.Strings;

with Glib.Object;
with Glib.Values;
with Gtk.Cell_Renderer_Text;
with Gtk.Cell_Renderer_Toggle;
with Gtk.Enums;
with Gtk.Handlers;
with Gtk.Object;
with Gtk.Tree_Model;
with Gtk.Tree_View_Column;

with GPS.Intl; use GPS.Intl;

package body Code_Peer.Message_Categories_Criteria_Editors is

   procedure On_Toggle_Category_Visibility
     (Object : access
        Gtk.Cell_Renderer_Toggle.Gtk_Cell_Renderer_Toggle_Record'Class;
      Path   : Interfaces.C.Strings.chars_ptr;
      Self   : Message_Categories_Criteria_Editor);
   --  Called on click on the list's item.

   procedure On_Select_All_Toggled
     (Object : access Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record'Class;
      Self   : Message_Categories_Criteria_Editor);
   --  Called on click on the column header.

   procedure On_Model_Row_Changed
     (Object : access
        Code_Peer.Message_Categories_Criteria_Models.
          Categories_Criteria_Model_Record'Class;
      Self   : Message_Categories_Criteria_Editor);
   --  Called on changes in the underlying model.

   procedure On_Destroy
     (Self : access Message_Categories_Criteria_Editor_Record'Class);
   --  Called on widget destroy.

   package Cell_Renderer_Toggle_Callbacks is
     new Gtk.Handlers.User_Callback
           (Gtk.Cell_Renderer_Toggle.Gtk_Cell_Renderer_Toggle_Record,
            Message_Categories_Criteria_Editor);

   package Cell_Renderer_Toggle_Callbacks_Marshallers is
     new Cell_Renderer_Toggle_Callbacks.Marshallers.Generic_Marshaller
           (Interfaces.C.Strings.chars_ptr, Glib.Values.Get_Chars);

   package Tree_View_Column_Callbacks is
     new Gtk.Handlers.User_Callback
           (Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record,
            Message_Categories_Criteria_Editor);

   package Message_Categories_Criteria_Editor_Callbacks is
     new Gtk.Handlers.Callback (Message_Categories_Criteria_Editor_Record);

   package Message_Categories_Criteria_Model_Callbacks is
     new Gtk.Handlers.User_Callback
           (Code_Peer.Message_Categories_Criteria_Models.
              Categories_Criteria_Model_Record,
            Message_Categories_Criteria_Editor);

   Class_Record : Glib.Object.GObject_Class := Glib.Object.Uninitialized_Class;

   Signals : constant Interfaces.C.Strings.chars_ptr_array :=
     (1 => Interfaces.C.Strings.New_String (String (Signal_Criteria_Changed)));

   Signal_Parameters : constant Glib.Object.Signal_Parameter_Types :=
     (1 => (1 => Glib.GType_None));

   ----------------------------
   -- Get_Visible_Categories --
   ----------------------------

   function Get_Visible_Categories
     (Self : access Message_Categories_Criteria_Editor_Record'Class)
      return Code_Peer.Message_Category_Sets.Set
   is
   begin
      return Self.Model.Get_Visible_Categories;
   end Get_Visible_Categories;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Editor     : in out Message_Categories_Criteria_Editor;
      Categories : Code_Peer.Message_Category_Sets.Set) is
   begin
      Editor := new Message_Categories_Criteria_Editor_Record;
      Initialize (Editor, Categories);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self       :
        not null access Message_Categories_Criteria_Editor_Record'Class;
      Categories : Code_Peer.Message_Category_Sets.Set)
   is
      Column          : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
      Text_Renderer   : Gtk.Cell_Renderer_Text.Gtk_Cell_Renderer_Text;
      Toggle_Renderer : Gtk.Cell_Renderer_Toggle.Gtk_Cell_Renderer_Toggle;
      Dummy           : Glib.Gint;
      pragma Warnings (Off, Dummy);

   begin
      Gtk.Scrolled_Window.Initialize (Self);
      Glib.Object.Initialize_Class_Record
        (Self,
         Signals,
         Class_Record,
         "CodePeerMessageCategoryCriteriaEditor",
         Signal_Parameters);
      Message_Categories_Criteria_Editor_Callbacks.Connect
        (Self,
         Gtk.Object.Signal_Destroy,
         Message_Categories_Criteria_Editor_Callbacks.To_Marshaller
           (On_Destroy'Access));

      Self.Set_Policy (Gtk.Enums.Policy_Automatic, Gtk.Enums.Policy_Automatic);

      Code_Peer.Message_Categories_Criteria_Models.Gtk_New
        (Self.Model, Categories);
      Message_Categories_Criteria_Model_Callbacks.Connect
        (Self.Model,
         Gtk.Tree_Model.Signal_Row_Changed,
         Message_Categories_Criteria_Model_Callbacks.To_Marshaller
           (On_Model_Row_Changed'Access),
         Message_Categories_Criteria_Editor (Self));
      Gtk.Tree_View.Gtk_New (Self.View, Self.Model);
      Self.Add (Self.View);

      Gtk.Tree_View_Column.Gtk_New (Column);
      Column.Set_Clickable (True);
      Tree_View_Column_Callbacks.Connect
        (Column,
         Gtk.Tree_View_Column.Signal_Clicked,
         Tree_View_Column_Callbacks.To_Marshaller
           (On_Select_All_Toggled'Access),
         Message_Categories_Criteria_Editor (Self));
      Gtk.Check_Button.Gtk_New (Self.Toggle, "");
         Self.Toggle.Set_Inconsistent (False);
         Self.Toggle.Set_Active (True);
      Self.Toggle.Show;
      Column.Set_Widget (Self.Toggle);
      Gtk.Cell_Renderer_Toggle.Gtk_New (Toggle_Renderer);
      Column.Pack_End (Toggle_Renderer, False);
      Column.Add_Attribute
        (Toggle_Renderer,
         "active",
         Code_Peer.Message_Categories_Criteria_Models.Active_Column);
      Dummy := Self.View.Append_Column (Column);
      Cell_Renderer_Toggle_Callbacks.Connect
        (Toggle_Renderer,
         Gtk.Cell_Renderer_Toggle.Signal_Toggled,
         Cell_Renderer_Toggle_Callbacks_Marshallers.To_Marshaller
           (On_Toggle_Category_Visibility'Access),
         Message_Categories_Criteria_Editor (Self),
         True);

      Gtk.Tree_View_Column.Gtk_New (Column);
      Column.Set_Title (-"Message categories");
      Gtk.Cell_Renderer_Text.Gtk_New (Text_Renderer);
      Column.Pack_End (Text_Renderer, False);
      Column.Add_Attribute
        (Text_Renderer,
         "text",
         Code_Peer.Message_Categories_Criteria_Models.Name_Column);
      Dummy := Self.View.Append_Column (Column);
   end Initialize;

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy
     (Self : access Message_Categories_Criteria_Editor_Record'Class)
   is
   begin
      Self.Model.Clear;
   end On_Destroy;

   --------------------------
   -- On_Model_Row_Changed --
   --------------------------

   procedure On_Model_Row_Changed
     (Object : access
        Code_Peer.Message_Categories_Criteria_Models.
          Categories_Criteria_Model_Record'Class;
      Self   : Message_Categories_Criteria_Editor)
   is
      pragma Unreferenced (Object);

   begin
      if Self.Model.Is_Empty then
         Self.Toggle.Set_Inconsistent (False);
         Self.Toggle.Set_Active (False);

      elsif Self.Model.Is_Full then
         Self.Toggle.Set_Inconsistent (False);
         Self.Toggle.Set_Active (True);

      else
         Self.Toggle.Set_Inconsistent (True);
      end if;
   end On_Model_Row_Changed;

   ---------------------------
   -- On_Select_All_Toggled --
   ---------------------------

   procedure On_Select_All_Toggled
     (Object : access Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record'Class;
      Self   : Message_Categories_Criteria_Editor)
   is
      pragma Unreferenced (Object);

   begin
      if Self.Toggle.Get_Inconsistent then
         Self.Model.Show_All;

      elsif Self.Toggle.Get_Active then
         Self.Model.Hide_All;

      else
         Self.Model.Show_All;
      end if;

      Message_Categories_Criteria_Editor_Callbacks.Emit_By_Name
        (Self, Signal_Criteria_Changed);
   end On_Select_All_Toggled;

   -----------------------------------
   -- On_Toggle_Category_Visibility --
   -----------------------------------

   procedure On_Toggle_Category_Visibility
     (Object : access
        Gtk.Cell_Renderer_Toggle.Gtk_Cell_Renderer_Toggle_Record'Class;
      Path   : Interfaces.C.Strings.chars_ptr;
      Self   : Message_Categories_Criteria_Editor)
   is
      Iter  : constant Gtk.Tree_Model.Gtk_Tree_Iter :=
                         Self.Model.Get_Iter_From_String
                           (Interfaces.C.Strings.Value (Path));

   begin
      if Object.Get_Active then
         Self.Model.Hide (Self.Model.Category_At (Iter));

      else
         Self.Model.Show (Self.Model.Category_At (Iter));
      end if;

      Message_Categories_Criteria_Editor_Callbacks.Emit_By_Name
        (Self, Signal_Criteria_Changed);
   end On_Toggle_Category_Visibility;

end Code_Peer.Message_Categories_Criteria_Editors;
