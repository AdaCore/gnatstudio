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

with Ada.Strings.Fixed;
with Ada.Tags;
with Interfaces.C.Strings;

with Glib.Object;
with Glib.Values;
with Gtk.Cell_Renderer_Text;
with Gtk.Cell_Renderer_Toggle;
with Gtk.Enums;
with Gtk.Handlers;
with Gtk.Tree_Model;
with Gtk.Tree_View_Column;
with Gtkada.Types; use Gtkada.Types;

package body CodePeer.Generic_Criteria_Editors is

   function Glib_Class_Name (Tag : Ada.Tags.Tag) return String;
   --  Converts external tag to Glib compatible form.

   procedure On_Toggle_Category_Visibility
     (Object : access
        Gtk.Cell_Renderer_Toggle.Gtk_Cell_Renderer_Toggle_Record'Class;
      Path   : Chars_Ptr;
      Self   : Criteria_Editor);
   --  Called on click on the list's item

   procedure On_Select_All_Toggled
     (Object : access Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record'Class;
      Self   : Criteria_Editor);
   --  Called on click on the column header

   procedure On_Model_Row_Changed
     (Object : access Criteria_Models.Criteria_Model_Record'Class;
      Self   : Criteria_Editor);
   --  Called on changes in the underlying model

   procedure On_Destroy (Self : access Criteria_Editor_Record'Class);
   --  Called on widget destroy

   procedure Update_Toggle_State
     (Self : not null access Criteria_Editor_Record'Class);
   --  Updates state of 'select/unselect all' toggle

   package Cell_Renderer_Toggle_Callbacks is
     new Gtk.Handlers.User_Callback
           (Gtk.Cell_Renderer_Toggle.Gtk_Cell_Renderer_Toggle_Record,
            Criteria_Editor);

   package Cell_Renderer_Toggle_Callbacks_Marshallers is
     new Cell_Renderer_Toggle_Callbacks.Marshallers.Generic_Marshaller
           (Gtkada.Types.Chars_Ptr, Glib.Values.Get_Chars);

   package Tree_View_Column_Callbacks is
     new Gtk.Handlers.User_Callback
           (Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record,
            Criteria_Editor);

   package Message_Categories_Criteria_Editor_Callbacks is
     new Gtk.Handlers.Callback (Criteria_Editor_Record);

   package Message_Categories_Criteria_Model_Callbacks is
     new Gtk.Handlers.User_Callback
           (Criteria_Models.Criteria_Model_Record, Criteria_Editor);

   Class_Record : Glib.Object.Ada_GObject_Class :=
      Glib.Object.Uninitialized_Class;

   Signals : constant Interfaces.C.Strings.chars_ptr_array :=
     (1 => Interfaces.C.Strings.New_String (String (Signal_Criteria_Changed)));

   Signal_Parameters : constant Glib.Object.Signal_Parameter_Types :=
     (1 => (1 => Glib.GType_None));

   -----------------------
   -- Get_Visible_Items --
   -----------------------

   function Get_Visible_Items
     (Self : access Criteria_Editor_Record'Class) return Item_Sets.Set is
   begin
      return Self.Model.Get_Visible_Items;
   end Get_Visible_Items;

   ---------------------
   -- Glib_Class_Name --
   ---------------------

   function Glib_Class_Name (Tag : Ada.Tags.Tag) return String is
      function Replace_Dot (Item : String) return String;

      -----------------
      -- Replace_Dot --
      -----------------

      function Replace_Dot (Item : String) return String is
         Dot : constant Natural := Ada.Strings.Fixed.Index (Item, ".");

      begin
         if Dot /= 0 then
            return
              Replace_Dot
                (Ada.Strings.Fixed.Replace_Slice (Item, Dot, Dot, "__"));

         else
            return Item;
         end if;
      end Replace_Dot;

   begin
      return Replace_Dot (Ada.Tags.External_Tag (Tag));
   end Glib_Class_Name;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Editor         : out Criteria_Editor;
      Kernel         : GPS.Kernel.Kernel_Handle;
      Title          : String;
      History_Prefix : String;
      Items          : Item_Sets.Set;
      Default        : Boolean) is
   begin
      Editor := new Criteria_Editor_Record;
      Initialize (Editor, Kernel, Title, History_Prefix, Items, Default);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self           : not null access Criteria_Editor_Record'Class;
      Kernel         : GPS.Kernel.Kernel_Handle;
      Title          : String;
      History_Prefix : String;
      Items          : Item_Sets.Set;
      Default        : Boolean)
   is
      Column          : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
      Text_Renderer   : Gtk.Cell_Renderer_Text.Gtk_Cell_Renderer_Text;
      Toggle_Renderer : Gtk.Cell_Renderer_Toggle.Gtk_Cell_Renderer_Toggle;
      Dummy           : Glib.Gint;
      pragma Warnings (Off, Dummy);

   begin
      Glib.Object.Initialize_Class_Record
        (Ancestor      => Gtk.Scrolled_Window.Get_Type,
         Signals       => Signals,
         Class_Record  => Class_Record,
         Type_Name     => Glib_Class_Name (Criteria_Editor_Record'Tag),
         Parameters    => Signal_Parameters);
      Glib.Object.G_New (Self, Class_Record);

      Message_Categories_Criteria_Editor_Callbacks.Connect
        (Self,
         Gtk.Widget.Signal_Destroy,
         Message_Categories_Criteria_Editor_Callbacks.To_Marshaller
           (On_Destroy'Access));

      Self.Set_Policy (Gtk.Enums.Policy_Automatic, Gtk.Enums.Policy_Automatic);

      Criteria_Models.Gtk_New
        (Self.Model, Kernel, History_Prefix, Items, Default);
      Message_Categories_Criteria_Model_Callbacks.Connect
        (Self.Model,
         Gtk.Tree_Model.Signal_Row_Changed,
         Message_Categories_Criteria_Model_Callbacks.To_Marshaller
           (On_Model_Row_Changed'Access),
         Criteria_Editor (Self));
      Gtk.Tree_View.Gtk_New (Self.View, Self.Model);
      Self.Add (Self.View);

      if Enable_Tooltips then
         Self.View.Set_Tooltip_Column (Criteria_Models.Tooltip_Column);
      end if;

      Gtk.Tree_View_Column.Gtk_New (Column);
      Column.Set_Clickable (True);
      Tree_View_Column_Callbacks.Connect
        (Column,
         Gtk.Tree_View_Column.Signal_Clicked,
         Tree_View_Column_Callbacks.To_Marshaller
           (On_Select_All_Toggled'Access),
         Criteria_Editor (Self));
      Gtk.Check_Button.Gtk_New (Self.Toggle, "");
         Self.Toggle.Set_Inconsistent (False);
         Self.Toggle.Set_Active (True);
      Self.Update_Toggle_State;
      Self.Toggle.Show;
      Column.Set_Widget (Self.Toggle);
      Gtk.Cell_Renderer_Toggle.Gtk_New (Toggle_Renderer);
      Column.Pack_End (Toggle_Renderer, False);
      Column.Add_Attribute
        (Toggle_Renderer,
         "active",
         Criteria_Models.Active_Column);
      Dummy := Self.View.Append_Column (Column);
      Cell_Renderer_Toggle_Callbacks.Connect
        (Toggle_Renderer,
         Gtk.Cell_Renderer_Toggle.Signal_Toggled,
         Cell_Renderer_Toggle_Callbacks_Marshallers.To_Marshaller
           (On_Toggle_Category_Visibility'Access),
         Criteria_Editor (Self),
         True);

      Gtk.Tree_View_Column.Gtk_New (Column);
      Column.Set_Title (Title);
      Gtk.Cell_Renderer_Text.Gtk_New (Text_Renderer);
      Column.Pack_End (Text_Renderer, False);
      Column.Add_Attribute
        (Text_Renderer,
         "text",
         Criteria_Models.Name_Column);
      Dummy := Self.View.Append_Column (Column);
   end Initialize;

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy (Self : access Criteria_Editor_Record'Class) is
   begin
      Self.Model.Clear;
   end On_Destroy;

   --------------------------
   -- On_Model_Row_Changed --
   --------------------------

   procedure On_Model_Row_Changed
     (Object : access Criteria_Models.Criteria_Model_Record'Class;
      Self   : Criteria_Editor)
   is
      pragma Unreferenced (Object);

   begin
      Self.Update_Toggle_State;
   end On_Model_Row_Changed;

   ---------------------------
   -- On_Select_All_Toggled --
   ---------------------------

   procedure On_Select_All_Toggled
     (Object : access Gtk.Tree_View_Column.Gtk_Tree_View_Column_Record'Class;
      Self   : Criteria_Editor)
   is
      pragma Unreferenced (Object);

   begin
      if Self.Toggle.Get_Inconsistent then
         Self.Model.Hide_All;

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
      Path   : Chars_Ptr;
      Self   : Criteria_Editor)
   is
      Iter : constant Gtk.Tree_Model.Gtk_Tree_Iter :=
        Gtk.Tree_Model.Get_Iter_From_String
          (Gtk.Tree_Model.To_Interface (Self.Model),
           Value (Path));

   begin
      if Object.Get_Active then
         Self.Model.Hide (Self.Model.Item_At (Iter));

      else
         Self.Model.Show (Self.Model.Item_At (Iter));
      end if;

      Message_Categories_Criteria_Editor_Callbacks.Emit_By_Name
        (Self, Signal_Criteria_Changed);
   end On_Toggle_Category_Visibility;

   -------------------------
   -- Update_Toggle_State --
   -------------------------

   procedure Update_Toggle_State
     (Self : not null access Criteria_Editor_Record'Class) is
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
   end Update_Toggle_State;

end CodePeer.Generic_Criteria_Editors;
