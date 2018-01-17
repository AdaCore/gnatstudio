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
with Gtk.Cell_Renderer_Text;
with Gtk.Cell_Renderer_Toggle;
with Gtk.Enums;
with Gtk.Handlers;
with Gtk.Tree_View_Column;
with Gtkada.Abstract_Tree_Model;

package body GNAThub.Generic_Criteria_Editors is

   use Glib;

   function Glib_Class_Name (Tag : Ada.Tags.Tag) return String;
   --  Converts external tag to Glib compatible form.

   procedure On_Toggle_Category_Visibility
     (Object : access
        Gtk.Cell_Renderer_Toggle.Gtk_Cell_Renderer_Toggle_Record'Class;
      Path   : Interfaces.C.Strings.chars_ptr;
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

   procedure Update_Toggle_State
     (Self : not null access Criteria_Editor_Record'Class);
   --  Updates state of 'select/unselect all' toggle

   package Cell_Renderer_Toggle_Callbacks is
     new Gtk.Handlers.User_Callback
           (Gtk.Cell_Renderer_Toggle.Gtk_Cell_Renderer_Toggle_Record,
            Criteria_Editor);

   package Cell_Renderer_Toggle_Callbacks_Marshallers is
     new Cell_Renderer_Toggle_Callbacks.Marshallers.Generic_Marshaller
           (Interfaces.C.Strings.chars_ptr, Glib.Values.Get_Chars);

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

   function Signal_Parameters return Glib.Object.Signal_Parameter_Types;

   ------------
   -- Choose --
   ------------

   procedure Choose
     (Self : access Criteria_Editor_Record'Class;
      Item : Item_Access) is
   begin
      Self.Model.Show (Item);
   end Choose;

   -------------------------
   -- Filter_Visible_Func --
   -------------------------

   function Filter_Visible_Func
     (Model : Gtk.Tree_Model.Gtk_Tree_Model;
      Iter  : Gtk.Tree_Model.Gtk_Tree_Iter;
      View  : Gtk.Widget.Gtk_Widget)
      return Boolean is
   begin
      if Is_Visible /= null then
         return Is_Visible
           (Criteria_Models.Criteria_Model
              (Gtkada.Abstract_Tree_Model."-" (Model)).Item_At (Iter),
            View);
      else
         return True;
      end if;
   end Filter_Visible_Func;

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
     (Editor         : in out Criteria_Editor;
      Kernel         : GPS.Kernel.Kernel_Handle;
      View           : Gtk.Widget.Gtk_Widget;
      Title          : String;
      History_Prefix : String;
      Items          : Item_Sets.Set;
      Default        : Boolean) is
   begin
      Editor := new Criteria_Editor_Record;
      Initialize (Editor, Kernel, View, Title, History_Prefix, Items, Default);
   end Gtk_New;

   ---------------
   -- Highlight --
   ---------------

   procedure Highlight
     (Self : access Criteria_Editor_Record'Class;
      Item : Item_Access)
   is
      Path : Gtk.Tree_Model.Gtk_Tree_Path := Self.Model.Get_Path (Item);
   begin
      if Is_Visible /= null then
         Path := Self.Filter.Convert_Child_Path_To_Path (Path);
      end if;

      Self.View.Get_Selection.Select_Path (Path);
      Gtk.Tree_Model.Path_Free (Path);
   end Highlight;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self           : not null access Criteria_Editor_Record'Class;
      Kernel         : GPS.Kernel.Kernel_Handle;
      View           : Gtk.Widget.Gtk_Widget;
      Title          : String;
      History_Prefix : String;
      Items          : Item_Sets.Set;
      Default        : Boolean)
   is
      Column          : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
      Toggle_Renderer : Gtk.Cell_Renderer_Toggle.Gtk_Cell_Renderer_Toggle;
      Dummy           : Glib.Gint;
      pragma Warnings (Off, Dummy);

   begin
      Self.Parent := View;

      Glib.Object.Initialize_Class_Record
        (Ancestor      => Gtk.Scrolled_Window.Get_Type,
         Signals       => Signals,
         Class_Record  => Class_Record,
         Type_Name     => Glib_Class_Name (Criteria_Editor_Record'Tag),
         Parameters    => Signal_Parameters);
      Glib.Object.G_New (Self, Class_Record);

      Self.Set_Policy (Gtk.Enums.Policy_Automatic, Gtk.Enums.Policy_Automatic);

      Criteria_Models.Gtk_New
        (Self.Model, Kernel, View, History_Prefix, Items, Default);
      Message_Categories_Criteria_Model_Callbacks.Connect
        (Self.Model,
         Gtk.Tree_Model.Signal_Row_Changed,
         Message_Categories_Criteria_Model_Callbacks.To_Marshaller
           (On_Model_Row_Changed'Access),
         Criteria_Editor (Self));

      if Is_Visible /= null then
         Self.Filter :=
           Gtk.Tree_Model_Filter.Gtk_Tree_Model_Filter_Filter_New
             (Gtkada.Abstract_Tree_Model."+" (Self.Model));
         Set_Visible_Func_With_Data.Set_Visible_Func
           (Self.Filter, Visible, View);
         Gtk.Tree_View.Gtk_New (Self.View, Self.Filter);
      else
         Gtk.Tree_View.Gtk_New (Self.View, Self.Model);
      end if;

      Self.Add (Self.View);

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
        (Toggle_Renderer, "active", Criteria_Models.Active_Column);
      Dummy := Self.View.Append_Column (Column);

      Cell_Renderer_Toggle_Callbacks.Connect
        (Toggle_Renderer,
         Gtk.Cell_Renderer_Toggle.Signal_Toggled,
         Cell_Renderer_Toggle_Callbacks_Marshallers.To_Marshaller
           (On_Toggle_Category_Visibility'Access),
         Criteria_Editor (Self),
         True);

      if Tooltips then
         Self.View.Set_Tooltip_Column (Gint (Columns'Last + 1));
      end if;

      for Index in Columns'Range loop
         case Columns (Index) is
            when GType_String | GType_Uint =>
               declare
                  Renderer : Gtk.Cell_Renderer_Text.Gtk_Cell_Renderer_Text;
               begin
                  Gtk.Tree_View_Column.Gtk_New (Column);
                  if Index = Columns'First then
                     Column.Set_Title (Title);
                  end if;

                  Gtk.Cell_Renderer_Text.Gtk_New (Renderer);
                  Column.Pack_End (Renderer, False);
                  Column.Add_Attribute (Renderer, "text", Gint (Index));
               end;

            when others =>
               raise Constraint_Error with
                 "Unsupported column type for " &
                 "GNATHub.Generic_Criteria_Editors";
         end case;
         Dummy := Self.View.Append_Column (Column);
      end loop;
   end Initialize;

   ------------------
   -- Item_By_Path --
   ------------------

   function Item_By_Path
     (Self : access Criteria_Editor_Record'Class;
      Path  : Gtk.Tree_Model.Gtk_Tree_Path)
      return Item_Access is
   begin
      return Self.Model.Item_At (Self.Model.Get_Iter (Path));
   end Item_By_Path;

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
      Path : Gtk.Tree_Model.Gtk_Tree_Path;

   begin
      if Self.Toggle.Get_Inconsistent then
         Self.Model.Hide_All;

      elsif Self.Toggle.Get_Active then
         Self.Model.Hide_All;

      else
         for I of Self.Model.All_Items loop
            if not Self.Model.Get_Visible_Items.Contains (I)
              and then (Is_Visible = null or else Is_Visible (I, Self.Parent))
            then
               Self.Model.Show (I);
            end if;
         end loop;
      end if;

      Message_Categories_Criteria_Editor_Callbacks.Emit_By_Name
        (Self, Signal_Criteria_Changed, Path);
   end On_Select_All_Toggled;

   -----------------------------------
   -- On_Toggle_Category_Visibility --
   -----------------------------------

   procedure On_Toggle_Category_Visibility
     (Object : access
        Gtk.Cell_Renderer_Toggle.Gtk_Cell_Renderer_Toggle_Record'Class;
      Path   : Interfaces.C.Strings.chars_ptr;
      Self   : Criteria_Editor)
   is
      P    : Gtk.Tree_Model.Gtk_Tree_Path;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter;

   begin
      if Is_Visible /= null then
         Self.Filter.Convert_Iter_To_Child_Iter
           (Iter, Self.Filter.Get_Iter_From_String
              (Interfaces.C.Strings.Value (Path)));

      else
         Iter := Gtk.Tree_Model.Get_Iter_From_String
          (Gtk.Tree_Model.To_Interface (Self.Model),
           Interfaces.C.Strings.Value (Path));
      end if;

      P := Gtk.Tree_Model.Get_Path
        (Gtk.Tree_Model.To_Interface (Self.Model), Iter);

      if Object.Get_Active then
         Self.Model.Hide (Self.Model.Item_At (Iter));

      else
         Self.Model.Show (Self.Model.Item_At (Iter));
      end if;

      Message_Categories_Criteria_Editor_Callbacks.Emit_By_Name
        (Self, Signal_Criteria_Changed, P);
   end On_Toggle_Category_Visibility;

   -----------------------
   -- Signal_Parameters --
   -----------------------

   function Signal_Parameters return Glib.Object.Signal_Parameter_Types is
      Result  : constant Glib.Object.Signal_Parameter_Types :=
        (1 => (1 => Gtk.Tree_Model.Path_Get_Type,
               2 => Glib.GType_None));
   begin
      return Result;
   end Signal_Parameters;

   --------------
   -- Unselect --
   --------------

   procedure Unselect
     (Self : access Criteria_Editor_Record'Class;
      Item : Item_Access) is
   begin
      Self.Model.Hide (Item);
   end Unselect;

   ------------
   -- Update --
   ------------

   procedure Update (Self : access Criteria_Editor_Record'Class) is
   begin
      Self.Model.Update;
   end Update;

   -------------------------
   -- Update_Toggle_State --
   -------------------------

   procedure Update_Toggle_State
     (Self : not null access Criteria_Editor_Record'Class)
   is
      function Is_Full return Boolean;

      -------------
      -- Is_Full --
      -------------

      function Is_Full return Boolean is
      begin
         if Is_Visible = null then
            return Self.Model.Is_Full;
         else
            for I of Self.Model.All_Items loop
               if Is_Visible (I, Self.Parent)
                 and then not Self.Model.Get_Visible_Items.Contains (I)
               then
                  return False;
               end if;
            end loop;

            return True;
         end if;
      end Is_Full;

   begin
      if Self.Model.Is_Empty then
         Self.Toggle.Set_Inconsistent (False);
         Self.Toggle.Set_Active (False);

      elsif Is_Full then
         Self.Toggle.Set_Inconsistent (False);
         Self.Toggle.Set_Active (True);

      else
         Self.Toggle.Set_Active (False);
         Self.Toggle.Set_Inconsistent (True);
      end if;
   end Update_Toggle_State;

end GNAThub.Generic_Criteria_Editors;
