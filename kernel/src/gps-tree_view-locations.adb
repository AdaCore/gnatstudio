-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                    Copyright (C) 2009, AdaCore                    --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
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
with Glib.Object;
with Glib.Properties;
with Glib.Values;
with Gdk.Color;
with Gdk.Rectangle;
with Gtk.Cell_Renderer_Pixbuf;
with Gtk.Tooltips;
with Gtk.Widget;

with GPS.Location_Model;

package body GPS.Tree_View.Locations is

   use Gdk.Color;
   use Gdk.Rectangle;
   use Glib;
   use Glib.Main;
   use Glib.Object;
   use Glib.Properties;
   use Glib.Values;
   use Gtk.Cell_Renderer_Pixbuf;
   use Gtk.Cell_Renderer_Text;
   use Gtk.Tooltips;
   use Gtk.Tree_Model;
   use Gtk.Tree_View_Column;
   use Gtk.Widget;
   use GPS.Location_Model;

   package View_Idles is
     new Glib.Main.Generic_Sources (GPS_Locations_Tree_View);

   function On_Row_Expanded_Idle
     (Self : GPS_Locations_Tree_View) return Boolean;
   --  Idle callback used to ensure that the proper path is visible

   function On_Query_Tooltip
     (Self   : access GPS_Locations_Tree_View_Record'Class;
      Params : Glib.Values.GValues) return Boolean;
   --  Handle "query-tooltip" request. Shows tooltip when the size of the
   --  renderer is larger than its visible size in the view.

   procedure Class_Initialize
     (Self : not null access GPS_Locations_Tree_View_Record'Class);
   --  Common initialization code to be shared between two implementations
   --  of Initialize.

   package Query_Tooltip_Callbacks is
     new Gtk.Handlers.Return_Callback
       (GPS_Locations_Tree_View_Record, Boolean);

   ----------------------
   -- Class_Initialize --
   ----------------------

   procedure Class_Initialize
     (Self : not null access GPS_Locations_Tree_View_Record'Class)
   is
      Pixbuf_Renderer : Gtk_Cell_Renderer_Pixbuf;
      Dummy           : Gint;
      pragma Unreferenced (Dummy);

   begin
      Self.Set_Rules_Hint (False);
      Self.Set_Headers_Visible (False);
      Self.Set_Enable_Search (False);

      --  Action column

      Gtk_New (Self.Action_Column);
      Gtk_New (Pixbuf_Renderer);
      Self.Action_Column.Pack_Start (Pixbuf_Renderer, False);
      Self.Action_Column.Add_Attribute
        (Pixbuf_Renderer, Property_Name (Pixbuf_Property), Button_Column);
      Dummy := Self.Append_Column (Self.Action_Column);

      --  Text column

      Gtk_New (Self.Location_Column);
      Gtk_New (Pixbuf_Renderer);
      Self.Location_Column.Pack_Start (Pixbuf_Renderer, False);
      Self.Location_Column.Add_Attribute
        (Pixbuf_Renderer, Property_Name (Pixbuf_Property), Icon_Column);

      Gtk_New (Self.Text_Renderer);
      Self.Location_Column.Pack_Start (Self.Text_Renderer, False);
      Self.Location_Column.Add_Attribute
        (Self.Text_Renderer,
         Property_Name (Markup_Property),
         Base_Name_Column);
      Self.Location_Column.Add_Attribute
        (Self.Text_Renderer,
         Property_Name (Foreground_Gdk_Property),
         Color_Column);
      Dummy := Self.Append_Column (Self.Location_Column);
      Self.Set_Expander_Column (Self.Location_Column);

      --  Connect callbacks

      Set_Property (Self, Has_Tooltip_Property, True);
      Query_Tooltip_Callbacks.Connect
        (Self, Signal_Query_Tooltip, On_Query_Tooltip'Access);
   end Class_Initialize;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Object : in out GPS_Locations_Tree_View) is
   begin
      Object := new GPS_Locations_Tree_View_Record;
      GPS.Tree_View.Locations.Initialize (Object);
   end Gtk_New;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Object : in out GPS_Locations_Tree_View;
      Model  : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class) is
   begin
      Object := new GPS_Locations_Tree_View_Record;
      GPS.Tree_View.Locations.Initialize (Object, Model);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self : not null access GPS_Locations_Tree_View_Record'Class) is
   begin
      GPS.Tree_View.Initialize (Self);
      Class_Initialize (Self);
   end Initialize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self  : not null access GPS_Locations_Tree_View_Record'Class;
      Model : access Gtk.Tree_Model.Gtk_Tree_Model_Record'Class) is
   begin
      GPS.Tree_View.Initialize (Self, Model);
      Class_Initialize (Self);
   end Initialize;

   -----------------------------------
   -- On_Lowerst_Model_Row_Inserted --
   -----------------------------------

   overriding procedure On_Lowerst_Model_Row_Inserted
     (Self : not null access GPS_Locations_Tree_View_Record;
      Path : Gtk.Tree_Model.Gtk_Tree_Path;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter;
      Node : not null Node_Access)
   is
      pragma Unreferenced (Self, Iter);

   begin
      if Get_Depth (Path) = 3 then
         Node.Expanded := True;
      end if;
   end On_Lowerst_Model_Row_Inserted;

   ----------------------
   -- On_Query_Tooltip --
   ----------------------

   function On_Query_Tooltip
     (Self   : access GPS_Locations_Tree_View_Record'Class;
      Params : Glib.Values.GValues) return Boolean
   is
      X             : Glib.Gint := Get_Int (Nth (Params, 1));
      Y             : Glib.Gint := Get_Int (Nth (Params, 2));
      Keyboard_Mode : constant Boolean := Get_Boolean (Nth (Params, 3));
      Stub          : Gtk_Tooltips_Record;
      Tooltip       : constant Gtk_Tooltips :=
        Gtk_Tooltips (Get_User_Data (Get_Address (Nth (Params, 4)), Stub));
      Success       : Boolean;
      Model         : Gtk_Tree_Model;
      Path          : Gtk_Tree_Path;
      Iter          : Gtk_Tree_Iter;
      Rect          : Gdk_Rectangle;
      X_Offset      : Gint;
      Y_Offset      : Gint;
      Start         : Gint;
      Width         : Gint;
      Height        : Gint;
      X1            : Gint;
      X2            : Gint;

   begin
      Self.Get_Tooltip_Context
        (X, Y, Keyboard_Mode, Model, Path, Iter, Success);

      if not Success then
         Path_Free (Path);

         return False;
      end if;

      Self.Location_Column.Cell_Set_Cell_Data (Model, Iter, False, False);

      Self.Get_Cell_Area (Path, Self.Location_Column, Rect);
      X1 := Rect.X;
      X2 := Rect.X;

      Self.Location_Column.Cell_Get_Position
        (Self.Text_Renderer, Start, Width, Success);

      if not Success then
         Path_Free (Path);

         return False;
      end if;

      X2 := X2 + Start;

      Self.Text_Renderer.Get_Size
        (Self, Rect, X_Offset, Y_Offset, Width, Height);
      X2 := X2 + Width;

      Self.Get_Visible_Rect (Rect);

      if X1 > Rect.X and X2 < (Rect.X + Rect.Width) then
         Gtk.Tree_Model.Path_Free (Path);

         return False;
      end if;

      Tooltip.Set_Markup (Model.Get_String (Iter, Base_Name_Column));
      Self.Set_Tooltip_Row (Tooltip, Path);

      Path_Free (Path);

      return True;
   end On_Query_Tooltip;

   ---------------------
   -- On_Row_Expanded --
   ---------------------

   overriding procedure On_Row_Expanded
     (Self : not null access GPS_Locations_Tree_View_Record;
      Path : Gtk.Tree_Model.Gtk_Tree_Path;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter;
      Node : not null Node_Access)
   is
      pragma Unreferenced (Node);

   begin
      --  Expansion of one node can raise an expansion of the large number of
      --  children nodes. We do scrolling in the idle callback to be sure the
      --  only first requested node is involved in scrolling.

      if Self.On_Row_Expanded_Handler = No_Source_Id then
         Self.On_Row_Expanded_Path := Copy (Path);
         Self.On_Row_Expanded_Iter := Iter;
         Self.On_Row_Expanded_Handler :=
           View_Idles.Idle_Add
             (On_Row_Expanded_Idle'Access, GPS_Locations_Tree_View (Self));
      end if;
   end On_Row_Expanded;

   --------------------------
   -- On_Row_Expanded_Idle --
   --------------------------

   function On_Row_Expanded_Idle
     (Self : GPS_Locations_Tree_View) return Boolean
   is
      Model      : Gtk_Tree_Model renames Self.Get_Model;
      Path       : Gtk_Tree_Path renames Self.On_Row_Expanded_Path;
      Iter       : Gtk_Tree_Iter renames Self.On_Row_Expanded_Iter;
      Start_Path : Gtk_Tree_Path;
      End_Path   : Gtk_Tree_Path;
      Success    : Boolean;

   begin
      Self.Get_Visible_Range (Start_Path, End_Path, Success);

      if Success
        and then Model.Has_Child (Iter)
      then
         --  Go down till not expanded node or node leaf node is found

         loop
            Down (Path);
            Iter := Model.Children (Iter);

            exit when not Self.Row_Expanded (Path)
              or else not Model.Has_Child (Iter);
         end loop;

         if Compare (Path, End_Path) >= 0 then
            Self.Scroll_To_Cell (Path, null, True, 0.9, 0.1);
         end if;
      end if;

      Path_Free (Start_Path);
      Path_Free (End_Path);
      Path_Free (Path);

      Self.On_Row_Expanded_Path := null;
      Self.On_Row_Expanded_Iter := Null_Iter;
      Self.On_Row_Expanded_Handler := No_Source_Id;

      return False;
   end On_Row_Expanded_Idle;

   --------------------
   -- Sorting_Column --
   --------------------

   function Sorting_Column
     (Self : not null access GPS_Locations_Tree_View_Record'Class)
      return Gtk.Tree_View_Column.Gtk_Tree_View_Column is
   begin
      return Self.Location_Column;
   end Sorting_Column;

   -------------------
   -- Action_Column --
   -------------------

   function Action_Column
     (Self : not null access GPS_Locations_Tree_View_Record'Class)
      return Gtk.Tree_View_Column.Gtk_Tree_View_Column is
   begin
      return Self.Action_Column;
   end Action_Column;

end GPS.Tree_View.Locations;
