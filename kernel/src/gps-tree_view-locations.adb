------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2009-2012, AdaCore                     --
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
with Interfaces.C.Strings;

with Glib.Object;
with Glib.Properties;
with Glib.Values;
with Gdk.Color;
with Gdk.Event;
with Gdk.Rectangle;
with Gtk.Cell_Renderer_Pixbuf;
with Gtk.Handlers;
with Gtk.Tooltip;
with Gtk.Widget;

with GPS.Location_View.Listener;
with Traces;

package body GPS.Tree_View.Locations is

   use Gdk.Color;
   use Gdk.Event;
   use Gdk.Rectangle;
   use Glib;
   use Glib.Main;
   use Glib.Object;
   use Glib.Properties;
   use Glib.Values;
   use Gtk.Cell_Renderer_Pixbuf;
   use Gtk.Cell_Renderer_Text;
   use Gtk.Tooltip;
   use Gtk.Tree_Model;
   use Gtk.Tree_View_Column;
   use Gtk.Widget;
   use GPS.Location_View.Listener;
   use GPS.Sort_Model.Locations;
   use Traces;

   function On_Button_Press
     (Self  : access GPS_Locations_Tree_View_Record'Class;
      Event : Gdk.Event.Gdk_Event) return Boolean;
   --  Handle "button-press" event. Emit "action-clicked" or "location-clicked"
   --  signal when click is done on action column or location column.

   function On_Row_Expanded_Idle
     (Self : GPS_Locations_Tree_View) return Boolean;
   --  Idle callback used to ensure that the proper path is visible

   function On_Query_Tooltip
     (Self   : access GPS_Locations_Tree_View_Record'Class;
      Params : Glib.Values.GValues) return Boolean;
   --  Handle "query-tooltip" request. Shows tooltip when the size of the
   --  renderer is larger than its visible size in the view.

   procedure Action_Clicked
     (Self : not null access GPS_Locations_Tree_View_Record'Class;
      Path : Gtk.Tree_Model.Gtk_Tree_Path;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter);
   --  Emits "action-clicked" signal.

   procedure Location_Clicked
     (Self : not null access GPS_Locations_Tree_View_Record'Class;
      Path : Gtk.Tree_Model.Gtk_Tree_Path;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter);
   --  Emits "location-clicked" signal.

   procedure Class_Initialize
     (Self : not null access GPS_Locations_Tree_View_Record'Class);
   --  Common initialization code to be shared between two implementations
   --  of Initialize.

   package View_Idles is
     new Glib.Main.Generic_Sources (GPS_Locations_Tree_View);

   package Query_Tooltip_Callbacks is
     new Gtk.Handlers.Return_Callback
       (GPS_Locations_Tree_View_Record, Boolean);

   package GPS_Locations_Tree_View_Boolean_Callbacks is
     new Gtk.Handlers.Return_Callback
       (GPS_Locations_Tree_View_Record, Boolean);

   package GPS_Locations_Tree_View_Callbacks is
     new Gtk.Handlers.Callback (GPS_Locations_Tree_View_Record);

   Class_Record : Glib.Object.GObject_Class := Glib.Object.Uninitialized_Class;

   Signals : constant Interfaces.C.Strings.chars_ptr_array (1 .. 2) :=
     (1 => Interfaces.C.Strings.New_String (String (Signal_Action_Clicked)),
      2 => Interfaces.C.Strings.New_String (String (Signal_Location_Clicked)));

   function Signals_Parameters return Glib.Object.Signal_Parameter_Types;

   --------------------
   -- Action_Clicked --
   --------------------

   procedure Action_Clicked
     (Self : not null access GPS_Locations_Tree_View_Record'Class;
      Path : Gtk.Tree_Model.Gtk_Tree_Path;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter)
   is
   begin
      GPS_Locations_Tree_View_Callbacks.Emit_By_Name
        (Self, Signal_Action_Clicked, Path, Iter);
   end Action_Clicked;

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
      Initialize_Class_Record
        (Self,
         Signals,
         Class_Record,
         "GPSLocationsTreeView",
         Signals_Parameters);

      Self.Set_Rules_Hint (False);
      Self.Set_Headers_Visible (False);
      Self.Set_Enable_Search (False);

      --  Action column

      Gtk_New (Self.Action_Column);
      Gtk_New (Pixbuf_Renderer);
      Self.Action_Column.Pack_Start (Pixbuf_Renderer, False);
      Self.Action_Column.Add_Attribute
        (Pixbuf_Renderer,
         Property_Name (Pixbuf_Property),
         Action_Pixbuf_Column);
      Dummy := Self.Append_Column (Self.Action_Column);

      --  Text column

      Gtk_New (Self.Location_Column);
      Gtk_New (Pixbuf_Renderer);
      Self.Location_Column.Pack_Start (Pixbuf_Renderer, False);
      Self.Location_Column.Add_Attribute
        (Pixbuf_Renderer, Property_Name (Pixbuf_Property), Node_Icon_Column);

      Gtk_New (Self.Text_Renderer);
      Self.Location_Column.Pack_Start (Self.Text_Renderer, False);
      Self.Location_Column.Add_Attribute
        (Self.Text_Renderer,
         Property_Name (Markup_Property),
         Node_Markup_Column);
      Self.Location_Column.Add_Attribute
        (Self.Text_Renderer,
         Property_Name (Foreground_Gdk_Property),
         Node_Foreground_Column);
      Dummy := Self.Append_Column (Self.Location_Column);
      Self.Set_Expander_Column (Self.Location_Column);

      --  Connect callbacks

      Set_Property (Self, Has_Tooltip_Property, True);
      Query_Tooltip_Callbacks.Connect
        (Self, Signal_Query_Tooltip, On_Query_Tooltip'Access);
      GPS_Locations_Tree_View_Boolean_Callbacks.Connect
        (Self,
         Signal_Button_Press_Event,
         GPS_Locations_Tree_View_Boolean_Callbacks.To_Marshaller
           (On_Button_Press'Access),
         After => False);
   end Class_Initialize;

   ----------------------
   -- Get_Filter_Model --
   ----------------------

   function Get_Filter_Model
     (Self : not null access GPS_Locations_Tree_View_Record)
      return GPS.Location_View_Filter.Location_View_Filter_Model is
   begin
      return Self.Filter;
   end Get_Filter_Model;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Object : in out GPS_Locations_Tree_View;
      Model  : not null Gtk.Tree_Model.Gtk_Tree_Model) is
   begin
      Object := new GPS_Locations_Tree_View_Record;
      GPS.Tree_View.Locations.Initialize (Object, Model);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self  : not null access GPS_Locations_Tree_View_Record'Class;
      Model : not null Gtk.Tree_Model.Gtk_Tree_Model)
   is

   begin
      GPS.Tree_View.Initialize (Self, Model);
      Class_Initialize (Self);
      Gtk_New (Self.Sort, Model);
      GPS.Location_View_Filter.Gtk_New (Self.Filter);
      Self.Filter.Set_Source_Model (Self.Sort);
      Self.Set_Source_Model (Gtk_Tree_Model (Self.Filter));
   end Initialize;

   ----------------------
   -- Location_Clicked --
   ----------------------

   procedure Location_Clicked
     (Self : not null access GPS_Locations_Tree_View_Record'Class;
      Path : Gtk.Tree_Model.Gtk_Tree_Path;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter)
   is
   begin
      GPS_Locations_Tree_View_Callbacks.Emit_By_Name
        (Self, Signal_Location_Clicked, Path, Iter);
   end Location_Clicked;

   ---------------------
   -- On_Button_Press --
   ---------------------

   function On_Button_Press
     (Self  : access GPS_Locations_Tree_View_Record'Class;
      Event : Gdk.Event.Gdk_Event) return Boolean
   is
      X         : constant Gint := Gint (Get_X (Event));
      Y         : constant Gint := Gint (Get_Y (Event));
      Path      : Gtk_Tree_Path;
      Column    : Gtk_Tree_View_Column;
      Buffer_X  : Gint;
      Buffer_Y  : Gint;
      Row_Found : Boolean;
      Cell_Rect : Gdk_Rectangle;
      Back_Rect : Gdk_Rectangle;

   begin
      if Get_Button (Event) = 1
        and then Get_Event_Type (Event) = Button_Press
      then
         Self.Get_Path_At_Pos
           (X, Y, Path, Column, Buffer_X, Buffer_Y, Row_Found);

         if Column /= Self.Action_Column then
            Self.Get_Cell_Area (Path, Self.Location_Column, Cell_Rect);
            Self.Get_Background_Area (Path, Self.Location_Column, Back_Rect);

            --  If we are clicking before the beginning of the cell, allow the
            --  event to pass. This allows clicking on expanders.

            if Buffer_X > Back_Rect.X
              and then Buffer_X < Cell_Rect.X
            then
               Path_Free (Path);

               return False;
            end if;
         end if;

         if Path /= null then
            if Get_Depth (Path) < 3 then
               Path_Free (Path);

               return False;

            else
               Self.Get_Selection.Select_Path (Path);

               if Column = Self.Action_Column then
                  Self.Action_Clicked (Path, Self.Get_Model.Get_Iter (Path));

               else
                  Self.Location_Clicked (Path, Self.Get_Model.Get_Iter (Path));
               end if;
            end if;

            Path_Free (Path);
         end if;

         return True;

      else
         Self.Grab_Focus;

         --  If there is no selection, select the item under the cursor

         Self.Get_Path_At_Pos
           (X, Y, Path, Column, Buffer_X, Buffer_Y, Row_Found);

         if Path /= null then
            if not Self.Get_Selection.Path_Is_Selected (Path) then
               Self.Get_Selection.Unselect_All;
               Self.Get_Selection.Select_Path (Path);
            end if;

            Path_Free (Path);
         end if;
      end if;

      return False;

   exception
      when E : others =>
         Trace (Exception_Handle, E);

         return False;
   end On_Button_Press;

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
      --  Automatically expand third level nodes.

      if Get_Depth (Path) = 3 then
         Node.Expanded := True;
      end if;

      --  For each added first level node schedule an automatic expansion of
      --  first and second level nodes. Note: we need to be carefully here,
      --  because different proxy models can have different filtering
      --  behavior.
      --  XXX Not implemented yet.
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
      Stub          : Gtk_Tooltip_Record;
      Tooltip       : constant Gtk_Tooltip :=
        Gtk_Tooltip (Get_User_Data (Get_Address (Nth (Params, 4)), Stub));
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
      Column        : Glib.Gint := Glib.Gint'Last;

   begin
      Self.Get_Tooltip_Context
        (X, Y, Keyboard_Mode, Model, Path, Iter, Success);

      if Success then
         Self.Location_Column.Cell_Set_Cell_Data (Model, Iter, False, False);

         --  Check whether mouse position is in the action column or in the
         --  text column.

         Self.Get_Cell_Area (Path, Self.Action_Column, Rect);

         if Rect.X <= X and X <= Rect.X + Rect.Width then
            Column := Action_Tooltip_Column;

         else
            --  Check whether text column is partially hidden.

            Self.Get_Cell_Area (Path, Self.Location_Column, Rect);
            X1 := Rect.X;
            X2 := Rect.X;

            Self.Location_Column.Cell_Get_Position
              (Self.Text_Renderer, Start, Width, Success);

            if Success then
               X2 := X2 + Start;

               Self.Text_Renderer.Get_Size
                 (Self, Rect, X_Offset, Y_Offset, Width, Height);
               X2 := X2 + Width;

               Self.Get_Visible_Rect (Rect);

               if X1 <= Rect.X or X2 >= (Rect.X + Rect.Width) then
                  --  Text column is partially hidden, display text tooltip.

                  Column := Node_Tooltip_Column;
               end if;
            end if;
         end if;
      end if;

      --  Set tooltip's markup

      if Column /= Glib.Gint'Last then
         Tooltip.Set_Markup (Model.Get_String (Iter, Column));
         Self.Set_Tooltip_Row (Tooltip, Path);
      end if;

      --  Cleanup

      Gtk.Tree_Model.Path_Free (Path);

      return Column /= Glib.Gint'Last;
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
      pragma Unreferenced (Node, Iter);

   begin
      --  Expansion of one node can raise an expansion of the large number of
      --  children nodes. We do scrolling in the idle callback to be sure the
      --  only first requested node is involved in scrolling.

      if Self.On_Row_Expanded_Handler = No_Source_Id then
         Self.On_Row_Expanded_Path := Copy (Path);
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
      Iter       : Gtk_Tree_Iter := Model.Get_Iter (Path);
      --  Iterator can't be stored and must be getted from the model because
      --  it expires each time filter model is changed.
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
      Self.On_Row_Expanded_Handler := No_Source_Id;

      return False;
   end On_Row_Expanded_Idle;

   ------------------------
   -- Signals_Parameters --
   ------------------------

   function Signals_Parameters return Glib.Object.Signal_Parameter_Types is
      Result : constant
        Glib.Object.Signal_Parameter_Types (1 .. 2, 1 .. 3) :=
        (1 => (1      => Path_Get_Type,
               2      => Iter_Get_Type,
               others => Glib.GType_None),
         2 => (1      => Path_Get_Type,
               2      => Iter_Get_Type,
               others => Glib.GType_None));

   begin
      return Result;
   end Signals_Parameters;

   ----------------------
   -- Sort_By_Location --
   ----------------------

   procedure Sort_By_Location
     (Self : not null access GPS_Locations_Tree_View_Record'Class) is
   begin
      Self.Sort.Set_Locations_Order;
   end Sort_By_Location;

   -------------------------
   -- Sort_By_Subcategory --
   -------------------------

   procedure Sort_By_Subcategory
     (Self : not null access GPS_Locations_Tree_View_Record'Class) is
   begin
      Self.Sort.Set_Weight_Order;
   end Sort_By_Subcategory;

   ---------------------------
   -- To_Lowerst_Model_Iter --
   ---------------------------

   overriding function To_Lowerst_Model_Iter
     (Self : not null access GPS_Locations_Tree_View_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Gtk.Tree_Model.Gtk_Tree_Iter is
   begin
      return Self.Sort.Map_To_Source (Self.Filter.Map_To_Source (Iter));
   end To_Lowerst_Model_Iter;

end GPS.Tree_View.Locations;
