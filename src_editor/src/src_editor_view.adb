------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2017, AdaCore                     --
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

with Ada.Strings.Maps.Constants; use Ada.Strings.Maps;
with Interfaces.C.Strings;
with System;

with GNATCOLL.Traces;            use GNATCOLL.Traces;
with GNATCOLL.VFS;               use GNATCOLL.VFS;

with Cairo.Surface;              use Cairo.Surface;

with Gdk;                        use Gdk;
with Gdk.Cairo;                  use Gdk.Cairo;
with Gdk.Event;                  use Gdk.Event;
with Gdk.Rectangle;              use Gdk.Rectangle;
with Gdk.RGBA;                   use Gdk.RGBA;
with Gdk.Window;                 use Gdk.Window;
with Gdk.Types;                  use Gdk.Types;
with Gdk.Types.Keysyms;          use Gdk.Types.Keysyms;

with Glib.Object;                use Glib.Object;
with Glib.Values;                use Glib.Values;

with Gtk;                        use Gtk;
with Gtk.Adjustment;             use Gtk.Adjustment;
with Gtk.Drawing_Area;           use Gtk.Drawing_Area;
with Gtk.Enums;                  use Gtk.Enums;
with Gtk.Scrolled_Window;        use Gtk.Scrolled_Window;
with Gtk.Style_Context;          use Gtk.Style_Context;
with Gtk.Text_Buffer;            use Gtk.Text_Buffer;
with Gtk.Text_Iter;              use Gtk.Text_Iter;
with Gtk.Widget;                 use Gtk.Widget;

with Gtkada.Handlers;            use Gtkada.Handlers;
with Gtkada.MDI;                 use Gtkada.MDI;
with Gtkada.Style;               use Gtkada.Style;

with Pango.Layout;               use Pango.Layout;
with Pango.Attributes;           use Pango.Attributes;

with Src_Editor_Buffer;          use Src_Editor_Buffer;
with Src_Editor_Buffer.Blocks;   use Src_Editor_Buffer.Blocks;
with Src_Editor_Buffer.Hooks;    use Src_Editor_Buffer.Hooks;
with Src_Editor_Module.Markers;  use Src_Editor_Module.Markers;
with Src_Editor_Module;          use Src_Editor_Module;

with Basic_Types;                use Basic_Types;
with Config;                     use Config;
with Default_Preferences;        use Default_Preferences;
with GPS.Intl;                   use GPS.Intl;
with GPS.Kernel;                 use GPS.Kernel;
with GPS.Kernel.Clipboard;       use GPS.Kernel.Clipboard;
with GPS.Kernel.Contexts;        use GPS.Kernel.Contexts;
with GPS.Kernel.Hooks;           use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;             use GPS.Kernel.MDI;
with GPS.Kernel.Project;         use GPS.Kernel.Project;
with GPS.Kernel.Preferences;     use GPS.Kernel.Preferences;
with Language;                   use Language;
with Language.Tree;              use Language.Tree;
with Src_Editor_Buffer.Line_Information;
use Src_Editor_Buffer.Line_Information;

with Src_Editor_Buffer.Cursors;  use Src_Editor_Buffer.Cursors;

with Src_Editor_View.Hyper_Mode; use Src_Editor_View.Hyper_Mode;
with Gdk.Drag_Contexts;          use Gdk.Drag_Contexts;
with Gtk.Selection_Data;
with Gtk.Dnd;                    use Gtk.Dnd;
with Gdk.Dnd;
with Gtk.Target_List;            use Gtk.Target_List;
with Gdk.Property;               use Gdk.Property;
with GNAT.Strings;
with Glib.Convert;               use Glib.Convert;

with GUI_Utils;    use GUI_Utils;
with String_Utils; use String_Utils;

--  Drawing the side info is organized this way:
--
--     <----  side area (View.Area) ----> <----  editor (View)      --->
--     |                                |
--     |              drawing of        |
--     |              side info data    |
--     |                                |
--
--  Both the speed bar and the side info data are drawn on the drawing area
--  next to the editor view.

package body Src_Editor_View is

   Me : constant Trace_Handle := Create ("Editor_View");

   Trace_Override_Middle_Click_Paste : constant Trace_Handle :=
     Create ("OVERRIDE_MIDDLE_CLICK_PASTE", GNATCOLL.Traces.On);
   --  When this is On, we do our own handling of middle mouse click to
   --  implement paste on Unix platforms. The default handling of the Xserver
   --  also copies the syntax highlighting which is unwanted if for instance we
   --  copy a highlighting on an entity.

   Margin : constant := 3;
   --  The margin left of the text

   procedure Setup (Data : Source_View; Id : Gtk.Handlers.Handler_Id);
   package Source_Buffer_Callback is new Gtk.Handlers.User_Callback_With_Setup
     (Widget_Type => Source_Buffer_Record,
      User_Type   => Source_View,
      Setup       => Setup);

   package Source_View_Timeout is new Glib.Main.Generic_Sources (Source_View);
   package Source_View_Idle renames Source_View_Timeout;

   Target_Table : constant Target_Entry_Array :=
     ((Interfaces.C.Strings.New_String ("text/uri-list"), 0, 0),
      (Interfaces.C.Strings.New_String ("text/plain"), 0, 1));

   procedure On_Draw_Layer
     (Widget : System.Address;
      Layer  : Gtk_Text_View_Layer;
      Cr     : Cairo.Cairo_Context)
     with Convention => C;

   View_Class_Record : Glib.Object.Ada_GObject_Class :=
     Glib.Object.Uninitialized_Class;
   procedure View_Class_Init (Self : GObject_Class)
     with Convention => C;
   function View_Get_Type return Glib.GType;
   --  Support subprograms for creating our own class type, so that we can
   --  override the "draw_layer" virtual function.

   --------------------------
   -- Forward declarations --
   --------------------------

   function Connect_Expose (View : Source_View) return Boolean;
   --  Connect Expose_Event_Cb to the expose event. Emit an expose event

   function Idle_Column_Redraw (View : Source_View) return Boolean;
   --  Redraw the side columns in an idle loop

   procedure Realize_Cb (Widget : access Gtk_Widget_Record'Class);
   --  This procedure is invoked when the Source_View widget is realized.
   --  It performs various operations that can not be done before the widget
   --  is realized, such as setting the default font or the left border window
   --  size for instance.

   function Focus_Out_Event_Cb
     (Widget : access Gtk_Widget_Record'Class) return Boolean;
   --  Save the current insert cursor position before the Source_View looses
   --  the focus. This will allow us to restore it as soon as the focus is
   --  gained back. This is used for the handling of multiple views, so that we
   --  can have a different cursor position in each of the view

   function Focus_In_Event_Cb
     (Widget : access Gtk_Widget_Record'Class) return Boolean;
   --  Restore the previously saved insert cursor position when the Source_View
   --  gains the focus back.

   procedure View_On_Drag_Data_Received
     (Self    : access Gtk_Widget_Record'Class;
      Context : not null access Gdk.Drag_Contexts.Drag_Context_Record'Class;
      X       : Gint;
      Y       : Gint;
      Data    : Gtk.Selection_Data.Gtk_Selection_Data;
      Info    : Guint;
      Time    : Guint);
   --  Callback handling the special case of dragging and dropping when both
   --  source and destination are a GPS text view. This was messed up by Gtk,
   --  which was doing a "clever" copy in this case, trying to copy Gtk tags
   --  along with the content, and inserted the content in chunks which was
   --  interacting badly with the Insert hooks

   function Button_Press_Event_Cb
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean;
   --  Callback for the "button_press_event" signal

   function Button_Release_Event_Cb
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean;
   --  Callback for the "button_release_event" signal

   function Speed_Bar_Button_Press_Event_Cb
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean;
   --  Callback for the "button_press_event" signal on the speed bar

   function Speed_Bar_Button_Release_Event_Cb
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean;
   --  Callback for the "button_press_event" signal on the speed bar

   function Key_Press_Event_Cb
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean;

   function Line_Highlight_Redraw (User : Source_View) return Boolean;
   --  Redraw the source view after a change in highlights

   procedure On_Destroy (View : access Gtk_Widget_Record'Class);
   --  Called when the view is destroyed

   procedure Cursor_Position_Changed
     (Buffer : access Source_Buffer_Record'Class;
      Params : Glib.Values.GValues;
      User   : Source_View);
   --  Callback for the "changed" signal

   procedure On_Mark_Set
     (Buffer : access Source_Buffer_Record'Class;
      Params : Glib.Values.GValues;
      User   : Source_View);
   --  Callback for "mark_set" signal

   procedure Buffer_Information_Change_Handler
     (Buffer : access Source_Buffer_Record'Class;
      Params : Glib.Values.GValues;
      User   : Source_View);
   --  Callback for the "buffer_information_changed" signal

   procedure Side_Columns_Change_Handler
     (Buffer : access Source_Buffer_Record'Class;
      Params : Glib.Values.GValues;
      User   : Source_View);
   --  Callback for the "side_columns_changed" signal

   procedure Side_Columns_Config_Change_Handler
     (Buffer : access Source_Buffer_Record'Class;
      Params : Glib.Values.GValues;
      User   : Source_View);
   --  Callback for the "side_columns_configuration_changed" signal

   procedure Invalidate_Window (User : access Source_View_Record'Class);
   --  Redraw the buffer window

   procedure Line_Highlight_Change_Handler
     (Buffer : access Source_Buffer_Record'Class;
      Params : Glib.Values.GValues;
      User   : Source_View);
   --  Callback for the "line_highlight_change" signal

   procedure Redraw_Columns
     (View : access Source_View_Record'Class;
      Cr   : Cairo_Context);
   --  Redraw the left area

   function On_Delete
     (View  : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean;
   --  Callback for the "delete_event" signal

   function On_Button_Press
     (View  : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean;
   --  Callback for the "button_press_event"

   procedure Restore_Cursor_Position
     (View : access Source_View_Record'Class);
   --  Restore the stored cursor position

   type On_Pref_Changed is new Preferences_Hooks_Function with record
      View : Source_View;
   end record;
   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Preference);
   --  Called when the preferences have changed, to refresh the editor
   --  appropriately.

   type On_File_Saved is new File_Hooks_Function with record
      View : Source_View;
   end record;
   overriding procedure Execute
     (Self   : On_File_Saved;
      Kernel : not null access Kernel_Handle_Record'Class;
      File   : Virtual_File);
   --  Callback for "File_Saved_Signal"

   procedure Size_Allocated_Before (View : access Gtk_Widget_Record'Class);
   --  Called before a new size is allocated

   procedure Size_Allocated (View : access Gtk_Widget_Record'Class);
   --  Called when a new size has been allocated

   procedure Paste_Clipboard_Before (View : access Gtk_Widget_Record'Class);
   --  Called before pasting the clipboard

   function Cursor_Screen_Position
     (View : access Source_View_Record'Class) return Gdouble;
   --  Return the cursor position on screen.
   --  0.0 for the top of the view, 1.0 for the bottom of the view.
   --  (< 0.0 or > 1.0 if the cursor is off-screen)

   procedure Remove_Synchronization
     (View : access Source_View_Record'Class);
   --  Remove the synchronized scrolling loop related to this editor

   procedure On_Scroll (View : access Gtk_Widget_Record'Class);
   --  Callback when the adjustments have changed

   function Scroll_Timeout (View : Source_View) return Boolean;
   --  Scroll to View.Scroll_To_Value

   function Side_Area_Expose_Event_Cb
     (Widget : access Gtk_Widget_Record'Class;
      Cr     : Cairo_Context) return Boolean;
   --  Callback for an "expose" event on the speed bar

   function Scroll_Bar_Draw
     (Object : access GObject_Record'Class;
      Cr     : Cairo_Context) return Boolean;
   --  Draw on the scroll bar

   procedure Speed_Bar_Size_Allocate_Cb
     (Widget : access Gtk_Widget_Record'Class);
   --  Callback for an "size_allocate" signal on the speed bar

   procedure Register_Idle_Column_Redraw (View : Source_View);
   --  Register an idle redrawing of the side columns

   procedure Recompute_Visible_Area (View : access Source_View_Record'Class);
   --  Recompute the currently visible area in the view

   procedure Size_Side_Column (View : access Source_View_Record'Class);
   --  Resize the side column area

   procedure Draw_Speed_Column_Data (View : access Source_View_Record'Class);
   --  Draw the data in the speed column on the cache.

   ----------------------
   -- Size_Side_Column --
   ----------------------

   procedure Size_Side_Column (View : access Source_View_Record'Class) is
   begin
      Set_Size_Request
        (View.Area, View.Speed_Bar_Width + 1 + View.Side_Info_Width, -1);
   end Size_Side_Column;

   -------------------
   -- As_Is_Enabled --
   -------------------

   function As_Is_Enabled
     (View : access Source_View_Record'Class) return Boolean is
   begin
      return View.As_Is_Mode in Enabled .. Sticky_Enabled;
   end As_Is_Enabled;

   ----------------------
   -- Reset_As_Is_Mode --
   ----------------------

   procedure Reset_As_Is_Mode (View : access Source_View_Record'Class) is
   begin
      if View.As_Is_Mode = Enabled then
         View.As_Is_Mode := Disabled;
      end if;
   end Reset_As_Is_Mode;

   ---------------------------------
   -- Register_Idle_Column_Redraw --
   ---------------------------------

   procedure Register_Idle_Column_Redraw (View : Source_View) is
   begin
      if View.Get_Realized
        and then not View.Idle_Redraw_Registered
      then
         View.Idle_Redraw_Registered := True;
         View.Idle_Redraw_Id := Source_View_Idle.Idle_Add
           (Idle_Column_Redraw'Access, View);
      end if;
   end Register_Idle_Column_Redraw;

   --------------------
   -- Scroll_Timeout --
   --------------------

   function Scroll_Timeout (View : Source_View) return Boolean is
   begin
      Set_Value (Get_Vadjustment (View.Scroll), View.Scroll_To_Value);
      View.Scroll_Requested := False;
      return False;
   end Scroll_Timeout;

   ----------------------------
   -- Cursor_Screen_Position --
   ----------------------------

   function Cursor_Screen_Position
     (View : access Source_View_Record'Class) return Gdouble
   is
      Y, Height   : Gint;
      Rect        : Gdk_Rectangle;
      Insert_Mark : constant Gtk_Text_Mark := View.Saved_Cursor_Mark;
      Iter        : Gtk_Text_Iter;
   begin
      Get_Iter_At_Mark (Get_Buffer (Source_View (View)), Iter, Insert_Mark);
      Get_Line_Yrange (Source_View (View), Iter, Y, Height);
      Get_Visible_Rect (Source_View (View), Rect);

      if Rect.Height = 0 then
         return 0.5;
      else
         return (Gdouble (Y) - Gdouble (Rect.Y)) / Gdouble (Rect.Height);
      end if;
   end Cursor_Screen_Position;

   -----------
   -- Setup --
   -----------

   procedure Setup (Data : Source_View; Id : Gtk.Handlers.Handler_Id) is
   begin
      Gtk.Handlers.Add_Watch (Id, Data);
   end Setup;

   --------------------------
   -- Save_Cursor_Position --
   --------------------------

   procedure Save_Cursor_Position (View : access Source_View_Record'Class) is
      Buffer      : constant Source_Buffer :=
                      Source_Buffer (Get_Buffer (View));
      Insert_Iter : Gtk_Text_Iter;
   begin
      Get_Iter_At_Mark (Buffer, Insert_Iter, Get_Insert (Buffer));
      Move_Mark (Buffer, View.Saved_Cursor_Mark, Insert_Iter);
   end Save_Cursor_Position;

   -------------------
   -- Acquire_Focus --
   -------------------

   procedure Acquire_Focus (View : access Source_View_Record) is
   begin
      Set_Focus_Child (View.Child);
   end Acquire_Focus;

   -----------------------------
   -- Restore_Cursor_Position --
   -----------------------------

   procedure Restore_Cursor_Position
     (View : access Source_View_Record'Class)
   is
      Buffer      : constant Source_Buffer :=
                      Source_Buffer (Get_Buffer (View));
      Insert_Iter : Gtk_Text_Iter;
      Cursor_Iter : Gtk_Text_Iter;
   begin
      Get_Iter_At_Mark (Buffer, Insert_Iter, View.Saved_Cursor_Mark);

      --  If the cursor has not moved, do not do anything

      Get_Iter_At_Mark (Buffer, Cursor_Iter, Get_Insert (Buffer));

      if Equal (Cursor_Iter, Insert_Iter) then
         return;
      end if;

      --  ??? Do we want to save/restore the selection bound as well ?
      Place_Cursor (Buffer, Insert_Iter);
   end Restore_Cursor_Position;

   ------------
   -- Delete --
   ------------

   procedure Delete (View : access Source_View_Record) is
   begin
      View.Area.Unref;
      View.Area := null;

      if View.Speed_Column_Buffer /= Null_Surface then
         Destroy (View.Speed_Column_Buffer);
         View.Speed_Column_Buffer := Null_Surface;
      end if;

      Delete_Mark (Get_Buffer (View), View.Saved_Cursor_Mark);

      if View.Connect_Expose_Registered then
         Glib.Main.Remove (View.Connect_Expose_Id);
      end if;

      if View.Idle_Redraw_Registered then
         Glib.Main.Remove (View.Idle_Redraw_Id);
      end if;

      if View.Redraw_Registered then
         Glib.Main.Remove (View.Redraw_Idle_Handler);
      end if;

      --  Mark the idle loops as registered, so that they can no longer be
      --  registered once the view has been destroyed.

      View.Idle_Redraw_Registered := True;
      View.Connect_Expose_Registered := True;

      Remove_Synchronization (View);

      if View.Scroll_Requested then
         Glib.Main.Remove (View.Scroll_Timeout);
      end if;
   end Delete;

   ---------------------
   -- On_Button_Press --
   ---------------------

   function On_Button_Press
     (View  : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean
   is
      pragma Unreferenced (Event);
      Src_View : constant Source_View := Source_View (View);
   begin
      if Src_View.Highlight_Blocks then
         Invalidate_Window (Src_View);
      end if;

      return False;
   exception
      when E : others =>
         Trace (Me, E);
         return False;
   end On_Button_Press;

   ---------------
   -- On_Delete --
   ---------------

   function On_Delete
     (View  : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean
   is
      pragma Unreferenced (Event);
      Src_View : constant Source_View := Source_View (View);
   begin
      Delete (Src_View);
      return False;

   exception
      when E : others =>
         Trace (Me, E);
         return False;
   end On_Delete;

   ----------------
   -- Realize_Cb --
   ----------------

   procedure Realize_Cb (Widget : access Gtk_Widget_Record'Class) is
      View : constant Source_View := Source_View (Widget);
   begin
      --  Now that the window is realized, we can set the font and
      --  the size of the left border window size.
      Modify_Font (View, Default_Style.Get_Pref_Font);

   exception
      when E : others => Trace (Me, E);
   end Realize_Cb;

   -----------------------
   -- Invalidate_Window --
   -----------------------

   procedure Invalidate_Window (User : access Source_View_Record'Class) is
      procedure Invalidate (Window : Gdk_Window);
      --  Invalidate Window

      procedure Invalidate (Window : Gdk_Window) is
         X, Y, W, H : Gint;
      begin
         if Window /= null then
            Get_Geometry (Window, X, Y, W, H);
            Gdk.Window.Invalidate_Rect (Window, (X, Y, W, H), True);
         end if;
      end Invalidate;

   begin
      Invalidate (Get_Window (User, Text_Window_Text));
      Invalidate (Get_Window (User.Area));
      Invalidate (User.Scroll.Get_Vscrollbar.Get_Window);
   end Invalidate_Window;

   ---------------------------
   -- Line_Highlight_Redraw --
   ---------------------------

   function Line_Highlight_Redraw (User : Source_View) return Boolean is
   begin
      User.Redraw_Registered := False;
      Invalidate_Window (User);

      return False;
   exception
      when E : others =>
         Trace (Me, E);
         return False;
   end Line_Highlight_Redraw;

   -----------------------------------
   -- Line_Highlight_Change_Handler --
   -----------------------------------

   procedure Line_Highlight_Change_Handler
     (Buffer : access Source_Buffer_Record'Class;
      Params : Glib.Values.GValues;
      User   : Source_View)
   is
      pragma Unreferenced (Params, Buffer);
   begin
      if not User.Redraw_Registered
        and then User.Get_Realized
      then
         User.Redraw_Idle_Handler :=
           Source_View_Idle.Idle_Add (Line_Highlight_Redraw'Access, User);
         User.Redraw_Registered := True;
      end if;

   exception
      when E : others => Trace (Me, E);
   end Line_Highlight_Change_Handler;

   ---------------------------------------
   -- Buffer_Information_Change_Handler --
   ---------------------------------------

   procedure Buffer_Information_Change_Handler
     (Buffer : access Source_Buffer_Record'Class;
      Params : Glib.Values.GValues;
      User   : Source_View)
   is
      pragma Unreferenced (Params);
   begin
      User.Side_Info_Width := Gint (Get_Total_Column_Width (Buffer));
      Size_Side_Column (User);

      --  Invalidate the window so that the columns and line and
      --  block highlightings are redrawn.
      Invalidate_Window (User);

   exception
      when E : others => Trace (Me, E);
   end Buffer_Information_Change_Handler;

   ---------------------------------
   -- Side_Columns_Change_Handler --
   ---------------------------------

   procedure Side_Columns_Change_Handler
     (Buffer : access Source_Buffer_Record'Class;
      Params : Glib.Values.GValues;
      User   : Source_View)
   is
      pragma Unreferenced (Params, Buffer);
   begin
      Register_Idle_Column_Redraw (User);
   end Side_Columns_Change_Handler;

   ----------------------------------------
   -- Side_Columns_Config_Change_Handler --
   ----------------------------------------

   procedure Side_Columns_Config_Change_Handler
     (Buffer : access Source_Buffer_Record'Class;
      Params : Glib.Values.GValues;
      User   : Source_View)
   is
      pragma Unreferenced (Buffer, Params);
   begin
      User.Side_Columns_Up_To_Date := False;
      Register_Idle_Column_Redraw (User);

   exception
      when E : others => Trace (Me, E);
   end Side_Columns_Config_Change_Handler;

   ------------------------
   -- Idle_Column_Redraw --
   ------------------------

   function Idle_Column_Redraw (View : Source_View) return Boolean is
   begin
      if View.Speed_Column_Buffer /= Null_Surface then
         Destroy (View.Speed_Column_Buffer);
         View.Speed_Column_Buffer := Null_Surface;
      end if;

      Invalidate_Window (View);

      View.Idle_Redraw_Registered := False;
      return False;

   exception
      when E : others =>
         Trace (Me, E);
         return False;
   end Idle_Column_Redraw;

   -----------------
   -- On_Mark_Set --
   -----------------

   procedure On_Mark_Set
     (Buffer : access Source_Buffer_Record'Class;
      Params : Glib.Values.GValues;
      User   : Source_View)
   is
      Mark : constant Gtk_Text_Mark :=
        Get_Text_Mark (Glib.Values.Nth (Params, 2));
   begin
      if Mark = Buffer.Get_Selection_Bound then
         --  Test whether we have the focus
         if not Buffer.Context_Is_Frozen
           and then Gtkada.MDI.MDI_Child (User.Child) =
           Get_Focus_Child (Get_MDI (User.Kernel))
         then
            --  We have changed the selection: emit "context_changed" here.
            User.Kernel.Context_Changed
              (Build_Editor_Context (User, Location_Cursor));
         end if;
      end if;
   end On_Mark_Set;

   -----------------------------
   -- Cursor_Position_Changed --
   -----------------------------

   procedure Cursor_Position_Changed
     (Buffer : access Source_Buffer_Record'Class;
      Params : Glib.Values.GValues;
      User   : Source_View)
   is
      Line : Gint;
      Iter : Gtk_Text_Iter;
      pragma Unreferenced (Params);
      Has_Focus : constant Boolean :=
        Gtkada.MDI.MDI_Child (User.Child) =
        Get_Focus_Child (Get_MDI (User.Kernel));
   begin
      Buffer.Get_Iter_At_Mark (Iter, Buffer.Get_Insert);
      Line := Get_Line (Iter) + 1;

      --  This call moves the Saved_Cursor_Mark, which is used by
      --  Scroll_To_Cursor_Location to leave the cursor visible on the screen.
      if Has_Focus then
         Save_Cursor_Position (User);

         --  We have changed the cursor position: emit "context_changed" here.
         if not Buffer.Context_Is_Frozen then
            User.Kernel.Context_Changed
              (Build_Editor_Context (User, Location_Cursor));
         end if;
      end if;

      --  If we are highlighting the current line, re-expose the entire view
      --  if the line has changed. Same thing if we are doing block
      --  highlighting and the block has changed.

      --  ??? Potential optimization here: this procedure is called a lot when
      --  the user keeps the down arrow key pressed.
      --  Do not remove: gtk+ will *not* properly force a redraw of the
      --  relevant areas.

      if (User.Highlight_Current
          and then User.Current_Line /= Line)
        or else
          (User.Highlight_Blocks
           and then User.Current_Block /=
             Get_Block (Buffer, Editable_Line_Type (Line), False,
                        Filter => Categories_For_Block_Highlighting))
      then
         Invalidate_Window (User);
      end if;

      User.Current_Line := Line;
   end Cursor_Position_Changed;

   ---------------------------
   -- Size_Allocated_Before --
   ---------------------------

   procedure Size_Allocated_Before (View : access Gtk_Widget_Record'Class) is
      V : constant Source_View := Source_View (View);
   begin
      if V.Cursor_Position = Gdouble'Last then
         --  The size has never been allocated before: the cursor should be in
         --  the middle.
         V.Cursor_Position := 0.5;
      else
         Source_View (View).Cursor_Position :=
           Cursor_Screen_Position (Source_View (View));
      end if;
   exception
      when E : others => Trace (Me, E);
   end Size_Allocated_Before;

   --------------------
   -- Size_Allocated --
   --------------------

   procedure Size_Allocated (View : access Gtk_Widget_Record'Class) is
      V      : constant Source_View := Source_View (View);
   begin
      --  Recompute the lines currently displayed
      Recompute_Visible_Area (V);

      --  Keep the cursor on screen when the editor is resized.
      --  Do not do this if the editor is synchronized with another editor.

      if V.Synchronized_Editor = null
        and then V.Cursor_Position >= 0.0
        and then V.Cursor_Position <= 1.0
      then
         if V.Position_Set_Explicitely (False) then
            Scroll_To_Cursor_Location (V, Center);
         end if;
      end if;
   end Size_Allocated;

   ----------------------------
   -- Paste_Clipboard_Before --
   ----------------------------

   procedure Paste_Clipboard_Before (View : access Gtk_Widget_Record'Class) is
      V      : constant Source_View := Source_View (View);
      Buffer : constant Source_Buffer := Source_Buffer (Get_Buffer (V));
   begin
      Prevent_CR_Insertion (Buffer, True);
   exception
      when E : others => Trace (Me, E);
   end Paste_Clipboard_Before;

   --------------------------------
   -- Speed_Bar_Size_Allocate_Cb --
   --------------------------------

   procedure Speed_Bar_Size_Allocate_Cb
     (Widget : access Gtk_Widget_Record'Class)
   is
      View : constant Source_View := Source_View (Widget);
   begin
      if View.Speed_Column_Buffer /= Null_Surface then
         Destroy (View.Speed_Column_Buffer);
         View.Speed_Column_Buffer := Null_Surface;
      end if;

   exception
      when E : others => Trace (Me, E);
   end Speed_Bar_Size_Allocate_Cb;

   ----------------------------
   -- Recompute_Visible_Area --
   ----------------------------

   procedure Recompute_Visible_Area (View : access Source_View_Record'Class) is
      Top_In_Buffer    : Gint;
      Bottom_In_Buffer : Gint;
      Dummy_Gint       : Gint;
      Iter             : Gtk_Text_Iter;
      Top_Line         : Buffer_Line_Type;
      Bottom_Line      : Buffer_Line_Type;

      Text_Window : constant Gdk.Gdk_Window :=
                           View.Get_Window (Text_Window_Text);

      Buffer : constant Source_Buffer := Source_Buffer (Get_Buffer (View));

      X, Y, Width, Height : Gint;
   begin
      if Text_Window = null then
         --  This can happen when the view is not realized.
         return;
      end if;

      --  Figure out the top and bottom line

      Get_Geometry (Text_Window, X, Y, Width, Height);

      Window_To_Buffer_Coords
        (View, Text_Window_Text,
         Window_X => 0, Window_Y => Y,
         Buffer_X => Dummy_Gint, Buffer_Y => Top_In_Buffer);
      Window_To_Buffer_Coords
        (View, Text_Window_Text,
         Window_X => 0, Window_Y => Y + Height,
         Buffer_X => Dummy_Gint, Buffer_Y => Bottom_In_Buffer);

      Get_Line_At_Y (View, Iter, Top_In_Buffer, Dummy_Gint);
      Top_Line := Buffer_Line_Type (Get_Line (Iter) + 1);

      Get_Line_At_Y (View, Iter, Bottom_In_Buffer, Dummy_Gint);
      Bottom_Line := Buffer_Line_Type (Get_Line (Iter) + 1);

      --  If one of the values hadn't been initialized, display the
      --  whole range of lines.
      View.Top_Line    := Top_Line;
      View.Bottom_Line := Bottom_Line;

      --  Compute the smallest connected area that needs refresh

      if View.Side_Columns_Up_To_Date then
         Find_Top_Line :
         while Top_Line <= Bottom_Line loop
            exit Find_Top_Line when
              Line_Needs_Refresh (Buffer, Top_Line);

            Top_Line := Top_Line + 1;
         end loop Find_Top_Line;

         Find_Bottom_Line :
         while Bottom_Line >= Top_Line loop
            exit Find_Bottom_Line when
              Line_Needs_Refresh (Buffer, Bottom_Line);

            Bottom_Line := Bottom_Line - 1;
         end loop Find_Bottom_Line;
      else
         View.Side_Columns_Up_To_Date := True;
      end if;
   end Recompute_Visible_Area;

   ---------------------
   -- Scroll_Bar_Draw --
   ---------------------

   function Scroll_Bar_Draw
     (Object : access GObject_Record'Class;
      Cr     : Cairo_Context) return Boolean
   is
      View   : constant Source_View := Source_View (Object);

   begin
      --  The drawing of the scrollbar is a two-step process: first we draw
      --  the regular scrollbar using the Gtk+ routines (so that it is rendered
      --  according to the theme). Then we draw on top of it the speed column
      --  data.
      --
      --  The Gtk+ mechanism goes through this subprogram: we use the flag
      --  View.Draw_The_Scrollbar to know whether to do the real drawing
      --  or the drawing with side info.

      if View.Draw_The_Scrollbar then
         --  The flag is set: let Gtk draw normally.
         return False;
      end if;

      --  If we reach this, this means we want to paint the scrollbar plus
      --  the line info data. First paint the scrollbar.
      View.Draw_The_Scrollbar := True;
      View.Scroll.Get_Vscrollbar.Draw (Cr);
      View.Draw_The_Scrollbar := False;

      --  Draw the speed column data on the cache
      Draw_Speed_Column_Data (View);

      --  Draw the cache on top of the scrollbar
      Set_Source_Surface (Cr, View.Speed_Column_Buffer, 0.0, 0.0);
      Paint (Cr);

      return True;
   end Scroll_Bar_Draw;

   -------------------------------
   -- Side_Area_Expose_Event_Cb --
   -------------------------------

   function Side_Area_Expose_Event_Cb
     (Widget : access Gtk_Widget_Record'Class;
      Cr     : Cairo_Context) return Boolean
   is
      View : constant Source_View := Source_View (Widget);

   begin
      --  Paint the background, common to both the speed column and the side
      --  info column

      Redraw_Columns (View, Cr);

      return True;
   end Side_Area_Expose_Event_Cb;

   -------------------
   -- On_Draw_Layer --
   -------------------

   procedure On_Draw_Layer
     (Widget : System.Address;
      Layer  : Gtk_Text_View_Layer;
      Cr     : Cairo.Cairo_Context)
   is
      View : constant Source_View :=
        Source_View (Get_User_Data_Or_Null (Widget));
      Buffer : constant Source_Buffer := Source_Buffer (Get_Buffer (View));
      Window : constant Gdk.Gdk_Window := View.Get_Window (Text_Window_Text);

      Rect             : Gdk_Rectangle;
      Top_Line         : Buffer_Line_Type;
      Bottom_Line      : Buffer_Line_Type;
      --  Range of the visible area

      Y, Height        : Gint;
      --  Geometry for the window

      procedure Draw_Below (Start_Iter : Gtk_Text_Iter);
      --  Draw anything that needs to be visible above the background, but
      --  below the text. In particular, highlighting the current line and
      --  any other full-line highlighting.
      --  The iter should point to the first visible line in the cursor

      procedure Draw_Above;
      --  Draw anything that should appear above the text and selection
      --  background.

      procedure Draw_Below (Start_Iter : Gtk_Text_Iter) is
         Line_Y           : Gint;
         Line_Height      : Gint;
         Dummy            : Gint := 0;
         Buffer_Line_Y    : Gint;
         Dummy_Gint       : Gint;
         Success          : Boolean;
         Iter             : Gtk_Text_Iter := Start_Iter;
         Color            : Gdk_RGBA;
      begin
         for Line in Top_Line .. Bottom_Line loop
            Color := Get_Highlight_Color
              (Buffer, Line, Context => Highlight_Editor);

            if Color /= Null_RGBA then
               Get_Line_Yrange (View, Iter, Line_Y, Line_Height);
               Buffer_To_Window_Coords
                 (View, Text_Window_Text,
                  Dummy, Line_Y, Dummy, Buffer_Line_Y);

               Set_Source_Color (Cr, Color);
               Cairo.Rectangle
                 (Cr,
                  Gdouble (Margin), Gdouble (Buffer_Line_Y),
                  Gdouble (Rect.Width), Gdouble (Line_Height));
               Cairo.Fill (Cr);
            end if;

            Forward_Line (Iter, Success);
         end loop;

         --  Highlight the line that contains the cursor

         if View.Highlight_Current then
            Get_Iter_At_Mark (Buffer, Iter, View.Saved_Cursor_Mark);
            Get_Line_Yrange (View, Iter, Line_Y, Line_Height);
            Buffer_To_Window_Coords
              (View, Text_Window_Text, Dummy, Line_Y, Dummy, Buffer_Line_Y);

            if View.Highlight_As_Line then
               Set_Line_Width (Cr, 1.0);

               Draw_Line (Cr, View.Current_Line_Color,
                          -1,
                          Buffer_Line_Y + Line_Height,
                          Rect.Width,
                          Buffer_Line_Y + Line_Height);
            else
               Set_Source_RGBA (Cr, View.Current_Line_Color);
               Cairo.Rectangle
                 (Cr,
                  0.0,
                  Gdouble (Buffer_Line_Y),
                  Gdouble (Rect.Width),
                  Gdouble (Line_Height));
               Cairo.Fill (Cr);
            end if;
         end if;
      end Draw_Below;

      ----------------
      -- Draw_Above --
      ----------------

      procedure Draw_Above is
         Column    : constant Gint := Gint (Highlight_Column.Get_Pref);

         procedure Draw_Block (B : in out Block_Record);
         --  Draw block B

         ----------------
         -- Draw_Block --
         ----------------

         procedure Draw_Block (B : in out Block_Record) is
            Bracket_Length : constant := 15;
            --  The length of upper and lower parts of the bracket

            Bracket_Offset : constant := 2;
            --  The distance between brackets and text

            Block_Begin_Y  : Gint;
            Block_End_Y    : Gint;
            Y              : Gint;
            Height         : Gint;
            X              : Gint;

            Buffer_First   : constant Buffer_Line_Type
              := Get_Buffer_Line (Buffer, B.First_Line);
            Buffer_Last    : constant Buffer_Line_Type
              := Get_Buffer_Line (Buffer, B.Last_Line);

            First          : constant Gint := Gint (Buffer_First - 1);
            Last           : Gint := Gint (Buffer_Last - 1);
            Offset         : Integer;
            Iter           : Gtk_Text_Iter;
            Dummy          : Gint := 0;

         begin
            if Buffer_First > Bottom_Line
              or else Buffer_Last < Top_Line
            then
               return;
            end if;

            Calculate_Screen_Offset (Buffer, B);
            Offset := B.Stored_Offset;

            --  Do not draw blocks that are on the first column

            if Offset <= 1 then
               return;
            end if;

            if Last < First then
               Last := First;
            end if;

            Get_Iter_At_Line_Offset (Buffer, Iter, First, 0);
            Get_Line_Yrange  (View, Iter, Block_Begin_Y, Dummy);

            Get_Iter_At_Line_Offset (Buffer, Iter, Last, 0);
            Get_Line_Yrange (View, Iter, Dummy, Height);

            Height := Dummy + Height - Block_Begin_Y;

            Buffer_To_Window_Coords
              (View,
               Text_Window_Text, Dummy, Block_Begin_Y, Dummy, Y);

            X := (Gint (Offset - 1) * View.Width_Of_256_Chars) / 256 -
              Bracket_Offset - Rect.X + Margin;

            if Y > 0 then
               Block_Begin_Y := Y;
            else
               Block_Begin_Y := 0;
            end if;

            Block_End_Y := Y + Height;

            Set_Line_Width (Cr, 1.0);

            if Block_End_Y > Rect.Height then
               Block_End_Y := Rect.Height;
            else
               Draw_Line
                 (Cr, View.Current_Block_Color,
                  X, Block_End_Y, X + Bracket_Length, Block_End_Y);
            end if;

            Draw_Line
              (Cr, View.Current_Block_Color,
               X, Block_Begin_Y, X, Block_End_Y);

            if Block_Begin_Y /= 0 then
               Draw_Line
               (Cr, View.Current_Block_Color,
                X, Block_Begin_Y, X + Bracket_Length, Block_Begin_Y);
            end if;
         end Draw_Block;

         X : Gint;

      begin
         --  Highlight the current block

         if View.Highlight_Blocks then
            declare
               Line : Editable_Line_Type;
               Column : Visible_Column_Type;
            begin
               Buffer.Get_Cursor_Position (Line, Column);
               View.Current_Block := Get_Block
                 (Buffer,
                  Line,
                  False,
                  Filter => Categories_For_Block_Highlighting,
                  Column => Column);
               Draw_Block (View.Current_Block);
            end;
         end if;

         --  Redraw the line showing the nth column if needed

         if Column > 0 then
            X := (Column * View.Width_Of_256_Chars) / 256 - Rect.X + Margin;

            Save (Cr);
            Set_Line_Width (Cr, 1.0);
            Draw_Line (Cr, View.Current_Block_Color, X, Y, X, Y + Rect.Height);
            Restore (Cr);
         end if;
      end Draw_Above;

      Dummy_Gint : Gint;
      X, Width   : Gint;
      Iter       : Gtk_Text_Iter;

      Top_In_Buffer    : Gint;
      Bottom_In_Buffer : Gint;
      --  buffer-coordinates for first and last visible line

   begin
      --  ??? Could we cache this between the two calls to On_Draw_Layer
      Get_Visible_Rect (View, Rect);
      Buffer_To_Window_Coords (View, Text_Window_Text, Rect.X, Rect.Y, X, Y);

      --  Get the window coordinates

      Get_Geometry (Window, X, Y, Width, Height);

      Window_To_Buffer_Coords
        (View, Text_Window_Text,
         Window_X => 0, Window_Y => Y,
         Buffer_X => Dummy_Gint, Buffer_Y => Top_In_Buffer);
      Window_To_Buffer_Coords
        (View, Text_Window_Text,
         Window_X => 0, Window_Y => Y + Height,
         Buffer_X => Dummy_Gint, Buffer_Y => Bottom_In_Buffer);

      Get_Line_At_Y (View, Iter, Bottom_In_Buffer, Dummy_Gint);
      Bottom_Line := Buffer_Line_Type (Get_Line (Iter) + 1);

      Get_Line_At_Y (View, Iter, Top_In_Buffer, Dummy_Gint);
      Top_Line := Buffer_Line_Type (Get_Line (Iter) + 1);

      case Layer is
         when Text_View_Layer_Below =>
            Draw_Below (Iter);
         when Text_View_Layer_Above =>
            Draw_Above;
      end case;

   exception
      when E : others =>
         Trace (Me, E);
   end On_Draw_Layer;

   ------------------------
   -- Focus_Out_Event_Cb --
   ------------------------

   function Focus_Out_Event_Cb
     (Widget : access Gtk_Widget_Record'Class) return Boolean
   is
      View   : constant Source_View := Source_View (Widget);
      Buffer : constant Source_Buffer := Source_Buffer (Get_Buffer (View));
      Result : Boolean;

      pragma Unreferenced (Result);

   begin
      Src_Editor_Module.On_Ed_View_Focus_Lost
        (MDI_Child (View.Child), Buffer.Get_Filename);
      View.As_Is_Mode := Disabled;

      Save_Cursor_Position (View);
      External_End_Action (Buffer);

      Stop_Selection_Drag (View);

      return False;

   exception
      when E : others =>
         Trace (Me, E);
         return False;
   end Focus_Out_Event_Cb;

   -----------------------
   -- Focus_In_Event_Cb --
   -----------------------

   function Focus_In_Event_Cb
     (Widget : access Gtk_Widget_Record'Class) return Boolean
   is
      View : constant Source_View   := Source_View (Widget);
   begin
      if not View.Button_Pressed
        and then not Selection_Exists (Get_Buffer (View))
      then
         Restore_Cursor_Position (View);
      end if;

      return False;

   exception
      when E : others =>
         Trace (Me, E);
         return False;
   end Focus_In_Event_Cb;

   --------------------------------
   -- View_On_Drag_Data_Received --
   --------------------------------

   procedure View_On_Drag_Data_Received
     (Self    : access Gtk_Widget_Record'Class;
      Context : not null access Gdk.Drag_Contexts.Drag_Context_Record'Class;
      X       : Gint;
      Y       : Gint;
      Data    : Gtk.Selection_Data.Gtk_Selection_Data;
      Info    : Guint;
      Time    : Guint)
   is
      use GNATCOLL.VFS;
      pragma Unreferenced (X, Y, Info, Time);
      View : constant Source_View := Source_View (Self);
   begin
      if Atom_Name (Data.Get_Target) = "text/uri-list" then
         Gtk.Handlers.Emit_Stop_By_Name
           (Object => Self, Name => "drag-data-received");
         declare
            Uris : constant GNAT.Strings.String_List := Data.Get_Uris;
         begin
            for Url of Uris loop
               Open_File_Action_Hook.Run
                 (View.Kernel,
                  Create (+Filename_From_URI (Url.all, null)),
                  Project  => No_Project,  --  will choose one at random
                  New_File => False);
               Gtk.Dnd.Finish (Context => Drag_Context (Context),
                               Success => True,
                               Del     => True);
            end loop;
         end;
      else

         --  This is not a list of URIs, so this is probably a text DND:
         --  drop it if the editor is read-only.
         if not Get_Editable (Source_View (Self)) then
            Gtk.Handlers.Emit_Stop_By_Name
              (Object => Self, Name => "drag-data-received");
         end if;
      end if;

      --  Notify the associated buffer that we are performing a text drag n
      --  drop;
      declare
         Buffer : constant Source_Buffer := Source_Buffer (Get_Buffer (View));
      begin
         Buffer.Notify_Text_Drag_N_Drop;
      end;
   end View_On_Drag_Data_Received;

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy (View : access Gtk_Widget_Record'Class) is
      V : constant Source_View := Source_View (View);
   begin
      Trace (Me, "Source_View is being destroyed");

      --  The buffer might outlive the view: deregister all callbacks to this
      --  view, since it is being destroyed. Otherwise the call to
      --  Register_View below could lead to, for instance, a mark being set
      --  or the cursor location changing, and the registered callbacks would
      --  access the view data.

      for J in V.Source_Buffer_Handlers'Range loop
         Gtk.Handlers.Disconnect
           (Get_Buffer (V),
            V.Source_Buffer_Handlers (J));
      end loop;

      Register_View (Source_Buffer (Get_Buffer (V)), Add => False);
   end On_Destroy;

   ---------------------
   -- View_Class_Init --
   ---------------------

   procedure View_Class_Init (Self : GObject_Class) is
   begin
      Set_Draw_Layer (Self, Handler => On_Draw_Layer'Access);
   end View_Class_Init;

   -------------------
   -- View_Get_Type --
   -------------------

   function View_Get_Type return Glib.GType is
   begin
      Glib.Object.Initialize_Class_Record
        (Ancestor     => Gtk.Text_View.Get_Type,
         Type_Name    => "SourceView",
         Class_Record => View_Class_Record,
         Class_Init   => View_Class_Init'Access);
      return View_Class_Record.The_Type;
   end View_Get_Type;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (View    : out Source_View;
      Project : GNATCOLL.Projects.Project_Type;
      Scroll  : access Gtk.Scrolled_Window.Gtk_Scrolled_Window_Record'Class;
      Area    : Gtk.Drawing_Area.Gtk_Drawing_Area;
      Buffer  : Src_Editor_Buffer.Source_Buffer;
      Kernel  : access GPS.Kernel.Kernel_Handle_Record'Class) is
   begin
      View := new Source_View_Record;
      Initialize (View, Project, Scroll, Area, Buffer, Kernel);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (View    : access Source_View_Record;
      Project : GNATCOLL.Projects.Project_Type;
      Scroll  : access Gtk.Scrolled_Window.Gtk_Scrolled_Window_Record'Class;
      Area    : Gtk.Drawing_Area.Gtk_Drawing_Area;
      Buffer  : Src_Editor_Buffer.Source_Buffer;
      Kernel  : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Insert_Iter : Gtk_Text_Iter;
      Hook        : access On_Pref_Changed;
      Hpolicy, Vpolicy : Gtk.Enums.Gtk_Policy_Type;
      Value       : GValue;
   begin
      --  Initialize the Source_View. Some of the fields can not be initialized
      --  until the widget is realized or mapped. Their initialization is thus
      --  done at that point.

      pragma Assert (Buffer /= null);

      G_New (View, View_Get_Type);
      View.Set_Buffer (Buffer);

      Gtk.Dnd.Dest_Set
        (View,
         Dest_No_Default, --  GtkTextView handles drag_drop signal
         Target_Table,
         Gdk.Dnd.Action_Any);

      View.Kernel  := Kernel_Handle (Kernel);
      View.Scroll  := Gtk_Scrolled_Window (Scroll);
      View.Area    := Area;
      View.Area.Ref;
      View.Set_Project (Project);

      --  Force the policy of the vertical scrollbar to "always", so it can
      --  display the speed info highlighting.
      View.Scroll.Get_Policy (Hpolicy, Vpolicy);
      View.Scroll.Set_Policy (Hpolicy, Policy_Always);

      --  Grab the size of the steppers. Needed to paint the highlighting
      --  on the "srcolling" part of the scrollbar.

      Init (Value, GType_Int);
      View.Scroll.Get_Vscrollbar.Style_Get_Property ("stepper-size", Value);
      View.Scrollbar_Stepper_Size := Get_Int (Value);
      View.Scroll.Get_Vscrollbar.Style_Get_Property ("stepper-spacing", Value);
      View.Scrollbar_Stepper_Size :=
        View.Scrollbar_Stepper_Size + Get_Int (Value);
      Unset (Value);

      Get_Style_Context (View.Area).Add_Class ("gps_gutter");
      Get_Style_Context (View.Scroll.Get_Vscrollbar).Add_Class ("gps_gutter");
      Get_Style_Context (View.Scroll.Get_Hscrollbar).Add_Class ("gps_gutter");

      Register_View (Buffer, Add => True);

      Set_Events
        (Area,
         Button_Motion_Mask or Button_Press_Mask or Button_Release_Mask);

      Return_Callback.Object_Connect
        (Area, Signal_Button_Press_Event,
         Marsh       => Return_Callback.To_Marshaller
           (Speed_Bar_Button_Press_Event_Cb'Access),
         After       => False,
         Slot_Object => View);

      Return_Callback.Object_Connect
        (Area, Signal_Motion_Notify_Event,
         Marsh       => Return_Callback.To_Marshaller
           (Speed_Bar_Button_Press_Event_Cb'Access),
         After       => False,
         Slot_Object => View);

      Return_Callback.Object_Connect
        (Area, Signal_Button_Release_Event,
         Marsh       => Return_Callback.To_Marshaller
           (Speed_Bar_Button_Release_Event_Cb'Access),
         After       => False,
         Slot_Object => View);

      Return_Callback.Object_Connect
        (View.Area, Signal_Draw,
         Marsh       => Return_Callback.To_Marshaller
           (Side_Area_Expose_Event_Cb'Access),
         After       => False,
         Slot_Object => View);

      View.Scroll.Get_Vscrollbar.On_Draw (Scroll_Bar_Draw'Access, View);

      View.On_Drag_Data_Received
        (View_On_Drag_Data_Received'Access);

      --  ??? Why connect twice to Size_Allocate
      Widget_Callback.Object_Connect
        (Area, Signal_Size_Allocate,
         Marsh       => Widget_Callback.To_Marshaller
           (Speed_Bar_Size_Allocate_Cb'Access),
         After       => False,
         Slot_Object => View);
      Widget_Callback.Connect
        (View, Signal_Size_Allocate,
         Widget_Callback.To_Marshaller (Size_Allocated'Access),
         After => True);
      Widget_Callback.Connect
        (View, Signal_Size_Allocate,
         Widget_Callback.To_Marshaller (Size_Allocated_Before'Access),
         After => False);

      Set_Left_Margin (View, Margin);

      Widget_Callback.Connect (View, Signal_Destroy, On_Destroy'Access);
      Widget_Callback.Connect
        (View, Signal_Realize,
         Marsh => Widget_Callback.To_Marshaller (Realize_Cb'Access),
         After => True);
      Return_Callback.Connect
        (View, Signal_Focus_In_Event,
         Marsh => Return_Callback.To_Marshaller (Focus_In_Event_Cb'Access),
         After => False);
      Return_Callback.Connect
        (View, Signal_Focus_Out_Event,
         Marsh => Return_Callback.To_Marshaller (Focus_Out_Event_Cb'Access),
         After => False);
      Return_Callback.Connect
        (View, Signal_Button_Press_Event,
         Marsh => Return_Callback.To_Marshaller (Button_Press_Event_Cb'Access),
         After => False);
      Return_Callback.Connect
        (View, Signal_Button_Release_Event,
         Marsh => Return_Callback.To_Marshaller
           (Button_Release_Event_Cb'Access),
         After => False);
      Return_Callback.Connect
        (View, Signal_Key_Press_Event,
         Marsh => Return_Callback.To_Marshaller (Key_Press_Event_Cb'Access),
         After => False);

      Widget_Callback.Connect
        (View, Signal_Paste_Clipboard,
         Widget_Callback.To_Marshaller (Paste_Clipboard_Before'Access),
         After => False);

      View.Source_Buffer_Handlers :=
        (Source_Buffer_Callback.Connect
           (Buffer, Signal_Cursor_Position_Changed,
            Cb        => Cursor_Position_Changed'Access,
            User_Data => Source_View (View),
            After     => True),

         Source_Buffer_Callback.Connect
           (Buffer, Signal_Mark_Set,
            Cb        => On_Mark_Set'Access,
            User_Data => Source_View (View),
            After     => True),

         Source_Buffer_Callback.Connect
           (Buffer, Signal_Side_Column_Changed,
            Cb        => Side_Columns_Change_Handler'Access,
            User_Data => Source_View (View),
            After     => True),

         Source_Buffer_Callback.Connect
           (Buffer, Signal_Side_Column_Configuration_Changed,
            Cb        => Side_Columns_Config_Change_Handler'Access,
            User_Data => Source_View (View),
            After     => True),

         Source_Buffer_Callback.Connect
           (Buffer, Signal_Buffer_Information_Changed,
            Cb        => Buffer_Information_Change_Handler'Access,
            User_Data => Source_View (View),
            After     => True),

         Source_Buffer_Callback.Connect
           (Buffer, Signal_Line_Highlights_Changed,
            Cb        => Line_Highlight_Change_Handler'Access,
            User_Data => Source_View (View),
            After     => True));

      Gtkada.Handlers.Return_Callback.Connect
        (View,
         Gtk.Widget.Signal_Delete_Event,
         Gtkada.Handlers.Return_Callback.To_Marshaller (On_Delete'Access),
         After => False);

      if Host = Windows then
         --  ??? Under Windows, a click does not refresh the entire area as it
         --  does under X. We need it to properly redraw current line and
         --  current block, therefore we do it manually.

         Gtkada.Handlers.Return_Callback.Object_Connect
           (View,
            Signal_Button_Press_Event,
            Gtkada.Handlers.Return_Callback.To_Marshaller
              (On_Button_Press'Access),
            View,
            After => False);
      end if;

      Hook := new On_Pref_Changed;
      Hook.View := Source_View (View);
      Hook.Execute (Kernel, null);
      Preferences_Changed_Hook.Add (Hook, Watch => View);

      File_Saved_Hook.Add
         (new On_File_Saved'
             (File_Hooks_Function with View => Source_View (View)),
          Watch => View);

      --  Connect in an idle callback, otherwise the lines-with-code in the
      --  debugger are recomputed all at once (before the editor has a size).

      View.Connect_Expose_Registered := True;
      View.Connect_Expose_Id := Source_View_Idle.Idle_Add
        (Connect_Expose'Access,
         Source_View (View));

      Get_Iter_At_Mark (Buffer, Insert_Iter, Get_Insert (Buffer));
      View.Saved_Cursor_Mark := Create_Mark (Buffer, "", Insert_Iter);

      Invalidate_Window (Source_View (View));

      Push_Current_Editor_Location_In_History (Kernel);

      Activate_Hyper_Mode (View);
   end Initialize;

   --------------------
   -- Connect_Expose --
   --------------------

   function Connect_Expose (View : Source_View) return Boolean is
   begin
      Widget_Callback.Object_Connect
        (Get_Vadjustment (View.Scroll),
         Signal_Value_Changed,
         Marsh       => Widget_Callback.To_Marshaller (On_Scroll'Access),
         After       => True,
         Slot_Object => View);

      Widget_Callback.Object_Connect
        (Get_Hadjustment (View.Scroll),
         Signal_Value_Changed,
         Marsh       => Widget_Callback.To_Marshaller (On_Scroll'Access),
         After       => True,
         Slot_Object => View);

      Invalidate_Window (View);

      View.Connect_Expose_Registered := False;

      --  If there is a synchronized editor, scroll this editor to align it
      --  with the synchronized editor. This is the opposite of the work done
      --  in On_Scroll.

      View.Scrolling := True;

      if View.Synchronized_Editor /= null
        and then View.Scroll /= null
        and then View.Synchronized_Editor.Scroll /= null
      then
         Set_Value
           (Get_Vadjustment (View.Scroll),
            Get_Value (Get_Vadjustment (View.Synchronized_Editor.Scroll)));

         Set_Value
           (Get_Hadjustment (View.Scroll),
            Get_Value (Get_Hadjustment (View.Synchronized_Editor.Scroll)));
      else
         if View.Position_Set_Explicitely (Reset => True) then
            Scroll_To_Cursor_Location (View, Center);
         end if;
      end if;

      View.Scrolling := False;

      return False;

   exception
      when E : others =>
         Trace (Me, E);
         return False;
   end Connect_Expose;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Preference)
   is
      pragma Unreferenced (Kernel);
      Source : constant Source_View := Self.View;
      Layout : Pango_Layout;
      Color  : Gdk_RGBA;
      Ink_Rect, Logical_Rect : Pango.Pango_Rectangle;
   begin
      --  Recompute the width of one character

      if Pref = null
        or else Pref = Preference (Default_Style)
      then
         Layout := Create_Pango_Layout (Source);
         Set_Attributes (Layout, Null_Pango_Attr_List);
         Set_Font_Description (Layout, Default_Style.Get_Pref_Font);
         Set_Text (Layout, (1 .. 256 => '0'));
         Get_Pixel_Extents (Layout, Ink_Rect, Logical_Rect);
         Source.Width_Of_256_Chars := Ink_Rect.Width;
         Unref (Layout);

         --  Modify the text background, color and font

         if Source.Get_Realized then
            Modify_Font (Source, Default_Style.Get_Pref_Font);
         end if;

         Source.Set_Background_Color;
      end if;

      --  Reset the color of the current line.
      --  This procedure might be called before the Map_Cb has been called,
      --  and therefore the Current_Line_GC might not be initialized at this
      --  point.

      Source.Current_Block_Color := Current_Block_Color.Get_Pref;
      Source.Highlight_Blocks := Block_Highlighting.Get_Pref;
      Source.Highlight_As_Line := Current_Line_Thin.Get_Pref;

      if Source.Speed_Column_Buffer /= Null_Surface then
         Destroy (Source.Speed_Column_Buffer);
         Source.Speed_Column_Buffer := Null_Surface;
      end if;

      Size_Side_Column (Source);

      Invalidate_Window (Source);

      Color := Default_Style.Get_Pref_Fg;

      if Color /= Source.Text_Color then
         Source.Text_Color := Color;
         Override_Color (Source, Gtk_State_Flag_Normal, Color);
      end if;

      Source.Current_Line_Color := Current_Line_Color.Get_Pref;
      Source.Highlight_Current := Source.Current_Line_Color /= White_RGBA;

   exception
      when E : others => Trace (Me, E);
   end Execute;

   --------------------------
   -- Set_Background_Color --
   --------------------------

   procedure Set_Background_Color
     (Self   : not null access Source_View_Record'Class;
      Forced : Gdk.RGBA.Gdk_RGBA := Null_RGBA)
   is
      Color : constant Gdk_RGBA := Default_Style.Get_Pref_Bg;
      C     : Cairo_Color := Color;
      Select_Color : Gdk_RGBA;
   begin
      if Self.Get_Editable then
         Get_Style_Context (Self).Remove_Class ("readonly");
      else
         Get_Style_Context (Self).Add_Class ("readonly");
      end if;

      if Forced /= Null_RGBA then
         Self.Forced_Bg_Color := True;
         C := Forced;
      elsif Self.Forced_Bg_Color then
         C := Self.Background_Color;
      end if;

      Self.Background_Color := C;

      if not Self.Get_Editable
        and then Alter_Bg_For_RO_Files.Get_Pref
      then
         C := Shade_Or_Lighten (C, Amount => 0.1);
      end if;

      --  Overridding background color also seems to set the selected color, so
      --  that selected text becomes invisible. So we have to reset it as well.

      Get_Style_Context (Self).Get_Background_Color
        (Gtk_State_Flag_Selected, Select_Color);
      Self.Override_Background_Color (Gtk_State_Flag_Normal, C);
      Self.Override_Background_Color
         (Gtk_State_Flag_Selected, Select_Color);

      Invalidate_Window (Self);
   end Set_Background_Color;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_File_Saved;
      Kernel : not null access Kernel_Handle_Record'Class;
      File   : Virtual_File)
   is
      pragma Unreferenced (Kernel, File);
   begin
      Invalidate_Window (Self.View);
   end Execute;

   -------------------------------
   -- Scroll_To_Cursor_Location --
   -------------------------------

   procedure Scroll_To_Cursor_Location
     (View      : access Source_View_Record;
      Centering : Centering_Type := Minimal)
   is
      function Fit (Adj : Gtk.Adjustment.Gtk_Adjustment) return Boolean
        with Inline => True;
      --  Check if given adjustment fits whole view

      ---------
      -- Fit --
      ---------

      function Fit (Adj : Gtk.Adjustment.Gtk_Adjustment) return Boolean is
      begin
         return Adj.Get_Page_Size >= Adj.Get_Upper;
      end Fit;

      Tmp : Boolean;
      pragma Unreferenced (Tmp);
   begin
      --  Don't scroll if whole text is visible inside view window
      if Fit (View.Scroll.Get_Vadjustment)
        and then Fit (View.Scroll.Get_Hadjustment)
      then
         return;
      end if;

      case Centering is
         when Minimal =>
            --  Perform minimal scrolling
            Scroll_To_Mark
              (View, View.Saved_Cursor_Mark, Use_Align => False,
               Within_Margin                           => 0.0,
               Xalign                                  => 0.5,
               Yalign                                  => 0.5);
         when Center =>
            --  Place the cursor in the exact center of the screen
            Scroll_To_Mark
              (View, View.Saved_Cursor_Mark, Use_Align => True,
               Within_Margin                           => 0.0,
               Xalign                                  => 0.5,
               Yalign                                  => 0.5);
         when With_Margin =>
            --  Perform minimal scrolling to place the cursor on the screen,
            --  with a margin to see context above and below the cursor.
            Scroll_To_Mark
              (View, View.Saved_Cursor_Mark, Use_Align => False,
               Within_Margin                           => 0.1,
               Xalign                                  => 0.5,
               Yalign                                  => 0.5);
      end case;
   end Scroll_To_Cursor_Location;

   -------------------
   -- Center_Cursor --
   -------------------

   procedure Center_Cursor (View : access Source_View_Record) is
      Insert_Mark : constant Gtk_Text_Mark := Get_Insert (Get_Buffer (View));
   begin
      Scroll_To_Mark
        (View, Insert_Mark, Use_Align => False,
         Within_Margin                => 0.1,
         Xalign                       => 0.5,
         Yalign                       => 0.5);
   end Center_Cursor;

   -------------------------
   -- Get_Cursor_Position --
   -------------------------

   procedure Get_Cursor_Position
     (View : access Source_View_Record'Class;
      Iter : out Gtk.Text_Iter.Gtk_Text_Iter) is
   begin
      if View.Has_Focus then
         Get_Cursor_Position (Source_Buffer (Get_Buffer (View)), Iter);
      else
         Get_Iter_At_Mark (Get_Buffer (View), Iter, View.Saved_Cursor_Mark);
      end if;
   end Get_Cursor_Position;

   -------------------------
   -- Stop_Selection_Drag --
   -------------------------

   procedure Stop_Selection_Drag (View : access Source_View_Record'Class) is
      Ignore : Boolean;
      pragma Unreferenced (Ignore);
   begin
      if View.Button_Pressed then
         View.Button_Event.Button.Time := 0;
         Ignore := Return_Callback.Emit_By_Name
           (View, Signal_Button_Release_Event, View.Button_Event);
      end if;
   end Stop_Selection_Drag;

   -----------------------------
   -- Window_To_Buffer_Coords --
   -----------------------------

   procedure Window_To_Buffer_Coords
     (View          : access Source_View_Record;
      X, Y          : Gint;
      Line          : out Gint;
      Column        : out Gint;
      Out_Of_Bounds : out Boolean)
   is
      Buffer_X      : Gint;
      Buffer_Y      : Gint;
      Iter          : Gtk_Text_Iter;
      Iter_Location : Gdk_Rectangle;

   begin
      Window_To_Buffer_Coords
        (View, Text_Window_Text,
         Window_X => X, Window_Y => Y,
         Buffer_X => Buffer_X, Buffer_Y => Buffer_Y);
      Get_Iter_At_Location (View, Iter, Buffer_X, Buffer_Y);
      Line   := Get_Line (Iter);
      Column := Get_Line_Offset (Iter);

      --  Get_Iter_At_Location does not behave quite exactly like I wished it
      --  did: The iterator returned is always located in a valid position,
      --  even if the user clicked outside of the the areas where there is some
      --  text. In our case, we don't want that, so we need to add some extra
      --  logic in order to detect these cases, and return -1,-1 to signal it.
      --
      --  We use the following algorithm to detect such cases:
      --     + Get the X window coordinate of the last insert position
      --       in line Line. If the X window coordinate of the event
      --       exceeds this position, we were beyond the end of the line,
      --       and hence should return -1,-1.
      --     + Get the Y window coordinates of the bottom of line Line
      --       (computed by getting the window coordinates of the top
      --       of line Line, plus the line height). If the Y window
      --       coordinates of the event exceed this position, we were
      --       beyond the end of the last line, in which case we also
      --       return -1,-1.

      if not Ends_Line (Iter) then
         Out_Of_Bounds := False;
      else
         Get_Iter_Location (View, Iter, Iter_Location);
         Out_Of_Bounds := Buffer_X > Iter_Location.X + Iter_Location.Width
           or else Buffer_Y > Iter_Location.Y + Iter_Location.Height;
      end if;
   end Window_To_Buffer_Coords;

   ----------------------------
   -- Event_To_Buffer_Coords --
   ----------------------------

   procedure Event_To_Buffer_Coords
     (View          : access Source_View_Record;
      Event         : Gdk_Event;
      Line          : out Gint;
      Column        : out Gint;
      Out_Of_Bounds : out Boolean)
   is
      X, Y : Gdouble;
   begin
      Get_Coords (Event, X, Y);
      Window_To_Buffer_Coords
        (View, Gint (X), Gint (Y), Line, Column, Out_Of_Bounds);
   end Event_To_Buffer_Coords;

   -------------------------------------
   -- Speed_Bar_Button_Press_Event_Cb --
   -------------------------------------

   function Speed_Bar_Button_Press_Event_Cb
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean
   is
      View             : constant Source_View := Source_View (Widget);
      Dummy_X, Dummy_Y : Gint;
      W, H             : Gint;
      Button_Y         : Gint;
      Lower, Upper     : Gdouble;
      Adj              : Gtk_Adjustment;
      X, Y         : Gdouble;

      procedure Handle_Press_On_Side_Info;
      --  Handle a click on the "side info" area

      procedure Handle_Press_On_Speed_Bar;
      --  Handle a click on the speed bar

      -------------------------------
      -- Handle_Press_On_Side_Info --
      -------------------------------

      procedure Handle_Press_On_Side_Info is
         Dummy_Gint         : Gint;
         Iter               : Gtk_Text_Iter;
         Line               : Buffer_Line_Type;
         Button_X, Button_Y : Gint;
         X, Y               : Gint;

         Buffer : constant Source_Buffer := Source_Buffer (Get_Buffer (View));
      begin
         --  Get the coordinates of the click

         Button_X := Gint (Event.Button.X);
         Button_Y := Gint (Event.Button.Y);

         --  Find the line number

         Window_To_Buffer_Coords
           (View, Text_Window_Text,
            Window_X => Button_X, Window_Y => Button_Y,
            Buffer_X => X, Buffer_Y => Y);

         Get_Line_At_Y (View, Iter, Y, Dummy_Gint);
         Line := Buffer_Line_Type (Get_Line (Iter) + 1);

         Set_Focus_Child (View.Child);
         On_Click (Buffer, Line, Button_X - View.Speed_Bar_Width);
      end Handle_Press_On_Side_Info;

      -------------------------------
      -- Handle_Press_On_Speed_Bar --
      -------------------------------

      procedure Handle_Press_On_Speed_Bar is
      begin

         Button_Y := Gint (Y);

         Get_Geometry (Get_Window (View.Area), Dummy_X, Dummy_Y, W, H);

         Adj := Get_Vadjustment (View.Scroll);
         Lower := Get_Lower (Adj);
         Upper := Get_Upper (Adj) - Get_Page_Size (Adj);

         if Button_Y > H then
            View.Scroll_To_Value := Upper;
         else
            View.Scroll_To_Value :=
              Lower + (Upper - Lower) * Gdouble (Button_Y) / Gdouble (H);
         end if;

         if not View.Scroll_Requested then
            View.Scroll_Requested := True;
            View.Scroll_Timeout := Source_View_Timeout.Timeout_Add
              (10, Scroll_Timeout'Access, View);
         end if;
      end Handle_Press_On_Speed_Bar;

      Event_Type : constant Gdk_Event_Type := Get_Event_Type (Event);
   begin
      if View.Scrolling
        or else View.Area = null
      then
         return False;
      end if;

      Get_Coords (Event, X, Y);

      if Gint (X) <= View.Speed_Bar_Width
        and then
          ((Event_Type = Button_Release and then Get_Button (Event) = 1)
           or else Event_Type = Motion_Notify)
      then
         --  This is  a click or a drag on the speed bar

         Handle_Press_On_Speed_Bar;

      elsif Event_Type = Button_Release
        and then Get_Button (Event) = 1
      then
         --  This is a click on the side info area

         Handle_Press_On_Side_Info;
      end if;

      return False;

   exception
      when E : others =>
         Trace (Me, E);
         return False;
   end Speed_Bar_Button_Press_Event_Cb;

   ---------------------------------------
   -- Speed_Bar_Button_Release_Event_Cb --
   ---------------------------------------

   function Speed_Bar_Button_Release_Event_Cb
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean is
   begin
      return Speed_Bar_Button_Press_Event_Cb (Widget, Event);

   exception
      when E : others =>
         Trace (Me, E);
         return False;
   end Speed_Bar_Button_Release_Event_Cb;

   -----------------------------
   -- Button_Release_Event_Cb --
   -----------------------------

   function Button_Release_Event_Cb
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean
   is
      View        : constant Source_View := Source_View (Widget);

   begin
      if Get_Event_Type (Event) = Button_Release
        and then Get_Button (Event) = 1
      then
         if View.Button_Pressed then
            View.Button_Pressed := False;
            Free (View.Button_Event);
         end if;

         if View.Double_Click then
            View.Double_Click := False;
            Select_Current_Word (Source_Buffer (Get_Buffer (View)));
         end if;
      end if;

      return False;

   exception
      when E : others =>
         Trace (Me, E);
         return False;
   end Button_Release_Event_Cb;

   ---------------------------
   -- Button_Press_Event_Cb --
   ---------------------------

   function Button_Press_Event_Cb
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean
   is
      View   : constant Source_View := Source_View (Widget);
      Buffer : constant Source_Buffer := Source_Buffer (Get_Buffer (View));
      Result : Boolean;
      Focus_Widget : Gtk_Widget;
      pragma Unreferenced (Result);
   begin
      External_End_Action (Buffer);

      --  Save the focus widget before acquiring the focus, in case we need
      --  it for the middle-mouse-paste function
      Focus_Widget := Get_Current_Focus_Widget (View.Kernel);

      View.Acquire_Focus;

      case Get_Event_Type (Event) is
         when Button_Press =>
            if Get_Button (Event) = 1 then
               if
                 (Get_State (Event) and Shift_Mask) /= 0
                 and then (Get_State (Event) and Mod1_Mask) /= 0
               then
                  declare
                     L, C    : Gint;
                     Iter    : Gtk_Text_Iter;
                  begin
                     Window_To_Buffer_Coords
                       (View, Text_Window_Text,
                        Gint (Event.Button.X), Gint (Event.Button.Y), L, C);
                     Get_Iter_At_Location (View, Iter, L, C);
                     Grab_Focus (View);
                     Cursors.Add_Cursor
                       (Buffer, Iter);
                  end;
                  return True;
               end if;
               if not View.Button_Pressed then
                  View.Button_Pressed := True;
                  View.Button_Event := Copy (Event);
               end if;

            elsif Get_Button (Event) = 2 then

               --  In hyper mode, we know that this won't cause a paste
               --  operation, since this will be interrupted by
               --  Src_Editor_View.Hyper_Mode.Button_Press_Event_Cb.
               if View.Hyper_Mode then
                  return False;
               end if;

               --  Short-circuit the textview handler to disable clipboard
               --  paste under Windows: this is not a standard mechanism
               --  on this platform, and it causes unwanted paste operations
               --  with some mouse wheels.

               if Host = Windows then
                  return True;

               elsif Active (Trace_Override_Middle_Click_Paste) then
                  --  On UNIX we intercept this to use our own paste function,
                  --  because the default one pastes the tags, which we do not
                  --  want.
                  Copy_Clipboard (Get_Clipboard (View.Kernel), Focus_Widget);

                  declare
                     L, C    : Gint;
                     Iter    : Gtk_Text_Iter;
                  begin
                     Window_To_Buffer_Coords
                       (View, Text_Window_Text,
                        Gint (Event.Button.X), Gint (Event.Button.Y), L, C);
                     Get_Iter_At_Location (View, Iter, L, C);
                     Grab_Focus (View);
                     Place_Cursor (Get_Buffer (View), Iter);
                     Paste_Clipboard (Get_Clipboard (View.Kernel), View);
                  end;

                  return True;
               end if;
            end if;

         when Gdk_2button_Press =>
            if Get_Button (Event) = 1 then
               View.Double_Click := True;
               --  ??? This is a tweak necessary to implement the feature
               --  "select an entire word containing '_' when double-clicking".
               --  See corresponding code in Button_Release_Event_Cb.
               --  Might be worth investigating whether it could be implemented
               --  at the gtk+ level (the proper fix would be to change the
               --  Pango word break algorithms probably in break.c ?) and to
               --  redefine the "is_word_break" behaviour of the underscore.
            end if;

         when others =>
            null;
      end case;

      return False;
   end Button_Press_Event_Cb;

   ------------------------
   -- Key_Press_Event_Cb --
   ------------------------

   function Key_Press_Event_Cb
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean
   is
      View        : constant Source_View   := Source_View (Widget);
      Buffer      : constant Source_Buffer :=
                      Source_Buffer (Get_Buffer (View));
      Ignore      : Boolean;
      pragma Unreferenced (Ignore);

      Key         : Gdk_Key_Type;

      use Interfaces.C.Strings;
   begin
      if not Active (Testsuite_Handle) then
         if not View.Get_Realized then
            return True;
         end if;
      end if;

      --  As soon as a key is pressed on an editor, reset the flag
      --  position_set_explicitely, so that scrolling does not occur, unless
      --  this is the Hyper Mode key

      Key := Get_Key_Val (Event);

      if Key /= GDK_Control_L and then Key /= GDK_Control_R then
         Ignore := View.Position_Set_Explicitely (Reset => True);
      end if;

      --  If we are not pressing the Ctrl key, check whether we are
      --  pressing a graphical key

      if not Get_Editable (View)
        and then (Get_State (Event) and Control_Mask) = 0
      then
         case Key is
            when GDK_BackSpace .. GDK_Return | GDK_Delete |
                 GDK_KP_Tab .. GDK_KP_Enter | GDK_KP_Delete =>
               return True;

            when others =>
               if Event.Key.String /= Null_Ptr then
                  declare
                     Str : constant String := Interfaces.C.Strings.Value
                       (Event.Key.String);
                  begin
                     if Str'Length >= 1 then
                        Insert
                          (View.Kernel,
                           -"Warning : attempting to edit a read-only editor.",
                           Mode => Error);

                        return True;
                     end if;
                  end;
               end if;
         end case;

      end if;

      --  Special case for cancelling selection

      case Key is
         when GDK_Return =>
            Clear_Typed_Chars (Buffer);

            --  If we are in a smart completion, let the Return character be
            --  caught by the completion window.

            if View.In_Completion then
               return False;
            end if;

            External_End_Action (Buffer);
            Buffer.Newline_And_Indent (View.As_Is_Enabled);
            View.Reset_As_Is_Mode;
            return True;

         when GDK_Linefeed | GDK_Tab | GDK_Home | GDK_Page_Up | GDK_Page_Down |
              GDK_End | GDK_Begin | GDK_Up | GDK_Down | GDK_Left | GDK_Right
            =>
            Clear_Typed_Chars (Buffer);
            External_End_Action (Buffer);

         when GDK_BackSpace =>
            if Event.Key.Send_Event /= 0 then
               --  Handle BackSpace event mostly for test scripts purpose
               declare
                  Line   : Editable_Line_Type;
                  Column : Character_Offset_Type;
               begin
                  Get_Cursor_Position (Buffer, Line, Column);
                  Delete (Buffer, Line, Column - 1, 1);
               end;
            end if;

         when GDK_space =>
            --  ??? We need to make a special case here because the call to
            --  Get_String (see "when => others" below) is not reliable. In
            --  particular when using PyGTK to simulate a key press event
            --  the key string is not set in the event object. As a result
            --  the "word_added" whould not be called.

            Clear_Typed_Chars (Buffer);

            if not View.As_Is_Enabled then
               Word_Added (Buffer);
            end if;

         when others =>
            if Event.Key.String /= Null_Ptr then
               declare
                  Key_Str : constant String := Interfaces.C.Strings.Value
                    (Event.Key.String);
               begin
                  if Key_Str'Length = 1
                    and then
                      not Is_In (Key_Str (Key_Str'First),
                                 Word_Character_Set (Get_Language (Buffer)))
                    and then
                      not Is_In (Key_Str (Key_Str'First),
                                 Constants.Control_Set)
                  then
                     if not View.As_Is_Enabled then
                        Word_Added (Buffer);
                     end if;
                  end if;
               end;
            end if;
      end case;

      return False;

   exception
      when E : others =>
         Trace (Me, E);
         return False;
   end Key_Press_Event_Cb;

   --------------------
   -- Redraw_Columns --
   --------------------

   procedure Redraw_Columns
     (View : access Source_View_Record'Class;
      Cr   : Cairo_Context)
   is
      Layout      : Pango_Layout;
      Src_Buffer  : constant Source_Buffer :=
                      Source_Buffer (Get_Buffer (View));
      Some_Lines  : constant Boolean :=
                      Display_Line_Numbers.Get_Pref = Preferences.Some_Lines;
      Num_Color   : Gdk_RGBA;

      Prev_Side_Info_Width : constant Gint := View.Side_Info_Width;
   begin

      --  Compute the size of the info to draw

      View.Side_Info_Width := Gint (Get_Total_Column_Width (Src_Buffer));

      --  If the size has changed, resize the window. We can return immediately
      --  since the window will be redrawn again as result of resizing.
      if View.Side_Info_Width /= Prev_Side_Info_Width then
         Size_Side_Column (View);

         return;
      end if;

      if View.Side_Info_Width = 0 then
         return;
      end if;

      if View.Side_Info_Width /= View.Buffer_Column_Size then
         View.Buffer_Column_Size := View.Side_Info_Width;
      end if;

      --  Create the graphical elements

      Layout := Create_Pango_Layout (View);

      if Some_Lines then
         --  For the alternative line number format, where every fifth line
         --  is numbered, use a smaller but slightly more constrasting type.

         Get_Style_Context (View.Area).Get_Background_Color
           (Gtk_State_Flag_Normal, Num_Color);
         Num_Color := Shade_Or_Lighten (Num_Color, 0.5);
         Set_Font_Description (Layout, Small_Font.Get_Pref);

      else
         --  This is the regular line numbering, with every line numbered

         Get_Style_Context (View.Area).Get_Color
           (Gtk_State_Flag_Normal, Num_Color);
         Set_Font_Description (Layout, Default_Style.Get_Pref_Font);
      end if;

      Draw_Line_Info
        (Src_Buffer, View.Top_Line, View.Bottom_Line,
         Buffer_Line_Type (View.Current_Line),
         View.Highlight_As_Line,
         Gtk_Text_View (View),
         View.Area,
         Num_Color,
         View.Current_Line_Color,
         Layout, Cr);

      Unref (Layout);
   end Redraw_Columns;

   ----------------------------
   -- Draw_Speed_Column_Data --
   ----------------------------

   procedure Draw_Speed_Column_Data (View : access Source_View_Record'Class) is
      Color          : Gdk_RGBA;
      Line_Height    : Gdouble;
      Buffer_Context : Cairo_Context;

      Height, Width, Offset : Gint;

      Window : Gdk_Window;
      A      : Gtk_Allocation;
      Src_Buffer  : Source_Buffer;
      Total_Lines : Gint;
   begin
      if View.Speed_Column_Buffer /= Null_Surface then
         --  The cache already contains the information, return now
         return;
      end if;

      Src_Buffer := Source_Buffer (Get_Buffer (View));
      Total_Lines := Get_Line_Count (Src_Buffer);

      Window := View.Scroll.Get_Vscrollbar.Get_Window;
      View.Scroll.Get_Vscrollbar.Get_Allocation (A);

      Height := A.Height - 2 * View.Scrollbar_Stepper_Size;
      Width  := A.Width;
      Offset := View.Scrollbar_Stepper_Size;

      View.Speed_Column_Buffer :=
        Create_Similar_Surface
          (Window, Cairo_Content_Color_Alpha,
           A.Width,
           A.Height);

      Buffer_Context := Create (View.Speed_Column_Buffer);

      Line_Height := Gdouble (Height) / Gdouble (Total_Lines + 1);

      --  Make the line height at least 2 pixels high

      if Line_Height < 2.0 then
         Line_Height := 2.0;
      end if;

      Set_Line_Width (Buffer_Context, Line_Height);
      Set_Line_Cap (Buffer_Context, Cairo_Line_Cap_Square);

      for J in 1 .. Total_Lines loop
         Color := Get_Highlight_Color
           (Src_Buffer, Buffer_Line_Type (J),
            Context => Highlight_Speedbar);

         if Color /= Null_RGBA then
            Set_Source_Color (Buffer_Context, Color);
            Draw_Line
              (Buffer_Context, Color,
               0,
               (Height * J) / Total_Lines + Offset,
               Width,
               (Height * J) / Total_Lines + Offset);
         end if;
      end loop;

      Destroy (Buffer_Context);
   end Draw_Speed_Column_Data;

   -----------------------------
   -- Set_Synchronized_Editor --
   -----------------------------

   procedure Set_Synchronized_Editor
     (View  : access Source_View_Record;
      Other : Source_View) is
   begin
      if View.Synchronized_Editor /= null then
         Remove_Synchronization (View);
      end if;

      View.Synchronized_Editor := Other;
   end Set_Synchronized_Editor;

   ----------------------------
   -- Remove_Synchronization --
   ----------------------------

   procedure Remove_Synchronization
     (View : access Source_View_Record'Class) is
   begin
      if View.Scrolling then
         return;
      end if;

      if View.Synchronized_Editor /= null then
         View.Scrolling := True;
         Remove_Synchronization (View.Synchronized_Editor);
         View.Synchronized_Editor := null;
         View.Scrolling := False;
      end if;
   end Remove_Synchronization;

   ---------------
   -- On_Scroll --
   ---------------

   procedure On_Scroll (View : access Gtk_Widget_Record'Class) is
      Src_View : constant Source_View := Source_View (View);

   begin
      Recompute_Visible_Area (Src_View);

      --  ??? We use to force a refresh of the window, but not sure why this
      --  would be needed.
      Invalidate_Window (Src_View);

      --  Ensure that synchronized editors are also scrolled

      if not Src_View.Scrolling
        and then Src_View.Synchronized_Editor /= null
        and then Src_View.Scroll /= null
        and then Src_View.Synchronized_Editor.Scroll /= null
      then
         Src_View.Scrolling := True;

         Set_Value
           (Get_Vadjustment (Src_View.Synchronized_Editor.Scroll),
            Get_Value (Get_Vadjustment (Src_View.Scroll)));

         Set_Value
           (Get_Hadjustment (Src_View.Synchronized_Editor.Scroll),
            Get_Value (Get_Hadjustment (Src_View.Scroll)));

         Src_View.Scrolling := False;
      end if;
   end On_Scroll;

   ---------------
   -- Set_Child --
   ---------------

   procedure Set_Child
     (View  : access Source_View_Record;
      Child : GPS.Kernel.MDI.GPS_MDI_Child) is
   begin
      View.Child := Child;
   end Set_Child;

   ---------------
   -- Get_Child --
   ---------------

   function Get_Child
     (View  : access Source_View_Record)
      return GPS.Kernel.MDI.GPS_MDI_Child is
   begin
      return View.Child;
   end Get_Child;

   ----------------------
   -- Start_Completion --
   ----------------------

   procedure Start_Completion
     (View : access Source_View_Record'Class;
      Win  : Completion_Window_Access) is
   begin
      Set_In_Completion (Source_Buffer (Get_Buffer (View)), True);
      View.Completion_Window := Win;
   end Start_Completion;

   --------------------
   -- End_Completion --
   --------------------

   procedure End_Completion (View : access Source_View_Record'Class) is
   begin
      Set_In_Completion (Source_Buffer (View.Get_Buffer), False);

      --  Force a refresh of the context
      View.Kernel.Context_Changed
        (Build_Editor_Context (View, Location_Cursor));
   end End_Completion;

   -------------------
   -- In_Completion --
   -------------------

   function In_Completion
     (View : access Source_View_Record'Class) return Boolean is
   begin
      return In_Completion (Source_Buffer (Get_Buffer (View)));
   end In_Completion;

   ------------------------------
   -- Position_Set_Explicitely --
   ------------------------------

   function Position_Set_Explicitely
     (Self   : access Source_View_Record;
      Reset  : Boolean) return Boolean
   is
      Set : constant Boolean := Self.Cursor_Set_Explicitely;
   begin
      if Reset then
         Self.Cursor_Set_Explicitely := False;
         Self.Initial_Scroll_Has_Occurred := True;
      end if;

      return Set;
   end Position_Set_Explicitely;

   ----------------------------------
   -- Set_Position_Set_Explicitely --
   ----------------------------------

   procedure Set_Position_Set_Explicitely (Self : access Source_View_Record) is
   begin
      if not Self.Initial_Scroll_Has_Occurred then
         Self.Cursor_Set_Explicitely := True;
      end if;
   end Set_Position_Set_Explicitely;

   -----------------
   -- Get_Project --
   -----------------

   function Get_Project
     (Self : not null access Source_View_Record'Class)
      return GNATCOLL.Projects.Project_Type
   is
   begin
      return Self.Kernel.Registry.Tree.Project_From_Path (Self.Project_Path);
   end Get_Project;

   -----------------
   -- Set_Project --
   -----------------

   procedure Set_Project
     (Self    : not null access Source_View_Record'Class;
      Project : GNATCOLL.Projects.Project_Type := No_Project)
   is
      Buffer : constant Source_Buffer := Source_Buffer (Self.Get_Buffer);
   begin
      if Project = No_Project then
         --  Pick a project at random
         declare
            F_Info : constant File_Info'Class :=
              File_Info'Class
                (Get_Registry (Self.Kernel).Tree.Info_Set
                 (Buffer.Get_Filename).First_Element);
         begin
            Self.Project_Path := F_Info.Project.Project_Path;
         end;
      else
         Self.Project_Path := Project.Project_Path;
      end if;

      --  Force a refresh of the MDI title. This changes for all views of the
      --  buffer, but it is fast.
      Buffer.Filename_Changed;
   end Set_Project;

   --------------------------
   -- Build_Editor_Context --
   --------------------------

   function Build_Editor_Context
     (View     : access Source_View_Record;
      Location : Location_Type := Location_Cursor;
      Event    : Gdk.Event.Gdk_Event := null)
      return GPS.Kernel.Selection_Context
   is
      V                          : constant Source_View := Source_View (View);
      B                          : constant Source_Buffer :=
        Source_Buffer (View.Get_Buffer);
      Filename                   : constant GNATCOLL.VFS.Virtual_File :=
        Get_Filename (B);
      Loc                        : Location_Type := Location;
      Line                       : Gint := 0;
      EL                         : Editable_Line_Type := 0;
      Col                        : Visible_Column_Type := 0;
      Column                     : Gint := 0;
      X, Y                       : Gint;
      Start_Iter                 : Gtk_Text_Iter;
      End_Iter                   : Gtk_Text_Iter;
      Entity_Start               : Gtk_Text_Iter;
      Entity_End                 : Gtk_Text_Iter;
      Has_Selection              : Boolean := False;
      Out_Of_Bounds              : Boolean := False;
      Start_Line, End_Line       : Integer;
      Selection_Is_Single_Entity : Boolean;
      Click_In_Selection         : Boolean;
      Mouse_X, Mouse_Y           : Gint;
      Mask                       : Gdk.Types.Gdk_Modifier_Type;
      Win                        : Gdk.Gdk_Window;
      Cursor_Location            : Gtk_Text_Iter;
      Xevent, Yevent             : Gdouble;
      Context                    : Selection_Context;
      Str                        : Src_String;
      The_Line                   : Editable_Line_Type;
      The_Column                 : Character_Offset_Type;
      Success                    : Boolean;

   begin
      Trace (Me, "Build_Editor_Context");

      if Location = Location_Event
        and then
        (Event = null
         or else Get_Event_Type (Event) not in Button_Press .. Button_Release)
      then
         Loc := Location_Cursor;
      end if;

      --  If we are reactiving to an event (for a contextual menu), then we
      --  need to cancel the selection drag.
      --  Otherwise, we are calling this function only to create a context (for
      --  instance when displaying a tooltip) and we should not cancel the
      --  selection drag.
      if Event /= null then
         Stop_Selection_Drag (V);
      end if;

      Context := New_Context (V.Kernel, Src_Editor_Module_Id);

      --  Compute the location for which we should compute the context.
      --  Output of this block is (Line, Column) within the editor.

      case Loc is
         when Location_Event =>
            if Get_Window (Event) = Get_Window (V, Text_Window_Left) then
               --  Click in the line numbers area
               Get_Coords (Event, Xevent, Yevent);
               Window_To_Buffer_Coords
                 (V, Text_Window_Left, Gint (Xevent), Gint (Yevent), X, Y);
               Get_Iter_At_Location (V, Start_Iter, X, Y);
               Line := Get_Line (Start_Iter);
               Place_Cursor (B, Start_Iter);

               Get_Iter_Position (B, Start_Iter, EL, Col);

               Set_File_Information
                 (Context,
                  Files           => (1 => Filename),
                  Project         => Get_Project (V),
                  Publish_Project => False,
                  Line            => Integer (EL),
                  Column          => 0);
               return Context;
            end if;

            Event_To_Buffer_Coords (V, Event, Line, Column, Out_Of_Bounds);
            if not Out_Of_Bounds then
               Get_Iter_At_Line_Offset (B, Start_Iter, Line, Column);
               Get_Iter_Position (B, Start_Iter, EL, Col);
            end if;

         when Location_Cursor =>
            Get_Iter_At_Mark (B, Start_Iter, Get_Insert (B));
            Line   := Get_Line (Start_Iter);
            Column := Get_Line_Offset (Start_Iter);
            Get_Iter_At_Line_Offset (B, Start_Iter, Line, Column);
            Get_Iter_Position (B, Start_Iter, EL, Col);

         when Location_Mouse =>
            Get_Pointer (Get_Window (V, Text_Window_Text),
                         Mouse_X, Mouse_Y, Mask, Win);
            Window_To_Buffer_Coords
              (V, Mouse_X, Mouse_Y, Line, Column, Out_Of_Bounds);
            Get_Iter_At_Line_Offset (B, Start_Iter, Line, Column);
            Get_Iter_Position (B, Start_Iter, EL, Col);

            if Out_Of_Bounds then
               Set_File_Information
                 (Context,
                  Files           => (1 => Filename),
                  Project         => Get_Project (V),
                  Publish_Project => False);
               return Context;
            end if;
      end case;

      --  If we have a current selection, use it as the context if the user
      --  clicked inside it (ie consider the selection as an opaque block and
      --  don't look inside)

      Get_Selection_Bounds (B, Start_Iter, End_Iter, Has_Selection);
      if Has_Selection then
         Get_Iter_Position (B, Start_Iter, EL, Col);
      end if;

      if Out_Of_Bounds and then not Has_Selection then
         --  The position we are looking at is outside the text, and there is
         --  no current selection. We move the cursor to a valid location, but
         --  there is no additional info to set in the context.
         Get_Iter_At_Line_Offset (B, Start_Iter, Line, Column);

         if Event /= null then
            Acquire_Focus (V);
            Place_Cursor (B, Start_Iter);
         end if;

         --  Set basic context information about current file

         Set_File_Information
           (Context,
            Files           => (1 => Filename),
            Project         => Get_Project (V),
            Publish_Project => False,
            Line            => Integer (EL),
            Column          => Col);
         return Context;
      end if;

      Get_Iter_At_Line_Offset (B, Entity_Start, Line, Column);

      Copy (Source => Entity_Start, Dest => Cursor_Location);

      Click_In_Selection :=
        Has_Selection
        and then Get_Offset (Start_Iter) <= Get_Offset (Entity_Start)
        and then Get_Offset (Entity_Start) <= Get_Offset (End_Iter);

      Str := Get_String
        (B,
         Get_Editable_Line
           (B, Buffer_Line_Type (Get_Line (Entity_Start)) + 1));

      Search_Entity_Bounds
        (Entity_Start, Entity_End,
         Maybe_File => Str.Contents /= null
         and then Has_Include_Directive (Str.Contents (1 .. Str.Length)));
      Selection_Is_Single_Entity :=
        Has_Selection
        and then Equal (Entity_Start, Start_Iter)
        and then Equal (Entity_End, End_Iter);

      --  If the location is in the current selection, use this as the
      --  context. However, if the selection is a single entity, we should
      --  create a context such that cross-references menus also appear.

      if not Selection_Is_Single_Entity
        and then Click_In_Selection
      then
         Start_Line := Integer (EL);

         End_Line   := Integer
           (Get_Editable_Line
              (B, Buffer_Line_Type (Get_Line (End_Iter) + 1)));

         --  Do not consider the last line selected if only the first
         --  character is selected.

         if Get_Line_Offset (End_Iter) = 0 then
            End_Line := End_Line - 1;
         end if;

         --  Do not consider the first line selected if only the last
         --  character is selected. Also, move Start_Iter to skip end of
         --  line characters in the buffer.

         if Ends_Line (Start_Iter) then
            Forward_Line (Start_Iter, Success);
            Start_Line := Start_Line + 1;
         end if;

         --  Set the column to the start of the selection

         Column := Get_Line_Offset (Start_Iter);
         Col := Expand_Tabs (B, EL, Character_Offset_Type (Column + 1));

         Set_File_Information
           (Context,
            Files           => (1 => Filename),
            Project         => Get_Project (V),
            Publish_Project => False,
            Line            => Integer (EL),
            Column          => Col);
         Set_Area_Information
           (Context,
            Get_Text (Start_Iter, End_Iter),
            Start_Line, End_Line);

         Free (Str);
         return Context;
      end if;

      --  Set basic context information about current file. Must be
      --  done before we set the entity information.

      Set_File_Information
        (Context,
         Files           => (1 => Filename),
         Project         => Get_Project (V),
         Publish_Project => False,
         Line            => Integer (EL),
         Column          => Col);

      --  Expand the tabs

      Get_Iter_Position (B, Entity_Start, The_Line, The_Column);

      if Str.Contents /= null then
         if Click_In_Selection then
            --  If there was a selection and we clicked in it, we only
            --  should take the selection into account. For instance, this
            --  is a way for the user to force a specific name to be sent
            --  to the debugger instead of the whole expression

            Set_Entity_Information
              (Context,
               Entity_Name   => Get_Text (Entity_Start, Entity_End),
               Entity_Column => Expand_Tabs (B, The_Line, The_Column));

         else
            Set_Entity_Information
              (Context,
               Entity_Name   => Get_Text (Entity_Start, Entity_End),
               Entity_Column => Expand_Tabs (B, The_Line, The_Column),
               From_Expression =>
                 Parse_Reference_Backwards
                   (Get_Language (B),
                    Buffer       => Str.Contents (1 .. Str.Length),
                    Start_Offset =>
                      String_Index_Type (Get_Line_Index (Entity_End))));
         end if;
      else
         Set_Entity_Information
           (Context,
            Entity_Name   => Get_Text (Entity_Start, Entity_End),
            Entity_Column => Expand_Tabs (B, The_Line, The_Column));
      end if;

      Free (Str);

      if Event /= null
        and then not Click_In_Selection
      then
         --  Move the cursor at the correct location. The cursor is
         --  grabbed automatically by the kernel when displaying the
         --  menu, and this would result in unwanted scrolling
         --  otherwise..
         --  Do not move the cursor if we have clicked in the
         --  selection, since otherwise that cancels the selection
         --
         --  Force the focus on the MDI window right away, instead of
         --  waiting for the editor to gain the focus later on.
         --  Otherwise, if the editor doesn't have the focus at this
         --  point, it will move back to its Saved_Cursor_Mark when
         --  it does, instead of where we have used Place_Cursor. Note
         --  that explicitly using Save_Cursor_Position doesn't work
         --  either, since it needs to be called after Place_Cursor,
         --  which does the scrolling to Saved_Cursor_Mark.

         Acquire_Focus (V);
         Place_Cursor (B, Cursor_Location);
      end if;

      return Context;
   end Build_Editor_Context;

end Src_Editor_View;
