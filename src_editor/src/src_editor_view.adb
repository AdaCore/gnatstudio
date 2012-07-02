------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2012, AdaCore                     --
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

with GNATCOLL.Traces;
with GNATCOLL.VFS;               use GNATCOLL.VFS;

with Cairo.Surface;              use Cairo.Surface;

with Gdk;                        use Gdk;
with Gdk.Cairo;                  use Gdk.Cairo;
with Gdk.Color;                  use Gdk.Color;
with Gdk.Event;                  use Gdk.Event;
with Gdk.Rectangle;              use Gdk.Rectangle;
with Gdk.Window;                 use Gdk.Window;
with Gdk.Types;                  use Gdk.Types;
with Gdk.Types.Keysyms;          use Gdk.Types.Keysyms;

with Glib.Object;                use Glib.Object;
with Glib.Properties;            use Glib.Properties;
with Glib.Values;                use Glib.Values;

with Gtk;                        use Gtk;
with Gtk.Adjustment;             use Gtk.Adjustment;
with Gtk.Drawing_Area;           use Gtk.Drawing_Area;
with Gtk.Enums;                  use Gtk.Enums;
with Gtk.Scrolled_Window;        use Gtk.Scrolled_Window;
with Gtk.Text_Buffer;            use Gtk.Text_Buffer;
with Gtk.Text_Iter;              use Gtk.Text_Iter;
with Gtk.Text_View;              use Gtk.Text_View;
with Gtk.Widget;                 use Gtk.Widget;
with Gtk.Window;                 use Gtk.Window;

with Gtkada.Handlers;            use Gtkada.Handlers;
with Gtkada.MDI;                 use Gtkada.MDI;
with Gtkada.Style;               use Gtkada.Style;
with Gtkada.Text_Buffer;         use Gtkada.Text_Buffer;

with Pango.Layout;               use Pango.Layout;

with Src_Editor_Buffer;          use Src_Editor_Buffer;
with Src_Editor_Buffer.Blocks;   use Src_Editor_Buffer.Blocks;
with Src_Editor_Buffer.Hooks;    use Src_Editor_Buffer.Hooks;
with Src_Editor_Module.Markers;  use Src_Editor_Module.Markers;

with Basic_Types;                use Basic_Types;
with Config;                     use Config;
with GPS.Intl;                   use GPS.Intl;
with GPS.Kernel;                 use GPS.Kernel;
with GPS.Kernel.Console;         use GPS.Kernel.Console;
with GPS.Kernel.Clipboard;       use GPS.Kernel.Clipboard;
with GPS.Kernel.Hooks;           use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;             use GPS.Kernel.MDI;
with GPS.Kernel.Standard_Hooks;  use GPS.Kernel.Standard_Hooks;
with Language;                   use Language;
with Src_Editor_Buffer.Line_Information;
use Src_Editor_Buffer.Line_Information;
with Traces;                     use Traces;

with Src_Editor_View.Hyper_Mode; use Src_Editor_View.Hyper_Mode;

package body Src_Editor_View is

   Me : constant Debug_Handle := Create ("Editor_View");

   Trace_Override_Middle_Click_Paste : constant Debug_Handle :=
     Create ("OVERRIDE_MIDDLE_CLICK_PASTE", GNATCOLL.Traces.On);
   --  When this is On, we do our own handling of middle mouse click to
   --  implement paste on Unix platforms. The default handling of the Xserver
   --  also copies the syntax highlighting which is unwanted if for instance we
   --  copy a highlighting on an entity.

   Speed_Column_Width : constant := 10;
   --  The width of the speed column

   Speed_Column_Timeout : constant Guint := 1000;
   --  The time (in milliseconds) after which the speed column should be hidden
   --  when the preference is auto-hide and there are no more lines.

   Margin : constant := 3;
   --  The margin left of the text

   procedure Setup (Data : Source_View; Id : Gtk.Handlers.Handler_Id);
   package Source_Buffer_Callback is new Gtk.Handlers.User_Callback_With_Setup
     (Widget_Type => Source_Buffer_Record,
      User_Type   => Source_View,
      Setup       => Setup);

   package Source_View_Timeout is new Glib.Main.Generic_Sources (Source_View);
   package Source_View_Idle renames Source_View_Timeout;

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

   function Expose_Event_Cb
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean;
   --  This procedure handles all expose events happening on the left border
   --  window. It will redraw the exposed area (this window may contains
   --  things such as line number, breakpoint icons, etc).

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

   procedure Change_Handler
     (Buffer : access Source_Buffer_Record'Class;
      Params : Glib.Values.GValues;
      User   : Source_View);
   --  Callback for the "changed" signal

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

   procedure Invalidate_Window (User : Source_View);
   --  Redraw the buffer window

   procedure Line_Highlight_Change_Handler
     (Buffer : access Source_Buffer_Record'Class;
      Params : Glib.Values.GValues;
      User   : Source_View);
   --  Callback for the "line_highlight_change" signal

   procedure Redraw_Columns (View : access Source_View_Record'Class);
   --  Redraw the left area

   procedure Redraw_Speed_Column (View : access Source_View_Record'Class);
   --  Redraw the speed column

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

   type Preferences_Hook_Record is new Function_No_Args with record
      View : Source_View;
   end record;
   type Preferences_Hook is access all Preferences_Hook_Record'Class;
   overriding procedure Execute
     (Hook   : Preferences_Hook_Record;
      Kernel : access Kernel_Handle_Record'Class);
   --  Called when the preferences have changed, to refresh the editor
   --  appropriately.

   type File_Hook_Record is new Function_With_Args with record
      View : Source_View;
   end record;
   type File_Hook is access all File_Hook_Record'Class;
   overriding procedure Execute
     (Hook   : File_Hook_Record;
      Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class);
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

   function Hide_Speed_Column_Timeout (View : Source_View) return Boolean;
   --  Hide the speed column

   function Speed_Bar_Expose_Event_Cb
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean;
   --  Callback for an "expose" event on the speed bar

   procedure Speed_Bar_Size_Allocate_Cb
     (Widget : access Gtk_Widget_Record'Class);
   --  Callback for an "size_allocate" signal on the speed bar

   procedure Register_Idle_Column_Redraw (View : Source_View);
   --  Register an idle redrawing of the side columns

   procedure Invalidate_Side_Column_Cache
     (View : access Source_View_Record'Class);
   pragma Inline (Invalidate_Side_Column_Cache);
   --  Factorizes code

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
      if Realized_Is_Set (View)
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

   -------------------------------
   -- Hide_Speed_Column_Timeout --
   -------------------------------

   function Hide_Speed_Column_Timeout (View : Source_View) return Boolean is
   begin
      Set_Size_Request (View.Area, 1, -1);
      View.Speed_Column_Hide_Registered := False;
      return False;
   end Hide_Speed_Column_Timeout;

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
      View.Area := null;

      Invalidate_Side_Column_Cache (View);

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

      if View.Speed_Column_Hide_Registered then
         Glib.Main.Remove (View.Speed_Column_Hide_Timeout);
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
         Trace (Exception_Handle, E);
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
         Trace (Exception_Handle, E);
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
      when E : others => Trace (Exception_Handle, E);
   end Realize_Cb;

   -----------------------
   -- Invalidate_Window --
   -----------------------

   procedure Invalidate_Window (User : Source_View) is
      Win           : Gdk.Window.Gdk_Window :=
                        Get_Window (User, Text_Window_Text);
      X, Y, W, H, D : Gint;

   begin
      if Win = null then
         return;
      end if;

      Get_Geometry (Win, X, Y, W, H, D);
      Gdk.Window.Invalidate_Rect (Win, (X, Y, W, H), True);

      Win := Get_Window (User.Area);

      if Win = null then
         return;
      end if;

      Get_Geometry (Win, X, Y, W, H, D);
      Gdk.Window.Invalidate_Rect (Win, (X, Y, W, H), True);
      Register_Idle_Column_Redraw (User);
   end Invalidate_Window;

   ---------------------------
   -- Line_Highlight_Redraw --
   ---------------------------

   function Line_Highlight_Redraw (User : Source_View) return Boolean is
   begin
      User.Redraw_Registered := False;

      if User.Speed_Column_Buffer /= Null_Surface then
         Destroy (User.Speed_Column_Buffer);
         User.Speed_Column_Buffer := Null_Surface;
      end if;

      Invalidate_Window (User);

      return False;
   exception
      when E : others =>
         Trace (Exception_Handle, E);
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
        and then Realized_Is_Set (User)
      then
         User.Redraw_Idle_Handler :=
           Source_View_Idle.Idle_Add (Line_Highlight_Redraw'Access, User);
         User.Redraw_Registered := True;
      end if;

   exception
      when E : others => Trace (Exception_Handle, E);
   end Line_Highlight_Change_Handler;

   ---------------------------------------
   -- Buffer_Information_Change_Handler --
   ---------------------------------------

   procedure Buffer_Information_Change_Handler
     (Buffer : access Source_Buffer_Record'Class;
      Params : Glib.Values.GValues;
      User   : Source_View)
   is
      pragma Unreferenced (Params, Buffer);
   begin
      --  Invalidate the window so that the columns and line and
      --  block highlightings are redrawn.
      Invalidate_Window (User);

   exception
      when E : others => Trace (Exception_Handle, E);
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
      --  Clear the side columns cache

      User.Buffer_Top_Line := 0;

      Invalidate_Side_Column_Cache (User);

      Register_Idle_Column_Redraw (User);

   exception
      when E : others => Trace (Exception_Handle, E);
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

      Invalidate_Side_Column_Cache (User);

      User.Buffer_Top_Line := 0;

      Register_Idle_Column_Redraw (User);

   exception
      when E : others => Trace (Exception_Handle, E);
   end Side_Columns_Config_Change_Handler;

   ------------------------
   -- Idle_Column_Redraw --
   ------------------------

   function Idle_Column_Redraw (View : Source_View) return Boolean is
   begin
      Invalidate_Side_Column_Cache (View);

      if View.Speed_Column_Buffer /= Null_Surface then
         Destroy (View.Speed_Column_Buffer);
         View.Speed_Column_Buffer := Null_Surface;
      end if;

      if Realized_Is_Set (View) then
         Redraw_Columns (View);
         Redraw_Speed_Column (View);
      end if;

      View.Idle_Redraw_Registered := False;
      return False;

   exception
      when E : others =>
         Trace (Exception_Handle, E);
         return False;
   end Idle_Column_Redraw;

   --------------------
   -- Change_Handler --
   --------------------

   procedure Change_Handler
     (Buffer : access Source_Buffer_Record'Class;
      Params : Glib.Values.GValues;
      User   : Source_View)
   is
      Line : constant Gint := Get_Int (Nth (Params, 1));
   begin
      --  If we are changing the cursor while this editor has the focus,
      --  we need save the cursor position
      if Gtkada.MDI.MDI_Child (User.Child) =
        Get_Focus_Child (Get_MDI (User.Kernel))
      then
         Save_Cursor_Position (User);
      end if;

      --  If we are highlighting the current line, re-expose the entire view
      --  if the line has changed. Same thing if we are doing block
      --  highlighting and the block has changed.

      if (User.Highlight_Current
          and then User.Current_Line /= Line)
        or else
          (User.Highlight_Blocks
           and then User.Current_Block /=
             Get_Block (Buffer, Editable_Line_Type (Line), False))
      then
         Invalidate_Window (User);
      end if;

      User.Current_Line := Line;

   exception
      when E : others => Trace (Exception_Handle, E);
   end Change_Handler;

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
      when E : others => Trace (Exception_Handle, E);
   end Size_Allocated_Before;

   --------------------
   -- Size_Allocated --
   --------------------

   procedure Size_Allocated (View : access Gtk_Widget_Record'Class) is
      V      : constant Source_View := Source_View (View);
      Buffer : constant Source_Buffer := Source_Buffer (Get_Buffer (V));

   begin
      --  Keep the cursor on screen when the editor is resized.
      --  Do not do this if the editor is synchronized with another editor.

      if V.Synchronized_Editor = null
        and then V.Cursor_Position >= 0.0
        and then V.Cursor_Position <= 1.0
      then
         if Position_Set_Explicitely (Buffer, False) then
            Scroll_To_Cursor_Location (V, Center);
         end if;
      end if;

   exception
      when E : others => Trace (Exception_Handle, E);
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
      when E : others => Trace (Exception_Handle, E);
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
      when E : others => Trace (Exception_Handle, E);
   end Speed_Bar_Size_Allocate_Cb;

   -------------------------------
   -- Speed_Bar_Expose_Event_Cb --
   -------------------------------

   function Speed_Bar_Expose_Event_Cb
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean
   is
      View : constant Source_View := Source_View (Widget);
      pragma Unreferenced (Event);

   begin
      Redraw_Speed_Column (View);

      return False;

   exception
      when E : others =>
         Trace (Exception_Handle, E);
         return False;
   end Speed_Bar_Expose_Event_Cb;

   ---------------------
   -- Expose_Event_Cb --
   ---------------------

   function Expose_Event_Cb
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean
   is
      View   : constant Source_View := Source_View (Widget);
      Buffer : constant Source_Buffer := Source_Buffer (Get_Buffer (View));
      Window : constant Gdk.Window.Gdk_Window := Get_Window (Event);

      Window_Type : constant Gtk_Text_Window_Type :=
                      Get_Window_Type (View, Window);

      X, Y, Width, Height, Depth : Gint;

      procedure Redraw_Side_Info;
      --  Redraw the side window information

      procedure Highlight_Text;
      --  Highlight the current text, in particular the current line and
      --  current block, if needed.

      ----------------------
      -- Redraw_Side_Info --
      ----------------------

      procedure Redraw_Side_Info is
         Top_In_Buffer    : Gint;
         Bottom_In_Buffer : Gint;
         Dummy_Gint       : Gint;
         Iter             : Gtk_Text_Iter;
         Top_Line         : Buffer_Line_Type;
         Bottom_Line      : Buffer_Line_Type;

      begin
         Get_Geometry (Window, X, Y, Width, Height, Depth);

         Window_To_Buffer_Coords
           (View, Text_Window_Left,
            Window_X => 0, Window_Y => Y,
            Buffer_X => Dummy_Gint, Buffer_Y => Top_In_Buffer);
         Window_To_Buffer_Coords
           (View, Text_Window_Left,
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
         --  If necessary, emit the Source_Lines_Revealed signal

         if Bottom_Line >= Top_Line then
            Source_Lines_Revealed (Buffer, Top_Line, Bottom_Line);
         end if;

         Redraw_Columns (View);
      end Redraw_Side_Info;

      --------------------
      -- Highlight_Text --
      --------------------

      procedure Highlight_Text is
         Column           : constant Gint := Gint (Highlight_Column.Get_Pref);
         Rect             : Gdk_Rectangle;
         Line_Y           : Gint;
         Line_Height      : Gint;
         Cursor_Iter      : Gtk_Text_Iter;
         Dummy            : Gint := 0;
         Buffer_Line_Y    : Gint;

         Dummy_Gint       : Gint;
         Success          : Boolean;
         Iter             : Gtk_Text_Iter;
         Top_Line         : Buffer_Line_Type;
         Bottom_Line      : Buffer_Line_Type;
         Top_In_Buffer    : Gint;
         Bottom_In_Buffer : Gint;
         Color            : Gdk_Color;
         Tmp_Color        : HSV_Color;
         Cr               : Cairo_Context;

         procedure Draw_Block (B : in out Block_Record);
         --  Draw block B at line L

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

            Set_Source_Color (Cr, View.Current_Block_Color);

            if Block_End_Y > Rect.Height then
               Block_End_Y := Rect.Height;
            else
               Move_To (Cr, Gdouble (X), Gdouble (Block_End_Y));
               Rel_Line_To (Cr, Gdouble (Bracket_Length), 0.0);
            end if;

            Move_To (Cr, Gdouble (X), Gdouble (Block_Begin_Y));
            Line_To (Cr, Gdouble (X), Gdouble (Block_End_Y));

            if Block_Begin_Y /= 0 then
               Move_To (Cr, Gdouble (X), Gdouble (Block_Begin_Y));
               Rel_Line_To (Cr, Gdouble (Bracket_Length), 0.0);
            end if;

            Stroke (Cr);
         end Draw_Block;

      begin
         Get_Visible_Rect (View, Rect);
         Cr := Create (Window);
         Set_Line_Width (Cr, 1.0);
         Set_Antialias (Cr, Cairo_Antialias_None);

         Buffer_To_Window_Coords
           (View, Text_Window_Text, Rect.X, Rect.Y, X, Y);

         --  Get the window coordinates

         Get_Geometry (Window, X, Y, Width, Height, Depth);

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

         for Line in Top_Line .. Bottom_Line loop
            Color := Get_Highlight_Color
              (Buffer, Line, Context => Highlight_Editor);

            if Color /= Null_Color then
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

         Get_Iter_At_Mark
           (Get_Buffer (View), Cursor_Iter, View.Saved_Cursor_Mark);

         Get_Line_Yrange (View, Cursor_Iter, Line_Y, Line_Height);

         --  Highlight the line that contains the cursor

         if View.Highlight_Current then
            Buffer_To_Window_Coords
              (View, Text_Window_Text, Dummy, Line_Y, Dummy, Buffer_Line_Y);

            Set_Source_Color (Cr, View.Current_Line_Color);

            if View.Highlight_As_Line then
               Move_To (Cr,
                        Gdouble (Margin),
                        Gdouble (Buffer_Line_Y + Line_Height));
               Rel_Line_To (Cr, Gdouble (Rect.Width), 0.0);
               Stroke (Cr);
            else
               Cairo.Rectangle
                 (Cr,
                  Gdouble (Margin),
                  Gdouble (Buffer_Line_Y),
                  Gdouble (Rect.Width),
                  Gdouble (Line_Height));
               Cairo.Fill (Cr);
            end if;

         end if;

         --  Highlight the current block

         if View.Highlight_Blocks then
            View.Current_Block := Get_Block
              (Buffer,
               Get_Editable_Line
                 (Buffer, Buffer_Line_Type (Get_Line (Cursor_Iter)) + 1),
               False);
            Draw_Block (View.Current_Block);
         end if;

         --  Redraw the line showing the nth column if needed

         if Column > 0 then
            X := (Column * View.Width_Of_256_Chars) / 256 - Rect.X + Margin;

            Save (Cr);
            Set_Line_Width (Cr, 1.0);
            Tmp_Color := To_HSV (To_Cairo (View.Text_Color));

            if Tmp_Color.V > 0.5 then
               --  Light color: let's reduce its luminance by 2
               Tmp_Color.V := Tmp_Color.V * 0.7;
            else
               --  Dark color, lighten it
               Tmp_Color.V := (1.0 - Tmp_Color.V) * 0.7;
            end if;

            Draw_Line
              (Cr, To_Cairo (Tmp_Color), X, Y, X, Y + Rect.Height);
            Restore (Cr);
         end if;

         Destroy (Cr);
      end Highlight_Text;

   begin  -- Expose_Event_Cb
      if Window_Type = Text_Window_Left then
         Redraw_Side_Info;
      elsif Window_Type = Text_Window_Text then
         Highlight_Text;
      end if;

      --  Return false, so that the signal is not blocked, and other
      --  clients can use it.

      return False;

   exception
      when E : others =>
         Trace (Exception_Handle, E);
         return False;
   end Expose_Event_Cb;

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
      View.As_Is_Mode := Disabled;

      Save_Cursor_Position (View);
      External_End_Action (Buffer);

      Stop_Selection_Drag (View);

      return False;

   exception
      when E : others =>
         Trace (Exception_Handle, E);
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
         Trace (Exception_Handle, E);
         return False;
   end Focus_In_Event_Cb;

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy (View : access Gtk_Widget_Record'Class) is
      V : constant Source_View := Source_View (View);
   begin
      Trace (Me, "Source_View is being destroyed");
      Register_View (Source_Buffer (Get_Buffer (V)), Add => False);
   end On_Destroy;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (View   : out Source_View;
      Scroll : Gtk.Scrolled_Window.Gtk_Scrolled_Window;
      Area   : Gtk.Drawing_Area.Gtk_Drawing_Area;
      Buffer : Src_Editor_Buffer.Source_Buffer;
      Kernel : access GPS.Kernel.Kernel_Handle_Record'Class) is
   begin
      View := new Source_View_Record;
      Initialize (View, Scroll, Area, Buffer, Kernel);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (View   : access Source_View_Record;
      Scroll : Gtk.Scrolled_Window.Gtk_Scrolled_Window;
      Area   : Gtk.Drawing_Area.Gtk_Drawing_Area;
      Buffer : Src_Editor_Buffer.Source_Buffer;
      Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Insert_Iter : Gtk_Text_Iter;
      Hook        : Preferences_Hook;
      F_Hook      : File_Hook;

   begin
      --  Initialize the Source_View. Some of the fields can not be initialized
      --  until the widget is realized or mapped. Their initialization is thus
      --  done at that point.

      pragma Assert (Buffer /= null);

      Initialize (View, Gtkada_Text_Buffer (Buffer));

      View.Kernel := Kernel_Handle (Kernel);
      View.Scroll := Scroll;
      View.Area   := Area;

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
        (View.Area, Signal_Expose_Event,
         Marsh       => Return_Callback.To_Marshaller
           (Speed_Bar_Expose_Event_Cb'Access),
         After       => False,
         Slot_Object => View);

      Widget_Callback.Object_Connect
        (View.Area, Signal_Size_Allocate,
         Marsh       => Widget_Callback.To_Marshaller
           (Speed_Bar_Size_Allocate_Cb'Access),
         After       => False,
         Slot_Object => View);

      Set_Border_Window_Size (View, Enums.Text_Window_Left, 1);
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
        (View, Signal_Size_Allocate,
         Widget_Callback.To_Marshaller (Size_Allocated'Access),
         After => True);
      Widget_Callback.Connect
        (View, Signal_Size_Allocate,
         Widget_Callback.To_Marshaller (Size_Allocated_Before'Access),
         After => False);

      Widget_Callback.Connect
        (View, Signal_Paste_Clipboard,
         Widget_Callback.To_Marshaller (Paste_Clipboard_Before'Access),
         After => False);

      Source_Buffer_Callback.Connect
        (Buffer, Signal_Cursor_Position_Changed,
         Cb        => Change_Handler'Access,
         User_Data => Source_View (View),
         After     => True);

      Source_Buffer_Callback.Connect
        (Buffer, Signal_Side_Column_Changed,
         Cb        => Side_Columns_Change_Handler'Access,
         User_Data => Source_View (View),
         After     => True);

      Source_Buffer_Callback.Connect
        (Buffer, Signal_Side_Column_Configuration_Changed,
         Cb        => Side_Columns_Config_Change_Handler'Access,
         User_Data => Source_View (View),
         After     => True);

      Source_Buffer_Callback.Connect
        (Buffer, Signal_Buffer_Information_Changed,
         Cb        => Buffer_Information_Change_Handler'Access,
         User_Data => Source_View (View),
         After     => True);

      Source_Buffer_Callback.Connect
        (Buffer, Signal_Line_Highlights_Changed,
         Cb        => Line_Highlight_Change_Handler'Access,
         User_Data => Source_View (View),
         After     => True);

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

      Hook := new Preferences_Hook_Record'
        (Function_No_Args with View => Source_View (View));
      Execute (Hook.all, Kernel);
      Add_Hook
        (Kernel, Preferences_Changed_Hook, Hook,
         Name  => "src_editor_view.preferences_changed",
         Watch => GObject (View));

      F_Hook := new File_Hook_Record'
        (Function_With_Args with View => Source_View (View));
      Add_Hook (Kernel, File_Saved_Hook, F_Hook,
                Name  => "src_editor_view.file_saved",
                Watch => GObject (View));

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
      Win           : constant Gdk.Window.Gdk_Window :=
                        Get_Window (View, Text_Window_Left);
      X, Y, W, H, D : Gint;

   begin
      --  ??? if Realized_Is_Set (View) then
      --  When will we connect Expose_Event_Cb in case the View isn't
      --  realized ???

      Return_Callback.Connect
        (View, Signal_Expose_Event,
         Marsh => Return_Callback.To_Marshaller (Expose_Event_Cb'Access),
         After => False);

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

      if Win /= null then
         Get_Geometry (Win, X, Y, W, H, D);
         Clear_Area_E (Win, X, Y, W, H);
         Invalidate_Window (View);
      end if;

      View.Connect_Expose_Registered := False;

      --  If there is a synchronized editor, scroll this editor to align it
      --  with the synchronized editor.

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
         if Position_Set_Explicitely
           (Source_Buffer (Get_Buffer (View)), True)
         then
            Scroll_To_Cursor_Location (View, Center);
         end if;
      end if;

      View.Scrolling := False;

      return False;

   exception
      when E : others =>
         Trace (Exception_Handle, E);
         return False;
   end Connect_Expose;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Hook   : Preferences_Hook_Record;
      Kernel : access Kernel_Handle_Record'Class)
   is
      pragma Unreferenced (Kernel);
      Source : constant Source_View := Hook.View;
      Layout : Pango_Layout;
      Color  : Gdk_Color;
      Mode   : constant Speed_Column_Policies := Source.Speed_Column_Mode;
      Ink_Rect, Logical_Rect : Gdk_Rectangle;
      Tmp    : HSV_Color;
   begin
      --  Recompute the width of one character

      Layout := Create_Pango_Layout (Source);
      Set_Attributes (Layout, null);
      Set_Font_Description (Layout, Default_Style.Get_Pref_Font);
      Set_Text (Layout, (1 .. 256 => '0'));
      Get_Pixel_Extents (Layout, Ink_Rect, Logical_Rect);
      Source.Width_Of_256_Chars := Ink_Rect.Width;
      Unref (Layout);

      --  Reset the color of the current line.
      --  This procedure might be called before the Map_Cb has been called,
      --  and therefore the Current_Line_GC might not be initialized at this
      --  point.

      Source.Current_Block_Color := Current_Block_Color.Get_Pref;
      Source.Highlight_Blocks := Block_Highlighting.Get_Pref;

      Source.Highlight_As_Line := Current_Line_Thin.Get_Pref;

      Source.Speed_Column_Mode := Speed_Column_Policy.Get_Pref;

      if Source.Speed_Column_Mode /= Mode then
         if Source.Speed_Column_Mode = Never then
            Set_Size_Request (Source.Area, 1, -1);

         elsif Source.Speed_Column_Mode = Always then
            Set_Size_Request (Source.Area, Speed_Column_Width, -1);
         end if;

         if Source.Speed_Column_Buffer /= Null_Surface then
            Destroy (Source.Speed_Column_Buffer);
            Source.Speed_Column_Buffer := Null_Surface;
         end if;

         if Realized_Is_Set (Source.Area) then
            Redraw_Speed_Column (Source);
         end if;
      end if;

      --  Modify the text background, color and font

      if Realized_Is_Set (Source) then
         Modify_Font (Source, Default_Style.Get_Pref_Font);
      end if;

      Color := Default_Style.Get_Pref_Bg;

      if Color /= Source.Background_Color then
         Source.Background_Color := Color;
         Tmp := To_HSV (To_Cairo (Color));
         Tmp.V := Tmp.V * 0.97;
         Source.Background_Color_Other := To_Cairo (Tmp);
         Modify_Base (Source, State_Normal, Color);
      end if;

      Color := Default_Style.Get_Pref_Fg;

      if Color /= Source.Text_Color then
         Source.Text_Color := Color;
         Modify_Text (Source, State_Normal, Color);
      end if;

      Source.Current_Line_Color := Current_Line_Color.Get_Pref;
      Source.Highlight_Current :=
        not Equal (Source.Current_Line_Color, White (Get_Default_Colormap));

   exception
      when E : others => Trace (Exception_Handle, E);
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Hook   : File_Hook_Record;
      Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class)
   is
      pragma Unreferenced (Kernel);
      D      : constant File_Hooks_Args := File_Hooks_Args (Data.all);
      Buffer : constant Source_Buffer :=
                 Source_Buffer (Get_Buffer (Hook.View));
      File   : GNATCOLL.VFS.Virtual_File := Get_Filename (Buffer);
   begin
      if File = GNATCOLL.VFS.No_File then
         File := Get_File_Identifier (Buffer);
      end if;

      if File = D.File then
         Redraw_Columns (Hook.View);
      end if;
   exception
      when E : others => Trace (Exception_Handle, E);
   end Execute;

   -------------------------------
   -- Scroll_To_Cursor_Location --
   -------------------------------

   procedure Scroll_To_Cursor_Location
     (View      : access Source_View_Record;
      Centering : Centering_Type := Minimal) is
   begin
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
      if Has_Focus_Is_Set (View) then
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
         Set_Time (View.Button_Event, 0);

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
      Line_Height   : Gint;
      Unused        : Gint;
      Result        : Boolean;

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
         Forward_To_Line_End (Iter, Result);
      end if;

      Get_Iter_Location (View, Iter, Iter_Location);
      Get_Line_Yrange (View, Iter, Unused, Line_Height);

      Out_Of_Bounds := Buffer_X > Iter_Location.X
        or else Buffer_Y > Iter_Location.Y + Line_Height;
   end Window_To_Buffer_Coords;

   ----------------------------
   -- Event_To_Buffer_Coords --
   ----------------------------

   procedure Event_To_Buffer_Coords
     (View          : access Source_View_Record;
      Event         : Gdk_Event;
      Line          : out Gint;
      Column        : out Gint;
      Out_Of_Bounds : out Boolean) is
   begin
      Window_To_Buffer_Coords
        (View, Gint (Get_X (Event)), Gint (Get_Y (Event)),
         Line, Column, Out_Of_Bounds);
   end Event_To_Buffer_Coords;

   -------------------------------------
   -- Speed_Bar_Button_Press_Event_Cb --
   -------------------------------------

   function Speed_Bar_Button_Press_Event_Cb
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean
   is
      View         : constant Source_View := Source_View (Widget);
      Dummy_Gint   : Gint;
      W, H, D      : Gint;
      Button_Y     : Gint;
      Lower, Upper : Gdouble;
      Adj          : Gtk_Adjustment;
   begin
      if (Get_Event_Type (Event) = Button_Release
          and then Get_Button (Event) = 1)
        or else Get_Event_Type (Event) = Motion_Notify
      then
         if View.Scrolling
           or else View.Area = null
         then
            return False;
         end if;

         Button_Y := Gint (Get_Y (Event));

         Get_Geometry
           (Get_Window (View.Area), Dummy_Gint, Dummy_Gint, W, H, D);

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
      end if;

      return False;

   exception
      when E : others =>
         Trace (Exception_Handle, E);
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
         Trace (Exception_Handle, E);
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
      Left_Window : constant Gdk.Window.Gdk_Window :=
                      Get_Window (View, Text_Window_Left);

   begin
      if Get_Event_Type (Event) = Button_Release
        and then Get_Button (Event) = 1
        and then Get_Window (Event) /= Left_Window
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
         Trace (Exception_Handle, E);
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

      Left_Window : constant Gdk.Window.Gdk_Window :=
                      Get_Window (View, Text_Window_Left);

      Window : Gdk.Window.Gdk_Window;

      Result : Boolean;
      pragma Unreferenced (Result);
   begin
      External_End_Action (Buffer);

      Window := Get_Window (Event);

      case Get_Event_Type (Event) is
         when Button_Press =>
            if Get_Button (Event) = 1 then
               if Window = Left_Window then
                  declare
                     Dummy_Gint         : Gint;
                     Iter               : Gtk_Text_Iter;
                     Line               : Buffer_Line_Type;
                     Button_X, Button_Y : Gint;
                     X, Y               : Gint;

                  begin
                     --  Get the coordinates of the click

                     Button_X := Gint (Get_X (Event));
                     Button_Y := Gint (Get_Y (Event));

                     --  Find the line number

                     Window_To_Buffer_Coords
                       (View, Text_Window_Left,
                        Window_X => Button_X, Window_Y => Button_Y,
                        Buffer_X => X, Buffer_Y => Y);

                     Get_Line_At_Y (View, Iter, Y, Dummy_Gint);
                     Line := Buffer_Line_Type (Get_Line (Iter) + 1);

                     Set_Focus_Child (View.Child);
                     On_Click (Buffer, Line, Button_X);
                  end;

               else
                  if not View.Button_Pressed then
                     View.Button_Pressed := True;
                     Deep_Copy (Event, View.Button_Event);
                  end if;
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
                  Copy_Clipboard
                    (Get_Clipboard (View.Kernel),
                     Get_Current_Focus_Widget (View.Kernel));

                  declare
                     L, C    : Gint;
                     Iter    : Gtk_Text_Iter;
                  begin
                     Window_To_Buffer_Coords
                       (View, Text_Window_Text,
                        Gint (Get_X (Event)), Gint (Get_Y (Event)), L, C);
                     Get_Iter_At_Location (View, Iter, L, C);
                     Grab_Focus (View);
                     Place_Cursor (Get_Buffer (View), Iter);
                     Paste_Clipboard (Get_Clipboard (View.Kernel), View);
                  end;

                  return True;
               end if;
            end if;

         when Gdk_2button_Press =>
            if Window /= Left_Window and then Get_Button (Event) = 1 then
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

   exception
      when E : others =>
         Trace (Exception_Handle, E);
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
      Start, Last : Gtk_Text_Iter;
      Result      : Boolean;
      Ignore      : Boolean;

      Key         : Gdk_Key_Type;
   begin
      if Realized_Is_Set (View)
        and then not Get_Property
          (Gtk_Window (Get_Toplevel (View)), Has_Toplevel_Focus_Property)
      then
         return True;
      end if;

      --  As soon as a key is pressed on an editor, reset the flag
      --  position_set_explicitely, so that scrolling does not occur, unless
      --  this is the Hyper Mode key

      Key := Get_Key_Val (Event);

      if Key /= GDK_Control_L and then Key /= GDK_Control_R then
         Ignore := Position_Set_Explicitely (Buffer, True);
      end if;

      if not Get_Editable (View) then
         if Get_String (Event)'Length >= 1 then
            Insert
              (View.Kernel,
               -"Warning: attempting to edit a read-only editor.",
               Mode => Error);
         end if;
         return False;
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

            if not View.As_Is_Enabled then
               Word_Added (Buffer);
            end if;

            External_End_Action (Buffer);

            --  If there is a selection, delete it

            if Selection_Exists (Buffer) then
               Ignore := Delete_Selection (Buffer, True, True);

               External_End_Action (Buffer);

            elsif Should_Indent (Buffer) then
               Result :=
                 Insert_Interactive_At_Cursor (Buffer, (1 => ASCII.LF), True);

               if Result then
                  --  ??? Could be a key handler as well
                  Get_Iter_At_Mark (Buffer, Last, Get_Insert (Buffer));
                  Copy (Last, Dest => Start);

                  --  We do not want to get the previous line if we have the
                  --  as-is modifier set.

                  if not View.As_Is_Enabled then
                     Backward_Line (Start, Ignore);
                  end if;

                  if not Ends_Line (Last) then
                     Forward_To_Line_End (Last, Ignore);
                  end if;

                  Ignore := Do_Indentation (Buffer, Start, Last);
               end if;

               View.Reset_As_Is_Mode;
               return True;
            end if;

         when GDK_Linefeed | GDK_Tab | GDK_Home | GDK_Page_Up | GDK_Page_Down |
              GDK_End | GDK_Begin | GDK_Up | GDK_Down | GDK_Left | GDK_Right
            =>
            Clear_Typed_Chars (Buffer);
            External_End_Action (Buffer);

         when GDK_BackSpace =>
            if Get_Send_Event (Event) then
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
            declare
               Key_Str : constant String := Get_String (Event);
            begin
               if Key_Str'Length = 1
                 and then
                   not Is_In (Key_Str (Key_Str'First),
                              Word_Character_Set (Get_Language (Buffer)))
                 and then
                   not Is_In (Key_Str (Key_Str'First), Constants.Control_Set)
               then
                  if not View.As_Is_Enabled then
                     Word_Added (Buffer);
                  end if;
               end if;
            end;
      end case;

      return False;

   exception
      when E : others =>
         Trace (Exception_Handle, E);
         return False;
   end Key_Press_Event_Cb;

   --------------------
   -- Redraw_Columns --
   --------------------

   procedure Redraw_Columns (View : access Source_View_Record'Class) is
      Left_Window : Gdk.Window.Gdk_Window;

      X, Y, Width, Height, Depth : Gint;
      Layout                     : Pango_Layout;

      Src_Buffer  : constant Source_Buffer :=
                      Source_Buffer (Get_Buffer (View));

      Total_Width : Gint;
      Cr          : Cairo_Context;
      Color       : Cairo_Color;

   begin
      Total_Width := Gint (Get_Total_Column_Width (Src_Buffer));

      if Total_Width = 0 then
         Set_Border_Window_Size (View, Enums.Text_Window_Left, 1);
         View.Buffer_Column_Size := 1;
         return;
      end if;

      if Total_Width /= View.Buffer_Column_Size then
         View.Buffer_Column_Size := Total_Width;
         Set_Border_Window_Size (View, Enums.Text_Window_Left, Total_Width);

         --  Force the redraw
         Invalidate_Side_Column_Cache (View);
      end if;

      --  Create the graphical elements

      Left_Window := Get_Window (View, Text_Window_Left);

      if Left_Window = null then
         return;
      end if;

      if View.Side_Column_Buffer /= Null_Surface
        and then View.Top_Line = View.Buffer_Top_Line
        and then View.Bottom_Line = View.Buffer_Bottom_Line
      then
         --  If the cache corresponds to the lines, redraw it

         Cr := Create (Left_Window);
         Set_Source_Surface (Cr, View.Side_Column_Buffer, 0.0, 0.0);
         Paint (Cr);
         Destroy (Cr);

      else
         --  The lines have changed or the cache is not created: create it

         Invalidate_Side_Column_Cache (View);

         View.Buffer_Top_Line    := View.Top_Line;
         View.Buffer_Bottom_Line := View.Bottom_Line;

         Layout := Create_Pango_Layout (View);
         Set_Font_Description (Layout, Default_Style.Get_Pref_Font);

         Get_Geometry (Left_Window, X, Y, Width, Height, Depth);

         View.Side_Column_Buffer := Gdk.Window.Create_Similar_Surface
           (Gdk_Drawable (Left_Window), Cairo_Content_Color_Alpha,
            Total_Width, Height);

         Cr := Create (View.Side_Column_Buffer);
         Set_Source_Color (Cr, View.Background_Color_Other);
         Cairo.Paint (Cr);

         Color := To_Cairo (View.Text_Color);
         Color.Alpha := 0.5;
         Set_Line_Width (Cr, 0.5);
         Draw_Line
           (Cr, Color,
            X + Total_Width - 1,
            0,
            X + Total_Width - 1,
            Height);

         Destroy (Cr);

         Draw_Line_Info
           (Src_Buffer, View.Top_Line, View.Bottom_Line,
            Gtk_Text_View (View), View.Text_Color,
            Layout, View.Side_Column_Buffer);

         Cr := Create (Left_Window);
         Set_Source_Surface (Cr, View.Side_Column_Buffer, 0.0, 0.0);
         Paint (Cr);
         Destroy (Cr);

         Unref (Layout);
      end if;
   end Redraw_Columns;

   -------------------------
   -- Redraw_Speed_Column --
   -------------------------

   procedure Redraw_Speed_Column (View : access Source_View_Record'Class) is
      Right_Window : Gdk.Window.Gdk_Window;

      X, Y, Width, Height, Depth : Gint;
      Color        : Gdk_Color;

      Src_Buffer   : constant Source_Buffer :=
                       Source_Buffer (Get_Buffer (View));

      Line_Height  : Gdouble;
      Total_Lines  : Gint;

      Info_Exists  : Boolean := False;
      Cr           : Cairo_Context;

   begin
      if View.Area = null
        or else View.Speed_Column_Mode = Never
      then
         return;
      end if;

      Right_Window := Get_Window (View.Area);

      if Right_Window = null then
         return;
      end if;

      Get_Geometry (Right_Window, X, Y, Width, Height, Depth);

      Total_Lines := Get_Line_Count (Src_Buffer);

      if View.Speed_Column_Buffer = Null_Surface then
         View.Speed_Column_Buffer :=
           Create_Similar_Surface
           (Right_Window, Cairo_Content_Color_Alpha,
            Speed_Column_Width,
            Height);

         Cr := Create (View.Speed_Column_Buffer);

         Set_Source_Color (Cr, View.Background_Color_Other);
         Cairo.Paint (Cr);

         Line_Height := Gdouble (Height) / Gdouble (Total_Lines + 1);

         --  Make the line height at least 2 pixels high

         if Line_Height < 1.0 then
            Line_Height := 1.0;
         end if;

         Set_Line_Width (Cr, Line_Height);
         Set_Line_Cap (Cr, Cairo_Line_Cap_Square);

         Info_Exists := False;

         for J in 1 .. Total_Lines loop
            Color := Get_Highlight_Color
              (Src_Buffer, Buffer_Line_Type (J),
               Context => Highlight_Speedbar);

            if Color /= Null_Color then
               Set_Source_Color (Cr, Color);
               Draw_Line
                 (Cr, To_Cairo (Color),
                  0,
                  (Height * J) / Total_Lines,
                  Speed_Column_Width,
                  (Height * J) / Total_Lines);

               Info_Exists := True;
            end if;
         end loop;

         Destroy (Cr);

         if Info_Exists then
            if View.Speed_Column_Hide_Registered then
               View.Speed_Column_Hide_Registered := False;
               Glib.Main.Remove (View.Speed_Column_Hide_Timeout);
            end if;

            if Width = 1
              and then View.Speed_Column_Mode /= Never
            then
               Set_Size_Request (View.Area, Speed_Column_Width, -1);
            end if;

         elsif Width = Speed_Column_Width
           and then View.Speed_Column_Mode /= Always
           and then not View.Speed_Column_Hide_Registered
         then
            View.Speed_Column_Hide_Registered := True;
            View.Speed_Column_Hide_Timeout := Source_View_Timeout.Timeout_Add
              (Speed_Column_Timeout, Hide_Speed_Column_Timeout'Access,
               Source_View (View));
         end if;
      end if;

      Cr := Create (Right_Window);
      Set_Source_Surface (Cr, View.Speed_Column_Buffer, 0.0, 0.0);
      Paint (Cr);

      if Width = 1 then
         Destroy (Cr);
         return;
      end if;

      Set_Line_Width (Cr, 0.5);
      Draw_Rectangle
        (Cr, (0.0, 0.0, 0.0, Alpha => 0.1), True,
         1,
         (Height * Gint (View.Top_Line - 1)) / Total_Lines,
         Speed_Column_Width - 2,
         (Height * Gint (View.Bottom_Line - View.Top_Line + 1)) / Total_Lines,
         2.0);

      Destroy (Cr);
   end Redraw_Speed_Column;

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
      Invalidate_Window (Src_View);

      if Src_View.Scrolling or else Src_View.Synchronized_Editor = null then
         return;
      end if;

      Src_View.Scrolling := True;

      if not Src_View.Synchronized_Editor.Scrolling
        and then Src_View.Scroll /= null
        and then Src_View.Synchronized_Editor.Scroll /= null
      then
         Set_Value
           (Get_Vadjustment (Src_View.Synchronized_Editor.Scroll),
            Get_Value (Get_Vadjustment (Src_View.Scroll)));

         Set_Value
           (Get_Hadjustment (Src_View.Synchronized_Editor.Scroll),
            Get_Value (Get_Hadjustment (Src_View.Scroll)));
      end if;

      Src_View.Scrolling := False;

   exception
      when E : others => Trace (Exception_Handle, E);
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
      Set_In_Completion (Source_Buffer (Get_Buffer (View)), False);
   end End_Completion;

   -------------------
   -- In_Completion --
   -------------------

   function In_Completion
     (View : access Source_View_Record'Class) return Boolean is
   begin
      return In_Completion (Source_Buffer (Get_Buffer (View)));
   end In_Completion;

   ----------------------------------
   -- Invalidate_Side_Column_Cache --
   ----------------------------------

   procedure Invalidate_Side_Column_Cache
     (View : access Source_View_Record'Class) is
   begin
      if View.Side_Column_Buffer /= Null_Surface then
         Destroy (View.Side_Column_Buffer);
         View.Side_Column_Buffer := Null_Surface;
      end if;
   end Invalidate_Side_Column_Cache;

end Src_Editor_View;
