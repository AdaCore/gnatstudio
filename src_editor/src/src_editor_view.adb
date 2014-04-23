------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2014, AdaCore                     --
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
with Gtk.Text_View;              use Gtk.Text_View;
with Gtk.Widget;                 use Gtk.Widget;

with Gtkada.Handlers;            use Gtkada.Handlers;
with Gtkada.MDI;                 use Gtkada.MDI;
with Gtkada.Style;               use Gtkada.Style;
with Gtkada.Text_Buffer;         use Gtkada.Text_Buffer;

with Pango.Layout;               use Pango.Layout;
with Pango.Attributes;           use Pango.Attributes;

with Src_Editor_Buffer;          use Src_Editor_Buffer;
with Src_Editor_Buffer.Blocks;   use Src_Editor_Buffer.Blocks;
with Src_Editor_Buffer.Hooks;    use Src_Editor_Buffer.Hooks;
with Src_Editor_Module.Markers;  use Src_Editor_Module.Markers;

with Basic_Types;                use Basic_Types;
with Config;                     use Config;
with Default_Preferences;        use Default_Preferences;
with GPS.Intl;                   use GPS.Intl;
with GPS.Kernel;                 use GPS.Kernel;
with GPS.Kernel.Clipboard;       use GPS.Kernel.Clipboard;
with GPS.Kernel.Hooks;           use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;             use GPS.Kernel.MDI;
with GPS.Kernel.Project;         use GPS.Kernel.Project;
with GPS.Kernel.Standard_Hooks;  use GPS.Kernel.Standard_Hooks;
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

--  Drawing the side info is organized this way:
--
--     <----  side area (View.Area)   ----><----  editor (View)      --->
--      |             ||                  |
--      | drawing of  || drawing of       |
--      | speed bar   || side info data   |
--      |             ||                  |
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

   Speed_Bar_Default_Width : constant := 10;
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

   Target_Table : constant Target_Entry_Array :=
     ((Interfaces.C.Strings.New_String ("text/uri-list"), 0, 0),
      (Interfaces.C.Strings.New_String ("text/plain"), 0, 1));

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
     (Widget  : access Gtk_Widget_Record'Class;
      Cr      : Cairo_Context) return Boolean;
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

   procedure Redraw_Speed_Column
     (View : access Source_View_Record'Class;
      Cr    : Cairo_Context);
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

   type Preferences_Hook_Record is new Function_With_Args with record
      View : Source_View;
   end record;
   type Preferences_Hook is access all Preferences_Hook_Record'Class;
   overriding procedure Execute
     (Hook   : Preferences_Hook_Record;
      Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class);
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

   function Side_Area_Expose_Event_Cb
     (Widget : access Gtk_Widget_Record'Class;
      Cr     : Cairo_Context) return Boolean;
   --  Callback for an "expose" event on the speed bar

   procedure Speed_Bar_Size_Allocate_Cb
     (Widget : access Gtk_Widget_Record'Class);
   --  Callback for an "size_allocate" signal on the speed bar

   procedure Register_Idle_Column_Redraw (View : Source_View);
   --  Register an idle redrawing of the side columns

   procedure Recompute_Visible_Area (View : access Source_View_Record'Class);
   --  Recompute the currently visible area in the view

   procedure Size_Side_Column (View : access Source_View_Record'Class);
   --  Resize the side column area

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

   -------------------------------
   -- Hide_Speed_Column_Timeout --
   -------------------------------

   function Hide_Speed_Column_Timeout (View : Source_View) return Boolean is
   begin
      View.Speed_Bar_Width := 0;
      Size_Side_Column (View);
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
      Win        : Gdk.Gdk_Window :=
                     Get_Window (User, Text_Window_Text);
      X, Y, W, H : Gint;

   begin
      if Win = null then
         return;
      end if;

      Get_Geometry (Win, X, Y, W, H);
      Gdk.Window.Invalidate_Rect (Win, (X, Y, W, H), True);

      Win := Get_Window (User.Area);

      if Win = null then
         return;
      end if;

      Get_Geometry (Win, X, Y, W, H);
      Gdk.Window.Invalidate_Rect (Win, (X, Y, W, H), True);
   end Invalidate_Window;

   ---------------------------
   -- Line_Highlight_Redraw --
   ---------------------------

   function Line_Highlight_Redraw (User : Source_View) return Boolean is
   begin
      case User.Speed_Column_Mode is
         when Never =>
            User.Speed_Bar_Width := 0;
         when Always =>
            User.Speed_Bar_Width := Speed_Bar_Default_Width;
         when Automatic =>
            declare
               Buffer : constant Source_Buffer := Source_Buffer
                 (Get_Buffer (User));
               Color  : Gdk_RGBA;
               Found_Width : Boolean := False;
            begin
               --  Change the size if there is new info

               for J in 1 .. Get_Line_Count (Buffer) loop
                  Color := Get_Highlight_Color
                    (Buffer, Buffer_Line_Type (J),
                     Context => Highlight_Speedbar);

                  if Color /= Null_RGBA then
                     User.Speed_Bar_Width := Speed_Bar_Default_Width;
                     Found_Width := True;
                     exit;
                  end if;
               end loop;

               if Found_Width then
                  --  A width has been allocated to the speed column: if we
                  --  had registered it to be hidden, unregister this now.

                  if User.Speed_Column_Hide_Registered then
                     User.Speed_Column_Hide_Registered := False;
                     Glib.Main.Remove (User.Speed_Column_Hide_Timeout);
                  end if;
               else
                  --  We have not found a width: register for the column
                  --  to be hidden.

                  if not User.Speed_Column_Hide_Registered then
                     User.Speed_Column_Hide_Registered := True;
                     User.Speed_Column_Hide_Timeout    :=
                       Source_View_Timeout.Timeout_Add
                         (Speed_Column_Timeout,
                          Hide_Speed_Column_Timeout'Access,
                          User);
                  end if;
               end if;
            end;
      end case;

      Size_Side_Column (User);

      User.Redraw_Registered := False;

      if User.Speed_Column_Buffer /= Null_Surface then
         Destroy (User.Speed_Column_Buffer);
         User.Speed_Column_Buffer := Null_Surface;
      end if;

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

   exception
      when E : others => Trace (Me, E);
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

   --------------------
   -- Change_Handler --
   --------------------

   procedure Change_Handler
     (Buffer : access Source_Buffer_Record'Class;
      Params : Glib.Values.GValues;
      User   : Source_View)
   is
      Line : constant Gint := Get_Int (Nth (Params, 1));
      Has_Focus : constant Boolean :=
        Gtkada.MDI.MDI_Child (User.Child) =
        Get_Focus_Child (Get_MDI (User.Kernel));
   begin
      if Has_Focus then
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
             Get_Block (Buffer, Editable_Line_Type (Line), False,
                        Filter => Categories_For_Block_Highlighting))
      then
         Invalidate_Window (User);
      end if;

      User.Current_Line := Line;
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
      --  If necessary, emit the Source_Lines_Revealed signal

      if Bottom_Line >= Top_Line then
         Buffer.Source_Lines_Revealed
           (View.Get_Project, Top_Line, Bottom_Line);
      end if;
   end Recompute_Visible_Area;

   -------------------------------
   -- Speed_Bar_Expose_Event_Cb --
   -------------------------------

   function Side_Area_Expose_Event_Cb
     (Widget : access Gtk_Widget_Record'Class;
      Cr     : Cairo_Context) return Boolean
   is
      View : constant Source_View := Source_View (Widget);

   begin
      --  Paint the background, common to both the speed column and the side
      --  info column

      Set_Source_Color (Cr, View.Background_Color_Other);
      Cairo.Paint (Cr);

      if View.Speed_Bar_Width > 0 then
         Redraw_Speed_Column (View, Cr);
         Translate (Cr, Gdouble (View.Speed_Bar_Width), 0.0);
      end if;

      Redraw_Columns (View, Cr);

      return True;

   exception
      when E : others =>
         Trace (Me, E);
         return True;
   end Side_Area_Expose_Event_Cb;

   ---------------------
   -- Expose_Event_Cb --
   ---------------------

   function Expose_Event_Cb
     (Widget  : access Gtk_Widget_Record'Class;
      Cr      : Cairo_Context) return Boolean
   is
      View   : constant Source_View := Source_View (Widget);
      Buffer : constant Source_Buffer := Source_Buffer (Get_Buffer (View));

      X, Y, Width, Height : Gint;

      procedure Highlight_Text;
      --  Highlight the current text, in particular the current line and
      --  current block, if needed.

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
         Color            : Gdk_RGBA;
         Tmp_Color        : HSV_Color;

         Window : constant Gdk.Gdk_Window :=
           View.Get_Window (Text_Window_Text);

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
         Set_Line_Width (Cr, 1.0);
         Set_Antialias (Cr, Cairo_Antialias_None);
         Save (Cr);

         Buffer_To_Window_Coords
           (View, Text_Window_Text, Rect.X, Rect.Y, X, Y);

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

         Get_Iter_At_Mark
           (Get_Buffer (View), Cursor_Iter, View.Saved_Cursor_Mark);

         Get_Line_Yrange (View, Cursor_Iter, Line_Y, Line_Height);

         --  Highlight the line that contains the cursor

         if View.Highlight_Current then
            Buffer_To_Window_Coords
              (View, Text_Window_Text, Dummy, Line_Y, Dummy, Buffer_Line_Y);

            Set_Source_RGBA (Cr, View.Current_Line_Color);

            if View.Highlight_As_Line then
               Move_To (Cr,
                        0.0,
                        Gdouble (Buffer_Line_Y + Line_Height));
               Rel_Line_To (Cr, Gdouble (Rect.Width), 0.0);
               Stroke (Cr);
            else
               Cairo.Rectangle
                 (Cr,
                  0.0,
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
               False,
               Filter => Categories_For_Block_Highlighting);
            Draw_Block (View.Current_Block);
         end if;

         --  Redraw the line showing the nth column if needed

         if Column > 0 then
            X := (Column * View.Width_Of_256_Chars) / 256 - Rect.X + Margin;

            Save (Cr);
            Set_Line_Width (Cr, 1.0);
            Tmp_Color := To_HSV (View.Text_Color);

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

         Restore (Cr);
      end Highlight_Text;

   begin  -- Expose_Event_Cb
      Highlight_Text;

      --  Return false, so that the signal is not blocked, and other
      --  clients can use it.

      return False;

   exception
      when E : others =>
         Trace (Me, E);
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
      Result : Boolean;
      pragma Unreferenced (Result, X, Y, Info, Time);
      View : constant Source_View   := Source_View (Self);
   begin
      if Atom_Name (Data.Get_Target) = "text/uri-list" then
         Gtk.Handlers.Emit_Stop_By_Name
           (Object => Self, Name => "drag-data-received");
         declare
            Uris : constant GNAT.Strings.String_List := Data.Get_Uris;
         begin
            for Url of Uris loop
               Open_File_Editor
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

            return;
         end if;
      end if;
   end View_On_Drag_Data_Received;

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
      Hook        : Preferences_Hook;
      F_Hook      : File_Hook;
   begin
      --  Initialize the Source_View. Some of the fields can not be initialized
      --  until the widget is realized or mapped. Their initialization is thus
      --  done at that point.

      pragma Assert (Buffer /= null);

      Gtkada.Text_View.Initialize (View, Gtkada_Text_Buffer (Buffer));

      Gtk.Dnd.Dest_Set
        (View, Dest_Default_All,
         Target_Table,
         Gdk.Dnd.Action_Any);

      View.Kernel  := Kernel_Handle (Kernel);
      View.Scroll  := Gtk_Scrolled_Window (Scroll);
      View.Area    := Area;

      if Project = No_Project then
         --  Pick a project at random
         View.Project_Path := Get_Registry (Kernel).Tree.Info_Set
           (Buffer.Get_Filename).First_Element.Project.Project_Path;
      else
         View.Project_Path := Project.Project_Path;
      end if;

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

      View.On_Drag_Data_Received
        (View_On_Drag_Data_Received'Access);

      Widget_Callback.Object_Connect
        (View.Area, Signal_Size_Allocate,
         Marsh       => Widget_Callback.To_Marshaller
           (Speed_Bar_Size_Allocate_Cb'Access),
         After       => False,
         Slot_Object => View);

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
        (Function_With_Args with View => Source_View (View));
      Execute (Hook.all, Kernel, Data => null);
      Add_Hook
        (Kernel, Preference_Changed_Hook, Hook,
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
   begin
      Return_Callback.Connect
        (View, Signal_Draw,
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

      Invalidate_Window (View);

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
     (Hook   : Preferences_Hook_Record;
      Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class)
   is
      pragma Unreferenced (Kernel);
      Source : constant Source_View := Hook.View;
      Layout : Pango_Layout;
      Color  : Gdk_RGBA;
      Mode   : constant Speed_Column_Policies := Source.Speed_Column_Mode;
      Ink_Rect, Logical_Rect : Pango.Pango_Rectangle;
      Pref : constant Preference := Get_Pref (Data);
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
      Source.Speed_Column_Mode := Speed_Column_Policy.Get_Pref;

      if Source.Speed_Column_Mode /= Mode then
         if Source.Speed_Column_Mode = Never then
            Source.Speed_Bar_Width := 0;

         elsif Source.Speed_Column_Mode = Always then
            Source.Speed_Bar_Width := Speed_Bar_Default_Width;
         end if;

         if Source.Speed_Column_Buffer /= Null_Surface then
            Destroy (Source.Speed_Column_Buffer);
            Source.Speed_Column_Buffer := Null_Surface;
         end if;
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
     (Self : not null access Source_View_Record'Class)
   is
      Color : constant Gdk_RGBA := Default_Style.Get_Pref_Bg;
      C     : Cairo_Color := Color;
      Select_Color : Gdk_RGBA;
   begin
      Get_Style_Context (Self).Get_Background_Color
        (Gtk_State_Flag_Selected, Select_Color);

      if not Self.Get_Editable then
         C := Shade_Or_Lighten (C, Amount => 0.1);
      end if;

      Self.Background_Color := C;
      Self.Background_Color_Other := Shade_Or_Lighten (C, Amount => 0.03);

      --  Overridding background color also seems to set the selected color, so
      --  that selected text becomes invisible. So we have to reset it as well.
      Self.Override_Background_Color (Gtk_State_Flag_Normal, C);
      Self.Override_Background_Color (Gtk_State_Flag_Selected, Select_Color);

      if Self.Get_Editable then
         Get_Style_Context (Self).Remove_Class ("readonly");
      else
         Get_Style_Context (Self).Add_Class ("readonly");
      end if;

      Invalidate_Window (Self);
   end Set_Background_Color;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Hook   : File_Hook_Record;
      Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class)
   is
      pragma Unreferenced (Kernel, Data);
   begin
      Invalidate_Window (Hook.View);
   exception
      when E : others => Trace (Me, E);
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
      pragma Unreferenced (Result);
   begin
      External_End_Action (Buffer);

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
                  Copy_Clipboard
                    (Get_Clipboard (View.Kernel),
                     Get_Current_Focus_Widget (View.Kernel));

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
         if not (View.Get_Realized) then
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

      if not Get_Editable (View) then
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

            if not View.As_Is_Enabled then
               Word_Added (Buffer);
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
      Num_Color   : Gdk_RGBA := View.Background_Color_Other;

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
         Num_Color := Shade_Or_Lighten (View.Background_Color_Other, 0.5);
         Set_Font_Description (Layout, Small_Font.Get_Pref);

      else
         --  This is the regular line numbering, with every line numbered
         Num_Color := Shade_Or_Lighten (View.Background_Color_Other, 0.4);
         Set_Font_Description (Layout, Default_Style.Get_Pref_Font);
      end if;

      Draw_Line_Info
        (Src_Buffer, View.Top_Line, View.Bottom_Line,
         Gtk_Text_View (View),
         Num_Color,
         Layout, Cr);

      Unref (Layout);
   end Redraw_Columns;

   -------------------------
   -- Redraw_Speed_Column --
   -------------------------

   procedure Redraw_Speed_Column
     (View : access Source_View_Record'Class;
      Cr   : Cairo_Context)
   is
      Right_Window : Gdk.Gdk_Window;

      X, Y, Width, Height : Gint;
      Color        : Gdk_RGBA;

      Src_Buffer   : constant Source_Buffer :=
                       Source_Buffer (Get_Buffer (View));

      Line_Height  : Gdouble;
      Total_Lines  : Gint;

      Info_Exists  : Boolean := False;

      Buffer_Context : Cairo_Context;
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

      Get_Geometry (Right_Window, X, Y, Width, Height);

      Total_Lines := Get_Line_Count (Src_Buffer);

      if View.Speed_Column_Buffer = Null_Surface then
         View.Speed_Column_Buffer :=
           Create_Similar_Surface
           (Right_Window, Cairo_Content_Color_Alpha,
            View.Speed_Bar_Width,
            Height);

         Buffer_Context := Create (View.Speed_Column_Buffer);

         Set_Source_Color (Buffer_Context, View.Background_Color_Other);
         Cairo.Paint (Buffer_Context);

         Line_Height := Gdouble (Height) / Gdouble (Total_Lines + 1);

         --  Make the line height at least 2 pixels high

         if Line_Height < 1.0 then
            Line_Height := 1.0;
         end if;

         Set_Line_Width (Buffer_Context, Line_Height);
         Set_Line_Cap (Buffer_Context, Cairo_Line_Cap_Square);

         Info_Exists := False;

         for J in 1 .. Total_Lines loop
            Color := Get_Highlight_Color
              (Src_Buffer, Buffer_Line_Type (J),
               Context => Highlight_Speedbar);

            if Color /= Null_RGBA then
               Set_Source_Color (Buffer_Context, Color);
               Draw_Line
                 (Buffer_Context, Color,
                  0,
                  (Height * J) / Total_Lines,
                  View.Speed_Bar_Width,
                  (Height * J) / Total_Lines);

               Info_Exists := True;
            end if;
         end loop;

         Destroy (Buffer_Context);

         if Info_Exists then
            View.Speed_Bar_Width := Speed_Bar_Default_Width;

            if View.Speed_Column_Hide_Registered then
               View.Speed_Column_Hide_Registered := False;
               Glib.Main.Remove (View.Speed_Column_Hide_Timeout);
            end if;

            Size_Side_Column (View);

         elsif View.Speed_Column_Mode /= Always
           and then not View.Speed_Column_Hide_Registered
         then
            View.Speed_Column_Hide_Registered := True;
            View.Speed_Column_Hide_Timeout := Source_View_Timeout.Timeout_Add
              (Speed_Column_Timeout, Hide_Speed_Column_Timeout'Access,
               Source_View (View));
         end if;
      end if;

      Set_Source_Surface (Cr, View.Speed_Column_Buffer, 0.0, 0.0);
      Paint (Cr);

      Set_Line_Width (Cr, 0.5);
      Draw_Rectangle
        (Cr, (0.0, 0.0, 0.0, Alpha => 0.1), True,
         1,
         (Height * Gint (View.Top_Line - 1)) / Total_Lines,
         View.Speed_Bar_Width - 2,
         (Height * Gint (View.Bottom_Line - View.Top_Line + 1)) / Total_Lines,
         2.0);
      Cairo.Fill (Cr);
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
      Recompute_Visible_Area (Src_View);

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
      when E : others => Trace (Me, E);
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

   --------------------------
   -- Get_Extend_Selection --
   --------------------------

   function Get_Extend_Selection
     (Self : access Source_View_Record) return Boolean
   is
   begin
      return Self.Extend_Selection;
   end Get_Extend_Selection;

   --------------------------
   -- Set_Extend_Selection --
   --------------------------

   procedure Set_Extend_Selection
     (Self : access Source_View_Record; Extend_Selection : Boolean)
   is
   begin
      Self.Extend_Selection := Extend_Selection;
   end Set_Extend_Selection;

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

end Src_Editor_View;
