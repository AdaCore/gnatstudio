-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2003                       --
--                            ACT-Europe                             --
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

with Glib;                        use Glib;
with Glib.Object;                 use Glib.Object;
with Glib.Values;                 use Glib.Values;
with Gdk;                         use Gdk;
with Gdk.Drawable;                use Gdk.Drawable;
with Gdk.Color;                   use Gdk.Color;
with Gdk.Event;                   use Gdk.Event;
with Gdk.GC;                      use Gdk.GC;
with Gdk.Rectangle;               use Gdk.Rectangle;
with Gdk.Window;                  use Gdk.Window;
with Gdk.Pixmap;                  use Gdk.Pixmap;
with Gdk.Rectangle;               use Gdk.Rectangle;
with Gdk.Types;                   use Gdk.Types;
with Gdk.Types.Keysyms;           use Gdk.Types.Keysyms;
with Gtk;                         use Gtk;
with Gtk.Adjustment;              use Gtk.Adjustment;
with Gtk.Drawing_Area;            use Gtk.Drawing_Area;
with Gtk.Enums;                   use Gtk.Enums;
with Gtk.Handlers;
with Gtk.Main;                    use Gtk.Main;
with Gtk.Scrolled_Window;         use Gtk.Scrolled_Window;
with Gtk.Text_Buffer;             use Gtk.Text_Buffer;
with Gtk.Text_Iter;               use Gtk.Text_Iter;
with Gtk.Text_Mark;               use Gtk.Text_Mark;
with Gtk.Text_View;               use Gtk.Text_View;
with Gtk.Widget;                  use Gtk.Widget;
with Gtkada.Handlers;             use Gtkada.Handlers;
with Src_Editor_Buffer;           use Src_Editor_Buffer;
with Src_Editor_Buffer.Blocks;    use Src_Editor_Buffer.Blocks;

with Src_Editor_Buffer.Line_Information;
use Src_Editor_Buffer.Line_Information;
with Pango.Font;                  use Pango.Font;
with Pango.Layout;                use Pango.Layout;
with Gtkada.MDI;                  use Gtkada.MDI;
with GVD;
with Ada.Exceptions;              use Ada.Exceptions;
with Traces;                      use Traces;
with Glide_Kernel;                use Glide_Kernel;
with Glide_Kernel.Preferences;    use Glide_Kernel.Preferences;
with VFS;                         use VFS;

package body Src_Editor_View is

   Speed_Column_Width : constant := 10;
   --  The width of the speed column

   Margin : constant := 3;
   --  The margin left of the text.

   Me : constant Debug_Handle := Create ("Source_View");

   use type Pango.Font.Pango_Font_Description;
   procedure Setup (Data : Source_View; Id : Gtk.Handlers.Handler_Id);
   package Source_Buffer_Callback is new Gtk.Handlers.User_Callback_With_Setup
     (Widget_Type => Source_Buffer_Record,
      User_Type   => Source_View,
      Setup       => Setup);

   package Source_View_Idle is new Gtk.Main.Idle (Source_View);
   package Source_View_Timeout is new Gtk.Main.Timeout (Source_View);

   --------------------------
   -- Forward declarations --
   --------------------------

   function Connect_Expose (View : Source_View) return Boolean;
   --  Connect Expose_Event_Cb to the expose event. Emit an expose event.

   function Idle_Column_Redraw (View : Source_View) return Boolean;
   --  Redraw the side columns in an idle loop.

   procedure Realize_Cb (Widget : access Gtk_Widget_Record'Class);
   --  This procedure is invoked when the Source_View widget is realized.
   --  It performs various operations that can not be done before the widget
   --  is realized, such as setting the default font or the left border window
   --  size for instance.

   function Expose_Event_Cb
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean;
   --  This procedure handles all expose events happening on the left border
   --  window. It will the redraw the exposed area (this window may contains
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
   --  Callback for the "button_press_event" signal.

   function Speed_Bar_Button_Press_Event_Cb
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean;
   --  Callback for the "button_press_event" signal on the speed bar.

   function Speed_Bar_Button_Release_Event_Cb
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean;
   --  Callback for the "button_press_event" signal on the speed bar.

   function Key_Press_Event_Cb
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean;
   --  Callback for the "key_press_event" signal.

   procedure Map_Cb (Widget : access Gtk_Widget_Record'Class);
   --  This procedure is invoked when the Source_View widget is mapped.
   --  It performs various operations that can not be done before the widget
   --  is mapped, such as creating GCs associated to the left border window
   --  for instance.

   procedure Change_Handler
     (Buffer : access Source_Buffer_Record'Class;
      Params : Glib.Values.GValues;
      User   : Source_View);
   --  Callback for the "changed" signal.

   procedure Buffer_Information_Change_Handler
     (Buffer : access Source_Buffer_Record'Class;
      Params : Glib.Values.GValues;
      User   : Source_View);
   --  Callback for the "buffer_information_changed" signal.

   procedure Side_Columns_Change_Handler
     (Buffer : access Source_Buffer_Record'Class;
      Params : Glib.Values.GValues;
      User   : Source_View);
   --  Callback for the "side_columns_changed" signal.

   procedure Side_Columns_Config_Change_Handler
     (Buffer : access Source_Buffer_Record'Class;
      Params : Glib.Values.GValues;
      User   : Source_View);
   --  Callback for the "side_columns_configuration_changed" signal.

   procedure Invalidate_Window (User : Source_View);
   --  Redraw the buffer window.

   procedure Line_Highlight_Change_Handler
     (Buffer : access Source_Buffer_Record'Class;
      Params : Glib.Values.GValues;
      User   : Source_View);
   --  Callback for the "line_highlight_change" signal.

   procedure Redraw_Columns (View : access Source_View_Record'Class);
   --  Redraw the left area.

   procedure Redraw_Speed_Column (View : access Source_View_Record'Class);
   --  Redraw the speed column

   procedure Set_Font
     (View : access Source_View_Record'Class;
      Font : Pango.Font.Pango_Font_Description);
   --  Change the font used in the given Source_View. Note that this service
   --  should not be used if the widget is not realized.

   function On_Delete
     (View  : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean;
   --  Callback for the "delete_event" signal.

   function On_Button_Press
     (View  : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean;
   --  Callback for the "button_press_event"

   procedure Save_Cursor_Position
     (View : access Source_View_Record'Class);
   --  Save the cursor position.

   procedure Restore_Cursor_Position
     (View : access Source_View_Record'Class);
   --  Restore the stored cursor position.

   procedure Preferences_Changed
     (View : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Called when the preferences have changed, to refresh the editor
   --  appropriately.

   procedure File_Saved
     (Widget  : access Glib.Object.GObject_Record'Class;
      Args    : GValues;
      Kernel  : Kernel_Handle);
   --  Callback for "File_Saved_Signal".

   procedure Size_Allocated (View : access Gtk_Widget_Record'Class);
   --  Called when a new size has been allocated

   function Cursor_Is_On_Screen (View : access Source_View_Record'Class)
      return Boolean;
   --  Return True if the cursor is currently visible on screen

   function Get_Line_Height
     (View : access Gtk_Text_View_Record'Class;
      Line : Gtk_Text_Iter) return Gint;
   pragma Inline (Get_Line_Height);
   --  Return the height of the line in the window.

   procedure Remove_Synchronization
     (View : access Source_View_Record'Class);
   --  Remove the synchronized scrolling loop related to this editor.

   procedure On_Scroll (View : access Gtk_Widget_Record'Class);
   --  Callback when the adjustments have changed.

   function Scroll_Timeout (View : Source_View) return Boolean;
   --  Scroll to View.Scroll_To_Value;

   function Speed_Bar_Expose_Event_Cb
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean;
   --  Callback for an "expose" event on the speed bar.

   procedure Speed_Bar_Size_Allocate_Cb
     (Widget : access Gtk_Widget_Record'Class);
   --  Callback for an "size_allocate" signal on the speed bar.

   --------------------
   -- Scroll_Timeout --
   --------------------

   function Scroll_Timeout (View : Source_View) return Boolean is
   begin
      Set_Value (Get_Vadjustment (View.Scroll), View.Scroll_To_Value);
      View.Scroll_Requested := False;
      return False;
   end Scroll_Timeout;

   -------------------------
   -- Cursor_Is_On_Screen --
   -------------------------

   function Cursor_Is_On_Screen (View : access Source_View_Record'Class)
      return Boolean
   is
      Y, Height   : Gint;
      Rect        : Gdk_Rectangle;
      Insert_Mark : constant Gtk_Text_Mark := View.Saved_Cursor_Mark;
      Iter        : Gtk_Text_Iter;
   begin
      Get_Iter_At_Mark (Get_Buffer (Source_View (View)), Iter, Insert_Mark);
      Get_Line_Yrange (Source_View (View), Iter, Y, Height);
      Get_Visible_Rect (Source_View (View), Rect);

      return Rect.Y <= Y and then Y + Height <= Rect.Y + Rect.Height;
   end Cursor_Is_On_Screen;

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
      Buffer : constant Source_Buffer := Source_Buffer (Get_Buffer (View));
      Insert_Iter : Gtk_Text_Iter;
   begin
      Get_Iter_At_Mark (Buffer, Insert_Iter, Get_Insert (Buffer));
      Move_Mark (Buffer, View.Saved_Cursor_Mark, Insert_Iter);
   end Save_Cursor_Position;

   -----------------------------
   -- Restore_Cursor_Position --
   -----------------------------

   procedure Restore_Cursor_Position
     (View : access Source_View_Record'Class)
   is
      Insert_Iter : Gtk_Text_Iter;
      Buffer : constant Source_Buffer := Source_Buffer (Get_Buffer (View));
   begin
      Get_Iter_At_Mark (Buffer, Insert_Iter, View.Saved_Cursor_Mark);

      --  ??? Do we want to save/restore the selection bound as well ?
      Place_Cursor (Buffer, Insert_Iter);
   end Restore_Cursor_Position;

   ------------
   -- Delete --
   ------------

   procedure Delete (View : access Source_View_Record) is
   begin
      View.Area := null;

      Unref (View.Side_Column_GC);
      Unref (View.Side_Background_GC);
      Unref (View.Default_GC);
      Unref (View.Current_Line_GC);
      Unref (View.Current_Block_GC);

      if View.Side_Column_Buffer /= null then
         Gdk.Pixmap.Unref (View.Side_Column_Buffer);
         View.Side_Column_Buffer := null;
      end if;

      if View.Speed_Column_Buffer /= null then
         Gdk.Pixmap.Unref (View.Speed_Column_Buffer);
         View.Speed_Column_Buffer := null;
      end if;

      Delete_Mark (Get_Buffer (View), View.Saved_Cursor_Mark);

      if View.Connect_Expose_Registered then
         Idle_Remove (View.Connect_Expose_Id);
      end if;

      if View.Idle_Redraw_Registered then
         Idle_Remove (View.Idle_Redraw_Id);
      end if;

      --  Mark the idle loops as registered, so that they can no longer be
      --  registered once the view has been destroyed.

      View.Idle_Redraw_Registered := True;
      View.Connect_Expose_Registered := True;

      Remove_Synchronization (View);

      if View.Scroll_Requested then
         Timeout_Remove (View.Scroll_Timeout);
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
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
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
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
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
      Set_Font (View, View.Pango_Font);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Realize_Cb;

   -----------------------
   -- Invalidate_Window --
   -----------------------

   procedure Invalidate_Window (User : Source_View) is
      Win           : constant Gdk.Window.Gdk_Window :=
        Get_Window (User, Text_Window_Text);
      X, Y, W, H, D : Gint;

   begin
      if Win = null then
         return;
      end if;

      Get_Geometry (Win, X, Y, W, H, D);
      Gdk.Window.Invalidate_Rect (Win, (X, Y, W, H), True);
   end Invalidate_Window;

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
      if User.Speed_Column_Buffer /= null then
         Gdk.Pixmap.Unref (User.Speed_Column_Buffer);
         User.Speed_Column_Buffer := null;
      end if;

      Invalidate_Window (User);

      if Realized_Is_Set (User)
        and then not User.Idle_Redraw_Registered
      then
         User.Idle_Redraw_Registered := True;
         User.Idle_Redraw_Id := Source_View_Idle.Add
           (Idle_Column_Redraw'Access, User);
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
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
      User.Highlight_Blocks := Has_Block_Information (Buffer);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
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
      --  Clear the side columns cache.

      User.Buffer_Top_Line := 0;

      if User.Side_Column_Buffer /= null then
         Gdk.Pixmap.Unref (User.Side_Column_Buffer);
         User.Side_Column_Buffer := null;
      end if;

      if Realized_Is_Set (User)
        and then not User.Idle_Redraw_Registered
      then
         User.Idle_Redraw_Registered := True;
         User.Idle_Redraw_Id := Source_View_Idle.Add
           (Idle_Column_Redraw'Access, User);
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
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

      if User.Side_Column_Buffer /= null then
         Gdk.Pixmap.Unref (User.Side_Column_Buffer);
         User.Side_Column_Buffer := null;
      end if;

      User.Buffer_Top_Line := 0;

      if not User.Idle_Redraw_Registered then
         User.Idle_Redraw_Registered := True;
         User.Idle_Redraw_Id := Source_View_Idle.Add
           (Idle_Column_Redraw'Access, User);
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Side_Columns_Config_Change_Handler;

   ------------------------
   -- Idle_Column_Redraw --
   ------------------------

   function Idle_Column_Redraw (View : Source_View) return Boolean is
   begin
      if View.Side_Column_Buffer /= null then
         Gdk.Pixmap.Unref (View.Side_Column_Buffer);
         View.Side_Column_Buffer := null;
      end if;

      if View.Speed_Column_Buffer /= null then
         Gdk.Pixmap.Unref (View.Speed_Column_Buffer);
         View.Speed_Column_Buffer := null;
      end if;

      if Realized_Is_Set (View) then
         Redraw_Columns (View);
         Redraw_Speed_Column (View);
      end if;

      View.Idle_Redraw_Registered := False;
      return False;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
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
      if User.Has_Focus then
         Save_Cursor_Position (User);
         Scroll_To_Cursor_Location (User);
      end if;

      --  If we are doing block highlighting, re-expose the entire view if the
      --  current block has changed.

      if User.Highlight_Blocks
        and then User.Current_Block /=
          Get_Block (Buffer, Buffer_Line_Type (Line), False)
      then
         Invalidate_Window (User);
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Change_Handler;

   --------------------
   -- Size_Allocated --
   --------------------

   procedure Size_Allocated (View : access Gtk_Widget_Record'Class) is
   begin
      --  Keep the cursor on screen when the editor is resized.

      if not Cursor_Is_On_Screen (Source_View (View)) then
         Scroll_To_Cursor_Location (Source_View (View), Center => True);
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Size_Allocated;

   ---------------------
   -- Get_Line_Height --
   ---------------------

   function Get_Line_Height
     (View : access Gtk_Text_View_Record'Class;
      Line : Gtk_Text_Iter) return Gint
   is
      Rect : Gdk_Rectangle;
   begin
      Get_Iter_Location (View, Line, Rect);
      return Rect.Height;
   end Get_Line_Height;

   --------------------------------
   -- Speed_Bar_Size_Allocate_Cb --
   --------------------------------

   procedure Speed_Bar_Size_Allocate_Cb
     (Widget : access Gtk_Widget_Record'Class)
   is
      View   : constant Source_View := Source_View (Widget);
   begin
      if View.Speed_Column_Buffer /= null then
         Gdk.Pixmap.Unref (View.Speed_Column_Buffer);
         View.Speed_Column_Buffer := null;
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Speed_Bar_Size_Allocate_Cb;

   -------------------------------
   -- Speed_Bar_Expose_Event_Cb --
   -------------------------------

   function Speed_Bar_Expose_Event_Cb
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean
   is
      View   : constant Source_View := Source_View (Widget);
      pragma Unreferenced (Event);

   begin
      Redraw_Speed_Column (View);

      return False;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
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

   begin
      --  If the event applies to the left border window, then redraw
      --  the side window information.

      if Window_Type = Text_Window_Left then
         declare
            Top_In_Buffer              : Gint;
            Bottom_In_Buffer           : Gint;
            Dummy_Gint                 : Gint;
            Iter                       : Gtk_Text_Iter;
            Top_Line                   : Buffer_Line_Type;
            Bottom_Line                : Buffer_Line_Type;
            Scroll                     : Boolean := False;

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

            if View.Top_Line /= Top_Line then
               Scroll := True;
            end if;

            View.Top_Line    := Top_Line;
            View.Bottom_Line := Bottom_Line;

            --  Compute the smallest connected area that needs refresh.

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
            --  If necessary, emit the Source_Lines_Revealed signal.

            if Bottom_Line >= Top_Line then
               Source_Lines_Revealed (Buffer, Top_Line, Bottom_Line);
            end if;

            if Scroll then
               On_Scroll (View);
            end if;
         end;

         Redraw_Columns (View);

      elsif Window_Type = Text_Window_Text then
         declare
            Column        : constant Gint :=
              Get_Pref (View.Kernel, Highlight_Column);
            Rect          : Gdk_Rectangle;
            Line_Y        : Gint;
            Line_Height   : Gint;
            Cursor_Iter   : Gtk_Text_Iter;
            Dummy         : Gint := 0;
            Buffer_Line_Y : Gint;

            Dummy_Gint                 : Gint;
            Success : Boolean;
            Iter                       : Gtk_Text_Iter;
            Top_Line                   : Buffer_Line_Type;
            Bottom_Line                : Buffer_Line_Type;
            Top_In_Buffer    : Gint;
            Bottom_In_Buffer : Gint;
            GC               : Gdk.GC.Gdk_GC;
            Buffer           : constant Source_Buffer :=
              Source_Buffer (Get_Buffer (View));

            procedure Draw_Block (B : in out Block_Record);
            --  Draw block B at line L.

            ----------------
            -- Draw_Block --
            ----------------

            procedure Draw_Block (B : in out Block_Record) is
               Block_Begin_Y : Gint;
               Y             : Gint;
               Height        : Gint;
               X             : Gint;

               Bracket_Length : constant := 15;
               --  The length of upper and lower parts of the bracket.

               Bracket_Offset : constant := 2;
               --  The distance between brackets and text.

               First         : constant Gint :=
                 Gint (Get_Buffer_Line (Buffer, B.First_Line) - 1);
               Last          : Gint :=
                 Gint (Get_Buffer_Line (Buffer, B.Last_Line) - 1);

               Offset        : Integer;
            begin
               Calculate_Screen_Offset (Buffer, B);
               Offset := B.Stored_Offset;

               --  Do not draw blocks that are on the first column.

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

               X := Gint (Offset - 1) * View.Char_Width
                 - Bracket_Offset - Rect.X + Margin;

               Draw_Line (Window, View.Current_Block_GC, X, Y, X, Y + Height);
               Draw_Line
                 (Window, View.Current_Block_GC, X, Y, X + Bracket_Length, Y);
               Draw_Line
                 (Window,
                  View.Current_Block_GC,
                  X, Y + Height, X + Bracket_Length, Y + Height);
            end Draw_Block;

         begin
            Get_Visible_Rect (View, Rect);

            Buffer_To_Window_Coords
              (View, Text_Window_Text, Rect.X, Rect.Y, X, Y);

            --  Get the window coordinates.

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
               GC := Get_Highlight_GC (Buffer, Line);

               if GC /= null then
                  Get_Line_Yrange (View, Iter, Line_Y, Line_Height);
                  Buffer_To_Window_Coords
                    (View, Text_Window_Text,
                     Dummy, Line_Y, Dummy, Buffer_Line_Y);

                  Draw_Rectangle
                    (Window,
                     GC,
                     True,
                     Margin, Buffer_Line_Y,
                     Rect.Width, Line_Height);
               end if;

               Forward_Line (Iter, Success);
            end loop;

            Get_Iter_At_Mark
              (Get_Buffer (View), Cursor_Iter, View.Saved_Cursor_Mark);

            Get_Line_Yrange (View, Cursor_Iter, Line_Y, Line_Height);

            --  Highlight the line that contains the cursor.

            if View.Highlight_Current then
               Buffer_To_Window_Coords
                 (View, Text_Window_Text, Dummy, Line_Y, Dummy, Buffer_Line_Y);

               Line_Height := Get_Line_Height (View, Cursor_Iter);

               Draw_Rectangle
                 (Window, View.Current_Line_GC, True, Margin, Buffer_Line_Y,
                  Rect.Width, Line_Height);
            end if;

            --  Highlight the current block.

            if View.Highlight_Blocks then
               View.Current_Block := Get_Block
                 (Buffer,
                  Buffer_Line_Type (Get_Line (Cursor_Iter)) + 1,
                  False);
               Draw_Block (View.Current_Block);
            end if;

            --  Redraw the line showing the nth column if needed

            if Column > 0 then
               X := Column * View.Char_Width - Rect.X + Margin;
               Draw_Line (Window, View.Default_GC, X, Y, X, Y + Rect.Height);
            end if;
         end;
      end if;

      --  Return false, so that the signal is not blocked, and other
      --  clients can use it.

      return False;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
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
   begin
      View.Has_Focus := False;
      Save_Cursor_Position (View);
      External_End_Action (Buffer);
      return False;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
         return False;
   end Focus_Out_Event_Cb;

   -----------------------
   -- Focus_In_Event_Cb --
   -----------------------

   function Focus_In_Event_Cb
     (Widget : access Gtk_Widget_Record'Class) return Boolean
   is
      View   : constant Source_View   := Source_View (Widget);
   begin
      if not Selection_Exists (Get_Buffer (View)) then
         Set_Disable_Scroll_On_Focus (View, True);
         Restore_Cursor_Position (View);
      end if;

      View.Has_Focus := True;

      return False;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
         return False;
   end Focus_In_Event_Cb;

   ------------
   -- Map_Cb --
   ------------

   procedure Map_Cb (Widget : access Gtk_Widget_Record'Class) is
      Color   : Gdk_Color;
      Success : Boolean;
      View    : constant Source_View := Source_View (Widget);
   begin
      --  ??? Here we are creating GCs and allocating colors for every instance
      --  of a text view, maybe this could be shared somewhere.

      --  Now that the Source_View is mapped, we can create the Graphic
      --  Context used for writing line numbers.
      Gdk_New
        (View.Side_Column_GC,
         Get_Window (View, Text_Window_Left));
      Gdk_New
        (View.Side_Background_GC,
         Get_Window (View, Text_Window_Left));

      Gdk_New
        (View.Current_Line_GC,
         Get_Window (View, Text_Window_Text));
      Color := Get_Pref (View.Kernel, Current_Line_Color);
      Set_Foreground (View.Current_Line_GC, Color);
      View.Highlight_Current :=
        not Equal (Color, White (Get_Default_Colormap));

      Gdk_New
        (View.Current_Block_GC,
         Get_Window (View, Text_Window_Text));
      Color := Get_Pref (View.Kernel, Current_Block_Color);
      Set_Foreground (View.Current_Block_GC, Color);
      View.Highlight_Blocks :=
        not Equal (Color, White (Get_Default_Colormap));

      Gdk_New
        (View.Default_GC,
         Get_Window (View, Text_Window_Text));
      Color := Parse ("#eeeeee");
      Alloc_Color (Get_Default_Colormap, Color, False, True, Success);

      if Success then
         Set_Foreground
           (View.Side_Background_GC, Color);
      else
         Set_Foreground
           (View.Side_Background_GC,
            White (Get_Default_Colormap));
      end if;

      Color := Parse ("#A0A0A0");
      Alloc_Color (Get_Default_Colormap, Color, False, True, Success);

      if Success then
         Set_Foreground (View.Default_GC, Color);
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Map_Cb;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (View   : out Source_View;
      Scroll : Gtk.Scrolled_Window.Gtk_Scrolled_Window;
      Area   : Gtk.Drawing_Area.Gtk_Drawing_Area;
      Buffer : Src_Editor_Buffer.Source_Buffer;
      Kernel : access Glide_Kernel.Kernel_Handle_Record'Class) is
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
      Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
   is
      Insert_Iter : Gtk_Text_Iter;

      use GVD;

   begin
      --  Initialize the Source_View. Some of the fields can not be initialized
      --  until the widget is realize or mapped. Their initialization is thus
      --  done at that point.

      pragma Assert (Buffer /= null);

      Gtk.Text_View.Initialize (View, Gtk_Text_Buffer (Buffer));

      View.Kernel := Kernel_Handle (Kernel);
      View.Scroll := Scroll;
      View.Area   := Area;

      Set_Events
        (Area,
         Button_Motion_Mask or Button_Press_Mask or Button_Release_Mask);

      Return_Callback.Object_Connect
        (Area, "button_press_event",
         Marsh       => Return_Callback.To_Marshaller
           (Speed_Bar_Button_Press_Event_Cb'Access),
         After       => False,
         Slot_Object => View);

      Return_Callback.Object_Connect
        (Area, "motion_notify_event",
         Marsh       => Return_Callback.To_Marshaller
           (Speed_Bar_Button_Press_Event_Cb'Access),
         After       => False,
         Slot_Object => View);

      Return_Callback.Object_Connect
        (Area, "button_release_event",
         Marsh       => Return_Callback.To_Marshaller
           (Speed_Bar_Button_Release_Event_Cb'Access),
         After       => False,
         Slot_Object => View);

      Return_Callback.Object_Connect
        (View.Area, "expose_event",
         Marsh       => Return_Callback.To_Marshaller
           (Speed_Bar_Expose_Event_Cb'Access),
         After       => False,
         Slot_Object => View);

      Widget_Callback.Object_Connect
        (View.Area, "size_allocate",
         Marsh       => Widget_Callback.To_Marshaller
           (Speed_Bar_Size_Allocate_Cb'Access),
         After       => False,
         Slot_Object => View);

      Set_Border_Window_Size (View, Enums.Text_Window_Left, 1);
      Set_Left_Margin (View, Margin);

      Preferences_Changed (View, Kernel_Handle (Kernel));

      Widget_Callback.Connect
        (View, "realize",
         Marsh => Widget_Callback.To_Marshaller (Realize_Cb'Access),
         After => True);
      Widget_Callback.Connect
        (View, "map",
         Marsh => Widget_Callback.To_Marshaller (Map_Cb'Access),
         After => True);
      Return_Callback.Connect
        (View, "focus_in_event",
         Marsh => Return_Callback.To_Marshaller (Focus_In_Event_Cb'Access),
         After => False);
      Return_Callback.Connect
        (View, "focus_out_event",
         Marsh => Return_Callback.To_Marshaller (Focus_Out_Event_Cb'Access),
         After => False);
      Return_Callback.Connect
        (View, "button_press_event",
         Marsh => Return_Callback.To_Marshaller (Button_Press_Event_Cb'Access),
         After => False);
      Return_Callback.Connect
        (View, "key_press_event",
         Marsh => Return_Callback.To_Marshaller (Key_Press_Event_Cb'Access),
         After => False);
      Widget_Callback.Connect
        (View, "size_allocate",
         Widget_Callback.To_Marshaller (Size_Allocated'Access),
         After => True);

      Source_Buffer_Callback.Connect
        (Buffer, "cursor_position_changed",
         Cb        => Change_Handler'Access,
         User_Data => Source_View (View),
         After     => True);

      Source_Buffer_Callback.Connect
        (Buffer, "side_column_changed",
         Cb        => Side_Columns_Change_Handler'Access,
         User_Data => Source_View (View),
         After     => True);

      Source_Buffer_Callback.Connect
        (Buffer, "side_column_configuration_changed",
         Cb        => Side_Columns_Config_Change_Handler'Access,
         User_Data => Source_View (View),
         After     => True);

      Source_Buffer_Callback.Connect
        (Buffer, "buffer_information_changed",
         Cb        => Buffer_Information_Change_Handler'Access,
         User_Data => Source_View (View),
         After     => True);

      Source_Buffer_Callback.Connect
        (Buffer, "line_highlights_changed",
         Cb        => Line_Highlight_Change_Handler'Access,
         User_Data => Source_View (View),
         After     => True);

      Gtkada.Handlers.Return_Callback.Object_Connect
        (View,
         "delete_event",
         Gtkada.Handlers.Return_Callback.To_Marshaller (On_Delete'Access),
         View,
         After => False);

      if Host = Windows then
         --  ??? Under Windows, a click does not refresh the entire area as it
         --  does under X. We need it to properly redraw current line and
         --  current block, therefore we do it manually.

         Gtkada.Handlers.Return_Callback.Object_Connect
           (View,
            "button_press_event",
            Gtkada.Handlers.Return_Callback.To_Marshaller
              (On_Button_Press'Access),
            View,
            After => False);
      end if;

      Kernel_Callback.Object_Connect
        (Kernel, Preferences_Changed_Signal,
         Kernel_Callback.To_Marshaller (Preferences_Changed'Access),
         Slot_Object => View,
         User_Data   => Kernel_Handle (Kernel));

      Kernel_Callback.Object_Connect
        (Kernel, File_Saved_Signal,
         File_Saved'Access,
         Slot_Object => View,
         User_Data   => Kernel_Handle (Kernel));

      --  Connect in an idle callback, otherwise the lines-with-code in the
      --  debugger are recomputed all at once (before the editor has a size).

      View.Connect_Expose_Registered := True;
      View.Connect_Expose_Id := Source_View_Idle.Add
        (Connect_Expose'Access,
         Source_View (View));

      Get_Iter_At_Mark (Buffer, Insert_Iter, Get_Insert (Buffer));
      View.Saved_Cursor_Mark := Create_Mark (Buffer, "", Insert_Iter);

      Invalidate_Window (Source_View (View));
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
        (View, "expose_event",
         Marsh => Return_Callback.To_Marshaller (Expose_Event_Cb'Access),
         After => False);

      if Win /= null then
         Get_Geometry (Win, X, Y, W, H, D);
         Clear_Area_E (Win, X, Y, W, H);
         Invalidate_Window (View);
      end if;

      View.Connect_Expose_Registered := False;

      return False;
   end Connect_Expose;

   -------------------------
   -- Preferences_Changed --
   -------------------------

   procedure Preferences_Changed
     (View : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      Source  : constant Source_View := Source_View (View);
      Descr   : Pango_Font_Description;
      Layout  : Pango_Layout;
      Color   : Gdk_Color;
      Height  : Gint;

   begin
      --  Set the font.

      Descr := Get_Pref (Kernel, Source_Editor_Font);
      Set_Font (Source, Descr);

      --  Recompute the width of one character.

      Layout := Create_Pango_Layout (Source);
      Set_Font_Description (Layout, Descr);
      Set_Text (Layout, "m");
      Get_Pixel_Size (Layout, Source.Char_Width, Height);
      Unref (Layout);

      --  Reset the color of the current line.
      --  This procedure might be called before the Map_Cb has been called,
      --  and therefore the Current_Line_GC might not be initialized at this
      --  point.

      if Source.Current_Line_GC /= null then
         Color := Get_Pref (Source.Kernel, Current_Line_Color);
         Set_Foreground (Source.Current_Line_GC, Color);
         Source.Highlight_Current :=
           not Equal (Color, White (Get_Default_Colormap));

         Color := Get_Pref (Source.Kernel, Current_Block_Color);
         Set_Foreground (Source.Current_Block_GC, Color);
         Source.Highlight_Blocks := Get_Pref (Kernel, Block_Highlighting);
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Preferences_Changed;

   ----------------
   -- File_Saved --
   ----------------

   procedure File_Saved
     (Widget  : access Glib.Object.GObject_Record'Class;
      Args    : GValues;
      Kernel  : Kernel_Handle)
   is
      pragma Unreferenced (Kernel);
      View : constant Source_View := Source_View (Widget);
      File : constant Virtual_File :=
        Create (Full_Filename => Get_String (Nth (Args, 1)));

   begin
      if Get_Filename (Source_Buffer (Get_Buffer (View))) = File then
         Redraw_Columns (View);
      end if;
   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end File_Saved;

   --------------
   -- Set_Font --
   --------------

   procedure Set_Font
     (View : access Source_View_Record'Class;
      Font : Pango.Font.Pango_Font_Description) is
   begin
      View.Pango_Font := Font;

      --  Make sure the widget is already realized. Otherwise, the
      --  layout and style are not created yet.
      if Realized_Is_Set (View) then
         Modify_Font (View, Font);
      end if;

      --  ??? Should recompute the width of the column on the side.
   end Set_Font;

   -------------------------------
   -- Scroll_To_Cursor_Location --
   -------------------------------

   procedure Scroll_To_Cursor_Location
     (View   : access Source_View_Record;
      Center : Boolean := False)
   is
      Insert_Mark : constant Gtk_Text_Mark := Get_Insert (Get_Buffer (View));
   begin
      --  Save the cursor location.
      Save_Cursor_Position (View);

      --  We want to use the alignments, so that the line appears in the middle
      --  of the screen if possible. This provides a more user-friendly
      --  behavior.

      Scroll_To_Mark
        (View, Insert_Mark, Use_Align => Center,
         Within_Margin => 0.0, Xalign => 0.5, Yalign => 0.5);
   end Scroll_To_Cursor_Location;

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

      Out_Of_Bounds := False;

      if Buffer_X > Iter_Location.X then
         Buffer_X := Iter_Location.X;
         Out_Of_Bounds := True;
      end if;

      if Buffer_Y > Iter_Location.Y + Line_Height then
         Buffer_Y := Iter_Location.Y + Line_Height;
         Out_Of_Bounds := True;
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
      View       : constant Source_View := Source_View (Widget);
      Dummy_Gint : Gint;
      W, H, D    : Gint;
      Button_Y   : Gint;
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
            View.Scroll_Timeout := Source_View_Timeout.Add
              (10, Scroll_Timeout'Access, View);
         end if;
      end if;

      return False;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
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
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
         return False;
   end Speed_Bar_Button_Release_Event_Cb;

   ---------------------------
   -- Button_Press_Event_Cb --
   ---------------------------

   function Button_Press_Event_Cb
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean
   is
      View   : constant Source_View := Source_View (Widget);
      Buffer : constant Source_Buffer := Source_Buffer (Get_Buffer (View));

      Left_Window  : constant Gdk.Window.Gdk_Window :=
        Get_Window (View, Text_Window_Left);

      Window : Gdk.Window.Gdk_Window;

      Result : Boolean;
      pragma Unreferenced (Result);
   begin
      External_End_Action (Buffer);

      Window := Get_Window (Event);

      if Window = Left_Window
        and then Get_Event_Type (Event) = Button_Press
        and then Get_Button (Event) = 1
      then
         declare
            Dummy_Gint         : Gint;
            Iter               : Gtk_Text_Iter;
            Line               : Buffer_Line_Type;
            Button_X, Button_Y : Gint;
            X, Y               : Gint;

         begin
            --  Get the coordinates of the click.

            Button_X := Gint (Get_X (Event));
            Button_Y := Gint (Get_Y (Event));

            --  Find the line number.
            Window_To_Buffer_Coords
              (View, Text_Window_Left,
               Window_X => Button_X, Window_Y => Button_Y,
               Buffer_X => X, Buffer_Y => Y);

            Get_Line_At_Y (View, Iter, Y, Dummy_Gint);
            Line := Buffer_Line_Type (Get_Line (Iter) + 1);

            View.Has_Focus := True;
            Set_Focus_Child (Get_MDI (View.Kernel), View);
            On_Click (Buffer, Line, Button_X);
         end;

      elsif Window /= Left_Window
        and then Get_Event_Type (Event) = Gdk_2button_Press
      then
         --  ??? This is a tweak necessary to implement the feature
         --  "select an entire word containing '_' when double-clicking".
         --  Might be worth investigating whether it could be implemented at
         --  the gtk+ level (the proper fix would be to change the Pango
         --  word break algorithms probably in break.c ?) and to redefine
         --  the "is_word_break" behaviour of the underscore.

         --  Here we send a button_release_event before selecting the word, so
         --  that we can stop propagating the "button_press_event" without
         --  entering the selection-drag mode.

         Result := Return_Callback.Emit_By_Name
           (Widget, "button_release_event", Event);

         Select_Current_Word (Buffer);
         return True;
      end if;

      return False;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
         return False;
   end Button_Press_Event_Cb;

   ------------------------
   -- Key_Press_Event_Cb --
   ------------------------

   function Key_Press_Event_Cb
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean
   is
      View   : constant Source_View   := Source_View (Widget);
      Buffer : constant Source_Buffer := Source_Buffer (Get_Buffer (View));
      Start, Last : Gtk_Text_Iter;
      Result : Boolean;

   begin
      if not Get_Editable (View) then
         return False;
      end if;

      case Get_Key_Val (Event) is
         when GDK_Return =>
            External_End_Action (Buffer);

            if Should_Indent (Buffer) then
               Insert_At_Cursor (Buffer, (1 => ASCII.LF));

               --  ??? Could be a key handler as well
               Get_Iter_At_Mark (Buffer, Last, Get_Insert (Buffer));
               Copy (Last, Dest => Start);
               Backward_Line (Start, Result);

               if not Ends_Line (Last) then
                  Forward_To_Line_End (Last, Result);
               end if;

               Result := Do_Indentation (Buffer, Start, Last);
               return True;
            end if;

         when GDK_Linefeed | GDK_Tab |
           GDK_Home | GDK_Page_Up | GDK_Page_Down | GDK_End | GDK_Begin |
           GDK_Up | GDK_Down | GDK_Left | GDK_Right
         =>
            External_End_Action (Buffer);

         when others =>
            null;
      end case;

      return False;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
         return False;
   end Key_Press_Event_Cb;

   --------------------
   -- Redraw_Columns --
   --------------------

   procedure Redraw_Columns (View : access Source_View_Record'Class) is
      Left_Window : Gdk.Window.Gdk_Window;

      X, Y, Width, Height, Depth : Gint;
      Layout                     : Pango_Layout;

      Src_Buffer : constant Source_Buffer := Source_Buffer (Get_Buffer (View));

      Total_Width : Gint;

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
         if View.Side_Column_Buffer /= null then
            Gdk.Pixmap.Unref (View.Side_Column_Buffer);
            View.Side_Column_Buffer := null;
         end if;
      end if;

      --  Create the graphical elements

      Left_Window := Get_Window (View, Text_Window_Left);

      if View.Side_Column_Buffer /= null
        and then View.Top_Line = View.Buffer_Top_Line
        and then View.Bottom_Line = View.Buffer_Bottom_Line
      then
         --  If the cache corresponds to the lines, redraw it.

         Draw_Pixmap
           (Drawable => Left_Window,
            Src      => View.Side_Column_Buffer,
            Gc       => View.Side_Column_GC,
            Xsrc     => 0,
            Ysrc     => 0,
            Xdest    => 0,
            Ydest    => 0);

      else
         --  The lines have changed or the cache is not created: create it.

         if View.Side_Column_Buffer /= null then
            Gdk.Pixmap.Unref (View.Side_Column_Buffer);
            View.Side_Column_Buffer := null;
         end if;

         View.Buffer_Top_Line    := View.Top_Line;
         View.Buffer_Bottom_Line := View.Bottom_Line;

         Layout := Create_Pango_Layout (View);
         Set_Font_Description (Layout, View.Pango_Font);

         Get_Geometry (Left_Window, X, Y, Width, Height, Depth);

         Gdk_New (View.Side_Column_Buffer, Left_Window, Total_Width, Height);
         Draw_Rectangle
           (View.Side_Column_Buffer,
            View.Side_Background_GC,
            True, X, Y, Total_Width, Height);

         Draw_Line_Info
           (Src_Buffer, View.Top_Line, View.Bottom_Line,
            Gtk_Text_View (View), View.Side_Column_GC,
            Layout, View.Side_Column_Buffer);

         Draw_Pixmap
           (Drawable => Left_Window,
            Src      => View.Side_Column_Buffer,
            Gc       => View.Side_Column_GC,
            Xsrc     => 0,
            Ysrc     => 0,
            Xdest    => 0,
            Ydest    => 0);

         Unref (Layout);
      end if;
   end Redraw_Columns;

   -------------------------
   -- Redraw_Speed_Column --
   -------------------------

   procedure Redraw_Speed_Column (View : access Source_View_Record'Class) is
      Right_Window : Gdk.Window.Gdk_Window;

      X, Y, Width, Height, Depth : Gint;
      GC                         : Gdk.GC.Gdk_GC;

      Src_Buffer : constant Source_Buffer := Source_Buffer (Get_Buffer (View));

      Line_Height : Gint;
      Total_Lines : Gint;

      Info_Exists : Boolean := False;

   begin
      if View.Area = null then
         return;
      end if;

      Right_Window := Get_Window (View.Area);

      Get_Geometry (Right_Window, X, Y, Width, Height, Depth);
      Total_Lines := Get_Line_Count (Src_Buffer);

      if View.Speed_Column_Buffer = null then
         Gdk_New
           (View.Speed_Column_Buffer,
            Right_Window,
            Speed_Column_Width,
            Height);

         Draw_Rectangle
           (View.Speed_Column_Buffer,
            View.Side_Background_GC,
            True, X, Y, Speed_Column_Width, Height);

         Line_Height := Height / Total_Lines + 1;

         --  Make the line height at least 2 pixels high.

         if Line_Height <= 1 then
            Line_Height := 2;
         end if;

         Info_Exists := False;

         for J in 1 .. Total_Lines loop
            GC := Get_Highlight_GC (Src_Buffer, Buffer_Line_Type (J));

            if GC /= null then
               Draw_Rectangle
                 (View.Speed_Column_Buffer,
                  GC,
                  True,
                  0, (Height * Gint (J)) / Total_Lines,
                  Speed_Column_Width, Line_Height);

               Info_Exists := True;
            end if;
         end loop;

         if Info_Exists then
            Show_All (View.Area);
         else
            Hide_All (View.Area);
            return;
         end if;
      end if;

      Draw_Pixmap
        (Drawable => Right_Window,
         Src      => View.Speed_Column_Buffer,
         Gc       => View.Side_Column_GC,
         Xsrc     => 0,
         Ysrc     => 0,
         Xdest    => 0,
         Ydest    => 0);

      Draw_Rectangle
        (Drawable => Right_Window,
         GC       => View.Current_Block_GC,
         Filled   => False,
         X        => 0,
         Y        => (Height * Gint (View.Top_Line)) / Total_Lines,
         Width    => Speed_Column_Width - 1,
         Height   =>
           (Gint (View.Bottom_Line - View.Top_Line) * Height)
         / Total_Lines);
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
      Redraw_Speed_Column (Src_View);

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
      end if;

      Src_View.Scrolling := False;
   end On_Scroll;

end Src_Editor_View;
