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
with Gdk.Pixbuf;                  use Gdk.Pixbuf;
with Gdk.Pixmap;                  use Gdk.Pixmap;
with Gdk.Rectangle;               use Gdk.Rectangle;
with Gdk.Types;                   use Gdk.Types;
with Gdk.Types.Keysyms;           use Gdk.Types.Keysyms;
with Gtk;                         use Gtk;
with Gtk.Enums;                   use Gtk.Enums;
with Gtk.Handlers;
with Gtk.Main;                    use Gtk.Main;
with Gtk.Text_Buffer;             use Gtk.Text_Buffer;
with Gtk.Text_Iter;               use Gtk.Text_Iter;
with Gtk.Text_Mark;               use Gtk.Text_Mark;
with Gtk.Text_View;               use Gtk.Text_View;
with Gtk.Widget;                  use Gtk.Widget;
with Gtkada.Handlers;             use Gtkada.Handlers;
with Gtkada.Types;                use Gtkada.Types;
with Src_Editor_Buffer;           use Src_Editor_Buffer;
with Pango.Font;                  use Pango.Font;
with Pango.Layout;                use Pango.Layout;

with Commands.Editor;             use Commands.Editor;
with Language;                    use Language;
with Interfaces.C.Strings;        use Interfaces.C.Strings;
with Ada.Exceptions;              use Ada.Exceptions;
with Traces;                      use Traces;
with Basic_Types;                 use Basic_Types;
with Commands;                    use Commands;
with Glide_Kernel;                use Glide_Kernel;
with Glide_Kernel.Preferences;    use Glide_Kernel.Preferences;

with Basic_Types;    use Basic_Types;

package body Src_Editor_View is

   Me : constant Debug_Handle := Create ("Source_View");

   function Strlen
     (Str : Interfaces.C.Strings.chars_ptr) return Interfaces.C.size_t;
   pragma Import (C, Strlen);

   use type Pango.Font.Pango_Font_Description;
   procedure Setup (Data : Source_View; Id : Gtk.Handlers.Handler_Id);
   package Source_Buffer_Callback is new Gtk.Handlers.User_Callback_With_Setup
     (Widget_Type => Source_Buffer_Record,
      User_Type   => Source_View,
      Setup       => Setup);

   package Source_View_Idle is new Gtk.Main.Idle (Source_View);

   --------------------------
   -- Forward declarations --
   --------------------------

   function Connect_Expose (View : Source_View) return Boolean;
   --  Connect Expose_Event_Cb to the expose event. Emit an expose event.

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

   procedure Side_Columns_Change_Handler
     (Buffer : access Source_Buffer_Record'Class;
      Params : Glib.Values.GValues;
      User   : Source_View);
   --  Callback for the "side_columns_changed" signal.

   procedure Line_Highlight_Change_Handler
     (Buffer : access Source_Buffer_Record'Class;
      Params : Glib.Values.GValues;
      User   : Source_View);
   --  Callback for the "line_highlight_change" signal.

   procedure Redraw_Columns (View : access Source_View_Record'Class);
   --  Redraw the left and right areas around View.

   procedure Set_Font
     (View : access Source_View_Record'Class;
      Font : Pango.Font.Pango_Font_Description);
   --  Change the font used in the given Source_View. Note that this service
   --  should not be used if the widget is not realized.

   function On_Delete
     (View  : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean;
   --  Callback for the "delete_event" signal.

   procedure Save_Cursor_Position
     (View : access Source_View_Record'Class);
   --  Save the cursor position.

   procedure Restore_Cursor_Position
     (View : access Source_View_Record'Class);
   --  Restore the stored cursor position.

   function Do_Indentation
     (Buffer   : Source_Buffer;
      Lang     : Language_Access;
      New_Line : Boolean := True) return Boolean;
   --  If supported by the language and if the preferences are activated,
   --  automatically indent at the current cursor position.
   --  If New_Line is True, insert a new line and insert spaces on the new line

   procedure Preferences_Changed
     (View : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Called when the preferences have changed, to refresh the editor
   --  appropriately.

   procedure File_Saved
     (Widget  : access Glib.Object.GObject_Record'Class;
      Args    : GValues;
      Kernel  : Kernel_Handle);
   --  Callback for "File_Saved_Signal".

   function Get_Side_Info
     (View   : access Source_View_Record'Class;
      Line   : Positive;
      Column : Positive) return Line_Info_Width;
   --  Return the side information corresponding to Line, Column in the
   --  Side window.
   --  Return (null, 0)  if the information was never set.
   --  Return (null, -1) if the information was set to null.

   procedure Size_Allocated (View : access Gtk_Widget_Record'Class);
   --  Called when a new size has been allocated

   function Cursor_Is_On_Screen (View : access Source_View_Record'Class)
      return Boolean;
   --  Return True if the cursor is currently visible on screen

   -------------------------
   -- Cursor_Is_On_Screen --
   -------------------------

   function Cursor_Is_On_Screen (View : access Source_View_Record'Class)
      return Boolean
   is
      Y, Height   : Gint;
      Rect        : Gdk_Rectangle;
      Insert_Mark : constant Gtk_Text_Mark := Get_Insert
        (Get_Buffer (Source_View (View)));
      Iter        : Gtk_Text_Iter;
   begin
      Get_Iter_At_Mark (Get_Buffer (Source_View (View)), Iter, Insert_Mark);
      Get_Line_Yrange (Source_View (View), Iter, Y, Height);
      Get_Visible_Rect (Source_View (View), Rect);

      return Rect.Y <= Y and then Y + Height <= Rect.Y + Rect.Height;
   end Cursor_Is_On_Screen;

   -------------------
   -- Get_Side_Info --
   -------------------

   function Get_Side_Info
     (View   : access Source_View_Record'Class;
      Line   : Positive;
      Column : Positive) return Line_Info_Width
   is
      Line_Info  : constant Line_Info_Display_Array_Access :=
        Get_Line_Info (Source_Buffer (Get_Buffer (View)));
      Real_Lines : constant Natural_Array_Access :=
        Get_Real_Lines (Source_Buffer (Get_Buffer (View)));
   begin
      if Line_Info (Column).Stick_To_Data then
         if Line > Real_Lines'Last
           or else Real_Lines (Line) not in
             Line_Info (Column).Column_Info'Range
         then
            return (null, -1);
         else
            return Line_Info (Column).Column_Info
              (Real_Lines (Line));
         end if;
      else
         if Line > Line_Info (Column).Column_Info'Last then
            return (null, 0);
         else
            return Line_Info (Column).Column_Info (Line);
         end if;
      end if;
   end Get_Side_Info;

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
      Unref (View.Side_Column_GC);
      Unref (View.Side_Background_GC);
      Unref (View.Default_GC);
      Unref (View.Current_Line_GC);
      Unref (View.Current_Block_GC);

      Delete_Mark (Get_Buffer (View), View.Saved_Cursor_Mark);

      if View.Connect_Expose_Id /= 0 then
         Idle_Remove (View.Connect_Expose_Id);
      end if;
   end Delete;

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

   -----------------------------------
   -- Line_Highlight_Change_Handler --
   -----------------------------------

   procedure Line_Highlight_Change_Handler
     (Buffer : access Source_Buffer_Record'Class;
      Params : Glib.Values.GValues;
      User   : Source_View)
   is
      pragma Unreferenced (Params, Buffer);

      Win           : constant Gdk.Window.Gdk_Window :=
        Get_Window (User, Text_Window_Text);
      X, Y, W, H, D : Gint;
      Rect          : Gdk_Rectangle;
   begin
      if Win = null then
         return;
      end if;

      Get_Geometry (Win, X, Y, W, H, D);
      Rect := Gdk_Rectangle'(X, Y, W, H);
      Gdk.Window.Invalidate_Rect (Win, Rect, True);
   end Line_Highlight_Change_Handler;

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
      if Realized_Is_Set (User) then
         Redraw_Columns (User);
      end if;
   end Side_Columns_Change_Handler;

   --------------------
   -- Change_Handler --
   --------------------

   procedure Change_Handler
     (Buffer : access Source_Buffer_Record'Class;
      Params : Glib.Values.GValues;
      User   : Source_View) is
   begin
      if User.Has_Focus then
         Save_Cursor_Position (User);
         Scroll_To_Cursor_Location (User);
      end if;

      --  If we are doing block highlighting, re-expose the entire view.

      if User.Highlight_Blocks then
         Line_Highlight_Change_Handler (Buffer, Params, User);
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
   end Size_Allocated;

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
      Line_Info   : constant Line_Info_Display_Array_Access :=
        Get_Line_Info (Buffer);
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
            Top_Line                   : Natural;
            Bottom_Line                : Natural;
            Info                       : Line_Info_Width;
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
            Top_Line := Natural (Get_Line (Iter) + 1);

            Get_Line_At_Y (View, Iter, Bottom_In_Buffer, Dummy_Gint);
            Bottom_Line := Natural (Get_Line (Iter) + 1);

            --  If one of the values hadn't been initialized, display the
            --  whole range of lines.

            View.Top_Line    := Top_Line;
            View.Bottom_Line := Bottom_Line;

            --  Compute the smallest connected area that needs refresh.

            Find_Top_Line :
            while Top_Line <= Bottom_Line loop
               for J in Line_Info'Range loop
                  if Line_Info (J).Every_Line then
                     Info := Get_Side_Info (View, Top_Line, J);

                     if Info.Width = 0 then
                        exit Find_Top_Line;
                     end if;
                  end if;
               end loop;

               Top_Line := Top_Line + 1;
            end loop Find_Top_Line;

            Find_Bottom_Line :
            while Bottom_Line >= Top_Line loop
               for J in Line_Info'Range loop
                  if Line_Info (J).Every_Line then
                     Info := Get_Side_Info (View, Bottom_Line, J);

                     if Info.Width = 0 then
                        exit Find_Bottom_Line;
                     end if;
                  end if;
               end loop;

               Bottom_Line := Bottom_Line - 1;
            end loop Find_Bottom_Line;

            --  If necessary, emit the Source_Lines_Revealed signal.

            if Bottom_Line >= Top_Line then
               Source_Lines_Revealed (Buffer, Top_Line, Bottom_Line);
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
            Top_Line                   : Natural;
            Bottom_Line                : Natural;
            Top_In_Buffer    : Gint;
            Bottom_In_Buffer : Gint;
            GC               : Gdk.GC.Gdk_GC;
            Buffer           : constant Source_Buffer :=
              Source_Buffer (Get_Buffer (View));
            Block_Start      : Natural;
            Block            : Block_Record;
            Block_Stack      : Integer;

            procedure Draw_Block (L : Integer; B : Block_Record);
            --  Draw block B at line L.

            procedure Draw_Block (L : Integer; B : Block_Record) is
               Block_Begin_Y : Gint;
               Y             : Gint;
               Height        : Gint;
               X             : Gint;

            begin
               Get_Iter_At_Line_Offset
                 (Buffer, Iter, Gint (L - 1), 0);
               Get_Line_Yrange  (View, Iter, Block_Begin_Y, Dummy);

               Get_Iter_At_Line_Offset
                 (Buffer, Iter, Gint (B.Other_Line - 1), 0);
               Get_Line_Yrange (View, Iter, Dummy, Height);

               Height := Dummy + Height - Block_Begin_Y;

               Buffer_To_Window_Coords
                 (View,
                  Text_Window_Text, Dummy, Block_Begin_Y, Dummy, Y);

               X := Gint (Block.Offset - 1) * View.Char_Width - 2;

               Draw_Line (Window, View.Current_Block_GC, X, Y, X, Y + Height);
               Draw_Line (Window, View.Current_Block_GC, X, Y, X + 6, Y);
               Draw_Line
                 (Window,
                  View.Current_Block_GC, X, Y + Height, X + 6, Y + Height);
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
            Bottom_Line := Natural (Get_Line (Iter) + 1);

            Get_Line_At_Y (View, Iter, Top_In_Buffer, Dummy_Gint);
            Top_Line := Natural (Get_Line (Iter) + 1);

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
                     0, Buffer_Line_Y,
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

               Draw_Rectangle
                 (Window, View.Current_Line_GC, True, 0, Buffer_Line_Y,
                  Rect.Width, Line_Height);
            end if;

            --  Highlight the current block.

            if View.Highlight_Blocks then
               Block_Start := Natural (Get_Line (Cursor_Iter));
               Block_Stack := 0;

               while Block_Start > 0 loop
                  Block := Get_Block (Buffer, Block_Start);

                  Block_Stack := Block_Stack + Block.Indentation_Level;

                  if Block_Stack > 0 then
                     Draw_Block (Block_Start, Block);

                     exit;
                  end if;

                  Block_Start := Block_Start - 1;
               end loop;
            end if;

            --  Redraw the line showing the nth column if needed

            if Column > 0 then
               X := Column * View.Char_Width - Rect.X;
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
      View   : constant Source_View := Source_View (Widget);
   begin
      if not Selection_Exists (Get_Buffer (View)) then
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
      --  Context used for writting line numbers.
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
      Buffer : Src_Editor_Buffer.Source_Buffer := null;
      Kernel : access Glide_Kernel.Kernel_Handle_Record'Class) is
   begin
      View := new Source_View_Record;
      Initialize (View, Buffer, Kernel);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (View   : access Source_View_Record;
      Buffer : Src_Editor_Buffer.Source_Buffer;
      Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
   is
      Insert_Iter : Gtk_Text_Iter;
   begin
      --  Initialize the Source_View. Some of the fields can not be initialized
      --  until the widget is realize or mapped. Their initialization is thus
      --  done at that point.

      pragma Assert (Buffer /= null);

      Gtk.Text_View.Initialize (View, Gtk_Text_Buffer (Buffer));

      View.Kernel := Kernel_Handle (Kernel);

      Set_Border_Window_Size (View, Enums.Text_Window_Left, 1);

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
      View.Connect_Expose_Id := Source_View_Idle.Add
        (Connect_Expose'Access,
         Source_View (View));

      Get_Iter_At_Mark (Buffer, Insert_Iter, Get_Insert (Buffer));
      View.Saved_Cursor_Mark := Create_Mark (Buffer, "", Insert_Iter);
   end Initialize;

   --------------------
   -- Connect_Expose --
   --------------------

   function Connect_Expose (View : Source_View) return Boolean is
      Win           : constant Gdk.Window.Gdk_Window :=
        Get_Window (View, Text_Window_Left);
      X, Y, W, H, D : Gint;

   begin
      Return_Callback.Connect
        (View, "expose_event",
         Marsh => Return_Callback.To_Marshaller (Expose_Event_Cb'Access),
         After => False);

      Get_Geometry (Win, X, Y, W, H, D);
      Clear_Area_E (Win, X, Y, W, H);

      View.Connect_Expose_Id := 0;
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
         Source.Highlight_Blocks :=
           not Equal (Color, White (Get_Default_Colormap));
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
      File : constant String := Get_String (Nth (Args, 1));

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

      Src_Editor_Buffer.Forward_To_Line_End (Iter);
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

      Line_Info  : constant Line_Info_Display_Array_Access :=
        Get_Line_Info (Buffer);
   begin
      External_End_Action (Buffer);

      if Get_Window (Event) = Left_Window
        and then Get_Event_Type (Event) = Button_Press
      then
         declare
            Dummy_Gint         : Gint;
            Iter               : Gtk_Text_Iter;
            Line               : Natural;
            Column_Index       : Integer := -1;
            Button_X, Button_Y : Gint;
            X, Y               : Gint;
            Dummy_Boolean      : Boolean;
            pragma Unreferenced (Dummy_Boolean);

            Info               : Line_Info_Width;

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
            Line := Natural (Get_Line (Iter)) + 1;

            --  Find the column number.
            for J in Line_Info'Range loop
               if Line_Info (J).Starting_X <= Natural (Button_X)
                 and then Natural (Button_X)
                 <= Line_Info (J).Starting_X + Line_Info (J).Width
               then
                  Column_Index := J;
                  exit;
               end if;
            end loop;

            --  If a command exists at the specified position, execute it.
            if Line > 0 and then Column_Index > 0 then
               Info := Get_Side_Info (View, Line, Column_Index);
            end if;

            if Info.Info /= null
              and then Info.Info.Associated_Command /= null
            then
               Dummy_Boolean := Execute (Info.Info.Associated_Command);
            end if;
         end;
      end if;

      return False;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
         return False;
   end Button_Press_Event_Cb;

   --------------------
   -- Do_Indentation --
   --------------------

   function Do_Indentation
     (Buffer   : Source_Buffer;
      Lang     : Language_Access;
      New_Line : Boolean := True) return Boolean
   is
      Indent_Style  : Indentation_Kind;

      Start         : Gtk_Text_Iter;
      Pos           : Gtk_Text_Iter;
      Iter          : Gtk_Text_Iter;
      Indent        : Natural;
      Next_Indent   : Natural;
      Ignore        : Natural;
      Line, Col     : Gint;
      Current_Line  : Gint;
      Offset        : Integer;
      Global_Offset : Integer := 0;

      Blanks        : Natural;
      C_Str         : Gtkada.Types.Chars_Ptr := Gtkada.Types.Null_Ptr;
      Line_Ends     : Boolean := True;
      Line_Start    : Natural;
      Line_End      : Natural;
      Result        : Boolean;
      Slice_Length  : Natural;
      Slice         : Unchecked_String_Access;
      pragma Suppress (Access_Check, Slice);
      Index         : Integer;
      Replace_Cmd   : Editor_Replace_Slice;
      Tabs_Used     : Boolean := False;
      Char          : Character;
      Indent_Params : Indent_Parameters;
      Use_Tabs      : Boolean := False;

      Cursor_Line   : Gint;
      Cursor_Column : Gint;

      function Blank_Slice (Count : Natural; Use_Tabs : Boolean) return String;
      --  Return a string representing count blanks.
      --  If Use_Tabs is True, use ASCII.HT characters as much as possible,
      --  otherwise use only spaces.

      procedure Find_Non_Blank (Last : Natural);
      --  Set Index to the first non blank character in Slice (Index .. Last)
      --  Also set Tabs_Used to True if any tab character is found.

      procedure Local_Next_Indentation
        (Lang          : Language_Access;
         Buffer        : String;
         Indent        : out Natural;
         Next_Indent   : out Natural;
         Indent_Params : Indent_Parameters);
      --  Wrapper around Next_Indentation to take into account Indent_Style.

      -----------------
      -- Blank_Slice --
      -----------------

      function Blank_Slice
        (Count : Natural; Use_Tabs : Boolean) return String is
      begin
         if Use_Tabs then
            return (1 .. Count / Indent_Params.Tab_Width => ASCII.HT) &
              (1 .. Count mod Indent_Params.Tab_Width => ' ');
         else
            return (1 .. Count => ' ');
         end if;
      end Blank_Slice;

      ---------------------
      -- First_Non_Blank --
      ---------------------

      procedure Find_Non_Blank (Last : Natural) is
      begin
         while Index <= Last
           and then (Slice (Index) = ' '
                     or else Slice (Index) = ASCII.HT)
         loop
            if Slice (Index) = ASCII.HT then
               Tabs_Used := True;
            end if;

            Index := Index + 1;
         end loop;
      end Find_Non_Blank;

      ----------------------------
      -- Local_Next_Indentation --
      ----------------------------

      procedure Local_Next_Indentation
        (Lang          : Language_Access;
         Buffer        : String;
         Indent        : out Natural;
         Next_Indent   : out Natural;
         Indent_Params : Indent_Parameters) is
      begin
         if Indent_Style = Simple then
            Next_Indentation
              (Language_Root (Lang.all)'Access,
               Buffer, Indent, Next_Indent, Indent_Params);

         else
            Next_Indentation
              (Lang, Buffer, Indent, Next_Indent, Indent_Params);
         end if;
      end Local_Next_Indentation;

   begin  --  Do_Indentation
      if Lang = null
        or else not Get_Language_Context (Lang).Can_Indent
      then
         return False;
      end if;

      Get_Indentation_Parameters
        (Lang         => Lang,
         Use_Tabs     => Use_Tabs,
         Params       => Indent_Params,
         Indent_Style => Indent_Style);

      if Indent_Style = None then
         return False;
      end if;

      if Use_Tabs then
         Get_Screen_Position (Buffer, Cursor_Line, Cursor_Column);
      else
         Get_Cursor_Position (Buffer, Cursor_Line, Cursor_Column);
      end if;

      Get_Selection_Bounds (Buffer, Start, Pos, Result);

      if Result then
         --  Do not consider a line selected if only the first character
         --  is selected.

         if Get_Line_Offset (Pos) = 0 then
            Backward_Char (Pos, Result);
         end if;

         --  Do not consider a line selected if only the last character is
         --  selected.

         if Ends_Line (Start) then
            Forward_Char (Start, Result);
         end if;
      end if;

      Copy (Pos, Iter);

      --  Go to last char of the line pointed by Iter

      if not Ends_Line (Iter) then
         Forward_To_Line_End (Iter, Result);
         Line_Ends := False;
      end if;

      Line := Get_Line (Iter);
      Col  := Get_Line_Offset (Iter);

      --  We're spending most of our time getting this string.
      --  Consider saving the current line, indentation level and
      --  the stacks used by Next_Indentation to avoid parsing
      --  the buffer from scratch each time.

      C_Str        := Get_Slice (Buffer, 0, 0, Line, Col);
      Slice        := To_Unchecked_String (C_Str);
      Slice_Length := Natural (Strlen (C_Str));

      if New_Line then
         Offset := Natural (Col - Get_Line_Offset (Pos));
         Blanks := Slice_Length - Offset + 1;

         if Line_Ends then
            Slice_Length := Slice_Length + 1;
            Slice (Slice_Length) := ASCII.LF;
         end if;

         while Blanks <= Slice_Length
           and then (Slice (Blanks) = ' ' or else Slice (Blanks) = ASCII.HT)
         loop
            Blanks := Blanks + 1;
         end loop;

         if Line_Ends then
            Local_Next_Indentation
              (Lang, Slice (1 .. Slice_Length), Indent, Next_Indent,
               Indent_Params);
         else
            Index := Integer (Get_Offset (Pos)) + 1;
            Char  := Slice (Index);
            Slice (Index) := ASCII.LF;
            Local_Next_Indentation
              (Lang, Slice (1 .. Index), Indent, Ignore, Indent_Params);
            Slice (Index) := Char;

            --  ??? Would be nice to call Local_Next_Indentation once, which
            --  would be possible with e.g a Prev_Indent parameter

            Local_Next_Indentation
              (Lang,
               Slice (1 .. Index - 1) & ASCII.LF &
                 Slice (Index .. Slice_Length),
               Next_Indent, Ignore, Indent_Params);
         end if;

         Set_Line_Offset (Iter, 0);
         Line_Start := Natural (Get_Offset (Iter)) + 1;
         Index      := Line_Start;

         Find_Non_Blank (Slice_Length);

         --  Replace everything at once, important for efficiency
         --  and also because otherwise, undo/redo won't work properly.

         if Line_Ends then
            Slice_Length := Slice_Length - 1;
         end if;

         Create
           (Replace_Cmd,
            Buffer,
            Integer (Line), 0,
            Integer (Line), Integer (Col),
            Blank_Slice (Indent, Use_Tabs) &
            Slice (Index .. Slice_Length - Offset) & ASCII.LF &
            Blank_Slice (Next_Indent, Use_Tabs) &
            Slice (Blanks .. Slice_Length));
         Enqueue (Buffer, Command_Access (Replace_Cmd));

         --  Need to recompute iter, since the slice replacement that
         --  we just did has invalidated iter.

         if Is_Valid_Position (Buffer, Line + 1, Gint (Next_Indent)) then
            Get_Iter_At_Line_Offset
              (Buffer, Pos, Line + 1, Gint (Next_Indent));
            Place_Cursor (Buffer, Pos);
         end if;

      else
         Current_Line := Get_Line (Start);
         Set_Line_Offset (Start, 0);
         Col := Get_Line_Offset (Pos);

         --  In the loop below, Global_Offset contains the offset between
         --  the modified buffer, and the original buffer, caused by the
         --  buffer replacements.

         loop
            Line_Start := Natural (Get_Offset (Start)) + 1 - Global_Offset;
            Index      := Line_Start;

            if not Ends_Line (Start) then
               Forward_To_Line_End (Start, Result);
            end if;

            Line_End := Natural (Get_Offset (Start)) + 1 - Global_Offset;
            Local_Next_Indentation
              (Lang, Slice (1 .. Line_End),
               Indent, Next_Indent, Indent_Params);

            Find_Non_Blank (Line_End);
            Offset := Index - Line_Start;

            if Tabs_Used or else Offset /= Indent then
               --  Only indent if the current indentation is wrong
               --  ??? Would be nice to indent the whole selection at once,
               --  this would make the undo/redo behavior more intuitive.

               if Current_Line = Cursor_Line then
                  Cursor_Column := Cursor_Column - Gint (Offset - Indent);

                  if Cursor_Column < 0 then
                     Cursor_Column := 0;
                  end if;
               end if;

               Create
                 (Replace_Cmd,
                  Buffer,
                  Integer (Current_Line), 0,
                  Integer (Current_Line), Offset,
                  Blank_Slice (Indent, Use_Tabs));
               Enqueue (Buffer, Command_Access (Replace_Cmd));
               Global_Offset := Global_Offset - Offset + Indent;
            end if;

            exit when Current_Line >= Line;

            Current_Line := Current_Line + 1;
            Get_Iter_At_Line_Offset (Buffer, Start, Current_Line);
         end loop;

         --  Replace the cursor.

         if Use_Tabs then
            Set_Screen_Position (Buffer, Cursor_Line, Cursor_Column);
         else
            Set_Cursor_Position (Buffer, Cursor_Line, Cursor_Column);
         end if;
      end if;

      g_free (C_Str);
      return True;

   exception
      when others =>
         --  Stop propagation of exception, since doing nothing
         --  in this callback is harmless.

         if C_Str /= Gtkada.Types.Null_Ptr then
            g_free (C_Str);
         end if;

         return False;
   end Do_Indentation;

   ------------------------
   -- Key_Press_Event_Cb --
   ------------------------

   function Key_Press_Event_Cb
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean
   is
      View           : constant Source_View   := Source_View (Widget);
      Buffer         : constant Source_Buffer :=
        Source_Buffer (Get_Buffer (View));
      Key            : constant Gdk_Key_Type  := Get_Key_Val (Event);

      Indent_Key     : Gdk_Key_Type;
      Indent_Modif   : Gdk_Modifier_Type;

      Complete_Key   : Gdk_Key_Type;
      Complete_Modif : Gdk_Modifier_Type;

      Delim_Jump_Key   : Gdk_Key_Type;
      Delim_Jump_Modif : Gdk_Modifier_Type;

   begin
      Get_Pref
        (Get_Kernel (Buffer),
         Delimiters_Jump_Key,
         Delim_Jump_Modif,
         Delim_Jump_Key);

      if Key = Delim_Jump_Key
        and then Get_State (Event) = Delim_Jump_Modif
      then
         Jump_To_Delimiter (Buffer);
         return True;
      end if;

      if not Get_Editable (View) then
         return False;
      end if;

      Get_Pref
        (Get_Kernel (Buffer), Indentation_Key, Indent_Modif, Indent_Key);

      if Key = Indent_Key and then Get_State (Event) = Indent_Modif then
         if Do_Indentation (Buffer, Get_Language (Buffer), False) then
            return True;
         end if;
      end if;

      Get_Pref
        (Get_Kernel (Buffer), Completion_Key, Complete_Modif, Complete_Key);

      if Key = Complete_Key and then Get_State (Event) = Complete_Modif then
         Do_Completion (Buffer);
         return True;
      end if;

      case Key is
         when GDK_Return =>
            External_End_Action (Buffer);

            if Do_Indentation (Buffer, Get_Language (Buffer), True) then
               return True;
            end if;

         when GDK_Linefeed | GDK_Tab |
           GDK_Home | GDK_Page_Up | GDK_Page_Down | GDK_End | GDK_Begin |
           GDK_Up | GDK_Down | GDK_Left | GDK_Right
         =>
            External_End_Action (Buffer);

         when GDK_LC_z | GDK_Z =>
            if (Get_State (Event) and Control_Mask) /= 0 then
               Undo (Buffer);
            end if;

         when GDK_LC_r | GDK_R =>
            if (Get_State (Event) and Control_Mask) /= 0 then
               Redo (Buffer);
            end if;

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

      Top_In_Buffer              : Gint;
      Bottom_In_Buffer           : Gint;
      Dummy_Gint                 : Gint;
      Iter                       : Gtk_Text_Iter;
      Y_In_Buffer                : Gint;
      Y_Pix_In_Window            : Gint;
      Line_Height                : Gint;
      Current_Line               : Natural;
      X, Y, Width, Height, Depth : Gint;
      Dummy_Boolean              : Boolean;
      Data                       : Line_Info_Width;
      Buffer                     : Gdk.Pixmap.Gdk_Pixmap;
      Layout                     : Pango_Layout;


      Line_Info  : constant Line_Info_Display_Array_Access :=
        Get_Line_Info (Source_Buffer (Get_Buffer (View)));

      Total_Width : Gint;
   begin
      Total_Width := 2;

      for J in Line_Info'Range loop
         Total_Width := Total_Width + Gint (Line_Info (J).Width) + 2;
      end loop;

      Set_Border_Window_Size (View, Enums.Text_Window_Left, Total_Width);

      --  Create the graphical elements.
      Left_Window := Get_Window (View, Text_Window_Left);
      Layout := Create_Pango_Layout (View);
      Set_Font_Description (Layout, View.Pango_Font);

      Get_Geometry (Left_Window, X, Y, Width, Height, Depth);

      Gdk_New (Buffer, Left_Window, Width, Height);
      Draw_Rectangle
        (Buffer, View.Side_Background_GC, True, X, Y, Width, Height);

      Window_To_Buffer_Coords
        (View, Text_Window_Left,
         Window_X => 0, Window_Y => Y,
         Buffer_X => Dummy_Gint, Buffer_Y => Top_In_Buffer);
      Window_To_Buffer_Coords
        (View, Text_Window_Left,
         Window_X => 0, Window_Y => Y + Height,
         Buffer_X => Dummy_Gint, Buffer_Y => Bottom_In_Buffer);

      Get_Line_At_Y (View, Iter, Top_In_Buffer, Dummy_Gint);
      Current_Line := View.Top_Line;

      Drawing_Loop :
      while Current_Line <= View.Bottom_Line loop
         --  Get buffer coords and line height of current line

         Get_Line_Yrange (View, Iter, Y_In_Buffer, Line_Height);

         --  Convert the buffer coords back to window coords

         Buffer_To_Window_Coords
           (View, Text_Window_Left,
            Buffer_X => 0, Buffer_Y => Y_In_Buffer,
            Window_X => Dummy_Gint, Window_Y => Y_Pix_In_Window);

         --  And finally add the font height (ascent + descent) to get
         --  the Y coordinates of the line base

         for J in Line_Info'Range loop
            Data := Get_Side_Info (View, Current_Line, J);

            if Data.Info /= null then
               if Data.Info.Text /= null then
                  Set_Text (Layout, Data.Info.Text.all);
                  Draw_Layout
                    (Drawable => Buffer,
                     GC       => View.Side_Column_GC,
                     X        =>  Gint (Line_Info (J).Starting_X
                                 + Line_Info (J).Width
                                 - Data.Width),
                     Y        => Y_Pix_In_Window,
                     Layout   => Layout);
               end if;

               if Data.Info.Image /= Null_Pixbuf then
                  Render_To_Drawable
                    (Pixbuf   => Data.Info.Image,
                     Drawable => Buffer,
                     Gc       => View.Side_Column_GC,
                     Src_X    => 0,
                     Src_Y    => 0,
                     Dest_X   => Gint (Line_Info (J).Starting_X
                                       + Line_Info (J).Width
                                       - Data.Width),
                     Dest_Y   => Y_Pix_In_Window,
                     Width    => -1,
                     Height   => -1);
               end if;
            end if;
         end loop;

         Forward_Line (Iter, Dummy_Boolean);

         exit Drawing_Loop when Dummy_Boolean = False;

         Current_Line := Natural (Get_Line (Iter)) + 1;
      end loop Drawing_Loop;

      Unref (Layout);

      Draw_Pixmap
        (Drawable => Left_Window,
         Src      => Buffer,
         Gc       => View.Side_Column_GC,
         Xsrc     => 0,
         Ysrc     => 0,
         Xdest    => 0,
         Ydest    => 0);

      Gdk.Pixmap.Unref (Buffer);
   end Redraw_Columns;

end Src_Editor_View;
