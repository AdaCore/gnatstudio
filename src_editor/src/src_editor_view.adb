-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
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
with Gdk.Font;                    use Gdk.Font;
with Gdk.GC;                      use Gdk.GC;
with Gdk.Window;                  use Gdk.Window;
with Gdk.Pixbuf;                  use Gdk.Pixbuf;
with Gdk.Pixmap;                  use Gdk.Pixmap;
with Gdk.Rectangle;               use Gdk.Rectangle;
with Gdk.Types;                   use Gdk.Types;
with Gdk.Types.Keysyms;           use Gdk.Types.Keysyms;
with Gtk;                         use Gtk;
with Gtk.Enums;                   use Gtk.Enums;
with Gtk.Handlers;
with Gtk.Text_Buffer;             use Gtk.Text_Buffer;
with Gtk.Text_Iter;               use Gtk.Text_Iter;
with Gtk.Text_Mark;               use Gtk.Text_Mark;
with Gtk.Text_View;               use Gtk.Text_View;
with Gtk.Widget;                  use Gtk.Widget;
with Gtkada.Handlers;             use Gtkada.Handlers;
with Gtkada.Types;                use Gtkada.Types;
with Src_Editor_Buffer;           use Src_Editor_Buffer;
with Pango.Font;                  use Pango.Font;

with Commands.Editor;             use Commands.Editor;
with Language;                    use Language;
with Language.C;                  use Language.C;
with Language.Ada;                use Language.Ada;
with Interfaces.C.Strings;        use Interfaces.C.Strings;
with Ada.Exceptions;              use Ada.Exceptions;
with Traces;                      use Traces;
with Basic_Types;                 use Basic_Types;
with Commands;                    use Commands;
with Glide_Kernel;                use Glide_Kernel;
with Glide_Kernel.Modules;        use Glide_Kernel.Modules;
with Glide_Kernel.Preferences;    use Glide_Kernel.Preferences;

with Unchecked_Deallocation;

package body Src_Editor_View is

   Me : constant Debug_Handle := Create ("Source_View");

   use type Pango.Font.Pango_Font_Description;
   package Source_Buffer_Callback is new Gtk.Handlers.User_Callback
     (Widget_Type => Source_Buffer_Record,
      User_Type => Source_View);

   procedure Free (X : in out Line_Info_Width);
   --  Free memory associated to X.

   --------------------------
   -- Forward declarations --
   --------------------------

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

   procedure Map_Cb (View : access Gtk_Widget_Record'Class);
   --  This procedure is invoked when the Source_View widget is mapped.
   --  It performs various operations that can not be done before the widget
   --  is mapped, such as creating GCs associated to the left border window
   --  for instance.

   procedure Insert_Text_Handler
     (Buffer : access Source_Buffer_Record'Class;
      Params : Glib.Values.GValues;
      User   : Source_View);
   --  Callback for the "insert_text" signal.

   procedure Delete_Range_Handler
     (Buffer : access Source_Buffer_Record'Class;
      Params : Glib.Values.GValues;
      User   : Source_View);
   --  Callback for the "delete_range" signal.

   procedure Change_Handler
     (Buffer : access Source_Buffer_Record'Class;
      Params : Glib.Values.GValues;
      User   : Source_View);
   --  Callback for the "changed" signal.

   procedure Redraw_Columns (View : access Source_View_Record'Class);
   --  Redraw the left and right areas around View.

   procedure Set_Font
     (View : access Source_View_Record'Class;
      Font : Pango.Font.Pango_Font_Description);
   --  Change the font used in the given Source_View. Note that this service
   --  should not be used if the widget is not realized.

   procedure Add_Lines
     (View   : access Source_View_Record'Class;
      Start  : Integer;
      Number : Integer);
   --  Add Number blank lines to the column info, after Start.

   procedure Remove_Lines
     (View       : access Source_View_Record'Class;
      Start_Line : Integer;
      End_Line   : Integer);
   --  Remove lines from the column info.

   procedure Insert_At_Position
     (View   : access Source_View_Record;
      Info   : Line_Information_Record;
      Column : Integer;
      Line   : Integer;
      Width  : Integer);
   --  Insert Info at the correct line position in L.

   procedure Get_Column_For_Identifier
     (View       : access Source_View_Record;
      Identifier : String;
      Width      : Integer;
      Column     : out Integer);
   --  Return the index of the column corresponding to the identifier.
   --  Create such a column if necessary.

   function Get_Side_Info
     (View   : access Source_View_Record'Class;
      Line   : Positive;
      Column : Positive) return Line_Info_Width;
   --  Return the side information corresponding to Line, Column in the
   --  Side window.
   --  Return (null, 0) if the information was never set.
   --  Return (null, -1) if the information was set to null.

   function On_Delete
     (View  : access Gtk_Widget_Record'Class;
      Event : Gdk_Event) return Boolean;
   --  Callback for the "delete_event" signal.

   procedure Remove_Line_Information_Column
     (View   : access Source_View_Record'Class;
      Column : Integer);
   --  Remove the column from the side window information in View.

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

   --------------------------
   -- Save_Cursor_Position --
   --------------------------

   procedure Save_Cursor_Position (View : access Source_View_Record'Class) is
   begin
      Get_Cursor_Position
        (Source_Buffer (Get_Buffer (View)),
         View.Saved_Cursor_Line,
         View.Saved_Cursor_Column);
   end Save_Cursor_Position;

   -----------------------------
   -- Restore_Cursor_Position --
   -----------------------------

   procedure Restore_Cursor_Position
     (View : access Source_View_Record'Class) is
   begin
      Set_Cursor_Position
        (Source_Buffer (Get_Buffer (View)),
         View.Saved_Cursor_Line,
         View.Saved_Cursor_Column);
   end Restore_Cursor_Position;

   ------------
   -- Delete --
   ------------

   procedure Delete (View : access Source_View_Record) is
      procedure Free is new Unchecked_Deallocation
        (Natural_Array, Natural_Array_Access);
   begin
      while View.Line_Info'Length > 0 loop
         Remove_Line_Information_Column (View, View.Line_Info'Last);
      end loop;

      Free (View.Real_Lines);
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

   --------------------------
   -- Delete_Range_Handler --
   --------------------------

   procedure Delete_Range_Handler
     (Buffer : access Source_Buffer_Record'Class;
      Params : Glib.Values.GValues;
      User   : Source_View)
   is
      pragma Unreferenced (Buffer);
      Start_Iter : Gtk_Text_Iter;
      End_Iter   : Gtk_Text_Iter;

      Start_Line : Integer;
      End_Line   : Integer;
   begin
      Get_Text_Iter (Nth (Params, 1), Start_Iter);
      Get_Text_Iter (Nth (Params, 2), End_Iter);
      Start_Line := Integer (Get_Line (Start_Iter));
      End_Line   := Integer (Get_Line (End_Iter));

      if Start_Line /= End_Line then
         Remove_Lines (User, Start_Line + 1, End_Line + 1);
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Delete_Range_Handler;

   --------------------
   -- Change_Handler --
   --------------------

   procedure Change_Handler
     (Buffer : access Source_Buffer_Record'Class;
      Params : Glib.Values.GValues;
      User   : Source_View)
   is
      pragma Unreferenced (Params, Buffer);
   begin
      Save_Cursor_Position (User);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Change_Handler;

   -------------------------
   -- Insert_Text_Handler --
   -------------------------

   procedure Insert_Text_Handler
     (Buffer : access Source_Buffer_Record'Class;
      Params : Glib.Values.GValues;
      User   : Source_View)
   is
      pragma Unreferenced (Buffer);
      Pos    : Gtk_Text_Iter;
      Length : constant Gint := Get_Int (Nth (Params, 3));
      Dummy  : Boolean;
      Start  : Integer;
      Iter   : Gtk_Text_Iter;
   begin
      Get_Text_Iter (Nth (Params, 1), Pos);
      Copy (Pos, Iter);
      Start := Integer (Get_Line (Pos));
      Backward_Chars (Pos, Length, Dummy);
      Add_Lines (User, Start, Start - Integer (Get_Line (Pos)));

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Insert_Text_Handler;

   -------------------
   -- Get_Side_Info --
   -------------------

   function Get_Side_Info
     (View   : access Source_View_Record'Class;
      Line   : Positive;
      Column : Positive) return Line_Info_Width is
   begin
      if View.Line_Info (Column).Stick_To_Data then
         if Line > View.Real_Lines'Last
           or else View.Real_Lines (Line) not in
             View.Line_Info (Column).Column_Info'Range
         then
            return (null, -1);
         else
            return View.Line_Info (Column).Column_Info
              (View.Real_Lines (Line));
         end if;
      else
         if Line > View.Line_Info (Column).Column_Info'Last then
            return (null, 0);
         else
            return View.Line_Info (Column).Column_Info (Line);
         end if;
      end if;
   end Get_Side_Info;

   ---------------------
   -- Expose_Event_Cb --
   ---------------------

   function Expose_Event_Cb
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean
   is
      View   : constant Source_View := Source_View (Widget);
      Buffer : constant Source_Buffer := Source_Buffer (Get_Buffer (View));
      Left_Window : constant Gdk.Window.Gdk_Window :=
        Get_Window (View, Text_Window_Left);
   begin
      --  If the event applies to the left border window, then redraw
      --  the side window information.
      if Get_Window (Event) = Left_Window then
         declare
            Top_In_Buffer              : Gint;
            Bottom_In_Buffer           : Gint;
            Dummy_Gint                 : Gint;
            Iter                       : Gtk_Text_Iter;
            Top_Line                   : Natural;
            Bottom_Line                : Natural;
            X, Y, Width, Height, Depth : Gint;
            Info                       : Line_Info_Width;
         begin
            Get_Geometry (Left_Window, X, Y, Width, Height, Depth);

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

            if View.Real_Lines'Last < Bottom_Line then
               declare
                  A : constant Natural_Array := View.Real_Lines.all;
               begin
                  Unchecked_Free (View.Real_Lines);
                  View.Real_Lines := new Natural_Array (1 .. Bottom_Line * 2);
                  View.Real_Lines (A'Range) := A;
                  View.Real_Lines (A'Last + 1 .. View.Real_Lines'Last)
                    := (others => 0);
               end;
            end if;

            --  If one of the values hadn't been initialized, display the
            --  whole range of lines.

            if View.Bottom_Line = 0 then
               View.Top_Line    := Top_Line;
               View.Bottom_Line := Bottom_Line;
               Source_Lines_Revealed (Buffer, Top_Line, Bottom_Line);
            else
               View.Top_Line    := Top_Line;
               View.Bottom_Line := Bottom_Line;
            end if;

            --  Compute the smallest connected area that needs refresh.

            Find_Top_Line :
            while Top_Line <= Bottom_Line loop
               for J in View.Line_Info'Range loop
                  if View.Line_Info (J).Every_Line then
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
               for J in View.Line_Info'Range loop
                  if View.Line_Info (J).Every_Line then
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
      Save_Cursor_Position (View);
      End_Action (Buffer);
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

      return False;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
         return False;
   end Focus_In_Event_Cb;

   ------------
   -- Map_Cb --
   ------------

   procedure Map_Cb (View : access Gtk_Widget_Record'Class) is
      Color   : Gdk_Color;
      Success : Boolean;
   begin
      --  Now that the Source_View is mapped, we can create the Graphic
      --  Context used for writting line numbers.
      Gdk_New
        (Source_View (View).Side_Column_GC,
         Get_Window (Source_View (View), Text_Window_Left));
      Gdk_New
        (Source_View (View).Side_Background_GC,
         Get_Window (Source_View (View), Text_Window_Left));
      Color := Parse ("#eeeeee");
      Alloc_Color (Get_System, Color, False, True, Success);

      if Success then
         Set_Foreground
           (Source_View (View).Side_Background_GC, Color);
      else
         Set_Foreground
           (Source_View (View).Side_Background_GC, White (Get_System));
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

      View.Line_Info := new Line_Info_Display_Array (1 .. 0);
      View.Real_Lines := new Natural_Array (1 .. 1);
      View.Real_Lines (1) := 0;

      Gtk.Text_View.Initialize (View, Gtk_Text_Buffer (Buffer));

      Set_Border_Window_Size (View, Enums.Text_Window_Left, 1);

      Get_Iter_At_Mark (Buffer, Insert_Iter, Get_Insert (Buffer));

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
        (View, "expose_event",
         Marsh => Return_Callback.To_Marshaller (Expose_Event_Cb'Access),
         After => False);
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

      Gtk.Handlers.Add_Watch
        (Source_Buffer_Callback.Connect
         (Buffer, "insert_text",
          Cb        => Insert_Text_Handler'Access,
          User_Data => Source_View (View),
          After     => True),
         View);

      Gtk.Handlers.Add_Watch
        (Source_Buffer_Callback.Connect
         (Buffer, "delete_range",
          Cb        => Delete_Range_Handler'Access,
          User_Data => Source_View (View),
          After     => False),
         View);

      Gtk.Handlers.Add_Watch
        (Source_Buffer_Callback.Connect
         (Buffer, "cursor_position_changed",
          Cb        => Change_Handler'Access,
          User_Data => Source_View (View),
          After     => True),
         View);

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
   end Initialize;

   -------------------------
   -- Preferences_Changed --
   -------------------------

   procedure Preferences_Changed
     (View : access GObject_Record'Class; Kernel : Kernel_Handle) is
   begin
      Set_Font (Source_View (View), Get_Pref (Kernel, Source_Editor_Font));
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
      View : Source_View := Source_View (Widget);
      File : constant String := Get_String (Nth (Args, 1));

      Max : Natural := 1;
   begin
      if Get_Filename (Source_Buffer (Get_Buffer (View))) = File then

         --  The file corresponding to the view was saved:
         --  resynchronize the real lines with the original lines.

         for J in View.Real_Lines'Range loop
            if View.Real_Lines (J) /= 0 then
               Max := J;
            end if;
         end loop;

         Unchecked_Free (View.Real_Lines);
         View.Real_Lines := new Natural_Array (1 .. Max * 2);

         for J in 1 .. Max loop
            View.Real_Lines (J) := J;
         end loop;

         View.Real_Lines (Max + 1 .. Max * 2) := (others => 0);

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
      Font : Pango.Font.Pango_Font_Description)
   is
      use type Gdk.Gdk_Font;
      F : Gdk.Gdk_Font;
   begin
      View.Pango_Font := Font;
      F := Gdk.Font.From_Description (Font);

      if F /= null then
         View.Font := F;

         --  Make sure the widget is already realized. Otherwise, the
         --  layout and style are not created yet.
         if not Realized_Is_Set (View) then
            return;
         end if;

         Modify_Font (View, Font);

         --  ??? Should recompute the width of the column on the side.
      end if;
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

   begin
      End_Action (Buffer);

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
            for J in View.Line_Info'Range loop
               if View.Line_Info (J).Starting_X <= Natural (Button_X)
                 and then Natural (Button_X)
                 <= View.Line_Info (J).Starting_X + View.Line_Info (J).Width
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
      Kernel        : constant Kernel_Handle := Get_Kernel (Buffer);
      Tab_Len       : constant Integer :=
        Integer (Get_Pref (Kernel, Tab_Width));

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
      Indented      : Boolean := False;
      Char          : Character;
      Indent_Params : Indent_Parameters;
      Use_Tabs      : Boolean := False;

      function Blank_Slice (Count : Natural; Use_Tabs : Boolean) return String;
      --  Return a string representing count blanks.
      --  If Use_Tabs is True, use ASCII.HT characters as much as possible,
      --  otherwise use only spaces.

      procedure Find_Non_Blank (Last : Natural);
      --  Set Index to the first non blank character in Slice (Index .. Last)
      --  Also set Tabs_Used to True if any tab character is found.

      -----------------
      -- Blank_Slice --
      -----------------

      function Blank_Slice
        (Count : Natural; Use_Tabs : Boolean) return String is
      begin
         if Use_Tabs then
            return (1 .. Count / Tab_Len => ASCII.HT) &
              (1 .. Count mod Tab_Len => ' ');
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

   begin
      if Lang = null
        or else not Can_Indent (Lang)
      then
         return False;
      end if;

      if Lang.all in Ada_Language'Class then
         if not Get_Pref (Kernel, Ada_Automatic_Indentation) then
            return False;
         end if;

         Use_Tabs := Get_Pref (Kernel, Ada_Use_Tabs);
         Indent_Params :=
           (Indent_Level    =>
              Integer (Get_Pref (Kernel, Ada_Indentation_Level)),
            Indent_Continue =>
              Integer (Get_Pref (Kernel, Ada_Continuation_Level)),
            Indent_Decl     =>
              Integer (Get_Pref (Kernel, Ada_Declaration_Level)),
            Tab_Width       => Tab_Len);

      elsif Lang.all in C_Language'Class then
         if not Get_Pref (Kernel, C_Automatic_Indentation) then
            return False;
         end if;

         Use_Tabs := Get_Pref (Kernel, C_Use_Tabs);
         Indent_Params :=
           (Indent_Level    =>
              Integer (Get_Pref (Kernel, C_Indentation_Level)),
            Indent_Continue => 0,
            Indent_Decl     => 0,
            Tab_Width       => Tab_Len);

      else
         return False;
      end if;

      Get_Selection_Bounds (Buffer, Start, Pos, Result);
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

      --  ??? Should take into account UTF-8 characters and do a conversion

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
            Next_Indentation
              (Lang, Slice (1 .. Slice_Length), Indent, Next_Indent,
               Indent_Params);
         else
            Index := Integer (Get_Offset (Pos)) + 1;
            Char  := Slice (Index);
            Slice (Index) := ASCII.LF;
            Next_Indentation
              (Lang, Slice (1 .. Index), Indent, Ignore, Indent_Params);
            Slice (Index) := Char;

            --  ??? Would be nice to call Next_Indentation once, which would
            --  be possible with e.g a Prev_Indent parameter

            Next_Indentation
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
         Enqueue (Get_Queue (Buffer), Replace_Cmd);

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
            Next_Indentation
              (Lang, Slice (1 .. Line_End),
               Indent, Next_Indent, Indent_Params);

            Find_Non_Blank (Line_End);
            Offset := Index - Line_Start;

            if Tabs_Used or else Offset /= Indent then
               --  Only indent if the current indentation is wrong
               --  ??? Would be nice to indent the whole selection at once,
               --  this would make the undo/redo behavior more intuitive.

               Indented := True;
               Create
                 (Replace_Cmd,
                  Buffer,
                  Integer (Current_Line), 0,
                  Integer (Current_Line), Offset,
                  Blank_Slice (Indent, Use_Tabs));
               Enqueue (Get_Queue (Buffer), Replace_Cmd);
               Global_Offset := Global_Offset - Offset + Indent;
            end if;

            exit when Current_Line >= Line - 1;

            Current_Line := Current_Line + 1;
            Get_Iter_At_Line_Offset (Buffer, Start, Current_Line);
         end loop;

         if Indented then
            Get_Iter_At_Line_Offset (Buffer, Pos, Current_Line, Gint (Indent));
            Place_Cursor (Buffer, Pos);
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
      View   : constant Source_View   := Source_View (Widget);
      Buffer : constant Source_Buffer := Source_Buffer (Get_Buffer (View));
      Key    : constant Gdk_Key_Type  := Get_Key_Val (Event);

   begin
      if Key = GDK_Tab and then (Get_State (Event) and Control_Mask) /= 0 then
         --  ??? Should use a preference for the key value
         if Do_Indentation (Buffer, Get_Language (Buffer), False) then
            return True;
         end if;
      end if;

      case Key is
         when GDK_Return =>
            if Do_Indentation (Buffer, Get_Language (Buffer), True) then
               return True;
            else
               End_Action (Buffer);
            end if;

         when GDK_Linefeed | GDK_Tab |
           GDK_Home | GDK_Page_Up | GDK_Page_Down | GDK_End | GDK_Begin |
           GDK_Up | GDK_Down | GDK_Left | GDK_Right
         =>
            End_Action (Buffer);

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

   ------------------------
   -- Insert_At_Position --
   ------------------------

   procedure Insert_At_Position
     (View   : access Source_View_Record;
      Info   : Line_Information_Record;
      Column : Integer;
      Line   : Integer;
      Width  : Integer) is
   begin
      if Line not in View.Real_Lines'Range then
         declare
            A : constant Natural_Array := View.Real_Lines.all;
         begin
            Unchecked_Free (View.Real_Lines);
            View.Real_Lines := new Natural_Array
              (1 .. Line * 2);

            View.Real_Lines (A'Range) := A;
            View.Real_Lines (A'Last + 1 .. View.Real_Lines'Last) :=
              (others => 0);
         end;
      end if;

      --  If needed, increase the size of the target array

      if (not View.Line_Info (Column).Stick_To_Data)
        and then Line > View.Line_Info (Column).Column_Info'Last
      then
         declare
            A : Line_Info_Width_Array (1 .. Line * 2);
         begin
            A (1 .. View.Line_Info (Column).Column_Info'Last) :=
              View.Line_Info (Column).Column_Info.all;
            View.Line_Info (Column).Column_Info :=
              new Line_Info_Width_Array' (A);
         end;
      end if;

      --  Insert the data in the array

      if not (View.Line_Info (Column).Stick_To_Data
              and then Line > View.Original_Lines_Number)
      then
         if View.Line_Info (Column).Column_Info (Line).Info /= null then
            Free (View.Line_Info (Column).Column_Info (Line));
         end if;

         View.Line_Info (Column).Column_Info (Line) :=
           Line_Info_Width' (new Line_Information_Record' (Info), Width);
      end if;
   end Insert_At_Position;

   --------------------
   -- Redraw_Columns --
   --------------------

   procedure Redraw_Columns (View : access Source_View_Record'Class) is
      Left_Window : constant Gdk.Window.Gdk_Window :=
        Get_Window (View, Text_Window_Left);

      Top_In_Buffer              : Gint;
      Bottom_In_Buffer           : Gint;
      Dummy_Gint                 : Gint;
      Iter                       : Gtk_Text_Iter;
      Y_In_Buffer                : Gint;
      Y_In_Window                : Gint;
      Y_Pix_In_Window            : Gint;
      Line_Height                : Gint;
      Current_Line               : Natural;
      X, Y, Width, Height, Depth : Gint;
      Dummy_Boolean              : Boolean;
      Data                       : Line_Info_Width;
      Buffer                     : Gdk.Pixmap.Gdk_Pixmap;

   begin
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
            Window_X => Dummy_Gint, Window_Y => Y_In_Window);

         --  And finally add the font height (ascent + descent) to get
         --  the Y coordinates of the line base

         Y_Pix_In_Window := Y_In_Window;

         Y_In_Window :=
           Y_In_Window + Get_Ascent (View.Font);

         for J in View.Line_Info'Range loop
            Data := Get_Side_Info (View, Current_Line, J);

            if Data.Info /= null then
               if Data.Info.Text /= null then
                  Draw_Text
                    (Drawable => Buffer,
                     Font => View.Font,
                     Gc => View.Side_Column_GC,
                     X =>  Gint (View.Line_Info (J).Starting_X
                                 + View.Line_Info (J).Width
                                 - Data.Width
                                 - 2),
                     Y => Y_In_Window,
                     Text => Data.Info.Text.all);
               end if;

               if Data.Info.Image /= Null_Pixbuf then
                  Render_To_Drawable
                    (Pixbuf   => Data.Info.Image,
                     Drawable => Buffer,
                     Gc       => View.Side_Column_GC,
                     Src_X    => 0,
                     Src_Y    => 0,
                     Dest_X   => Gint (View.Line_Info (J).Starting_X
                                       + View.Line_Info (J).Width
                                       - Data.Width
                                       - 2),
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

   --------------------------
   -- Add_File_Information --
   --------------------------

   procedure Add_File_Information
     (View       : access Source_View_Record;
      Identifier : String;
      Info       : Glide_Kernel.Modules.Line_Information_Data)
   is
      Column : Integer;
      Buffer : Integer;
      Width  : Integer := -1;
      Widths : array (Info'Range) of Integer;

   begin
      --  Compute the maximum width of the items to add.
      for J in Info'Range loop
         Widths (J) := -1;
         if Info (J).Text /= null then
            Buffer := Integer
              (String_Width (View.Font, String' (Info (J).Text.all)));

            Widths (J) := Buffer;

            if Buffer > Width then
               Width := Buffer;
            end if;
         end if;

         if Info (J).Image /= Null_Pixbuf then
            Buffer := Integer (Get_Width (Info (J).Image));

            if Buffer > Width then
               Widths (J) := Buffer;
               Width := Buffer;
            end if;
         end if;
      end loop;

      --  Get the column that corresponds to Identifier,
      --  create it if necessary.
      Get_Column_For_Identifier
        (View,
         Identifier,
         Width,
         Column);

      --  Update the stored data.
      for J in Info'Range loop
         Insert_At_Position
           (View, Info (J), Column, J, Widths (J));
      end loop;

      --  If some of the data was in the display range, draw it.

      if not (Info'Last < View.Top_Line
              or else Info'First > View.Bottom_Line)
      then
         Redraw_Columns (View);
      end if;
   end Add_File_Information;

   ------------------------------------
   -- Create_Line_Information_Column --
   ------------------------------------

   procedure Create_Line_Information_Column
     (View          : access Source_View_Record;
      Identifier    : String;
      Stick_To_Data : Boolean;
      Every_Line    : Boolean)
   is
      Col : Integer;
   begin
      Get_Column_For_Identifier
        (View, Identifier, -1, Col);
      View.Line_Info (Col).Stick_To_Data := Stick_To_Data;
      View.Line_Info (Col).Every_Line    := Every_Line;
   end Create_Line_Information_Column;

   ------------------------------------
   -- Remove_Line_Information_Column --
   ------------------------------------

   procedure Remove_Line_Information_Column
     (View   : access Source_View_Record'Class;
      Column : Integer)
   is
      Width : Integer;

      procedure Free is new Unchecked_Deallocation
        (Line_Info_Width_Array, Line_Info_Width_Array_Access);
      procedure Free is new Unchecked_Deallocation
        (Line_Info_Display_Record, Line_Info_Display_Access);
   begin
      Width := View.Line_Info (Column).Width;

      for J in View.Line_Info (Column).Column_Info'Range loop
         Free (View.Line_Info (Column).Column_Info (J));
      end loop;

      Free (View.Line_Info (Column).Identifier);
      Free (View.Line_Info (Column).Column_Info);
      Free (View.Line_Info (Column));

      declare
         A : Line_Info_Display_Array
           (View.Line_Info.all'First .. View.Line_Info.all'Last - 1);
      begin
         A (View.Line_Info.all'First .. Column - 1) :=
           View.Line_Info (View.Line_Info.all'First .. Column - 1);
         A (Column .. View.Line_Info.all'Last - 1) :=
           View.Line_Info (Column + 1 .. View.Line_Info.all'Last);
         Unchecked_Free (View.Line_Info);
         View.Line_Info := new Line_Info_Display_Array' (A);
      end;

      for J in Column .. View.Line_Info.all'Last loop
         View.Line_Info (J).Starting_X :=
           View.Line_Info (J).Starting_X - Width - 2;
      end loop;

      View.Total_Column_Width := View.Total_Column_Width - Width - 2;
      Set_Border_Window_Size
        (View, Enums.Text_Window_Left, Gint (View.Total_Column_Width));
   end Remove_Line_Information_Column;

   ------------------------------------
   -- Remove_Line_Information_Column --
   ------------------------------------

   procedure Remove_Line_Information_Column
     (View       : access Source_View_Record;
      Identifier : String)
   is
      Col : Integer;
   begin
      Get_Column_For_Identifier (View, Identifier, -1, Col);
      Remove_Line_Information_Column (View, Col);

      Redraw_Columns (View);
   end Remove_Line_Information_Column;

   -------------------------------
   -- Get_Column_For_Identifier --
   -------------------------------

   procedure Get_Column_For_Identifier
     (View       : access Source_View_Record;
      Identifier : String;
      Width      : Integer;
      Column     : out Integer) is
   begin
      --  Browse through existing columns and try to match Identifier

      for J in View.Line_Info'Range loop
         if View.Line_Info (J).Identifier.all = Identifier then
            Column := J;

            --  Set the new width of the column

            if View.Line_Info (J).Width < Width then
               for K in (J + 1) .. View.Line_Info.all'Last loop
                  View.Line_Info (K).Starting_X :=
                    View.Line_Info (K).Starting_X + Width
                    - View.Line_Info (J).Width;
               end loop;

               View.Total_Column_Width :=
                 View.Total_Column_Width + Width
                 - View.Line_Info (J).Width;

               View.Line_Info (J).Width := Width;

               Set_Border_Window_Size
                 (View, Enums.Text_Window_Left,
                  Gint (View.Total_Column_Width));
            end if;

            return;
         end if;
      end loop;

      --  If we reach this point, that means no column was found that
      --  corresponds to Identifier. Therefore we create one.

      declare
         A : Line_Info_Display_Array
           (View.Line_Info.all'First .. View.Line_Info.all'Last + 1);
         New_Column : Line_Info_Width_Array
           (1 .. View.Original_Lines_Number);
      begin
         A (View.Line_Info'First .. View.Line_Info'Last) := View.Line_Info.all;

         A (A'Last) := new Line_Info_Display_Record'
           (Identifier  => new String' (Identifier),
            Starting_X  => View.Total_Column_Width + 2,
            Width       => Width,
            Column_Info => new Line_Info_Width_Array' (New_Column),
            Stick_To_Data => False,
            Every_Line    => False);
         Unchecked_Free (View.Line_Info);
         View.Line_Info := new Line_Info_Display_Array' (A);

         Column := View.Line_Info.all'Last;

         View.Total_Column_Width := View.Total_Column_Width + Width + 2;

         Set_Border_Window_Size
           (View, Enums.Text_Window_Left,
            Gint (View.Total_Column_Width));
      end;
   end Get_Column_For_Identifier;

   ---------------
   -- Add_Lines --
   ---------------

   procedure Add_Lines
     (View   : access Source_View_Record'Class;
      Start  : Integer;
      Number : Integer) is
   begin
      if Number <= 0 then
         return;
      end if;

      if not View.Original_Text_Inserted then
         View.Original_Lines_Number := Number;

         if View.Original_Lines_Number > View.Real_Lines'Last then
            declare
               A : constant Natural_Array := View.Real_Lines.all;
            begin
               View.Real_Lines := new Natural_Array
                 (1 .. Number * 2);
               View.Real_Lines (A'Range) := A;
               View.Real_Lines (A'Last + 1 .. View.Real_Lines'Last)
                 := (others => 0);
            end;
         end if;

         for J in 1 .. Number loop
            View.Real_Lines (J) := J;
         end loop;

         View.Original_Text_Inserted := True;
      end if;

      --  ??? Loop needs comment, and might be implemented more efficiently
      --  through the use of aggregates (to be checked).

      for J in reverse Start + 1 .. View.Real_Lines.all'Last loop
         if J <= View.Real_Lines.all'First - 1 + Number then
            View.Real_Lines (J) := 0;
         else
            View.Real_Lines (J) := View.Real_Lines (J - Number);
         end if;
      end loop;

      --  Reset the last lines

      View.Real_Lines
        (Start + 1 .. Integer'Min (Start + Number, View.Real_Lines'Last)) :=
        (others => 0);
   end Add_Lines;

   ------------------
   -- Remove_Lines --
   ------------------

   procedure Remove_Lines
     (View       : access Source_View_Record'Class;
      Start_Line : Integer;
      End_Line   : Integer) is
   begin
      if End_Line <= Start_Line then
         return;
      end if;

      View.Real_Lines
        (Start_Line + 1 .. View.Real_Lines'Last + Start_Line - End_Line) :=
        View.Real_Lines (End_Line + 1 .. View.Real_Lines'Last);

      View.Real_Lines
        (View.Real_Lines'Last + Start_Line - End_Line + 1
           .. View.Real_Lines'Last) := (others => 0);
   end Remove_Lines;

   ----------
   -- Free --
   ----------

   procedure Free (X : in out Line_Info_Width) is
      procedure Free is new Unchecked_Deallocation
        (Line_Information_Record, Line_Information_Access);
   begin
      if X.Info /= null then
         Free (X.Info.all);
      end if;

      Free (X.Info);
   end Free;

end Src_Editor_View;
