-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
--                            ACT-Europe                             --
--                                                                   --
-- GPS is free  software; you can  redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Glib;                        use Glib;
with Glib.Values;                 use Glib.Values;
with Gdk;                         use Gdk;
with Gdk.Drawable;                use Gdk.Drawable;
with Gdk.Event;                   use Gdk.Event;
with Gdk.Font;                    use Gdk.Font;
with Gdk.GC;                      use Gdk.GC;
with Gdk.Window;                  use Gdk.Window;
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
with Src_Editor_Buffer;           use Src_Editor_Buffer;
with String_Utils;                use String_Utils;

with Basic_Types;                 use Basic_Types;
with Glide_Kernel.Modules;        use Glide_Kernel.Modules;

with Unchecked_Deallocation;
package body Src_Editor_View is

   use type Pango.Font.Pango_Font_Description;
   use Line_Info_List;

   Minimal_Number_Of_Digits_In_LNA : constant := 3;
   --  Minimal number of digits for a line number that the Line Numbers Area
   --  (LNA) should be able to diplay.

   LNA_Border_Width : constant := 4;
   --  The number of pixels between the the LNA borders and the line number.

   package Source_Buffer_Callback is new Gtk.Handlers.User_Callback
     (Widget_Type => Source_Buffer_Record,
      User_Type => Source_View);

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
   --  gained back.

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

   function LNA_Width_In_Digits
     (View : access Source_View_Record'Class) return Natural;
   --  Return the number of digits the LNA should be able to display in order
   --  to accomodate any line number for the given view. This value is computed
   --  by returning the number of digits of the last line number. If this
   --  number is smaller than the Minimal_Number_Of_Digits_In_LNA constant,
   --  then this constant is returned instead. This is to ensure that the LNA
   --  width does not change too often when editing small files.

   function LNA_Width_In_Pixels
     (View : access Source_View_Record'Class) return Gint;
   --  Return the needed width in pixels of the LNA to fit any line number for
   --  the given view. Just as in LNA_Width_In_Digits, the width returned will
   --  always be able to accomodate at least Minimal_Number_Of_Digits_In_LNA,
   --  to ensure that the LNA width does not change too often when editing
   --  small files.

   procedure Reset_Left_Border_Window_Size
     (View : access Source_View_Record'Class);
   --  Recalculate the size of the small window on the left used to display
   --  entities such as line numbers, breakpoint icons, etc.

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
   --  Add blank lines to the column info.

   procedure Remove_Lines
     (View       : access Source_View_Record'Class;
      Start_Line : Integer;
      End_Line   : Integer);
   --  Remove lines from the column info.

   procedure Insert_At_Position
     (L    : in out Line_Info_List.List;
      Info : Line_Information_Record);
   --  Insert Info at the correct line position in L.

   procedure Get_Column_For_Identifier
     (View       : access Source_View_Record;
      Identifier : String;
      Width      : Integer;
      Column     : out Integer);
   --  Return the index of the column corresponding to the identifier.
   --  Create such a column if necessary.

   ----------------
   -- Realize_Cb --
   ----------------

   procedure Realize_Cb (Widget : access Gtk_Widget_Record'Class) is
      View : constant Source_View := Source_View (Widget);
   begin
      --  Now that the window is realized, we can set the font and
      --  the size of the left border window size.
      Set_Font (View, View.Pango_Font);
      Reset_Left_Border_Window_Size (View);
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
   end Delete_Range_Handler;

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
   end Insert_Text_Handler;

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
      --  the line numbers.
      if Get_Window (Event) = Left_Window then
         declare
            Top_In_Buffer              : Gint;
            Bottom_In_Buffer           : Gint;
            Dummy_Gint                 : Gint;
            Iter                       : Gtk_Text_Iter;
            Top_Line                   : Natural;
            Bottom_Line                : Natural;
            X, Y, Width, Height, Depth : Gint;
            Previous_Top_Line          : Natural := View.Min_Top_Line;
            Previous_Bottom_Line       : Natural := View.Max_Bottom_Line;
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

            --  If one of the values hadn't been initialized, display the
            --  whole range of lines.
            View.Top_Line := Top_Line;
            View.Bottom_Line := Bottom_Line;

            if Previous_Top_Line = 0
              or else Previous_Top_Line = 0
            then
               Source_Lines_Revealed (Buffer, Top_Line, Bottom_Line);
               View.Min_Top_Line := Top_Line;
               View.Max_Bottom_Line := Bottom_Line;

            elsif Top_Line < Previous_Top_Line then
               Source_Lines_Revealed (Buffer, Top_Line, Previous_Top_Line);
               View.Min_Top_Line := Top_Line;
            elsif Bottom_Line > Previous_Bottom_Line then
               Source_Lines_Revealed
                 (Buffer, Previous_Bottom_Line, Bottom_Line);
               View.Max_Bottom_Line := Bottom_Line;
            end if;

            Redraw_Columns (View);
            --  return True;
         end;
      end if;

      --  Return false, so that the signal is not blocked, and other
      --  clients can use it.
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
      Insert_Iter : Gtk_Text_Iter;
   begin
      --  Save the current insert cursor position by moving the
      --  Saved_Insert_Mark to the location where the "insert" mark
      --  currently is.
      Get_Iter_At_Mark (Buffer, Insert_Iter, Get_Insert (Buffer));
      Move_Mark (Buffer, View.Saved_Insert_Mark, Insert_Iter);
      End_Action (Buffer);
      return False;
   end Focus_Out_Event_Cb;

   -----------------------
   -- Focus_In_Event_Cb --
   -----------------------

   function Focus_In_Event_Cb
     (Widget : access Gtk_Widget_Record'Class) return Boolean
   is
      View   : constant Source_View := Source_View (Widget);
      Buffer : constant Source_Buffer := Source_Buffer (Get_Buffer (View));
      Saved_Insert_Iter : Gtk_Text_Iter;
   begin
      --  Restore the old cursor position before we left the Source_View
      --  by moving the Insert Mark to the location where the Saved_Insert_Mark
      --  currently is.

      Get_Iter_At_Mark (Buffer, Saved_Insert_Iter, View.Saved_Insert_Mark);
      Place_Cursor (Buffer, Saved_Insert_Iter);
      return False;
   end Focus_In_Event_Cb;

   ------------
   -- Map_Cb --
   ------------

   procedure Map_Cb (View : access Gtk_Widget_Record'Class) is
   begin
      --  Now that the Source_View is mapped, we can create the Graphic
      --  Context used for writting line numbers.
      Gdk_New
        (Source_View (View).Line_Numbers_GC,
         Get_Window (Source_View (View), Text_Window_Left));
   end Map_Cb;

   -------------------------
   -- LNA_Width_In_Digits --
   -------------------------

   function LNA_Width_In_Digits
     (View : access Source_View_Record'Class) return Natural
   is
      Line_Count           : constant Natural :=
        Natural (Get_Line_Count (Get_Buffer (View)));
      Max_Number_Of_Digits : constant Natural :=
        Natural'Max
          (Number_Of_Digits (Line_Count), Minimal_Number_Of_Digits_In_LNA);

   begin
      return Max_Number_Of_Digits;
   end LNA_Width_In_Digits;

   -------------------------
   -- LNA_Width_In_Pixels --
   -------------------------

   function LNA_Width_In_Pixels
     (View : access Source_View_Record'Class) return Gint
   is
      Max_Number_Of_Digits : constant Natural := LNA_Width_In_Digits (View);
      Templ : constant String (1 .. Max_Number_Of_Digits) := (others => '0');
   begin
      return 2 * LNA_Border_Width + Gdk.Font.String_Width (View.Font, Templ);
   end LNA_Width_In_Pixels;

   -----------------------------------
   -- Reset_Left_Border_Window_Size --
   -----------------------------------

   procedure Reset_Left_Border_Window_Size
     (View : access Source_View_Record'Class)
   is
      Max_Number_Of_Digits : constant Natural := LNA_Width_In_Digits (View);
      Border_Size : Gint := 0;
   begin
      if View.Show_Line_Numbers then
         Border_Size := LNA_Width_In_Pixels (View);
         View.LNA_Width_In_Digits := Max_Number_Of_Digits;
      end if;
      if Border_Size /= 0 then
         --  ??? Destroy the window first, or the resize will not be effective
         Set_Border_Window_Size (View, Enums.Text_Window_Left, 0);
      end if;
      Set_Border_Window_Size (View, Enums.Text_Window_Left, Border_Size);
   end Reset_Left_Border_Window_Size;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (View              : out Source_View;
      Buffer            : Src_Editor_Buffer.Source_Buffer := null;
      Font              : Pango.Font.Pango_Font_Description;
      Show_Line_Numbers : Boolean := False) is
   begin
      View := new Source_View_Record;
      Initialize (View, Buffer, Font, Show_Line_Numbers);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (View              : access Source_View_Record;
      Buffer            : Src_Editor_Buffer.Source_Buffer;
      Font              : Pango.Font.Pango_Font_Description;
      Show_Line_Numbers : Boolean)
   is
      Insert_Iter : Gtk_Text_Iter;
   begin
      --  Initialize the Source_View. Some of the fields can not be initialized
      --  until the widget is realize or mapped. Their initialization is thus
      --  done at that point.

      Gtk.Text_View.Initialize (View, Gtk_Text_Buffer (Buffer));

      Get_Iter_At_Mark (Buffer, Insert_Iter, Get_Insert (Buffer));
      View.Saved_Insert_Mark := Create_Mark (Buffer, Where => Insert_Iter);

      View.Pango_Font := Font;

      View.Font := Gdk.Font.From_Description (View.Pango_Font);
      View.Show_Line_Numbers := Show_Line_Numbers;
      View.LNA_Width_In_Digits := Minimal_Number_Of_Digits_In_LNA;

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

      Source_Buffer_Callback.Connect
        (Buffer, "insert_text",
         Cb        => Insert_Text_Handler'Access,
         User_Data => Source_View (View),
         After     => True);

      Source_Buffer_Callback.Connect
        (Buffer, "delete_range",
         Cb        => Delete_Range_Handler'Access,
         User_Data => Source_View (View),
         After     => False);

      View.Line_Info := new Line_Info_Display_Array (1 .. 0);
      View.Queue := New_Queue;
      --  ??? when is this freed ?
   end Initialize;

   --------------
   -- Set_Font --
   --------------

   procedure Set_Font
     (View : access Source_View_Record'Class;
      Font : Pango.Font.Pango_Font_Description)
   is
   begin
      View.Pango_Font := Font;
      View.Font := Gdk.Font.From_Description (Font);

      --  Make sure the widget is already realized. Otherwise, the
      --  layout and style are not created yet.
      if not Realized_Is_Set (View) then
         return;
         --  ??? We should probably log a warning...
      end if;

      Modify_Font (View, Font);
   end Set_Font;

   ---------------------------
   -- Set_Show_Line_Numbers --
   ---------------------------

   procedure Set_Show_Line_Numbers
     (View         : access Source_View_Record;
      Show_Numbers : Boolean := True)
   is
   begin
      if Show_Numbers /= View.Show_Line_Numbers then
         View.Show_Line_Numbers := Show_Numbers;
         Reset_Left_Border_Window_Size (View);
      end if;
   end Set_Show_Line_Numbers;

   ---------------------------
   -- Get_Show_Line_Numbers --
   ---------------------------

   function Get_Show_Line_Numbers
     (View : access Source_View_Record) return Boolean is
   begin
      return View.Show_Line_Numbers;
   end Get_Show_Line_Numbers;

   -------------------------------
   -- Scroll_To_Cursor_Location --
   -------------------------------

   procedure Scroll_To_Cursor_Location (View : access Source_View_Record) is
      Insert_Mark : constant Gtk_Text_Mark := Get_Insert (Get_Buffer (View));
   begin
      --  We want to use the alignments, so that the line appears in the middle
      --  of the screen if possible. This provides a more user-friendly
      --  behavior.

      Scroll_To_Mark
        (View, Insert_Mark, Use_Align => True,
         Within_Margin => 0.0, Xalign => 0.5, Yalign => 0.5);
   end Scroll_To_Cursor_Location;

   -----------------------------
   -- Window_To_Buffer_Coords --
   -----------------------------

   procedure Window_To_Buffer_Coords
     (View   : access Source_View_Record;
      X, Y   : Gint;
      Line   : out Gint;
      Column : out Gint)
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

      if Buffer_X > Iter_Location.X
        or else Buffer_Y > Iter_Location.Y + Line_Height
      then
         Line   := -1;
         Column := -1;
      end if;
   end Window_To_Buffer_Coords;

   ----------------------------
   -- Event_To_Buffer_Coords --
   ----------------------------

   procedure Event_To_Buffer_Coords
     (View     : access Source_View_Record;
      Event    : Gdk_Event;
      Line     : out Gint;
      Column   : out Gint) is
   begin
      Window_To_Buffer_Coords
        (View, Gint (Get_X (Event)), Gint (Get_Y (Event)), Line, Column);
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
            Dummy_Gint                 : Gint;
            Iter                       : Gtk_Text_Iter;
            Line                       : Natural;
            Column_Index               : Integer := -1;
            Button_X, Button_Y         : Gint;
            X, Y                       : Gint;
            Node                       : List_Node;

         begin
            --  Get the coordinates of the click.

            Button_X := Gint (Get_X (Event));
            Button_Y := Gint (Get_Y (Event));

            Window_To_Buffer_Coords
              (View, Text_Window_Left,
               Window_X => Button_X, Window_Y => Button_Y,
               Buffer_X => X, Buffer_Y => Y);

            Get_Line_At_Y (View, Iter, Y, Dummy_Gint);
            Line := Natural (Get_Line (Iter)) + 1;

            for J in View.Line_Info.all'Range loop
               if View.Line_Info (J).Starting_X <= Natural (Button_X)
                 and then Natural (Button_X)
                 <= View.Line_Info (J).Starting_X + View.Line_Info (J).Width
               then
                  Column_Index := J;
                  exit;
               end if;
            end loop;

            if Column_Index > 0 then
               Node := First (View.Line_Info (Column_Index).Column_Info);

               while Node /= Null_Node
                 and then Data (Node).Line < Line
               loop
                  Node := Next (Node);
               end loop;

               if Node /= Null_Node
                 and then Data (Node).Line = Line
                 and then Data (Node).Associated_Command /= null
               then
                  Enqueue (View.Queue, Data (Node).Associated_Command);
               end if;
            end if;
         end;
      end if;

      return False;
   end Button_Press_Event_Cb;

   ------------------------
   -- Key_Press_Event_Cb --
   ------------------------

   function Key_Press_Event_Cb
     (Widget : access Gtk_Widget_Record'Class;
      Event  : Gdk_Event) return Boolean
   is
      View   : constant Source_View := Source_View (Widget);
      Buffer : constant Source_Buffer := Source_Buffer (Get_Buffer (View));
   begin
      case Get_Key_Val (Event) is
         when GDK_Tab | GDK_Return | GDK_Linefeed |
           GDK_Home | GDK_Page_Up | GDK_Page_Down | GDK_End |
           GDK_Begin | GDK_Up | GDK_Down | GDK_Left | GDK_Right
         =>
            End_Action (Buffer);

         when others =>
            null;
      end case;

      return False;
   end Key_Press_Event_Cb;

   ------------------------
   -- Insert_At_Position --
   ------------------------

   procedure Insert_At_Position
     (L    : in out Line_Info_List.List;
      Info : Line_Information_Record)
   is
      Node : List_Node;
   begin
      if Is_Empty (L) then
         Append (L, new Line_Information_Record' (Info));
      else
         Node := First (L);

         while Next (Node) /= Null_Node
           and then Data (Next (Node)).Line <= Info.Line
         loop
            Node := Next (Node);
         end loop;

         if Data (Node).Line = Info.Line then
            Set_Data (Node, new Line_Information_Record' (Info));
         else
            Append (L, Node, new Line_Information_Record' (Info));
         end if;
      end if;
   end Insert_At_Position;

   --------------------
   -- Redraw_Columns --
   --------------------

   procedure Redraw_Columns (View : access Source_View_Record'Class)
   is
      Left_Window : constant Gdk.Window.Gdk_Window :=
        Get_Window (View, Text_Window_Left);

      Top_In_Buffer              : Gint;
      Bottom_In_Buffer           : Gint;
      Dummy_Gint                 : Gint;
      Iter                       : Gtk_Text_Iter;
      Y_In_Buffer                : Gint;
      Y_In_Window                : Gint;
      Line_Height                : Gint;
      Current_Line               : Natural;
      X, Y, Width, Height, Depth : Gint;
      Dummy_Boolean              : Boolean;

      Nodes : array (View.Line_Info.all'Range) of List_Node;
   begin
      for J in Nodes'Range loop
         Nodes (J) := First (View.Line_Info (J).Column_Info);

         while Nodes (J) /= Null_Node
           and then Data (Nodes (J)).Line < View.Top_Line
         loop
            Nodes (J) := Next (Nodes (J));
         end loop;
      end loop;

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
         Y_In_Window :=
           Y_In_Window + Get_Ascent (View.Font) + Get_Descent (View.Font) - 2;

         for J in Nodes'Range loop
            while Nodes (J) /= Null_Node
              and then Data (Nodes (J)).Line < Current_Line
            loop
               Nodes (J) := Next (Nodes (J));
            end loop;

            if Nodes (J) /= Null_Node
              and then Data (Nodes (J)).Line = Current_Line
            then
               if Data (Nodes (J)).Text /= null then
                  Draw_Text
                    (Drawable => Left_Window,
                     Font => View.Font,
                     Gc => View.Line_Numbers_GC,
                     X =>  Gint (View.Line_Info (J).Starting_X),
                     Y => Y_In_Window,
                     Text => Data (Nodes (J)).Text.all);
               end if;
            end if;
         end loop;

         Forward_Line (Iter, Dummy_Boolean);
         exit Drawing_Loop when Dummy_Boolean = False;

         Current_Line := Natural (Get_Line (Iter)) + 1;
      end loop Drawing_Loop;
   end Redraw_Columns;

   --------------------------
   -- Add_File_Information --
   --------------------------

   procedure Add_File_Information
     (View          : access Source_View_Record;
      Identifier    : String;
      Width         : Integer;
      Info          : Glide_Kernel.Modules.Line_Information_Data;
      Stick_To_Data : Boolean := True)
   is
      Column : Integer;
   begin
      --  Refresh the stored data.
      Get_Column_For_Identifier
        (View,
         Identifier,
         Width,
         Column);

      View.Line_Info (Column).Stick_To_Data := Stick_To_Data;

      for J in Info.all'Range loop
         Insert_At_Position (View.Line_Info (Column).Column_Info, Info (J));
      end loop;

      --  If some of the data was in the display range, draw it.

      Redraw_Columns (View);
   end Add_File_Information;

   -------------------------------
   -- Get_Column_For_Identifier --
   -------------------------------

   procedure Get_Column_For_Identifier
     (View       : access Source_View_Record;
      Identifier : String;
      Width      : Integer;
      Column     : out Integer)
   is
   begin
      for J in View.Line_Info.all'Range loop
         if View.Line_Info (J).Identifier.all = Identifier then
            Column := J;

            if View.Line_Info (J).Width < Width then
               for K in (J + 1) .. View.Line_Info.all'Last loop
                  View.Line_Info (K).Starting_X :=
                    View.Line_Info (K).Starting_X + Width
                    - View.Line_Info (J).Width;
               end loop;

               View.Total_Column_Width :=
                 View.Total_Column_Width + Width
                 - View.Line_Info (J).Width;

               Set_Border_Window_Size (View, Enums.Text_Window_Left,
                                       Gint (View.Total_Column_Width));

               View.Line_Info (J).Width := Width;
            end if;

            return;
         end if;
      end loop;

      declare
         A : Line_Info_Display_Array
           (View.Line_Info.all'First .. View.Line_Info.all'Last + 1);
      begin
         A (View.Line_Info.all'First .. View.Line_Info.all'Last)
           := View.Line_Info.all;
         A (View.Line_Info.all'Last + 1) :=
           (Identifier  => new String' (Identifier),
            Starting_X  => View.Total_Column_Width + 2,
            Width       => Width,
            Column_Info => Null_List,
            Stick_To_Data => True);
         View.Line_Info := new Line_Info_Display_Array' (A);
         Column := View.Line_Info.all'Last;

         View.Total_Column_Width := View.Total_Column_Width + Width + 2;

         Set_Border_Window_Size (View, Enums.Text_Window_Left,
                                 Gint (View.Total_Column_Width));
      end;
   end Get_Column_For_Identifier;

   ---------------
   -- Add_Lines --
   ---------------

   procedure Add_Lines
     (View   : access Source_View_Record'Class;
      Start  : Integer;
      Number : Integer)
   is
      Node : List_Node;
      Info : Line_Information_Access;
   begin
      if Number <= 0 then
         return;
      end if;

      for J in View.Line_Info.all'Range loop
         if View.Line_Info (J).Stick_To_Data then
            Node := First (View.Line_Info (J).Column_Info);

            while Node /= Null_Node
              and then Data (Node).Line <= Start
            loop
               Node := Next (Node);
            end loop;

            while Node /= Null_Node loop
               Info := Data (Node);
               Info.Line := Info.Line + Number;
               Node := Next (Node);
            end loop;
         end if;
      end loop;
   end Add_Lines;

   ------------------
   -- Remove_Lines --
   ------------------

   procedure Remove_Lines
     (View       : access Source_View_Record'Class;
      Start_Line : Integer;
      End_Line   : Integer)
   is
      Node       : List_Node;
      First_Node : List_Node := Null_Node;
      Info       : Line_Information_Access;
   begin
      if End_Line <= Start_Line then
         return;
      end if;

      for J in View.Line_Info.all'Range loop
         if View.Line_Info (J).Stick_To_Data then
            First_Node := Null_Node;
            Node := First (View.Line_Info (J).Column_Info);

            while Node /= Null_Node
              and then Data (Node).Line < Start_Line
              and then Data (Node).Line <= End_Line
            loop
               Node := Next (Node);
            end loop;

            if Node /= Null_Node
              and then Data (Node).Line <= End_Line
            then
               First_Node := Node;

               --  Find the end node.
               while Node /= Null_Node
                 and then Next (Node) /= Null_Node
                 and then Data (Next (Node)).Line <= End_Line
               loop
                  Node := Next (Node);
               end loop;

               if First_Node = Node then
                  First_Node := Prev (View.Line_Info (J).Column_Info,
                                      First_Node);
               end if;

               Remove_Nodes (View.Line_Info (J).Column_Info,
                             First_Node,
                             Node);

            end if;

            if First_Node /= Null_Node then
               Node := Next (First_Node);
            end if;

            while Node /= Null_Node loop
               Info := Data (Node);
               Info.Line := Info.Line + Start_Line - End_Line;
               Node := Next (Node);
            end loop;
         end if;
      end loop;
   end Remove_Lines;

   ----------
   -- Free --
   ----------

   procedure Free (X : in out Line_Information_Access)
   is
      procedure Free is new Unchecked_Deallocation
        (Line_Information_Record, Line_Information_Access);
   begin
      Free (X.all);
      Free (X);
   end Free;

end Src_Editor_View;
