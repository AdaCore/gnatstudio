-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--                     Copyright (C) 2001                            --
--                         ACT-Europe                                --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

with Glib;                        use Glib;
with Gdk;                         use Gdk;
with Gdk.Drawable;                use Gdk.Drawable;
with Gdk.Event;                   use Gdk.Event;
with Gdk.Font;                    use Gdk.Font;
with Gdk.GC;                      use Gdk.GC;
with Gdk.Window;                  use Gdk.Window;
with Gdk.Rectangle;               use Gdk.Rectangle;
with Gtk;                         use Gtk;
with Gtk.Enums;                   use Gtk.Enums;
with Gtk.Handlers;                use Gtk.Handlers;
with Gtk.Text_Attributes;         use Gtk.Text_Attributes;
with Gtk.Text_Buffer;             use Gtk.Text_Buffer;
with Gtk.Text_Iter;               use Gtk.Text_Iter;
with Gtk.Text_Layout;             use Gtk.Text_Layout;
with Gtk.Text_Mark;               use Gtk.Text_Mark;
with Gtk.Widget;                  use Gtk.Widget;
with Src_Editor;                  use Src_Editor;
with Src_Editor_Buffer;           use Src_Editor_Buffer;
with Src_Editor_Defaults;         use Src_Editor_Defaults;

package body Src_Editor_View is

   use type Pango.Font.Pango_Font_Description;

   Minimal_Number_Of_Digits_In_LNA : constant := 3;
   --  Minimal number of digits for a line number that the Line Numbers Area
   --  (LNA) should be able to diplay.

   LNA_Border_Width : constant := 4;
   --  The number of pixels between the the LNA borders and the line number.

   package Source_View_Callback is new Gtk.Handlers.Callback
     (Widget_Type => Source_View_Record);

   package Source_View_Return_Callback is new Gtk.Handlers.Return_Callback
     (Widget_Type => Source_View_Record,
      Return_Type => Boolean);

   package Source_Buffer_Callback is new Gtk.Handlers.User_Callback
     (Widget_Type => Source_Buffer_Record,
      User_Type => Source_View);

   --------------------------
   -- Forward declarations --
   --------------------------

   procedure Realize_Cb (View : access Source_View_Record'Class);
   --  This procedure is invoked when the Source_View widget is realized.
   --  It performs various operations that can not be done before the widget
   --  is realized, such as setting the default font or the left border window
   --  size for instance.

   function Expose_Event_Cb
     (View  : access Source_View_Record'Class;
      Event : Gdk_Event) return Boolean;
   --  This procedure handles all expose events happening on the left border
   --  window. It will the redraw the exposed area (this window may contains
   --  things such as line number, breakpoint icons, etc).

   function Focus_Out_Event_Cb
     (View   : access Source_View_Record'Class;
      Event  : Gdk.Event.Gdk_Event_Focus)
     return Boolean;
   --  Save the current insert cursor position before the Source_View looses
   --  the focus. This will allow us to restore it as soon as the focus is
   --  gained back.

   function Focus_In_Event_Cb
     (View   : access Source_View_Record'Class;
      Event  : Gdk.Event.Gdk_Event_Focus)
     return Boolean;
   --  Restore the previously saved insert cursor position when the Source_View
   --  gains the focus back.

   procedure Map_Cb (View : access Source_View_Record'Class);
   --  This procedure is invoked when the Source_View widget is mapped.
   --  It performs various operations that can not be done before the widget
   --  is mapped, such as creating GCs associated to the left border window
   --  for instance.

   procedure Modified_Cb
     (Buffer : access Source_Buffer_Record'Class;
      View   : Source_View);
   --  This procedure re-evaluates the number of lines in the buffer, and
   --  changes the width of the LNA if necessary.

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

   procedure Redraw_Line_Numbers
     (View        : access Source_View_Record'Class;
      Left_Window : Gdk.Window.Gdk_Window;
      Area        : Gdk_Rectangle);
   --  Redraw the line numbers for Area of the given Left_Window.

   ----------------
   -- Realize_Cb --
   ----------------

   procedure Realize_Cb (View : access Source_View_Record'Class) is
   begin
      --  Now that the window is realized, we can set the font and
      --  the size of the left border window size.
      Set_Font (View, View.Pango_Font);
      Reset_Left_Border_Window_Size (View);
   end Realize_Cb;

   ---------------------
   -- Expose_Event_Cb --
   ---------------------

   function Expose_Event_Cb
     (View  : access Source_View_Record'Class;
      Event : Gdk_Event) return Boolean
   is
      Left_Window : constant Gdk.Window.Gdk_Window :=
        Get_Window (View, Text_Window_Left);
      Area : constant Gdk_Rectangle := Get_Area (Event);
   begin
      --  If the event applies to the left border window, then redraw
      --  the line numbers.
      if Get_Window (Event) = Left_Window then
         Redraw_Line_Numbers (View, Left_Window, Area);
      end if;

      --  Return false, so that the signal is not blocked, and other
      --  clients can use it.
      return False;
   end Expose_Event_Cb;

   ------------------------
   -- Focus_Out_Event_Cb --
   ------------------------

   function Focus_Out_Event_Cb
     (View   : access Source_View_Record'Class;
      Event  : Gdk.Event.Gdk_Event_Focus) return Boolean
   is
      Buffer : constant Source_Buffer := Source_Buffer (Get_Buffer (View));
      Insert_Iter : Gtk_Text_Iter;
   begin
      --  Save the current insert cursor position by moving the
      --  Saved_Insert_Mark to the location where the "insert" mark
      --  currently is.
      Get_Iter_At_Mark (Buffer, Insert_Iter, Get_Insert (Buffer));
      Move_Mark (Buffer, View.Saved_Insert_Mark, Insert_Iter);
      return False;
   end Focus_Out_Event_Cb;

   -----------------------
   -- Focus_In_Event_Cb --
   -----------------------

   function Focus_In_Event_Cb
     (View   : access Source_View_Record'Class;
      Event  : Gdk.Event.Gdk_Event_Focus) return Boolean
   is
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

   procedure Map_Cb (View : access Source_View_Record'Class) is
   begin
      --  Now that the Source_View is mapped, we can create the Graphic
      --  Context used for writting line numbers.
      Gdk_New (View.Line_Numbers_GC, Get_Window (View, Text_Window_Left));
   end Map_Cb;

   -----------------
   -- Modified_Cb --
   -----------------

   procedure Modified_Cb
     (Buffer : access Source_Buffer_Record'Class;
      View   : Source_View)
   is
      New_Number_Of_Digits : constant Natural := LNA_Width_In_Digits (View);
   begin
      if View.Show_Line_Numbers
        and then New_Number_Of_Digits /= View.LNA_Width_In_Digits
      then
         Reset_Left_Border_Window_Size (View);
      end if;
   end Modified_Cb;

   -------------------------
   -- LNA_Width_In_Digits --
   -------------------------

   function LNA_Width_In_Digits
     (View : access Source_View_Record'Class) return Natural
   is
      Line_Count : constant Gint := Get_Line_Count (Get_Buffer (View));
      Max_Number_Of_Digits : constant Natural :=
        Natural'Max
          (Number_Of_Digits_In (Natural (Line_Count)),
           Minimal_Number_Of_Digits_In_LNA);
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

   -------------------------
   -- Redraw_Line_Numbers --
   -------------------------

   procedure Redraw_Line_Numbers
     (View        : access Source_View_Record'Class;
      Left_Window : Gdk.Window.Gdk_Window;
      Area        : Gdk_Rectangle)
   is
      Top : constant Gint := Area.Y;
      Bottom : constant Gint := Top + Area.Height;
      Number_Of_Lines : constant Gint := Get_Line_Count (Get_Buffer (View));
      Line_Number_Image_Length : constant Natural :=
        Natural'Max
          (Number_Of_Digits_In (Natural (Number_Of_Lines)),
           Minimal_Number_Of_Digits_In_LNA);
      Top_In_Buffer : Gint;
      Bottom_In_Buffer : Gint;
      Iter : Gtk_Text_Iter;
      Y_In_Buffer : Gint;
      Y_In_Window : Gint;
      Line_Height : Gint;
      Line_Number : Gint;
      Dummy_Gint : Gint;
      Dummy_Boolean : Boolean;
   begin
      --  First, convert the window coords into buffer coords
      Window_To_Buffer_Coords
        (View, Text_Window_Left,
         Window_X => 0, Window_Y => Top,
         Buffer_X => Dummy_Gint, Buffer_Y => Top_In_Buffer);
      Window_To_Buffer_Coords
        (View, Text_Window_Left,
         Window_X => 0, Window_Y => Bottom,
         Buffer_X => Dummy_Gint, Buffer_Y => Bottom_In_Buffer);

      --  Initialize the iterator on the first line number to redraw
      Get_Line_At_Y (View, Iter, Top_In_Buffer, Dummy_Gint);

      --  Redraw each exposed line...
      Drawing_Loop :
      while not Is_End (Iter) loop
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
           Y_In_Window + Get_Ascent (View.Font) + Get_Descent (View.Font) - 1;
         --  ??? Verify that Get_Ascent and Get_Descent do not cause a
         --  ??? X11 query. Otherwise, cache it in the source_view object.

         --  Get the line number...
         Line_Number := Get_Line (Iter) + 1;
         --  ... and draw.
         Draw_Text
           (Drawable => Left_Window,
            Font => View.Font,
            Gc => View.Line_Numbers_GC,
            X => LNA_Border_Width,
            Y => Y_In_Window,
            Text => Image (Line_Number, Line_Number_Image_Length));

         exit Drawing_Loop when Y_In_Buffer + Line_Height >= Bottom_In_Buffer;
         Forward_Line (Iter, Dummy_Boolean);
      end loop Drawing_Loop;

   end Redraw_Line_Numbers;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (View              : out Source_View;
      Buffer            : Src_Editor_Buffer.Source_Buffer := null;
      Font              : Pango.Font.Pango_Font_Description := null;
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

      if Font = null then
         View.Pango_Font := Pango.Font.From_String (Default_Font_Description);
      else
         View.Pango_Font := Font;
      end if;
      Gdk.Font.From_Description (View.Font, View.Pango_Font);
      View.Show_Line_Numbers := Show_Line_Numbers;
      View.LNA_Width_In_Digits := Minimal_Number_Of_Digits_In_LNA;

      Source_View_Callback.Connect
        (View, "realize",
         Marsh => Source_View_Callback.To_Marshaller (Realize_Cb'Access),
         After => True);
      Source_View_Callback.Connect
        (View, "map",
         Marsh => Source_View_Callback.To_Marshaller (Map_Cb'Access),
         After => True);
      Source_View_Return_Callback.Connect
        (View, "expose_event",
         Marsh => Source_View_Return_Callback.To_Marshaller
                    (Expose_Event_Cb'Access),
         After => False);
      Source_Buffer_Callback.Connect
        (Buffer, "changed",
         Marsh => Source_Buffer_Callback.To_Marshaller (Modified_Cb'Access),
         User_Data => Source_View (View),
         After => True);
      Source_View_Return_Callback.Connect
        (View, "focus_in_event",
         Marsh => Source_View_Return_Callback.To_Marshaller
                    (Focus_In_Event_Cb'Access),
         After => False);
      Source_View_Return_Callback.Connect
        (View, "focus_out_event",
         Marsh => Source_View_Return_Callback.To_Marshaller
                    (Focus_Out_Event_Cb'Access),
         After => False);

   end Initialize;

   --------------
   -- Set_Font --
   --------------

   procedure Set_Font
     (View : access Source_View_Record;
      Font : Pango.Font.Pango_Font_Description)
   is
      View_Layout : Text_Layout.Gtk_Text_Layout;
      View_Style : Text_Attributes.Gtk_Text_Attributes;
   begin
      View.Pango_Font := Font;
      Gdk.Font.From_Description (View.Font, Font);

      --  Make sure the widget is already realized. Otherwise, the
      --  layout and style are not created yet.
      if not Realized_Is_Set (View) then
         return;
         --  ??? We should probably log a warning...
      end if;
      View_Layout := Get_Layout (View);
      View_Style := Text_Layout.Get_Default_Style (View_Layout);
      Text_Attributes.Set_Font (Text_Attr => View_Style, Font => Font);
      Text_Layout.Default_Style_Changed (View_Layout);
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
      Scroll_To_Mark
        (View, Insert_Mark, Use_Align => False,
         Within_Margin => 0.0, Xalign => 0.5, Yalign => 0.5);
   end Scroll_To_Cursor_Location;

end Src_Editor_View;
