-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                   Copyright (C) 2001-2004                         --
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

--  <description>
--  This package provides a high-level text view extended to support
--  many functionalities related to source code editing.
--  </description>

with Glib; use Glib;
with Gdk.GC;
with Gdk.Color;
with Gdk.Event;
with Gdk.Pixmap;

with Pango.Font;

with Gtk.Drawing_Area;
with Gtk.Text_View;
with Gtk.Scrolled_Window;
with Gtk.Main;

with Glide_Kernel;
with Src_Editor_Buffer;
with Gtk.Text_Mark; use Gtk.Text_Mark;

with Glide_Kernel.Preferences; use Glide_Kernel.Preferences;

package Src_Editor_View is

   type Source_View_Record is new Gtk.Text_View.Gtk_Text_View_Record
     with private;
   type Source_View is access all Source_View_Record'Class;

   procedure Gtk_New
     (View   : out Source_View;
      Scroll : Gtk.Scrolled_Window.Gtk_Scrolled_Window;
      Area   : Gtk.Drawing_Area.Gtk_Drawing_Area;
      Buffer : Src_Editor_Buffer.Source_Buffer;
      Kernel : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Create a new Source_View from the given parameters.
   --  If no Buffer is given, then a new one will be created. For tasks such
   --  as source code edition, it is recommended to specify a fixed-width font,
   --  as the default font used when not specified is proportional (which means
   --  that 'i's will be smaller than 'm's for instance).
   --
   --  Scroll is the scrolled window that contains the text view, if any.
   --
   --  If requested, the line numbers are displayed in a small area on
   --  the left of the text view.

   procedure Initialize
     (View   : access Source_View_Record;
      Scroll : Gtk.Scrolled_Window.Gtk_Scrolled_Window;
      Area   : Gtk.Drawing_Area.Gtk_Drawing_Area;
      Buffer : Src_Editor_Buffer.Source_Buffer;
      Kernel : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Internal initialization procedure.
   --  See the section "Creating your own widgets" in the documentation.

   procedure Scroll_To_Cursor_Location
     (View   : access Source_View_Record;
      Center : Boolean := False);
   --  Scroll the Source View if the position of the insert cursor is not
   --  within the part of the text currently visible.
   --  If Center is True, the view will scroll so that the cursor line is
   --  in the middle, otherwise only a minimal scrolling is performed.

   procedure Center_Cursor (View : access Source_View_Record);
   --  Place the cursor within a small margin of the border of the view. This
   --  will scroll the view, if needed, to show a small context of text around
   --  the cursor.

   procedure Window_To_Buffer_Coords
     (View          : access Source_View_Record;
      X, Y          : Gint;
      Line          : out Gint;
      Column        : out Gint;
      Out_Of_Bounds : out Boolean);
   --  Translate the window coordinates (X, Y) into a Line/Column
   --  position in the buffer of the given Source_View_Record.
   --  If X, Y is outside the text area (for instance too far to the right of
   --  a line), then Line and Column are set to the closest matching position,
   --  and Out_Of_Bounds is set to True.

   procedure Event_To_Buffer_Coords
     (View          : access Source_View_Record;
      Event         : Gdk.Event.Gdk_Event;
      Line          : out Gint;
      Column        : out Gint;
      Out_Of_Bounds : out Boolean);
   --  Translate the window coordinates of the Event into a Line/Column
   --  position in the buffer of the given Source_View_Record.
   --  If X, Y is outside the text area (for instance too far to the right of
   --  a line), then Line and Column are set to the closest matching position,
   --  and Out_Of_Bounds is set to True.

   procedure Delete (View : access Source_View_Record);
   --  Free memory associated to View.

   procedure Set_Synchronized_Editor
     (View  : access Source_View_Record;
      Other : Source_View);
   --  Set the synchronized editor.
   --  In order to synchronize two editors, call
   --     Set_Synchronized_Editor (A, B);
   --  and
   --     Set_Synchronized_Editor (B, A);
   --  In order to synchronize N editors, it is necessary to synchronize
   --  them in a loop.
   --  When one editor is closed, all the other editors that were synchronized
   --  with it are no longer synchronized between themselves.

private

   type Source_View_Record is new Gtk.Text_View.Gtk_Text_View_Record
   with record
      Scroll              : Gtk.Scrolled_Window.Gtk_Scrolled_Window := null;
      --  The Gtk_Scrolled_Window that contains the source view.

      Area                : Gtk.Drawing_Area.Gtk_Drawing_Area;
      --  The drawing area used for the speed column.

      Kernel              : Glide_Kernel.Kernel_Handle;

      Saved_Cursor_Mark   : Gtk_Text_Mark;

      Pango_Font          : Pango.Font.Pango_Font_Description;
      Side_Column_GC      : Gdk.GC.Gdk_GC;
      Side_Background_GC  : Gdk.GC.Gdk_GC;
      Default_GC          : Gdk.GC.Gdk_GC;
      Current_Line_GC     : Gdk.GC.Gdk_GC;
      Highlight_Current   : Boolean := False;

      Top_Line            : Src_Editor_Buffer.Buffer_Line_Type := 1;
      Bottom_Line         : Src_Editor_Buffer.Buffer_Line_Type := 0;

      Buffer_Top_Line     : Src_Editor_Buffer.Buffer_Line_Type := 1;
      Buffer_Bottom_Line  : Src_Editor_Buffer.Buffer_Line_Type := 0;
      Buffer_Column_Size  : Gint := 1;
      Side_Column_Buffer  : Gdk.Pixmap.Gdk_Pixmap;
      --  Cache for avoiding to redraw the side columns too often.

      Side_Columns_Up_To_Date : Boolean := False;

      Connect_Expose_Id : Gtk.Main.Idle_Handler_Id := 0;
      --  Handler ID for the Connect_Expose idle callback.
      Connect_Expose_Registered : Boolean := False;
      --  Whether the Connect_Expose idle callback has been registered.

      Idle_Redraw_Id : Gtk.Main.Idle_Handler_Id := 0;
      --  Handler ID for Idle redraw of the side columns.
      Idle_Redraw_Registered : Boolean := False;
      --  Whether the Idle_Redraw has been registered.

      Has_Focus           : Boolean := False;
      --  Whether the view currently has the focus.

      Char_Width          : Gint;

      Current_Block_GC    : Gdk.GC.Gdk_GC;
      Highlight_Blocks    : Boolean := False;
      --  Whether source blocks should be highlighted.

      Current_Block       : Src_Editor_Buffer.Block_Record;
      --  Cache used to prevent redrawing the whole buffer when the cursor
      --  doesn't leave the current block.

      Scrolling           : Boolean := False;
      --  Whether the editor is currently scrolling. Used as a flag to avoid
      --  loops in circuitries when using synchronized scrolling.

      Synchronized_Editor : Source_View := null;
      --  An editor which should have a scrolling synchronized with this
      --  editor.

      Speed_Column_Buffer  : Gdk.Pixmap.Gdk_Pixmap;
      --  Cache for avoiding to redraw the speed column too often.

      Speed_Column_Mode    : Speed_Column_Policies := Automatic;
      --  The display policy for the speed column.

      Speed_Column_Hide_Timeout : Gtk.Main.Timeout_Handler_Id := 0;
      --  The timeout to hide the speed column.

      Speed_Column_Hide_Registered : Boolean := False;
      --  Whether the timeout to hide the speed column has been registered.

      Scroll_Timeout       : Gtk.Main.Timeout_Handler_Id := 0;
      Scroll_To_Value      : Gdouble := 0.0;
      Scroll_Requested     : Boolean := False;

      Background_Color     : Gdk.Color.Gdk_Color := Gdk.Color.Null_Color;
      --  The editor background color.

      Text_Color           : Gdk.Color.Gdk_Color := Gdk.Color.Null_Color;
      --  The editor text color.

      Button_Pressed       : Boolean := False;
      --  Whether the button 1 is pressed.

      Button_Event         : Gdk.Event.Gdk_Event;
      --  Stores a copy of the last button press event for button 1.
   end record;

end Src_Editor_View;
