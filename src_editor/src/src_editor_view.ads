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

--  <description>
--  This package provides a high-level text view extended to support
--  many functionalities related to source code editing.
--  </description>

with Cairo;                  use Cairo;
with Glib.Main;
with Gdk.Color;
with Gdk.Event;
with Glib;                   use Glib;
with Gtk.Drawing_Area;
with Gtk.Scrolled_Window;
with Gtk.Text_Iter;
with Gtk.Text_Mark;          use Gtk.Text_Mark;
with Gtk.Handlers;
with Gtkada.Style;
with Gtkada.Text_View;       use Gtkada.Text_View;

with GPS.Editors;            use GPS.Editors;
with GPS.Kernel;
with GPS.Kernel.MDI;
with GPS.Kernel.Preferences; use GPS.Kernel.Preferences;
with Src_Editor_Buffer;

with Completion_Window;      use Completion_Window;

package Src_Editor_View is

   type Source_View_Record is new Gtkada_Text_View_Record with private;
   type Source_View is access all Source_View_Record'Class;

   procedure Gtk_New
     (View   : out Source_View;
      Scroll : Gtk.Scrolled_Window.Gtk_Scrolled_Window;
      Area   : Gtk.Drawing_Area.Gtk_Drawing_Area;
      Buffer : Src_Editor_Buffer.Source_Buffer;
      Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
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
      Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Internal initialization procedure.
   --  See the section "Creating your own widgets" in the documentation.

   procedure Scroll_To_Cursor_Location
     (View      : access Source_View_Record;
      Centering : Centering_Type := Minimal);
   --  Scroll the Source View if the position of the insert cursor is not
   --  within the part of the text currently visible.

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
   --  Free memory associated to View

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

   procedure Set_Child
     (View  : access Source_View_Record;
      Child : GPS.Kernel.MDI.GPS_MDI_Child);
   --  Inform View that it is being contained in Child

   procedure Acquire_Focus (View : access Source_View_Record);
   --  Get the MDI focus on the view

   procedure Save_Cursor_Position (View : access Source_View_Record'Class);
   --  Save the cursor position

   procedure Get_Cursor_Position
     (View : access Source_View_Record'Class;
      Iter : out Gtk.Text_Iter.Gtk_Text_Iter);
   --  Return the cursor location in that view

   procedure Start_Completion
     (View : access Source_View_Record'Class;
      Win  : Completion_Window_Access);
   procedure End_Completion (View : access Source_View_Record'Class);
   --  Inform the view that autocompletion is starting/ending

   function In_Completion
     (View : access Source_View_Record'Class) return Boolean;
   --  Return whether the view is currently completing

   procedure Stop_Selection_Drag (View : access Source_View_Record'Class);
   --  Stop the selection drag if it's currently in done

   function As_Is_Enabled
     (View : access Source_View_Record'Class) return Boolean;
   pragma Inline (As_Is_Enabled);
   --  Return true is the view is currently in as-is mode (no autocasing for
   --  next character or sticky as-is mode).

   procedure Reset_As_Is_Mode (View : access Source_View_Record'Class);
   --  Set As_Mode to false

private

   type As_Is_Status is (Disabled, Enabled, Sticky_Enabled);

   type Source_View_Record is new Gtkada_Text_View_Record with record
      Scroll              : Gtk.Scrolled_Window.Gtk_Scrolled_Window := null;
      --  The Gtk_Scrolled_Window that contains the source view

      Area                : Gtk.Drawing_Area.Gtk_Drawing_Area;
      --  The drawing area used for the speed column

      Kernel              : GPS.Kernel.Kernel_Handle;
      Saved_Cursor_Mark   : Gtk_Text_Mark;

      Current_Line_Color  : Gdk.Color.Gdk_Color;

      Highlight_Current   : Boolean := False;
      Highlight_As_Line   : Boolean := False;

      Top_Line            : Src_Editor_Buffer.Buffer_Line_Type := 1;
      Bottom_Line         : Src_Editor_Buffer.Buffer_Line_Type := 0;

      Buffer_Top_Line     : Src_Editor_Buffer.Buffer_Line_Type := 1;
      Buffer_Bottom_Line  : Src_Editor_Buffer.Buffer_Line_Type := 0;
      Buffer_Column_Size  : Gint := 1;
      Side_Column_Buffer  : Cairo_Surface := Null_Surface;
      --  Cache for avoiding to redraw the side columns too often

      Side_Columns_Up_To_Date : Boolean := False;

      Connect_Expose_Id : Glib.Main.G_Source_Id := 0;
      --  Handler ID for the Connect_Expose idle callback
      Connect_Expose_Registered : Boolean := False;
      --  Whether the Connect_Expose idle callback has been registered

      Idle_Redraw_Id : Glib.Main.G_Source_Id := 0;
      --  Handler ID for Idle redraw of the side columns
      Idle_Redraw_Registered : Boolean := False;
      --  Whether the Idle_Redraw has been registered

      Width_Of_256_Chars  : Gint;

      Current_Block_Color : Gdk.Color.Gdk_Color;

      Highlight_Blocks    : Boolean := False;
      --  Whether source blocks should be highlighted

      Current_Line        : Gint := -1;
      --  The line that contains the cursor

      Current_Block       : Src_Editor_Buffer.Block_Record;
      --  Cache used to prevent redrawing the whole buffer when the cursor
      --  doesn't leave the current block.

      Scrolling           : Boolean := False;
      --  Whether the editor is currently scrolling. Used as a flag to avoid
      --  loops in circuitries when using synchronized scrolling.

      Synchronized_Editor : Source_View := null;
      --  An editor which should have a scrolling synchronized with this
      --  editor.

      Speed_Column_Buffer  : Cairo_Surface := Null_Surface;
      --  Cache for avoiding to redraw the speed column too often

      Speed_Column_Mode    : Speed_Column_Policies := Automatic;
      --  The display policy for the speed column

      Speed_Column_Hide_Timeout : Glib.Main.G_Source_Id := 0;
      --  The timeout to hide the speed column

      Speed_Column_Hide_Registered : Boolean := False;
      --  Whether the timeout to hide the speed column has been registered

      Scroll_Timeout       : Glib.Main.G_Source_Id := 0;
      Scroll_To_Value      : Gdouble := 0.0;
      Scroll_Requested     : Boolean := False;

      Background_Color       : Gdk.Color.Gdk_Color := Gdk.Color.Null_Color;
      Background_Color_Other : Gtkada.Style.Cairo_Color := (others => 0.0);
      --  The editor background color and its ligthened/darkened version

      Text_Color           : Gdk.Color.Gdk_Color := Gdk.Color.Null_Color;
      --  The editor text color

      Button_Pressed       : Boolean := False;
      --  Whether the button 1 is pressed

      Double_Click         : Boolean := False;
      --  Whether a double-click on button 1 was performed

      As_Is_Mode           : As_Is_Status := Disabled;
      --  Set to True when the as-is-key has been pressed, in this case the
      --  indentation and casing are disabled for the next key.

      Button_Event         : Gdk.Event.Gdk_Event;
      --  Stores a copy of the last button press event for button 1

      Cursor_Position      : Gdouble := Gdouble'Last;
      --  Stores the cursor position relative to the screen

      Child                : GPS.Kernel.MDI.GPS_MDI_Child := null;
      --  The child that contains Editor

      Completion_Window    : Completion_Window_Access;
      --  The current completion window

      Redraw_Registered : Boolean := False;
      --  Whether we have registered an idle redraw of the highlights

      Redraw_Idle_Handler : Glib.Main.G_Source_Id;
      --  The idle handler corresponding to Redraw_Registered

      --  Handling of hyper mode

      Hyper_Mode                   : Boolean := False;

      Hyper_Mode_Motion_Handler    : Gtk.Handlers.Handler_Id :=
                                       (Gtk.Handlers.Null_Handler_Id, null);
      --  The handler id for the callback that reacts to the motion

      Hyper_Mode_Button_Handler    : Gtk.Handlers.Handler_Id :=
                                       (Gtk.Handlers.Null_Handler_Id, null);
      --  The handler id for the callback that reacts to mouse button presses

      Cursor_Needs_Change          : Boolean := False;
      --  Whether we just entered hyper mode, and the cursor aspect needs
      --  to be changed.
   end record;

end Src_Editor_View;
