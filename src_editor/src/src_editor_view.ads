------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2019, AdaCore                     --
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
with Gdk.Event;
with Gdk.RGBA;
with Glib;                   use Glib;
with Gtk.Drawing_Area;
with Gtk.Scrolled_Window;
with Gtk.Text_Iter;
with Gtk.Text_Mark;          use Gtk.Text_Mark;
with Gtk.Text_View;          use Gtk.Text_View;
with Gtk.Handlers;
with Gtkada.Style;

with GNATCOLL.Projects;      use GNATCOLL.Projects;
with GNATCOLL.VFS;
with GPS.Editors;            use GPS.Editors;
with GPS.Kernel;
with GPS.Kernel.MDI;
with GPS.Kernel.Preferences; use GPS.Kernel.Preferences;
with Src_Editor_Buffer;

with Completion_Window;      use Completion_Window;

package Src_Editor_View is

   type Source_View_Record is new Gtk_Text_View_Record with private;
   type Source_View is access all Source_View_Record'Class;

   procedure Gtk_New
     (View    : out Source_View;
      Project : GNATCOLL.Projects.Project_Type;
      Scroll  : access Gtk.Scrolled_Window.Gtk_Scrolled_Window_Record'Class;
      Area    : Gtk.Drawing_Area.Gtk_Drawing_Area;
      Buffer  : Src_Editor_Buffer.Source_Buffer;
      Kernel  : access GPS.Kernel.Kernel_Handle_Record'Class);
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
   --
   --  Project is the one controlling the file. There might be several
   --  possibilities when using aggregate projects, and we need to know the
   --  exact project to resolve things like cross-references.
   --  If left to No_Project, this function will attempt to compute it directly
   --  from the loaded project.

   procedure Initialize
     (View    : access Source_View_Record;
      Project : GNATCOLL.Projects.Project_Type;
      Scroll  : access Gtk.Scrolled_Window.Gtk_Scrolled_Window_Record'Class;
      Area    : Gtk.Drawing_Area.Gtk_Drawing_Area;
      Buffer  : Src_Editor_Buffer.Source_Buffer;
      Kernel  : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Internal initialization procedure.
   --  See the section "Creating your own widgets" in the documentation.

   procedure Set_Project
     (Self    : not null access Source_View_Record'Class;
      Project : GNATCOLL.Projects.Project_Type := No_Project);
   function Get_Project
     (Self : not null access Source_View_Record'Class)
      return GNATCOLL.Projects.Project_Type;
   --  Return the project for this editor.
   --  When using aggregate projects, this can be ambiguous since a given file
   --  can be associated with multiple projects. But this information is also
   --  necessary in a number of contexts like cross-references and completion.
   --
   --  Set_Project should not be reserved for internal use in the src_editor
   --  module. Project will be computed automatically if none is given.

   procedure Set_Background_Color
     (Self : not null access Source_View_Record'Class;
      Forced : Gdk.RGBA.Gdk_RGBA := Gdk.RGBA.Null_RGBA);
   --  Update the background color of the window, based on the user preferences
   --  and whether the editor is editable.
   --  If Forced is given, the background will use this color from now on,
   --  until is overridden by another call also passing Forced).

   procedure Scroll_To_Cursor_Location
     (View        : access Source_View_Record;
      Centering   : Centering_Type := Minimal;
      Synchronous : Boolean := True);
   --  Scroll the Source View if the position of the insert cursor is not
   --  within the part of the text currently visible.
   --  If Synchronous is True, the scrolling will occur imediately, which means
   --  that the source view should be properly validated and drawn before
   --  calling this function. When False, the scrolling will occur in an idle
   --  function instead.

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
   function Get_Child
     (View  : access Source_View_Record)
      return GPS.Kernel.MDI.GPS_MDI_Child;
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

   function As_Is_Enabled
     (View : access Source_View_Record'Class) return Boolean;
   pragma Inline (As_Is_Enabled);
   --  Return true is the view is currently in as-is mode (no autocasing for
   --  next character or sticky as-is mode).

   procedure Reset_As_Is_Mode (View : access Source_View_Record'Class);
   --  Set As_Mode to false

   function Position_Set_Explicitely
     (Self   : access Source_View_Record;
      Reset  : Boolean) return Boolean;
   --  Return True if the position of the cursor has been set explicitely (ie
   --  not as a side effect of a text change)
   --  If Reset is true, deactivate the flag saying that the cursor has been
   --  set explicitely: further calls to Position_Set_Explicitely will return
   --  False.

   procedure Set_Position_Set_Explicitely (Self : access Source_View_Record);
   --  Set the flag "Position_Set_Explicitely".
   --  This should only be called when opening an editor or when jumping to
   --  a location. This flag will not do anything on editors that are already
   --  open and scrolled.

   type Location_Type is (Location_Mouse, Location_Cursor, Location_Event);
   function Build_Editor_Context
     (View     : access Source_View_Record;
      Location : Location_Type := Location_Cursor;
      Event    : Gdk.Event.Gdk_Event := null)
      return GPS.Kernel.Selection_Context;
   --  Describe the current editor context, at the specified location. If
   --  Location is Location_Event and no event is specified, the context
   --  for the cursor is returned.

private

   type As_Is_Status is (Disabled, Enabled, Sticky_Enabled);

   type Handlers_Array is array (Natural range <>) of Gtk.Handlers.Handler_Id;

   type Scrolling_Command_Type (To_Cursor : Boolean := False) is record
      case To_Cursor is
         when True =>
            Centering : Centering_Type := Minimal;
         when False =>
            Value     : Gdouble;
      end case;
   end record;

   No_Scroll_Command : constant Scrolling_Command_Type :=
                         Scrolling_Command_Type'(To_Cursor => False,
                                                 Value     => 0.0);

   type Source_View_Record is new Gtk_Text_View_Record with record
      Project_Path : GNATCOLL.VFS.Virtual_File;
      --  The project that this file is from.
      --  We do not cache the Project_Type itself, since that becomes invalid
      --  when the project is recomputed, and we would then have to revert to
      --  the first possible project for that file, which might not be the one
      --  that was originally intended.

      Scroll              : Gtk.Scrolled_Window.Gtk_Scrolled_Window := null;
      --  The Gtk_Scrolled_Window that contains the source view

      Scrollbar_Stepper_Size : Gint := 0;
      --  The size taken on the scrollbar by the stepper and its offset.

      Draw_The_Scrollbar  : Boolean := False;
      --  Whether to do the actual scrollbar drawing

      Area                : Gtk.Drawing_Area.Gtk_Drawing_Area;
      --  The drawing area used for the speed column

      Side_Info_Width     : Gint := 0;
      --  The width of the side info

      Speed_Bar_Width     : Gint := 0;
      --  The width of the speed bar
      Kernel              : GPS.Kernel.Kernel_Handle;
      Saved_Cursor_Mark   : Gtk_Text_Mark;

      Current_Line_Color  : Gdk.RGBA.Gdk_RGBA;

      Highlight_Current   : Boolean := False;
      Highlight_As_Line   : Current_Line_Highlighting_Type := Gutter_Only;

      Cursor_Set_Explicitely : Boolean := False;
      --  True when the user requested to scroll to this position when the
      --  editor was first opened. This is used to scroll to this position in
      --  the callbacks that display the editor.

      Initial_Scroll_Has_Occurred : Boolean := False;
      --  Whether the initial scroll has occurred.
      --  This flag, in cunjunction with Cursor_Set_Explicitely above, are used
      --  to make sure that a newly-created editor will scroll to the given
      --  location when it is first opened, but keep the user location
      --  at other times.

      Top_Line            : Src_Editor_Buffer.Buffer_Line_Type := 1;
      Bottom_Line         : Src_Editor_Buffer.Buffer_Line_Type := 0;

      Buffer_Column_Size  : Gint := 1;

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

      Current_Block_Color : Gdk.RGBA.Gdk_RGBA;

      Highlight_Blocks    : Boolean := False;
      --  Whether source blocks should be highlighted

      Current_Line        : Gint := 0;
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

      Scroll_Timeout       : Glib.Main.G_Source_Id := 0;
      Scroll_Command       : Scrolling_Command_Type := No_Scroll_Command;

      Forced_Bg_Color       : Boolean := False;
      Background_Color      : Gtkada.Style.Cairo_Color := (0.0, 0.0, 0.0, 1.0);
      --  The editor background color and its ligthened/darkened version.
      --  Forced_Bg_Color is true if the python's set_background_color was
      --  called.

      Text_Color           : Gdk.RGBA.Gdk_RGBA := Gdk.RGBA.Null_RGBA;
      --  The editor text color

      Button_Pressed       : Boolean := False;
      --  Whether the button 1 is pressed

      Double_Click         : Boolean := False;
      --  Whether a double-click on button 1 was performed

      As_Is_Mode           : As_Is_Status := Disabled;
      --  Set to True when the as-is-key has been pressed, in this case the
      --  indentation and casing are disabled for the next key.

      Cursor_Position      : Gdouble := Gdouble'Last;
      --  Stores the cursor position relative to the screen

      Child                : GPS.Kernel.MDI.GPS_MDI_Child := null;
      --  The child that contains Editor

      Completion_Window    : Completion_Window_Access;
      --  The current completion window

      Redraw_Registered    : Boolean := False;
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

      Source_Buffer_Handlers       : Handlers_Array (1 .. 5);
   end record;

end Src_Editor_View;
