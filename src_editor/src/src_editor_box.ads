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

--  <description>
--  This package provides an object that represents the source editor.
--  The source editor is composed of the following entities:
--    - A Source_View, with vertical and horizontal scrollbars
--    - A status bar at the bottom containing
--        - the current file name
--        - the line and column number of the insert cursor
--  </description>

with Glib;
with Glib.Object;
with Gdk.Rectangle;
with Gdk.GC;
with Gdk.Event;

with Gtk.Box;
with Gtk.Container;
with Gtk.Frame;
with Gtk.Label;
with Gtk.Menu;
with Gtk.Handlers;
with Gtk.Text_Mark;

with GVD.Tooltips;
with Language;
with Language_Handlers;
with Glide_Kernel;
with Glide_Kernel.Modules;
with Src_Editor_Buffer;
with Src_Editor_View;
with VFS;

with Ada.Unchecked_Deallocation;

package Src_Editor_Box is

   type Source_Editor_Box_Record is new Glib.Object.GObject_Record
     with private;
   type Source_Editor_Box is access all Source_Editor_Box_Record;

   procedure Gtk_New
     (Box    : out Source_Editor_Box;
      Kernel : Glide_Kernel.Kernel_Handle;
      Lang   : Language.Language_Access := null);
   --  Create a new Source_Editor_Box. It must be destroyed after use
   --  (see procedure Destroy below).

   procedure Initialize
     (Box    : access Source_Editor_Box_Record;
      Kernel : Glide_Kernel.Kernel_Handle;
      Lang   : Language.Language_Access);
   --  Initialize the newly created Source_Editor_Box.

   procedure Create_New_View
     (Box    : out Source_Editor_Box;
      Kernel : access Glide_Kernel.Kernel_Handle_Record'Class;
      Source : access Source_Editor_Box_Record);
   --  Create a new view of the given box.
   --  ??? Do we want to copy the font attributes as well, or do we want
   --  to add another parameter?

   procedure Destroy (Box : in out Source_Editor_Box);
   --  Destroy the given Source_Editor_Box, then set it to null.

   procedure Attach
     (Box    : access Source_Editor_Box_Record;
      Parent : access Gtk.Container.Gtk_Container_Record'Class);
   --  Attach Box to the given Parent, if possible.

   procedure Detach
     (Box : access Source_Editor_Box_Record);
   --  Detach Box of its Parent, if possible.

   function Get_Kernel
     (Box : access Source_Editor_Box_Record) return Glide_Kernel.Kernel_Handle;
   --  Accessor to the Kernel field.

   function Get_View (Editor : access Source_Editor_Box_Record)
      return Src_Editor_View.Source_View;
   --  Return the source view associated with the box

   function Get_Buffer (Editor : access Source_Editor_Box_Record)
      return Src_Editor_Buffer.Source_Buffer;
   --  Return the source buffer associated with the box

   function Get_Writable
     (Editor : access Source_Editor_Box_Record) return Boolean;
   --  Return whether the Editor is writable.

   ------------------------------------
   -- Source_Buffer related services --
   ------------------------------------

   function Needs_To_Be_Saved
     (Editor : access Source_Editor_Box_Record)
      return Boolean;
   --  Tell if the Editor's buffer is in a non-saved state.

   procedure Set_Filename
     (Editor   : access Source_Editor_Box_Record;
      Filename : VFS.Virtual_File);
   --  Change the filename of the given editor to be Filename.
   --  This doesn't change the file itself, just the name under which it is
   --  referenced.

   function Get_Filename
     (Editor : access Source_Editor_Box_Record) return VFS.Virtual_File;
   --  Return the filename associated the given Editor. Return the empty
   --  string if Editor does not have any filename.

   procedure Set_File_Identifier
     (Editor   : access Source_Editor_Box_Record;
      Filename : VFS.Virtual_File);
   --  Change the file identifier of the given editor to be Filename.

   procedure Load_File
     (Editor          : access Source_Editor_Box_Record;
      Filename        : VFS.Virtual_File;
      Lang_Autodetect : Boolean := True;
      Force_Focus     : Boolean := True;
      Success         : out Boolean);
   --  Load the file into the buffer. If Lang_Autodetect is set to True, then
   --  the editor tries to automatically set the language based on the
   --  Filename. Otherwise, the language remains uchanged. After the file is
   --  loaded into the buffer, the buffer is syntax-highlighted if Lang is set.
   --  Filename is also stored in the Editor.
   --
   --  Note that if Lang_Autodetect is True, and the editor could not guess
   --  the language from the filename, then Lang will be unset, and syntax
   --  highlighting will be deactivated.

   procedure Load_Empty_File
     (Editor          : access Source_Editor_Box_Record;
      Filename        : VFS.Virtual_File;
      Lang_Handler    : Language_Handlers.Language_Handler;
      Lang_Autodetect : Boolean := True);
   --  Similar to Load_File, but assume that Filename is a new file that
   --  is not present on the file system.

   procedure Save_To_File
     (Editor   : access Source_Editor_Box_Record;
      Filename : VFS.Virtual_File := VFS.No_File;
      Success  : out Boolean);
   --  Save the buffer to the given file.
   --  Success is set to false if the buffer could not be saved.
   --  If filename is null, use the filename associated with Editor.

   procedure Set_Language
     (Editor : access Source_Editor_Box_Record;
      Lang   : Language.Language_Access := null);
   --  Change the language of the source editor. If the new language is
   --  not null, then causes the syntax-highlighting to be recomputed.

   function Get_Language
     (Editor : access Source_Editor_Box_Record)
      return Language.Language_Access;
   --  Return the current language.

   function Is_Valid_Location
     (Editor : access Source_Editor_Box_Record;
      Line   : Positive;
      Column : Positive := 1) return Boolean;
   --  Return True if the given cursor location is valid. If Column is set
   --  to 1, then this function just verifies the given line number (Column 1
   --  of a given line always exists).

   procedure Set_Screen_Location
     (Editor      : access Source_Editor_Box_Record;
      Line        : Positive;
      Column      : Positive := 1;
      Force_Focus : Boolean  := True);
   --  Move the insert cursor to the given location, after expanding Tabs.
   --  If Force_Focus is False, then the editor will not grab the focus
   --  before setting the cursor position.

   procedure Set_Cursor_Location
     (Editor      : access Source_Editor_Box_Record;
      Line        : Positive;
      Column      : Positive := 1;
      Force_Focus : Boolean  := True);
   --  Move the insert cursor to the given location. Success is set to False
   --  if the position is outside of the buffer.
   --  If Force_Focus is False, then the editor will not grab the focus
   --  before setting the cursor position.
   --
   --  The position must be verified before invoking this procedure. An invalid
   --  position leads to an Assertion_Failure when compiled with assertion
   --  checks, or to an undefined behavior otherwise.

   procedure Get_Cursor_Location
     (Editor  : access Source_Editor_Box_Record;
      Line    : out Positive;
      Column  : out Positive);
   --  Get the current cursor position.

   procedure Scroll_To_Cursor_Location
     (Editor : access Source_Editor_Box_Record);
   --  Scroll the view so that the given position is visible on the screen.

   procedure Get_Selection_Bounds
     (Editor       : access Source_Editor_Box_Record;
      Start_Line   : out Positive;
      Start_Column : out Positive;
      End_Line     : out Positive;
      End_Column   : out Positive;
      Found        : out Boolean);
   --  If a portion of the buffer is currently selected, then return the
   --  location of the beginning and the end of the selection. Otherwise,
   --  Found is set to False and the location returned both point to the
   --  begining of the buffer.

   function Get_Slice
     (Editor       : access Source_Editor_Box_Record;
      Start_Line   : Positive;
      Start_Column : Positive;
      End_Line     : Natural := 0;
      End_Column   : Natural := 0) return String;
   --  Return the text located between (Start_Line, Start_Column) and
   --  (End_Line, End_Column). The first line is 1, the first column is 1.
   --  If End_Line = 0, contents until the end of the buffer will be retrieved.
   --
   --  The validity of both locations should be verified before invoking this
   --  function. An incorrect location will cause an Assertion_Failure when
   --  compiled with assertion checks, or an undefined behavior otherwise.

   procedure Replace_Slice
     (Editor       : access Source_Editor_Box_Record;
      Start_Line   : Positive;
      Start_Column : Positive;
      End_Line     : Positive;
      End_Column   : Positive;
      Text         : String);
   --  Replace the text between the start and end locations by Text.
   --
   --  The validity of the given locations must be verified before invoking
   --  this procedure. An incorrect location  will cause an Assertion_Failure
   --  when compiled with assertion checks, or an undefined behavior otherwise.

   procedure Select_Region
     (Editor       : access Source_Editor_Box_Record;
      Start_Line   : Positive;
      Start_Column : Positive;
      End_Line     : Positive;
      End_Column   : Positive;
      Expand_Tabs  : Boolean := True);
   --  Select the given region.
   --  Both start and end positions must be verified before calling this
   --  procedure. An incorrect position will cause an Assertion_Failure
   --  when compiled with assertion checks, or an undefined behavior otherwise.
   --  Takes Tabs into account when Expand_Tabs = True.

   procedure Select_All (Editor : access Source_Editor_Box_Record);
   --  Set the selection bounds from the begining to the end of the buffer.

   procedure Cut_Clipboard (Editor : access Source_Editor_Box_Record);
   --  Copy the currently-selected text to the clipboard and then delete it.

   procedure Copy_Clipboard (Editor : access Source_Editor_Box_Record);
   --  Copy the currently-selected text to the clipboard.

   procedure Paste_Clipboard (Editor : access Source_Editor_Box_Record);
   --  Paste the contents of the clipboard.

   --  procedure Paste_Primary (Editor : access Source_Editor_Box_Record);
   --  Paste the contents of the primary selection.
   --  ??? The primary selection is the X11 cut/copy/paste buffer I think.
   --  ??? what is the relationship with the clipboard?
   --  ??? This function is commented out since it was removed in gtk-1.3.7.

   procedure Goto_Declaration_Or_Body
     (Kernel  : access Glide_Kernel.Kernel_Handle_Record'Class;
      To_Body : Boolean;
      Editor  : access Source_Editor_Box_Record'Class;
      Context : access Glide_Kernel.Modules.Entity_Selection_Context'Class);
   --  Perform a Declaration-or-Body cross-reference for the entity
   --  located at (Line, Column) in Editor, or in the current editor if
   --  Editor is null. If To_Body is True, then display the next body of the
   --  entity, otherwise display its declaration.
   --  If either Line or Column is null, then the position of the insert cursor
   --  is used instead. Highlight the entity found, opening a new editor if
   --  needed (this may depend on the user preferences).

   procedure Grab_Focus (Editor : access Source_Editor_Box_Record);
   --  Set the focus on the source view.

   procedure Add_Line_Highlighting
     (Editor : access Source_Editor_Box_Record;
      Line   : Src_Editor_Buffer.Editable_Line_Type;
      Id     : String);
   --  Enable the highlighting of Line using colors defined in category
   --  corresponding to Id.
   --  If Line is 0, highlight all lines in file.

   procedure Remove_Line_Highlighting
     (Editor : access Source_Editor_Box_Record;
      Line   : Src_Editor_Buffer.Editable_Line_Type;
      Id     : String);
   --  Disable the highlighting of Line using colors defined in category
   --  corresponding to Id.
   --  If Line is 0, unhighlight all lines in file.

   function Create_Mark
     (Editor : access Source_Editor_Box_Record;
      Line   : Src_Editor_Buffer.Editable_Line_Type;
      Column : Positive) return Gtk.Text_Mark.Gtk_Text_Mark;
   --  Create a mark at Line, Column in the Editor buffer.
   --  If the position specified by Line, Column, the mark is created
   --  at the beginning of the buffer.

   procedure Scroll_To_Mark
     (Editor : access Source_Editor_Box_Record;
      Mark   : Gtk.Text_Mark.Gtk_Text_Mark;
      Length : Natural := 0);
   --  Scroll Mark onscreen, and place the cursor on Mark.
   --  Lenght is the length of text that should be selected after
   --  Mark.

   procedure On_Goto_Line
     (Editor : access Glib.Object.GObject_Record'Class;
      Kernel : Glide_Kernel.Kernel_Handle);
   --  Navigate->Goto Line... menu

   function Get_Last_Line
     (Editor   : access Source_Editor_Box_Record)
     return Positive;
   --  Return the number of the last line in the file.

   function Get_Buffer
     (Editor : access Source_Editor_Box_Record)
     return String;
   --  Return the contents of the entire buffer.

   procedure Check_Timestamp
     (Editor : access Source_Editor_Box_Record);
   --  Check whether the timestamp corresponds on the file on disk.
   --  If the timestamp is not up to date, then
   --     - if the file was never modified, modify the buffer to match
   --       the contents of the disk
   --     - if the file was modified, bring up a dialog asking
   --       confirmation.

   function Get_Ref_Count
     (Editor : access Source_Editor_Box_Record)
      return Integer;
   --  Return the total number of current references to this buffer.

   function Get_Total_Ref_Count
     (Editor : access Source_Editor_Box_Record)
      return Integer;
   --  Return the total number of times the Source_Buffer was referenced.

   ---------------------
   -- Contextual menu --
   ---------------------

   function Get_Contextual_Menu
     (Kernel       : access Glide_Kernel.Kernel_Handle_Record'Class;
      Object       : access Glib.Object.GObject_Record'Class;
      Event        : Gdk.Event.Gdk_Event;
      Menu         : Gtk.Menu.Gtk_Menu)
      return Glide_Kernel.Selection_Context_Access;
   --  Return the contextual menu to use for the source box.
   --  This function is also used to create the context for
   --  Glide_Kernel.Get_Current_Context, and might be called with Event and
   --  Menu set to null.

   ----------------------
   -- Line information --
   ----------------------

   procedure Add_File_Information
     (Editor     : access Source_Editor_Box_Record;
      Identifier : String;
      Info       : Glide_Kernel.Modules.Line_Information_Data);
   --  See Glide_Kernel.Modules for more information.

   procedure Create_Line_Information_Column
     (Editor         : access Source_Editor_Box_Record;
      Identifier     : String;
      Stick_To_Data  : Boolean;
      Every_Line     : Boolean);
   --  See Glide_Kernel.Modules for more information.

   procedure Remove_Line_Information_Column
     (Editor         : access Source_Editor_Box_Record;
      Identifier     : String);
   --  See Glide_Kernel.Modules for more information.

   procedure Undo (Editor : access Source_Editor_Box_Record);
   procedure Redo (Editor : access Source_Editor_Box_Record);
   --  Undo/Redo last edit command.

   function Add_Blank_Lines
     (Editor : access Source_Editor_Box_Record;
      Line   : Src_Editor_Buffer.Editable_Line_Type;
      GC     : Gdk.GC.Gdk_GC;
      Text   : String;
      Number : Positive) return Gtk.Text_Mark.Gtk_Text_Mark;
   --  See Src_Editor_Buffer.Add_Blank_Lines.

private

   ----------------------
   -- Tooltip Handling --
   ----------------------

   type Editor_Tooltip_Data is record
      Box : Source_Editor_Box;
   end record;

   procedure Draw_Tooltip
     (Widget : access Src_Editor_View.Source_View_Record'Class;
      Data   : in out Editor_Tooltip_Data;
      Pixmap : out Gdk.Gdk_Pixmap;
      Width  : out Glib.Gint;
      Height : out Glib.Gint;
      Area   : out Gdk.Rectangle.Gdk_Rectangle);
   --  Draw a tooltip in a text view.
   --  See GVD.Tooltips for a complete spec of this procedure.

   package Editor_Tooltips is new GVD.Tooltips
     (Editor_Tooltip_Data, Src_Editor_View.Source_View_Record, Draw_Tooltip);

   type Timestamp_Check_Mode is (Checking, Check_At_Focus, Check_At_Modify);
   --  When should the source box test the timestamp of the file on disk ?
   --  - Checking: we are already asking the user whether he wants to edit
   --  - Check_At_Focus: check at next focus event only
   --  - Check_At_Modify: check the next time the buffer is modified only
   --  The goal of this type is to avoid asking the question multiple times to
   --  the user. If he answers no the first time, we forbid editing until he
   --  has said yes.

   type Frames_Array is array (Natural range <>) of Gtk.Frame.Gtk_Frame;
   type Frames_Array_Access is access Frames_Array;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Frames_Array, Frames_Array_Access);

   type Source_Editor_Box_Record is new Glib.Object.GObject_Record with record
      Kernel               : Glide_Kernel.Kernel_Handle;

      Timestamp_Mode       : Timestamp_Check_Mode := Check_At_Focus;

      Root_Container       : Gtk.Box.Gtk_Box;
      Never_Attached       : Boolean := True;
      Source_View          : Src_Editor_View.Source_View;
      Source_Buffer        : Src_Editor_Buffer.Source_Buffer;

      Label_Box            : Gtk.Box.Gtk_Hbox;

      --  The status bar
      Filename_Label       : Gtk.Label.Gtk_Label;
      Read_Only_Label      : Gtk.Label.Gtk_Label;
      Modified_Label       : Gtk.Label.Gtk_Label;
      Overwrite_Label      : Gtk.Label.Gtk_Label;
      Cursor_Loc_Label     : Gtk.Label.Gtk_Label;

      --  The non graphical attributes

      Tooltip              : Editor_Tooltips.Tooltips;
      --  Those tooltips display the value of variables pointed to by the
      --  mouse.

      Writable             : Boolean := True;
      Overwrite            : Boolean := False;

      Default_GC, Bg_GC    : Gdk.Gdk_GC;

      Cursor_Handler       : Gtk.Handlers.Handler_Id;
      --  Handler connected to the signal "cursor_position_changed" in
      --  the Source_Buffer.

      Status_Handler       : Gtk.Handlers.Handler_Id;
      --  Handler connected to the signal "status_changed"
      --  from the source buffer.

      Buffer_Info_Handler  : Gtk.Handlers.Handler_Id;
      --  Handler connected to the signal "buffer_information_changed" from the
      --  source buffer.

      Buffer_Info_Frames   : Frames_Array_Access := null;

      Primary              : Boolean := False;
      --  Indicates whether the box is the primary editor of Source_Buffer.
      --  Only the primary editor needs to be saved.
      --  When closing a Primary editor, other editors will be looked up
      --  and another editor becomes Primary.
      --  This attribute is used mainly for determining whether the user
      --  should be prompted for saving the contents.
   end record;
   --  Note that it is straightforward to retrieve the Source_Buffer from
   --  the Source_View, thus making the Source_View field not absolutely
   --  necessary. But it is kept nonetheless for performance reasons, since
   --  we have to retrieve the buffer for lots of operation...
   --  ??? Is the latter true?

end Src_Editor_Box;
