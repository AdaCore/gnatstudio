-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2005                       --
--                            AdaCore                                --
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
--  This package provides a text editor buffer with lots of extensions
--  to support many functionalities related to source code editing.
--  This includes for example syntax highlighting, auto-indent, cross-
--  reference support, etc.
--  </description>

with Glib; use Glib;
with Gtk;
with Gtk.Main;
with Gtk.Text_Buffer;
with Gtk.Text_Iter;
with Gtk.Text_Mark;
with Gtk.Text_Tag;

with Language;
with Src_Highlighting;

with GNAT.OS_Lib;                 use GNAT.OS_Lib;
with Ada.Unchecked_Deallocation;
with Ada.Calendar;
with GPS.Kernel;
with GPS.Kernel.Standard_Hooks; use GPS.Kernel.Standard_Hooks;
with Generic_List;
with VFS;

with String_List_Utils;
with Gdk.GC; use Gdk.GC;

with Commands; use Commands;

package Src_Editor_Buffer is

   type Source_Buffer_Record is new Gtk.Text_Buffer.Gtk_Text_Buffer_Record
     with private;
   type Source_Buffer is access all Source_Buffer_Record'Class;

   procedure Gtk_New
     (Buffer : out Source_Buffer;
      Kernel : GPS.Kernel.Kernel_Handle;
      Lang   : Language.Language_Access := null);
   --  Create a new Source_Buffer with the given Language.

   --  The following types define the different line types that are involved
   --  in the buffer:

   type Editable_Line_Type is new Natural;
   --  Editable lines are the lines in the buffer that can be edited (ie
   --  all lines except blank lines, post-it notes, etc), plus lines that
   --  could be edited but are not displayed in the buffer (hidden lines in
   --  folded blocks belong to that category).
   --  The Editable lines are the lines that are saved to disk when the
   --  Source_Buffer is saved.

   type Buffer_Line_Type is new Natural;
   --  Buffer lines correspond to lines actually in the buffer, ie all lines
   --  that are visible on the screen.

   type File_Line_Type is new Natural;
   --  File lines identify lines that were in the file the last time that the
   --  buffer was saved.

   procedure Initialize
     (Buffer : access Source_Buffer_Record'Class;
      Kernel : GPS.Kernel.Kernel_Handle;
      Lang   : Language.Language_Access := null);
   --  Internal initialization procedure.
   --  See the section "Creating your own widgets" in the documentation.

   procedure Load_File
     (Buffer          : access Source_Buffer_Record;
      Filename        : VFS.Virtual_File;
      Lang_Autodetect : Boolean := True;
      Success         : out Boolean);
   --  Load the file into the buffer. If Lang_Autodetect is set to True, then
   --  the editor tries to automatically set the language based on the
   --  Filename. Otherwise, the language remains uchanged. After the file is
   --  loaded into the buffer, the buffer is syntax-highlighted if Lang is set.
   --
   --  Note that if Lang_Autodetect is True, and the editor could not guess
   --  the language from the filename, then Lang will be unset, and syntax
   --  highlighting will be deactivated.

   procedure Save_To_File
     (Buffer   : access Source_Buffer_Record;
      Filename : VFS.Virtual_File;
      Success  : out Boolean;
      Internal : Boolean := False);
   --  Save the current buffer into a file. Success is set to False if this
   --  operation failed and the buffer could not be saved.
   --  If Internal is True, save the file to disk but do not modify the buffer
   --  status.

   procedure Set_Language
     (Buffer : access Source_Buffer_Record;
      Lang   : Language.Language_Access);
   --  Set the language of the given buffer. The syntax highlighting
   --  is redone using the new language.

   function Get_Language
     (Buffer : access Source_Buffer_Record) return Language.Language_Access;
   --  Get the current language. Return null if the language is not set.

   function Is_Valid_Position
     (Buffer : access Source_Buffer_Record;
      Line   : Gint;
      Column : Gint := 0) return Boolean;
   --  Return True if the given cursor position is valid. If Column is
   --  set to 0, then this function just verifies the given line number
   --  (column 0 of a given line always exists).
   --
   --  Note that Get_Line_Count (inherited from Gtk_Text_Buffer) is also
   --  available when only the Line number needs to be checked.
   --  Obsolete, should use Is_Valid_Position below.

   function Is_Valid_Position
     (Buffer : access Source_Buffer_Record;
      Line   : Editable_Line_Type;
      Column : Natural := 1) return Boolean;
   pragma Inline (Is_Valid_Position);
   --  Return True if the given cursor position is valid. If Column is
   --  set to 0, then this function just verifies the given line number
   --  (column 0 of a given line always exists).
   --
   --  Note that Get_Line_Count (inherited from Gtk_Text_Buffer) is also
   --  available when only the Line number needs to be checked.

   procedure Set_Cursor_Position
     (Buffer : access Source_Buffer_Record;
      Line   : Editable_Line_Type;
      Column : Natural);
   --  Move the insert cursor to the given position.
   --
   --  The validity of the cursor position must be verified before invoking
   --  this procedure. An incorrect position will cause an Assertion_Failure
   --  when compiled with assertion checks, or an undefined behavior otherwise.

   procedure Get_Cursor_Position
     (Buffer : access Source_Buffer_Record;
      Line   : out Gint;
      Column : out Gint);
   --  Return the current cursor position

   procedure Get_Cursor_Position
     (Buffer : access Source_Buffer_Record;
      Line   : out Editable_Line_Type;
      Column : out Positive);
   --  Return the current editable cursor position

   procedure Get_Screen_Position
     (Buffer : access Source_Buffer_Record;
      Iter   : Gtk.Text_Iter.Gtk_Text_Iter;
      Line   : out Gint;
      Column : out Gint);
   --  Return the cursor position corresponding to Iter, after expanding all
   --  the tabs.

   procedure Get_Screen_Position
     (Buffer : access Source_Buffer_Record;
      Line   : out Gint;
      Column : out Gint);
   --  Same as above, for the cursor position

   procedure Get_Selection_Bounds
     (Buffer       : access Source_Buffer_Record;
      Start_Line   : out Editable_Line_Type;
      Start_Column : out Natural;
      End_Line     : out Editable_Line_Type;
      End_Column   : out Natural;
      Found        : out Boolean);
   --  If a portion of the buffer is currently selected, then return the
   --  position of the beginning and the end of the selection. Otherwise,
   --  Found is set to False and the positions returned both point to the
   --  begining of the buffer.

   function Get_Selection (Buffer : access Source_Buffer_Record) return String;
   --  If a portion of the buffer is currently selected, then return this
   --  portion. Otherwise, return the empty string.
   --
   --  This procedure is faster than the Get_Selection_Bounds + Get_Slice
   --  sequence because it does not work with (line, column) positions but
   --  directly with buffer iterators.

   function Get_Text
     (Buffer       : access Source_Buffer_Record;
      Start_Line   : Editable_Line_Type;
      Start_Column : Natural;
      End_Line     : Editable_Line_Type := 0;
      End_Column   : Natural := 0) return String;
   --  Return (as UTF-8) the text in range [Start, end).
   --  If End_Line is 0, get the entire range between start position and end
   --  of text.

   procedure Forward_Position
     (Buffer       : access Source_Buffer_Record;
      Start_Line   : Editable_Line_Type;
      Start_Column : Natural;
      Length       : Integer;
      End_Line     : out Editable_Line_Type;
      End_Column   : out Natural);
   --  Return the position Length characters after Start_Line/Start_Column.

   procedure Insert
     (Buffer      : access Source_Buffer_Record;
      Line        : Editable_Line_Type;
      Column      : Natural;
      Text        : String;
      Enable_Undo : Boolean := True);
   --  Insert the given text in at the specified position.
   --
   --  The validity of the given position must be verified before invoking this
   --  procedure. An incorrect position  will cause an Assertion_Failure when
   --  compiled with assertion checks, or an undefined behavior
   --  otherwise.
   --  If Enable_Undo is True, then the insertion action will be
   --  stored in the undo/redo queue.

   procedure Delete
     (Buffer      : access Source_Buffer_Record;
      Line        : Editable_Line_Type;
      Column      : Natural;
      Length      : Natural;
      Enable_Undo : Boolean := True);
   --  Delete Length caracters after the specified position.
   --
   --  The validity of the given position must be verified before invoking this
   --  procedure. An incorrect position  will cause an Assertion_Failure when
   --  compiled with assertion checks, or an undefined behavior
   --  otherwise.
   --  If Enable_Undo is True, then the deletion action will be
   --  stored in the undo/redo queue.

   procedure Replace_Slice
     (Buffer       : access Source_Buffer_Record;
      Start_Line   : Editable_Line_Type;
      Start_Column : Natural;
      End_Line     : Editable_Line_Type;
      End_Column   : Natural;
      Text         : String;
      Enable_Undo  : Boolean := True);
   --  Replace the text between the start and end positions by Text.
   --
   --  The validity of the given positions must be verified before invoking
   --  this procedure. An incorrect position will cause an Assertion_Failure
   --  when compiled with assertion checks, or an undefined behavior otherwise.

   procedure Select_All (Buffer : access Source_Buffer_Record);
   --  Set the selection bounds from the begining to the end of the buffer.

   procedure Select_Current_Word (Buffer : access Source_Buffer_Record);
   --  Select the word the cursor is on. The insert mark is placed at the end
   --  of the selection.

   procedure Select_Region
     (Buffer       : access Source_Buffer_Record;
      Start_Line   : Gint;
      Start_Column : Gint;
      End_Line     : Gint;
      End_Column   : Gint;
      Expand_Tabs  : Boolean := True);
   --  Select the given region.
   --  Both start and end positions must be verified before calling this
   --  procedure. An incorrect position will cause an Assertion_Failure
   --  when compiled with assertion checks, or an undefined behavior otherwise.
   --  Takes Tabs into account when Expand_Tabs = True.

   procedure Select_Region
     (Buffer       : access Source_Buffer_Record;
      Start_Line   : Editable_Line_Type;
      Start_Column : Natural;
      End_Line     : Editable_Line_Type;
      End_Column   : Natural;
      Expand_Tabs  : Boolean := True);
   --  Select the given region.

   procedure External_End_Action (Buffer : access Source_Buffer_Record);
   --  This procedure should be called every time that an external
   --  event should cancel the current user action: focus switching
   --  to another window, cursor moved, etc.

   procedure Undo (Buffer : access Source_Buffer_Record);
   --  Undo last user command

   procedure Redo (Buffer : access Source_Buffer_Record);
   --  Redo last undone command

   function Do_Indentation
     (Buffer   : Source_Buffer;
      From, To : Gtk.Text_Iter.Gtk_Text_Iter;
      Force    : Boolean := False) return Boolean;
   --  Reindent a specific range of lines (the ones containing From to To).
   --  Indentation depend on the language and the setup the user has chosen
   --  (either simple or extended indentation).
   --  Do nothing if the preference is not activated.
   --  Return whether the current range could be indented correctly.
   --  If Force, perform indentation in Extended mode, even if auto
   --  indentation is disabled.

   function Do_Indentation
     (Buffer            : Source_Buffer;
      Current_Line_Only : Boolean := False;
      Force             : Boolean := False) return Boolean;
   --  Same as above, but for the current line (or current selection if there
   --  is one and Current_Line_Only is False).

   function Do_Refill (Buffer : Source_Buffer) return Boolean;
   --  Refill selected text or the current line if no selection is active. The
   --  text is wrapped depending on the right margin defined by the column
   --  highlight preference. This routine handles simple text and comments.
   --  The indentation and comment detection is done according to the first
   --  line selected.

   function Should_Indent (Buffer : Source_Buffer) return Boolean;
   --  Return true if auto-indentation is supported for this buffer, and if
   --  the user has activated it.

   procedure Enqueue
     (Buffer  : access Source_Buffer_Record;
      Command : Command_Access);
   --  Enqueue an action in the Buffer queue

   function Get_Kernel
     (Buffer : access Source_Buffer_Record) return GPS.Kernel.Kernel_Handle;
   --  Return the kernel associated to Buffer

   function Get_Filename
     (Buffer : access Source_Buffer_Record) return VFS.Virtual_File;
   --  Return the name of the file associated with Buffer

   procedure Set_Filename
     (Buffer : access Source_Buffer_Record;
      Name   : VFS.Virtual_File);
   --  Set the name of the file associated with Buffer to Name

   function Get_File_Identifier
     (Buffer : access Source_Buffer_Record) return VFS.Virtual_File;
   --  Return the identifier of the file associated with Buffer

   procedure Set_File_Identifier
     (Buffer : access Source_Buffer_Record;
      Name   : VFS.Virtual_File);
   --  Set the file identifier for Buffer. This identifier is used for
   --  unnamed files, so that they can be uniquely identified.
   --
   --  ??? This is not really a file name, although we use it as such in
   --  various contexts

   procedure Source_Lines_Revealed
     (Buffer     : access Source_Buffer_Record;
      Start_Line : Buffer_Line_Type;
      End_Line   : Buffer_Line_Type);
   --  Emit the signal to the kernel saying that an area in the source
   --  has been revealed.

   function Check_Timestamp
     (Buffer : access Source_Buffer_Record;
      Update : Boolean := False) return Boolean;
   --  Check whether the timestamp changed on the disk.
   --  Return True if the file is up-to-date.
   --  If Update is true, the internal timestamp is also updated, so that a
   --  second call to this function will always return False.

   procedure Ref (Buffer : access Source_Buffer_Record);
   --  Should be called every time that a view is showing Buffer

   procedure Unref (Buffer : access Source_Buffer_Record);
   --  Should be called whenever a view showing Buffer is about to be deleted.
   --  If it was the last reference on Buffer, then free the memory associated
   --  to Buffer.

   function Get_Ref_Count
     (Buffer : access Source_Buffer_Record) return Integer;
   --  Return the number of times the buffer was referenced.

   function Get_Total_Ref_Count
     (Buffer : access Source_Buffer_Record) return Integer;
   --  Return the total number of times the buffer was referenced.

   procedure Add_Controls (Buffer : access Source_Buffer_Record);
   --  Connect the Undo/Redo buttons to the queue containing the buffer
   --  commands. This MUST be called every time that the Buffer.Queue
   --  pointer is modified.

   procedure Remove_Controls (Buffer : access Source_Buffer_Record);
   --  Disconnect the Undo/Redo buttons from the queue containing the buffer
   --  commands. This MUST be called every time that the Buffer.Queue
   --  pointer is modified and the controls actually refer to Queue.

   -------------------
   -- Buffer Status --
   -------------------

   type Status_Type is (Unmodified, Modified, Saved);

   function Get_Status
     (Buffer : access Source_Buffer_Record)
      return Status_Type;
   --  Return the status of the buffer.
   --  Calculate the status from the queue position.

   procedure Status_Changed
     (Buffer : access Source_Buffer_Record'Class);
   --  Emit the "status_changed" signal.

   function Get_Last_Status
     (Buffer : access Source_Buffer_Record'Class)
      return Status_Type;
   --  Return the last calculated status.

   procedure Set_Last_Status
     (Buffer : access Source_Buffer_Record'Class;
      Status : Status_Type);
   --  Set the last calculated status.

   function Needs_To_Be_Saved
     (Buffer : access Source_Buffer_Record'Class)
      return Boolean;
   --  Return True if the buffer needs to be saved.

   -----------------------
   -- Extra Information --
   -----------------------

   type Line_Information_Access is access Line_Information_Record;
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Line_Information_Record, Line_Information_Access);

   --  The following is related to extra information associated to the buffer,
   --  such as VCS status of the file.

   type Extra_Information_Record is record
      Identifier : String_Access;
      Info       : Line_Information_Access;
   end record;
   type Extra_Information_Access is access Extra_Information_Record;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Extra_Information_Record, Extra_Information_Access);

   type Extra_Information_Array is
     array (Natural range <>) of Extra_Information_Access;
   type Extra_Information_Array_Access is access Extra_Information_Array;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Extra_Information_Array, Extra_Information_Array_Access);

   function Get_Extra_Information
     (Buffer : Source_Buffer)
      return Extra_Information_Array_Access;
   --  Return the extra information associated with the buffer.

   --  The following is related to information to be put in the side column.

   type Line_Info_Width is record
      Info  : Line_Information_Access;
      Width : Integer := 0;
      Set   : Boolean := False;
   end record;

   type Line_Info_Width_Array is array (Natural range <>) of
     Line_Info_Width;
   type Line_Info_Width_Array_Access is access Line_Info_Width_Array;
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
    (Line_Info_Width_Array, Line_Info_Width_Array_Access);

   type Line_Info_Display_Record is record
      Identifier    : String_Access;
      --  This identifies the column.

      Starting_X    : Integer;
      --  The pixel distance between the left border of the column and
      --  the left border of the left window.

      Width         : Integer;
      --  The pixel width of the column.

      Every_Line : Boolean;
      --  If Every_Line is True, then there must be data at every line in
      --  this column.
   end record;
   type Line_Info_Display_Access is access Line_Info_Display_Record;
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Line_Info_Display_Record, Line_Info_Display_Access);

   type Line_Info_Display_Array is array (Natural range <>)
     of Line_Info_Display_Access;

   type Line_Info_Display_Array_Access is access Line_Info_Display_Array;
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Line_Info_Display_Array, Line_Info_Display_Array_Access);

   function Line_Needs_Refresh
     (Buffer : access Source_Buffer_Record;
      Line   : Buffer_Line_Type) return Boolean;
   --  Return True if Line needs to be refreshed.

   function Get_Total_Column_Width
     (Buffer : access Source_Buffer_Record) return Natural;
   --  Return the size of the total column width, in pixels.

   type Highlight_Location is (Highlight_Speedbar, Highlight_Editor);
   type Highlight_Location_Array is array (Highlight_Location) of Boolean;
   pragma Pack (Highlight_Location_Array);
   --  The various locations where a highlight can be made visible

   procedure Add_Line_Highlighting
     (Editor : access Source_Buffer_Record;
      Line   : Editable_Line_Type;
      Id     : String;
      Highlight_In : Highlight_Location_Array);
   --  Enable the highlighting of Line using colors defined in category
   --  corresponding to Id.
   --  See Src_Editor_Box.Add_Line_Highlighting.

   procedure Remove_Line_Highlighting
     (Editor : access Source_Buffer_Record;
      Line   : Editable_Line_Type;
      Id     : String);
   --  Disable the highlighting of Line using colors defined in category
   --  corresponding to Id.
   --  See Src_Editor_Box.Remove_Line_Highlighting.

   function Get_Highlight_GC
     (Editor  : access Source_Buffer_Record;
      Line    : Buffer_Line_Type;
      Context : Highlight_Location) return Gdk_GC;
   pragma Inline (Get_Highlight_GC);
   --  Return the current highlighting for Line, or null if no highlighting
   --  is set.

   type Block_Record is record
      Indentation_Level : Integer := 0;
      --  Represent the indentation level of the block.

      Offset_Start      : Integer := 0;
      --  The indentation offset of the first line of the block, in characters.

      Stored_Offset     : Integer := 0;
      --  Stores the last calculated offset of this block. Mainly used to be
      --  able to draw the blocks almost correctly even as we are editing the
      --  text.

      First_Line        : Editable_Line_Type := 0;
      Last_Line         : Editable_Line_Type := 0;
      --  Indicate the lines that bound the block.

      Name              : String_Access;
      --  The name of the block, this is the subprogram or package name. This
      --  pointer is null for a block where name has no meaning.

      Block_Type        : Language.Language_Category := Language.Cat_Unknown;
      --  Indicates the type of the block, if Indentation_Level /= 0.

      GC                : Gdk_GC := null;
      --  The color to use when highlighting this block.
   end record;

   function Get_Block
     (Editor        : access Source_Buffer_Record;
      Line          : Buffer_Line_Type;
      Force_Compute : Boolean := True) return Block_Record;
   --  Return the block information associated with Line.
   --  If Force_Compute is True, then the buffer blocks will be parsed
   --  on-the-fly if needed. If Force_Compute is False and the buffer blocks
   --  are not up-to-date, the latest known block at this line will be
   --  returned.

   function Has_Block_Information
     (Editor : access Source_Buffer_Record) return Boolean;
   --  Returh whether the buffer has relevant block information.

   type Src_Editor_Action_Context is new GPS.Kernel.Action_Filter_Record
      with null record;
   function Filter_Matches_Primitive
     (Context : access Src_Editor_Action_Context;
      Ctxt    : access GPS.Kernel.Selection_Context'Class)
      return Boolean;
   --  A key context that matches if the current widget is a source editor

   --------------
   --  Signals --
   --------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "cursor_position_changed"
   --    procedure Handler (Buffer : Gtk_Object_Record'Class;
   --                       Line   : Gint;
   --                       Column : Gint);
   --    Emitted when the cursor position changes.
   --
   --  - "side_column_changed"
   --    procedure Handler (Buffer : Gtk_Object_Record'Class);
   --    Emitted when the information in the side column has been
   --    changed.
   --
   --  - "side_column_configuration_changed"
   --    procedure Handler (Buffer : Gtk_Object_Record'Class);
   --    Emitted when the side column configuration has changed (ie lines have
   --    been added or removed.)
   --
   --  - "line_highlights_changed"
   --    procedure Handler (Buffer : Gtk_Object_Record'Class);
   --    Emitted when the line highlightings have been updated.
   --
   --  - "buffer_information_changed"
   --    procedure Handler (Buffer : Gtk_Object_Record'Class);
   --    Emitted when the buffer information (such as VCS status)
   --    has been changed.
   --
   --  - "status_changed"
   --    procedure Handler (Buffer : Gtk_Object_Record'Class);
   --    Emitted when the status of the buffer has been changed.
   --
   --  </signals>

   function Get_Buffer_Line
     (Buffer : access Source_Buffer_Record;
      Line   : Editable_Line_Type) return Buffer_Line_Type;
   pragma Inline (Get_Buffer_Line);
   --  Get the buffer line corresponding to Line.
   --  Return 0 if no buffer line was found.
   --  Note: Buffer lines are indexes in Buffer.Line_Data, ie are equal to
   --  lines in the actual Gtk_Text_Buffer, plus 1.

   function Blocks_Valid (Buffer : access Source_Buffer_Record) return Boolean;
   pragma Inline (Blocks_Valid);
   --  Return whether the blocks stored in the buffer are valid.
   --  (Ie if the text has not been modified since the last computation).

   function Get_String
     (Buffer : Source_Buffer) return GNAT.OS_Lib.String_Access;
   --  Return the entire editable string.
   --  The caller is responsible for freeing the returned value.

   function Get_Editable_Line
     (Buffer : access Source_Buffer_Record;
      Line   : File_Line_Type) return Editable_Line_Type;
   --  Return the editable line corresponding to Line

   function Get_Editable_Line
     (Buffer : access Source_Buffer_Record'Class;
      Line   : Buffer_Line_Type) return Editable_Line_Type;
   --  Return the editable line corresponding to Line.
   --  Return 0 if no editable line was found.

   function Get_Buffer_Line
     (Buffer : access Source_Buffer_Record;
      Line   : File_Line_Type) return Buffer_Line_Type;
   --  Return the buffer line corresponding to file line Line

   procedure Refresh_Side_Column (Buffer : access Source_Buffer_Record);
   --  Refresh the side columns in Buffer.

   function Position_Set_Explicitely
     (Buffer : access Source_Buffer_Record) return Boolean;
   --  Return True if the position of the cursor has been set explicitely (ie
   --  not as a side effect of a text change)

private

   procedure Get_Cursor_Position
     (Buffer : access Source_Buffer_Record;
      Iter   : out Gtk.Text_Iter.Gtk_Text_Iter);
   --  Return the current editable cursor position

   procedure Get_Cursor_Position
     (Buffer : Source_Buffer;
      Iter   : Gtk.Text_Iter.Gtk_Text_Iter;
      Line   : out Editable_Line_Type;
      Column : out Positive);
   --  Return the current editable cursor position for Iter

   procedure Set_Cursor_Position
     (Buffer : access Source_Buffer_Record;
      Line   : Gint;
      Column : Gint);
   --  Move the insert cursor to the given position.
   --
   --  The validity of the cursor position must be verified before invoking
   --  this procedure. An incorrect position will cause an Assertion_Failure
   --  when compiled with assertion checks, or an undefined behavior otherwise.
   --  This is obsolete, Set_Cursor_Position above should be called.

   procedure Get_Iter_At_Screen_Position
     (Buffer : access Source_Buffer_Record;
      Iter   : out Gtk.Text_Iter.Gtk_Text_Iter;
      Line   : Editable_Line_Type;
      Column : Positive);
   --  Return the iter at position (Line, Column), tab expansion included.
   --  If Line is not in the text, return the Iter at beginning of text.

   function Is_In_Comment
     (Buffer : Source_Buffer;
      Iter   : Gtk.Text_Iter.Gtk_Text_Iter) return Boolean;
   --  Retruns true if Iter is in a comment. This is only supported for non
   --  case sensitive languages that have one-line comment construct only.

   function Is_In_String
     (Buffer : Source_Buffer;
      Iter   : Gtk.Text_Iter.Gtk_Text_Iter) return Boolean;
   --  Returns true if Iter is inside a string

   procedure Highlight_Slice
     (Buffer     : access Source_Buffer_Record'Class;
      Start_Iter : Gtk.Text_Iter.Gtk_Text_Iter;
      End_Iter   : Gtk.Text_Iter.Gtk_Text_Iter);
   --  Re-compute the highlighting for at least the given region.
   --  If the text creates non-closed comments or string regions, then
   --  the re-highlighted area is automatically extended to the right.
   --  When the re-highlighted area is extended to the right, the extension
   --  is computed in a semi-intelligent fashion.

   procedure End_Action (Buffer : access Source_Buffer_Record'Class);
   --  This procedure should be called every time that an internal
   --  event should cancel the current user action: focus switching
   --  to another window, cursor moved, etc.

   procedure Buffer_Information_Changed
     (Buffer : access Source_Buffer_Record'Class);
   --  Emit the "buffer_information_changed" signal.

   procedure Register_Edit_Timeout
     (Buffer : access Source_Buffer_Record'Class);
   --  Indicate that the text has been edited, and that a timeout should be
   --  registered to call the corresponding "after-timeout" hook.

   procedure Emit_New_Cursor_Position
     (Buffer : access Source_Buffer_Record'Class);
   --  Signal the new cursor position by emitting the "cursor_position_changed"
   --  signal.

   -----------------------
   -- Line highlighting --
   -----------------------

   type Block_Access is access Block_Record;

   procedure Free_Block (Block : in out Block_Access);

   package Block_List is new Generic_List
     (Block_Access, Free => Free_Block);

   type Boolean_Array is array (Natural range <>) of Boolean;
   type Boolean_Array_Access is access Boolean_Array;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Boolean_Array, Boolean_Array_Access);

   New_Block : constant Block_Record :=
     (0, 0, 0, 0, 0, null, Language.Cat_Unknown, null);

   procedure Create_Side_Info
     (Buffer : access Source_Buffer_Record;
      Line   : Editable_Line_Type);
   --  Create blank Side_Info_Data.

   type Line_Data_Record is record
      Editable_Line      : Editable_Line_Type;
      --  The line in the real buffer.

      --  The following corresponds to line highlighting. This is the category
      --  to use for highlighting. We cannot store the GC directly, since GPS
      --  might not be realized when the line_data is created
      Highlight_Category : Natural;

      Enabled_Highlights : Boolean_Array_Access;
      --  This array corresponds to the categories in Source_Editor_Module_Id.
      --  If an item is set to True, that means that line highlighting is
      --  enabled for that categories.
      --  For simplicity, the range of this array should match the range of
      --  the array of categories in the cache.

      Block              : Block_Access;
      --  Points to the corresponding block, or null if the line doesn't belong
      --  to a block.

      File_Line          : File_Line_Type;
      --  The corresponding line in the file corresponding to Buffer.
      --  0 if the line is not in the file.

      Highlight_In : Highlight_Location_Array;
      --  Where the highlighting should take place
      --  ??? Should be part of the highlight category, since otherwise we do
      --  not know for sure what category this applies to if the line is
      --  associated with multiple categories. Might have conflict with the use
      --  of Mark_In_Speedbar in the category, though
   end record;

   New_Line_Data : constant Line_Data_Record :=
     (0, 0, null, null, 0, (others => False));

   type Line_Data_Array is array (Buffer_Line_Type range <>) of
     Line_Data_Record;
   type Line_Data_Array_Access is access Line_Data_Array;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Line_Data_Array, Line_Data_Array_Access);

   ----------------
   -- Completion --
   ----------------

   type Completion_Data is record
      Prefix : GNAT.OS_Lib.String_Access;
      --  The current prefix for the search.
      --  Warning : this is an UTF-8 string obtained from the buffer, and
      --  should only be compared with UTF-8 strings.

      List : String_List_Utils.String_List.List;
      --  The possible current completions. If empty, then there is no
      --  current completion operation.

      Node : String_List_Utils.String_List.List_Node;
      --  The current position in the completions list.

      Mark : Gtk.Text_Mark.Gtk_Text_Mark;
      --  The position of the start point for the completion,
      --  The insert mark must always be the end point of the completion.

      Previous_Mark : Gtk.Text_Mark.Gtk_Text_Mark;
      Next_Mark     : Gtk.Text_Mark.Gtk_Text_Mark;
      --  The marks for the current back/forward searches.

      Top_Reached    : Boolean;
      Bottom_Reached : Boolean;
      --  Whether the top and bottom of the buffer have been reached
      --  while searching.

      Complete : Boolean;
      --  Whether the search for the current prefix is complete;

      Backwards : Boolean;
      --  True if the last direction searched was backwards.

      Buffer : Gtk.Text_Buffer.Gtk_Text_Buffer;
      --  The buffer on which the marks are effective.
   end record;

   procedure Clear (Data : in out Completion_Data);
   --  Free memory associated to Data;

   function Is_Empty (Data : Completion_Data) return Boolean;
   --  return True if the completion data is unset.

   type Line_Terminator_Style is (Unknown, LF, CR, CR_LF);
   --  The line terminator style of the given buffer.

   procedure Free (X : in out Line_Info_Width);
   --  Free memory associated to X.

   --------------------
   -- Editable lines --
   --------------------

   type Line_Location_Type is (In_Buffer, In_Mark);

   type Editable_Line_Data (Where : Line_Location_Type := In_Buffer) is record
      Side_Info_Data : Line_Info_Width_Array_Access;
      --  The array corresponding to information to be displayed in columns,
      --  indexed on columns.

      case Where is
         when In_Buffer =>
            Buffer_Line : Buffer_Line_Type;

         when In_Mark =>
            Text : String_Access := null;
      end case;
   end record;

   type Editable_Line_Array is array (Editable_Line_Type range <>) of
     Editable_Line_Data;
   type Editable_Line_Array_Access is access Editable_Line_Array;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Editable_Line_Array, Editable_Line_Array_Access);

   type Columns_Config_Access is access Line_Info_Display_Array_Access;
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Line_Info_Display_Array_Access, Columns_Config_Access);

   --------------------------
   -- Source_Buffer_Record --
   --------------------------

   type Source_Buffer_Record is
     new Gtk.Text_Buffer.Gtk_Text_Buffer_Record
   with record
      Kernel          : GPS.Kernel.Kernel_Handle;
      Filename        : VFS.Virtual_File;
      File_Identifier : VFS.Virtual_File;
      --  This identifier is used to identify buffers for untitled files.

      Lang          : Language.Language_Access;
      Syntax_Tags   : Src_Highlighting.Highlighting_Tags;
      Delimiter_Tag : Gtk.Text_Tag.Gtk_Text_Tag;
      --  A tag used when highlighting delimiters (e.g. parens).

      Non_Editable_Tag : Gtk.Text_Tag.Gtk_Text_Tag;
      --  A tag for text that cannot be interactively deleted.

      Insert_Mark : Gtk.Text_Mark.Gtk_Text_Mark;
      --  This is a copy of the "insert" mark or the "gtk_drag_target" mark.
      --  This could be easily looked-up when needed, but having a copy is
      --  helping performance-wise, since a  lot of subprograms use it.
      --  This must always be a valid text mark.

      Inserting : Boolean := False;
      --  Used to avoid recursion, when using commands.

      Do_Not_Move_Cursor : Boolean := False;
      --  Used to disable functions moving the cursor or emit the
      --  "cursor_position_changed" signal when we know we are going to
      --  move the cursor a lot.

      Queue           : Command_Queue;
      --  Contains the queue of editor commands for this editor.

      Saved_Position  : Integer := 0;
      --  The saved position in the command queue.

      Current_Command : Command_Access := null;
      --  The current editor command. Belongs to Queue, defined above.

      Current_Status  : Status_Type := Unmodified;
      --  The current buffer status.

      Timestamp : Ada.Calendar.Time := VFS.No_Time;
      --  Timestamp of the file the last time it was checked. It it used to
      --  detect cases where the file was edited by an external editor.

      References : Integer := 0;
      --  The number of objects viewing the buffer.

      Total_References : Integer := 0;
      --  The total number of times the buffer was referenced.

      Modified_Auto : Boolean := False;
      --  Whether the buffer has been modified since last auto save.

      Line_Terminator : Line_Terminator_Style := Unknown;

      Timeout_Id         : Gtk.Main.Timeout_Handler_Id := 0;
      Timeout_Registered : Boolean := False;
      --  Whether Timeout corresponds to a registered timeout.

      Setting_Mark : Boolean := False;
      --  Used to prevent recursion when creating text marks.

      Has_Delimiters_Highlight   : Boolean := False;
      --  Whether delimiters are currently highlighted

      Start_Delimiters_Highlight : Gtk.Text_Mark.Gtk_Text_Mark;
      End_Delimiters_Highlight   : Gtk.Text_Mark.Gtk_Text_Mark;
      --  Bounds for the parenthesis highlighting.

      Completion : Completion_Data;
      --  Completion data.

      Controls_Set : Boolean := False;
      --  Whether the Queue is currently connected to the
      --  Undo/Redo buttons.

      --  The following is related to information regarding
      --  the side column information.

      Editable_Line_Info_Columns : Columns_Config_Access;
      --  The information concerning columns of data that should be displayed
      --  in the left window.
      --  Must never be null.

      Editable_Lines : Editable_Line_Array_Access;
      --  Reference array for editable lines.

      Last_Editable_Line : Editable_Line_Type := 1;
      --  The last editable line in the buffer (corresponds to an index in
      --  Editable_Lines)

      Modifying_Editable_Lines : Boolean := True;
      --  Whether we are currently making modifications to the
      --  editable lines. This is True in normal operations.

      Line_Data : Line_Data_Array_Access;
      --  This array contains all data that are relative to lines: current
      --  highlighting, indentation, collapsing, etc.

      Original_Lines_Number : Buffer_Line_Type := 1;
      --  The number of lines in the file on disk.

      Total_Column_Width : Natural := 0;
      --  Width of the Left Window, in pixels.

      Line_Numbers_Width : Natural := 0;
      --  Width allocated to line numbers in the Left Window, in pixels

      Original_Text_Inserted : Boolean := False;

      Extra_Information : Extra_Information_Array_Access;
      --  Extra information concerning the buffer.

      First_Removed_Line, Last_Removed_Line : Buffer_Line_Type;
      --  These line indicate the lines that have just been removed in the
      --  editor. If First_Removed_Line = 0, then no lines have been removed.

      Parse_Blocks : Boolean := False;
      --  Whether the block information should be parsed on-the-fly whenever
      --  the text is edited.

      Blocks_Need_Parsing : Boolean := False;
      --  Whether the block information should be computed next time it is
      --  needed.

      Blocks_Timeout_Registered : Boolean := False;
      --  Whether the blocks need to be recomputed.

      Blocks_Timeout : Gtk.Main.Timeout_Handler_Id;
      --  A timeout handling the refresh of the timeouts.

      Blocks_Request_Timestamp : Ada.Calendar.Time;
      --  The last time the blocks refresh was requested.

      Cursor_Timeout_Registered : Boolean := False;
      --  Whether the cursor timeout is registered

      Cursor_Timeout : Gtk.Main.Timeout_Handler_Id;
      --  A timeout handling the refresh of the timeouts.

      Cursor_Timestamp : Ada.Calendar.Time;
      --  The last time the cursor was moved;

      Blank_Lines : Natural := 0;
      --  The number of blank lines in the buffer.

      Hidden_Lines : Natural := 0;
      --  The number of hidden lines in the buffer.

      Block_Folding : Boolean := False;
      --  Whether the editor buffer should allow block folding.

      Block_Highlighting : Boolean := False;
      --  Whether the editor buffer should allow block highlighting.

      Block_Highlighting_Column : Integer := -1;
      --  The column (index in Buffer_Line_Info_Columns) that contains the
      --  block information. Set to a negative value if the column does not
      --  exist.

      Modifying_Real_Lines : Boolean := False;
      --  Set to True when we are currently modifying the range of the real
      --  lines (ie when we are folding/unfolding text or adding/removing
      --  blank lines.

      --  The following information are used for idle buffer highlighting.

      First_Highlight_Mark : Gtk.Text_Mark.Gtk_Text_Mark;
      Last_Highlight_Mark  : Gtk.Text_Mark.Gtk_Text_Mark;
      --  Those marks indicate the minimum area that need to be highlighted.
      --  They must be valid marks at all times.

      Highlight_Needed : Boolean := False;
      --  Whether the text should be re-highlighted.

      Auto_Syntax_Check : Boolean := False;
      --  Whether the syntax should be checked automatically.

      Highlight_Delimiters : Boolean := False;
      --  Cache corresponding preference.

      Tab_Width : Gint := 8;
      --  Width of a Tab character

      Cursor_Set_Explicitely : Natural := 0;
      --  > 0 when the cursor position has been set explicitely in the code
   end record;

end Src_Editor_Buffer;
