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
with Gtk.Widget;       use Gtk.Widget;
with Gtkada.Types;

with Language;
with Src_Highlighting;

with GNAT.OS_Lib; use GNAT.OS_Lib;
with Ada.Unchecked_Deallocation;
with Commands;    use Commands;
with Glide_Kernel;
with Glide_Kernel.Modules; use Glide_Kernel.Modules;
with Src_Info;

with String_List_Utils;
with Gdk.GC; use Gdk.GC;

package Src_Editor_Buffer is

   type Source_Buffer_Record is new Gtk.Text_Buffer.Gtk_Text_Buffer_Record
     with private;
   type Source_Buffer is access all Source_Buffer_Record'Class;

   procedure Gtk_New
     (Buffer : out Source_Buffer;
      Kernel : Glide_Kernel.Kernel_Handle;
      Lang   : Language.Language_Access := null);
   --  Create a new Source_Buffer with the given Language.

   procedure Initialize
     (Buffer : access Source_Buffer_Record'Class;
      Kernel : Glide_Kernel.Kernel_Handle;
      Lang   : Language.Language_Access := null);
   --  Internal initialization procedure.
   --  See the section "Creating your own widgets" in the documentation.

   procedure Load_File
     (Buffer          : access Source_Buffer_Record;
      Filename        : String;
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
      Filename : String;
      Success  : out Boolean);
   --  Save the current buffer into a file. Success is set to False if this
   --  operation failed and the buffer could not be saved.

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

   procedure Set_Cursor_Position
     (Buffer  : access Source_Buffer_Record;
      Line    : Gint;
      Column  : Gint);
   --  Move the insert cursor to the given position.
   --
   --  The validity of the cursor position must be verified before invoking
   --  this procedure. An incorrect position will cause an Assertion_Failure
   --  when compiled with assertion checks, or an undefined behavior otherwise.

   procedure Set_Screen_Position
     (Buffer  : access Source_Buffer_Record;
      Line    : Gint;
      Column  : Gint);
   --  Same as Set_Cursor_Position, after expanding all tabs.

   procedure Get_Cursor_Position
     (Buffer : access Source_Buffer_Record;
      Line   : out Gint;
      Column : out Gint);
   --  Return the current cursor position

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
      Start_Line   : out Gint;
      Start_Column : out Gint;
      End_Line     : out Gint;
      End_Column   : out Gint;
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

   procedure Search
     (Buffer             : access Source_Buffer_Record;
      Pattern            : String;
      Case_Sensitive     : Boolean := True;
      Whole_Word         : Boolean := False;
      Search_Forward     : Boolean := True;
      From_Line          : Gint := 0;
      From_Column        : Gint := 0;
      Found              : out Boolean;
      Match_Start_Line   : out Gint;
      Match_Start_Column : out Gint;
      Match_End_Line     : out Gint;
      Match_End_Column   : out Gint);
   --  Search function. Regular expressions for Pattern are not supported.
   --  If the pattern is found, then Found is set to True and the positions
   --  of the begining and of the end of the matching portion are returned.
   --
   --  The validity of the start position must be verified before invoking
   --  this function. An incorrect position will cause an Assertion_Failure
   --  when compiled with assertion checks, or an undefined behavior otherwise.

   function Get_Slice
     (Buffer       : access Source_Buffer_Record;
      Start_Line   : Gint;
      Start_Column : Gint;
      End_Line     : Gint := -1;
      End_Column   : Gint := -1) return String;
   --  Return the text located between (Start_Line, Start_Column) and
   --  (End_Line, End_Column). The first line is 0, the first column is 0
   --  If End_Line = -1, contents are taken until the end of the buffer.
   --
   --  The text returned is converted to the charset defined in the preferences
   --  (ISO-8859-1 by default).
   --
   --  The validity of both start and end positions must be verified before
   --  invoking this function. An incorrect position will cause an
   --  Assertion_Failure when compiled with assertion checks, or an undefined
   --  behavior otherwise.

   function Get_Slice
     (Buffer       : access Source_Buffer_Record;
      Start_Line   : Gint;
      Start_Column : Gint;
      End_Line     : Gint := -1;
      End_Column   : Gint := -1) return Gtkada.Types.Chars_Ptr;
   --  Same as above but return the C pointer directly for efficiency.
   --  The caller is responsible for freeing the memory (with g_free).

   procedure Insert
     (Buffer      : access Source_Buffer_Record;
      Line        : Gint;
      Column      : Gint;
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
      Line        : Gint;
      Column      : Gint;
      Length      : Gint;
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
      Start_Line   : Gint;
      Start_Column : Gint;
      End_Line     : Gint;
      End_Column   : Gint;
      Text         : String;
      Enable_Undo  : Boolean := True);
   --  Replace the text between the start and end positions by Text.
   --
   --  The validity of the given positions must be verified before invoking
   --  this procedure. An incorrect position will cause an Assertion_Failure
   --  when compiled with assertion checks, or an undefined behavior otherwise.

   procedure Select_All (Buffer : access Source_Buffer_Record);
   --  Set the selection bounds from the begining to the end of the buffer.

   procedure Highlight_Line
     (Buffer  : access Source_Buffer_Record;
      Line    : Gint);
   --  Highlight the given line number. If another line was previously
   --  highlighted, then restore this line unhighlighted.
   --
   --  The validity of the line number must be verified before invoking this
   --  procedure. An incorrect line number will cause an Assertion_Failure
   --  when compiled with assertion checks, or an undefined behavior otherwise.

   procedure Unhighlight_Line
     (Buffer  : access Source_Buffer_Record;
      Line    : Gint);
   --  Restore the given line unhighlighted.
   --
   --  The validity of the line number must be verified before invoking this
   --  procedure. An incorrect line number will cause an Assertion_Failure when
   --  compiled with assertion checks, or an undefined behavior otherwise.

   procedure Cancel_Highlight_Line
     (Buffer : access Source_Buffer_Record);
   --  If a line in the given buffer is highlighted (from using
   --  Highlight_Line), then restores this line un-highlighted.

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

   procedure Highlight_Region
     (Buffer       : access Source_Buffer_Record;
      Start_Line   : Gint;
      Start_Column : Gint;
      End_Line     : Gint;
      End_Column   : Gint);
   --  Highlight the given region. The color used in this case is different
   --  from the color used for highlighting lines.
   --
   --  Both start and end positions must be verified before calling this
   --  procedure. An incorrect position will cause an Assertion_Failure
   --  when compiled with assertion checks, or an undefined behavior otherwise.

   procedure Unhighlight_Region
     (Buffer : access Source_Buffer_Record;
      Start_Line   : Gint;
      Start_Column : Gint;
      End_Line     : Gint;
      End_Column   : Gint);
   --  Restore the given region unhighlighted.
   --
   --  Both start and end positions must be verified before calling this
   --  procedure. An incorrect position will cause an Assertion_Failure
   --  when compiled with assertion checks, or an undefined behavior otherwise.

   procedure Unhighlight_All (Buffer : access Source_Buffer_Record);
   --  Restore all highlighted regions to unhighlighted. Only the region
   --  highlighting is canceled, the other potential highlightings (line,
   --  syntax) are preserved.

   procedure Forward_To_Line_End (Iter : in out Gtk.Text_Iter.Gtk_Text_Iter);
   --  This is a temporary implementation of Gtk.Text_Iter.Forward_To_Line_End
   --  because the gtk+ one is broken at the moment, and causes Critical
   --  warnings.
   --  ??? Remove this procedure when the problem is fixed.

   procedure External_End_Action (Buffer : access Source_Buffer_Record);
   --  This procedure should be called every time that an external
   --  event should cancel the current user action: focus switching
   --  to another window, cursor moved, etc.

   procedure Undo (Buffer : access Source_Buffer_Record);
   --  Undo last user command.

   procedure Redo (Buffer : access Source_Buffer_Record);
   --  Redo last undone command.

   procedure Enqueue
     (Buffer  : access Source_Buffer_Record;
      Command : Command_Access);
   --  Enqueue an action in the Buffer queue.

   function Get_Kernel
     (Buffer : access Source_Buffer_Record) return Glide_Kernel.Kernel_Handle;
   --  Return the kernel associated to Buffer.

   function Get_Filename (Buffer : access Source_Buffer_Record) return String;
   --  Return the name of the file associated with Buffer.

   procedure Set_Filename
     (Buffer : access Source_Buffer_Record;
      Name   : String);
   --  Set the name of the file associated with Buffer to Name.

   function Get_File_Identifier
     (Buffer : access Source_Buffer_Record) return String;
   --  Return the identifier of the file associated with Buffer.

   procedure Set_File_Identifier
     (Buffer : access Source_Buffer_Record;
      Name   : String);
   --  Set the file identifier for Buffer.

   procedure Source_Lines_Revealed
     (Buffer     : access Source_Buffer_Record;
      Start_Line : Integer;
      End_Line   : Integer);
   --  Emit the signal to the kernel saying that an area in the source
   --  has been revealed.

   function Check_Timestamp
     (Buffer : access Source_Buffer_Record;
      Ask_User : Boolean := False;
      Force    : Boolean := False) return Boolean;
   --  Check whether the timestamp changed on the disk. If yes, ask the user
   --  whether he really wants to edit (unless Ask_User is False). False is
   --  returned if the timestamp is more recent, and the user doesn't want to
   --  force the edition.
   --  If Force, then the buffer will be updated in any case.

   procedure Ref (Buffer : access Source_Buffer_Record);
   --  Should be called every time that a view is showing Buffer.

   procedure Unref (Buffer : access Source_Buffer_Record);
   --  Should be called whenever a view showing Buffer is about to be deleted.
   --  If it was the last reference on Buffer, then free the memory associated
   --  to Buffer.

   function Get_Ref_Count
     (Buffer : access Source_Buffer_Record)
      return Integer;
   --  Return the number of times the buffer was referenced.

   procedure Do_Completion (Buffer : access Source_Buffer_Record);
   --  Complete the current insertion, or continue the current completion.

   procedure Jump_To_Delimiter (Buffer : access Source_Buffer_Record);
   --  Jump to the other delimiter, if applicable.

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
   end record;

   type Line_Info_Width_Array is array (Natural range <>) of Line_Info_Width;
   type Line_Info_Width_Array_Access is access Line_Info_Width_Array;

   type Line_Info_Display_Record is record
      Identifier    : String_Access;
      --  This identifies the column.

      Starting_X    : Integer;
      --  The pixel distance between the left border of the column and
      --  the left border of the left window.

      Width         : Integer;
      --  The pixel width of the column.

      Column_Info   : Line_Info_Width_Array_Access;
      --  The information that should be displayed in the column.

      Stick_To_Data : Boolean;
      --  If Stick_To_Data is True, then the column contains information
      --  that are relative to the file as it was opened in the first
      --  place: when you insert or remove lines, the information should
      --  stick to the lines they are associated with.
      --  If Stick_To_Data is False, then the information "sticks" to
      --  the line numbers.

      Every_Line : Boolean;
      --  If Every_Line is True, then there must be data at every line in
      --  this column.
   end record;
   type Line_Info_Display_Access is access Line_Info_Display_Record;

   type Line_Info_Display_Array is array (Natural range <>)
     of Line_Info_Display_Access;

   type Line_Info_Display_Array_Access is access Line_Info_Display_Array;
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Line_Info_Display_Array, Line_Info_Display_Array_Access);

   type Natural_Array is array (Natural range <>) of Natural;
   type Natural_Array_Access is access Natural_Array;
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Natural_Array, Natural_Array_Access);

   function Get_Line_Info
     (Buffer : access Source_Buffer_Record)
      return Line_Info_Display_Array_Access;
   --  Return Buffer.Line_Info.

   procedure Set_Line_Info
     (Buffer    : access Source_Buffer_Record;
      Line_Info : Line_Info_Display_Array_Access);
   --  Set Buffer.Line_Info.

   function Get_Real_Lines
     (Buffer : access Source_Buffer_Record)
      return Natural_Array_Access;
   --  Return Buffer.Real_Lines;

   procedure Create_Line_Information_Column
     (Buffer        : access Source_Buffer_Record;
      Identifier    : String;
      Stick_To_Data : Boolean;
      Every_Line    : Boolean);
   --  Add a column corresponding to Identifier in Buffer.

   procedure Remove_Line_Information_Column
     (Buffer     : access Source_Buffer_Record;
      Identifier : String);
   --  Remove a column from the side information in Buffer.

   procedure Add_File_Information
     (Buffer     : access Source_Buffer_Record;
      Identifier : String;
      Box        : Gtk_Widget;
      Info       : Glide_Kernel.Modules.Line_Information_Data);
   --  Add the line information to the Buffer.
   --  User must not free Info.

   procedure Add_Line_Highlighting
     (Editor : access Source_Buffer_Record;
      Line   : Natural;
      Id     : String);
   --  Enable the highlighting of Line using colors defined in category
   --  corresponding to Id.
   --  See Src_Editor_Box.Add_Line_Highlighting.

   procedure Remove_Line_Highlighting
     (Editor : access Source_Buffer_Record;
      Line   : Natural;
      Id     : String);
   --  Disable the highlighting of Line using colors defined in category
   --  corresponding to Id.
   --  See Src_Editor_Box.Remove_Line_Highlighting.

   function Get_Highlight_GC
     (Editor : access Source_Buffer_Record;
      Line   : Positive) return Gdk_GC;
   pragma Inline (Get_Highlight_GC);
   --  Return the current highlighting for Line, or null if no highlighting
   --  is set.

   type Block_Record is record
      Indentation_Level : Integer := 0;
      --  Represent the block-indentation level of the current line.
      --  Lines that start a block have a positive indentation level,
      --  Lines that end a block have a negative indentation level.
      --  Lines that don't modifiy the block indentation have a null
      --  indentation level.
      --  The absolute value represents the number of blocks started/ended.
      --
      --  Example of indentation levels :
      --
      --  void main (void)            0
      --  {                          +1
      --      int a,b;                0
      --                              0
      --      if (a)                  0
      --      {                      +1
      --         if (a) {  }          0
      --         if (a) {            +1
      --         }}                  -2
      --  }                          -1

      Offset            : Integer := 0;
      --  The indentation offset, of the block (ie typically the column of the
      --  starting entity).

      Other_Line        : Integer := 0;
      --  Indicates the other line that corresponds to the beginning/end of
      --  the block. For example, if the current block is a line end, indicates
      --  the starting line of the corresponding block.
      --  Only valid if Indentation_Level /= 0.

      Block_Type        : Language.Language_Category := Language.Cat_Unknown;
      --  Indicates the type of the block, if Indentation_Level /= 0.

      GC                : Gdk_GC := null;
      --  The color to use when highlighting this block.
   end record;

   function Get_Block
     (Editor : access Source_Buffer_Record;
      Line   : Positive) return Block_Record;
   pragma Inline (Get_Block);
   --  Return the block information associated with Line.

   --------------
   --  Signals --
   --------------

   --  <signals>
   --  The following new signals are defined for this widget:
   --
   --  - "side_column_changed"
   --    procedure Handler (Buffer : Gtk_Object_Record'Class);
   --    Emitted when the information in the side column has been
   --    changed.
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

private

   -----------------------
   -- Line highlighting --
   -----------------------

   type Boolean_Array is array (Natural range <>) of Boolean;
   type Boolean_Array_Access is access Boolean_Array;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Boolean_Array, Boolean_Array_Access);

   New_Block : constant Block_Record := (0, 0, 0, Language.Cat_Unknown, null);

   type Line_Data_Record is record
      --  The following corresponds to line highlighting.
      Current_Highlight  : Gdk_GC;

      Enabled_Highlights : Boolean_Array_Access;
      --  This array corresponds to the categories in Source_Editor_Module_Id.
      --  If an item is set to True, that means that line highlighting is
      --  enabled for that categories.
      --  For simplicity, the range of this array should match the range of
      --  the array of categories in the cache.

      Block              : Block_Record;
      --  Data relative to possible block start/end.

      Collapsed          : Boolean := False;
      --  Whether the line is currently collapsed.
   end record;

   New_Line_Data : constant Line_Data_Record := (null, null, New_Block, False);

   type Line_Data_Array is array (Natural range <>) of Line_Data_Record;
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

   type Source_Buffer_Record is new Gtk.Text_Buffer.Gtk_Text_Buffer_Record with
   record
      Kernel        : Glide_Kernel.Kernel_Handle;
      Filename      : String_Access;
      File_Identifier : String_Access;
      --  This identifier is used to identify buffers for untitled files.

      Lang          : Language.Language_Access;
      Syntax_Tags   : Src_Highlighting.Highlighting_Tags;
      HL_Line_Tag   : Gtk.Text_Tag.Gtk_Text_Tag;
      --  A tag used when highlighting lines

      HL_Region_Tag : Gtk.Text_Tag.Gtk_Text_Tag;
      --  A tag used when highlighting regions

      Insert_Mark   : Gtk.Text_Mark.Gtk_Text_Mark;
      --  This is a copy of the "insert" mark or the "gtk_drag_target" mark.
      --  This could be easily looked-up when needed, but having a copy is
      --  helping performance-wise, since a  lot of subprograms use it.
      --  This must always be a valid text mark.

      Inserting     : Boolean := False;
      --  Used to avoid recursion, when

      Queue         : Command_Queue;
      --  Contains the queue of editor commands for this editor.

      Saved_Position      : Integer := 0;
      --  The saved position in the command queue.

      Last_Saved_Position : Integer := 0;
      --  The position the last time the user requested a save.

      Current_Command : Command_Access := null;
      --  The current editor command. Belongs to Queue, defined above.

      Current_Status  : Status_Type := Unmodified;
      --  The current buffer status.

      Timestamp      : Src_Info.Timestamp := 0;
      --  Timestamp of the file the last time it was checked. It it used to
      --  detect cases where the file was edited by an external editor.

      References     : Integer := 0;
      --  The number of objects viewing the buffer.

      Total_References : Integer := 0;
      --  The total number of times the buffer was referenced.

      Modified_Auto  : Boolean := False;
      --  Whether the buffer has been modified since last auto save.

      Line_Terminator : Line_Terminator_Style := Unknown;

      Timeout_Id     : Gtk.Main.Timeout_Handler_Id := 0;

      Setting_Mark   : Boolean := False;
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

      Line_Info           : Line_Info_Display_Array_Access;
      --  The information that should be displayed in the left window.

      Real_Lines          : Natural_Array_Access;
      --  This array associates original line numbers (ie lines that were
      --  in the view the last time it was saved) with lines in the current
      --  view.

      Line_Data           : Line_Data_Array_Access;
      --  This array contains all data that are relative to lines: current
      --  highlighting, indentation, collapsing, etc.

      Original_Lines_Number : Natural := 1;
      --  The number of lines in the file on disk.

      Total_Column_Width  : Natural := 0;
      --  Width of the Left Window, in pixels.

      Original_Text_Inserted : Boolean := False;

      Extra_Information : Extra_Information_Array_Access;
      --  Extra information concerning the buffer.

      First_Removed_Line, Last_Removed_Line : Integer;
      --  These line indicate the lines that have just been removed in the
      --  editor. If First_Removed_Line <= 0, then no lines have been removed.

      Parse_Blocks : Boolean := False;
      --  Whether the block information should be parsed.
   end record;

end Src_Editor_Buffer;
