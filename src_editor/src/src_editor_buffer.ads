------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2017, AdaCore                     --
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
--  This package provides a text editor buffer with lots of extensions
--  to support many functionalities related to source code editing.
--  This includes for example syntax highlighting, auto-indent, cross-
--  reference support, etc.
--  </description>

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Calendar;
with Ada.Unchecked_Deallocation;
with System;

with GNAT.Strings;
with GNATCOLL.Projects;
with GNATCOLL.Symbols;
with GNATCOLL.VFS;
with GNATCOLL.Scripts;                use GNATCOLL.Scripts;

with Gdk.RGBA;
with Glib;                            use Glib;
with Glib.Main;
with Gtk;
with Gtk.Text_Iter;
with Gtk.Text_Mark;                   use Gtk.Text_Mark;
with Gtk.Text_Tag;
with Gtkada.Text_Buffer;              use Gtkada.Text_Buffer;

with Basic_Types;                     use Basic_Types;
with Commands;                        use Commands;
with GPS.Editors;                     use GPS.Editors;
with GPS.Editors.Line_Information;    use GPS.Editors.Line_Information;
with GPS.Kernel;
with GPS.Kernel.Style_Manager;        use GPS.Kernel.Style_Manager;
with GPS.Kernel.Messages.References;  use GPS.Kernel.Messages.References;
with Language.Tree;
with Src_Highlighting;
with Ada.Strings.Unbounded;
with GPS.Core_Kernels;                use GPS.Core_Kernels;
with Gtk.Clipboard;
with Language.Abstract_Language_Tree; use Language.Abstract_Language_Tree;

package Src_Editor_Buffer is
   type Source_Buffer_Record is new Gtkada_Text_Buffer_Record with private;
   type Source_Buffer is access all Source_Buffer_Record'Class;

   function "+" (B : access Source_Buffer_Record) return Source_Buffer
   is (Source_Buffer (B));

   type Slave_Cursor is private;
   type Slave_Cursor_Access is access all Slave_Cursor;

   type Cursor_Sync_Mode_Type is (Auto, Manual_Master, Manual_Slave);
   --  This type represents the mode the buffer is in regarding multi cursors
   --  behaviour

   type Cursors_Sync_Type (Mode : Cursor_Sync_Mode_Type := Auto) is private;

   package Marks_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Gtk.Text_Mark.Gtk_Text_Mark);

   procedure Gtk_New
     (Buffer : out Source_Buffer;
      Kernel : GPS.Kernel.Kernel_Handle;
      Lang   : Language.Language_Access := null);
   --  Create a new Source_Buffer with the given Language

   procedure Initialize
     (Buffer : access Source_Buffer_Record'Class;
      Kernel : GPS.Kernel.Kernel_Handle;
      Lang   : Language.Language_Access := null);
   --  Internal initialization procedure.
   --  See the section "Creating your own widgets" in the documentation.

   --  The following types define the different line types that are involved
   --  in the buffer:

   ----------------
   -- Line types --
   ----------------

   type Buffer_Line_Type is new Natural;
   --  Buffer lines correspond to lines actually in the buffer, ie all lines
   --  that are visible on the screen.

   type File_Line_Type is new Natural;
   --  File lines identify lines that were in the file the last time that the
   --  buffer was saved.

   type Loc_T is record
      Line : Editable_Line_Type;
      Col  : Character_Offset_Type;
   end record;

   function "<" (A, B : Loc_T) return Boolean
   is ((A.Line <= B.Line and then A.Col < B.Col) or else A.Line < B.Line);
   function "<=" (A, B : Loc_T) return Boolean is (A = B or else A < B);
   function ">=" (A, B : Loc_T) return Boolean is (not (A < B));
   function ">" (A, B : Loc_T) return Boolean is (not (A < B) and not (A = B));
   function Min (A, B : Loc_T) return Loc_T is
      (if A < B then A else B);

   ------------------
   -- Column types --
   ------------------

   function Convert (L : Natural) return Editable_Line_Type;
   function Convert (L : Editable_Line_Type) return Natural;
   --  ??? temporary ?

   overriding procedure Paste_Clipboard
     (Buffer      : not null access Source_Buffer_Record;
      Clipboard   : not null access Gtk.Clipboard.Gtk_Clipboard_Record'Class;
      Default_Editable : Boolean := True);

   overriding procedure Cut_Clipboard
     (Buffer     : not null access Source_Buffer_Record;
      Clipboard  : not null access Gtk.Clipboard.Gtk_Clipboard_Record'Class;
      Default_Editable : Boolean);

   overriding procedure Copy_Clipboard
     (Buffer    : not null access Source_Buffer_Record;
      Clipboard : not null access Gtk.Clipboard.Gtk_Clipboard_Record'Class);

   function Expand_Tabs
     (Buffer : access Source_Buffer_Record;
      Line   : Editable_Line_Type;
      Column : Character_Offset_Type) return Visible_Column_Type;
   --  Return the visible column corresponding to the position

   function Collapse_Tabs
     (Buffer : access Source_Buffer_Record;
      Line   : Editable_Line_Type;
      Column : Visible_Column_Type) return Character_Offset_Type;
   --  Return the character position corresponding to the visible column

   procedure Load_File
     (Buffer          : access Source_Buffer_Record;
      Filename        : GNATCOLL.VFS.Virtual_File;
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

   procedure Load_Empty_File (Buffer : access Source_Buffer_Record);
   --  Inform the buffer that the initial data has been loaded.
   --  Call this on every buffer for a "new" file.

   procedure Save_To_File
     (Buffer   : access Source_Buffer_Record;
      Filename : GNATCOLL.VFS.Virtual_File;
      Success  : out Boolean;
      Internal : Boolean := False;
      Force    : Boolean := False);
   --  Save the current buffer into a file. Success is set to False if this
   --  operation failed and the buffer could not be saved.
   --  If Internal is True, save the file to disk but do not modify the buffer
   --  status.

   procedure Mark_Buffer_Writable
     (Buffer   : not null access Source_Buffer_Record;
      Writable : Boolean;
      Explicit : Boolean);
   --  Change the writable/read-only status of the buffer

   function Get_Writable
     (Buffer : not null access Source_Buffer_Record) return Boolean;
   --  Return True if the buffer is writable, False otherwise

   function Get_Explicit_Writable_Set
     (Buffer : not null access Source_Buffer_Record) return Boolean;
   --  Return True if the buffer has been explicitely marked as
   --  writable/read-only.

   procedure Set_Language
     (Buffer : access Source_Buffer_Record;
      Lang   : Language.Language_Access);
   --  Set the language of the given buffer. The syntax highlighting
   --  is redone using the new language.
   --  It also memorize the language in the GPS properties, so that future
   --  uses of the same file use the same language automatically.

   function Get_Language
     (Buffer : access Source_Buffer_Record) return Language.Language_Access;
   --  Get the current language. Return null if the language is not set

   procedure Set_Strip_Trailing_Blanks
     (Buffer : access Source_Buffer_Record;
      Value  : Boolean);
   --  Set stripping behavior of the given buffer.
   --  It also memorize the setting in the GPS properties, so that future
   --  uses of the same file use the same setting automatically.

   function Get_Strip_Trailing_Blanks
     (Buffer : access Source_Buffer_Record) return Boolean;
   --  Get stripping behavior of the given buffer.

   procedure Set_Strip_Trailing_Lines
     (Buffer : access Source_Buffer_Record;
      Value  : Boolean);
   --  Set trailing empty lines stripping behavior of the given buffer.
   --  It also memorize the setting in the GPS properties, so that future
   --  uses of the same file use the same setting automatically.

   function Get_Strip_Trailing_Lines
     (Buffer : access Source_Buffer_Record) return Boolean;
   --  Get stripping behavior of the given buffer.

   procedure Set_Charset
     (Buffer : access Source_Buffer_Record; Charset : String);
   --  Set the charset to use for this buffer. If unset, the buffer will use
   --  the default charset specified in the preferences.
   --  The charset in used is memorized for future use of this file if needed.
   --  If the charset has changed, the file is reloaded if possible.

   function Get_Charset (Buffer : access Source_Buffer_Record) return String;
   --  Return the charset used for the buffer

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
      Column : Character_Offset_Type := 1) return Boolean;
   pragma Inline (Is_Valid_Position);
   --  Return True if the given cursor position is valid. If Column is
   --  set to 0, then this function just verifies the given line number
   --  (column 0 of a given line always exists).
   --
   --  Note that Get_Line_Count (inherited from Gtk_Text_Buffer) is also
   --  available when only the Line number needs to be checked.

   function Is_Valid_Position
     (Buffer : access Source_Buffer_Record;
      Line   : Editable_Line_Type;
      Column : Visible_Column_Type) return Boolean;
   pragma Inline (Is_Valid_Position);
   --  Same as above

   procedure Set_Cursor_Position
     (Buffer    : access Source_Buffer_Record;
      Line      : Editable_Line_Type;
      Column    : Character_Offset_Type;
      Internal  : Boolean;
      Extend_Selection : Boolean := False);
   --  Move the insert cursor to the given position.
   --
   --  The validity of the cursor position must be verified before invoking
   --  this procedure. An incorrect position will cause an Assertion_Failure
   --  when compiled with assertion checks, or an undefined behavior otherwise.
   --
   --  Internal should be set to True if the call is due to internal
   --  mechanics of GPS (ie, implementation of editor commands), and False if
   --  it is due to something external (ie, through python/xml commands).
   --
   --  If Extend_Selection is True, extend the selection from the current
   --  bound to the given position.

   procedure Get_Cursor_Position
     (Buffer : access Source_Buffer_Record;
      Iter   : out Gtk.Text_Iter.Gtk_Text_Iter);
   --  Return the current cursor position. Such a query should rather be done
   --  on the specific view in which you are interested

   procedure Get_Cursor_Position
     (Buffer : access Source_Buffer_Record;
      Line   : out Gint;
      Column : out Gint);
   --  Return the current cursor position

   procedure Get_Cursor_Position
     (Buffer : access Source_Buffer_Record;
      Line   : out Editable_Line_Type;
      Column : out Character_Offset_Type);
   --  Return the current editable cursor position

   procedure Get_Cursor_Position
     (Buffer : access Source_Buffer_Record;
      Line   : out Editable_Line_Type;
      Column : out Visible_Column_Type);
   --  Return the current editable cursor position

   procedure Get_Delimiters
     (Buffer           : access Source_Buffer_Record;
      On_Cursor_Iter   : Gtk.Text_Iter.Gtk_Text_Iter;
      First_Delim_Iter : out Gtk.Text_Iter.Gtk_Text_Iter;
      Last_Delim_Iter  : out Gtk.Text_Iter.Gtk_Text_Iter;
      Found            : out Natural;
      Counter_Max      : Natural := 16_384);
   --  Return the two delimiters surrounding the iter given in parameter.
   --  Found holds the number of delimiters found (0, 1 or 2)
   --  Counter_Max indicates the number of characters to explore around the
   --  delimiter: give a low value when this needs to return quickly.

   procedure Get_Screen_Position
     (Buffer : access Source_Buffer_Record;
      Iter   : Gtk.Text_Iter.Gtk_Text_Iter;
      Line   : out Gint;
      Column : out Gint);
   --  Return the cursor position corresponding to Iter, after expanding all
   --  the tabs.

   procedure Get_Iter_Position
     (Buffer : Source_Buffer;
      Iter   : Gtk.Text_Iter.Gtk_Text_Iter;
      Line   : out Editable_Line_Type;
      Column : out Character_Offset_Type);
   --  Return the current editable cursor position for Iter

   procedure Get_Iter_Position
     (Buffer : Source_Buffer;
      Iter   : Gtk.Text_Iter.Gtk_Text_Iter;
      Line   : out Editable_Line_Type;
      Column : out Visible_Column_Type);
   --  Return the current editable cursor position for Iter

   procedure Get_Iter_Position
     (Buffer : Source_Buffer;
      Iter   : Gtk.Text_Iter.Gtk_Text_Iter;
      Loc    : out Loc_T);
   --  Return the current editable cursor position for Iter

   procedure Get_Mark_Position
     (Buffer : Source_Buffer;
      Mark   : Gtk.Text_Mark.Gtk_Text_Mark;
      Loc    : out Loc_T);
   --  Return the current editable cursor position for Mark

   procedure Get_Screen_Position
     (Buffer : access Source_Buffer_Record;
      Line   : out Gint;
      Column : out Gint);
   --  Same as above, for the cursor position

   procedure Set_Extend_Existing_Selection
     (Buffer : not null access Source_Buffer_Record;
      Extend : Boolean);
   function Extend_Existing_Selection
     (Buffer        : not null access Source_Buffer_Record) return Boolean;
   --  See GPS.Editors.Set_Extend_Existing_Selection

   function Should_Extend_Selection
     (Buffer           : not null access Source_Buffer_Record;
      Extend_Selection : Boolean) return Boolean;
   --  Computes whether the selection should be extended as part of moving a
   --  cursor.
   --  The behavior depends on the Extend_Selection parameter (which basically
   --  indicates whether the user has used the modifier, typically 'shift, to
   --  force this behavior, and on whether Extend_Existing_Selection is true,
   --  to emulate Emacs-like selection.
   --  When this function returns True, a new selection should be created, if
   --  none exists, and then extended to include the new cursor location.

   procedure Get_Selection_Bounds
     (Buffer       : access Source_Buffer_Record;
      Start_Line   : out Editable_Line_Type;
      Start_Column : out Character_Offset_Type;
      End_Line     : out Editable_Line_Type;
      End_Column   : out Character_Offset_Type;
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
      Start_Column : Character_Offset_Type;
      End_Line     : Editable_Line_Type := 0;
      End_Column   : Character_Offset_Type := 0) return String;
   --  Return (as UTF-8) the text in range [Start, end).
   --  If End_Line is 0, get the entire range between start position and end
   --  of text.
   --  ??? Should return UTF-8 string ?

   procedure Forward_Position
     (Buffer       : access Source_Buffer_Record;
      Start_Line   : Editable_Line_Type;
      Start_Column : Character_Offset_Type;
      Length       : Integer;
      End_Line     : out Editable_Line_Type;
      End_Column   : out Character_Offset_Type);
   --  Return the position Length characters after Start_Line/Start_Column
   --  Negative values for Length are supported, moving the position
   --  backwards.

   procedure Insert
     (Buffer      : access Source_Buffer_Record;
      Line        : Editable_Line_Type;
      Column      : Character_Offset_Type;
      Text        : String;
      Enable_Undo : Boolean := True);
   --  Insert the given text in at the specified position.
   --
   --  The validity of the given position must be verified before invoking this
   --  procedure. An incorrect position will cause an Assertion_Failure when
   --  compiled with assertion checks, or an undefined behavior otherwise.
   --  If Enable_Undo is True, then the insertion action will be
   --  stored in the undo/redo queue.

   procedure Delete
     (Buffer      : access Source_Buffer_Record;
      Line        : Editable_Line_Type;
      Column      : Character_Offset_Type;
      Length      : Natural;
      Enable_Undo : Boolean := True);
   --  Delete Length characters after the specified position.
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
      Start_Column : Character_Offset_Type;
      End_Line     : Editable_Line_Type;
      End_Column   : Character_Offset_Type;
      Text         : String;
      Enable_Undo  : Boolean := True);
   --  Replace the text between the start and end positions by Text.
   --
   --  The validity of the given positions must be verified before invoking
   --  this procedure. An incorrect position will cause an Assertion_Failure
   --  when compiled with assertion checks, or an undefined behavior otherwise.

   function Ends_Word (Iter : Gtk.Text_Iter.Gtk_Text_Iter) return Boolean;
   function Starts_Word (Iter : Gtk.Text_Iter.Gtk_Text_Iter) return Boolean;
   function Inside_Word (Iter : Gtk.Text_Iter.Gtk_Text_Iter) return Boolean;
   --  Whether the iterator ends or starts a word. This takes '_' properly
   --  into account

   procedure Select_All (Buffer : access Source_Buffer_Record);
   --  Set the selection bounds from the begining to the end of the buffer

   procedure Select_Current_Word (Buffer : access Source_Buffer_Record);
   --  Select the word the cursor is on. The insert mark is placed at the end
   --  of the selection.
   --  If there is a selection of more than a single Word, keep the current
   --  selection.

   procedure Select_Region
     (Buffer       : access Source_Buffer_Record;
      Cursor_Iter  : Gtk.Text_Iter.Gtk_Text_Iter;
      Bound_Iter   : Gtk.Text_Iter.Gtk_Text_Iter);
   --  Select the region between the two iterators, and leave the cursor on
   --  Cursor_Iter.

   procedure Select_Region
     (Buffer       : access Source_Buffer_Record;
      Start_Line   : Gint;
      Start_Column : Gint;
      End_Line     : Gint;
      End_Column   : Gint);
   --  Select the given region.
   --  Takes Tabs into account when Expand_Tabs = True.
   --  This unselects the current selection if Start_Line == End_Line and
   --  Start_Column == End_Column

   procedure Select_Region
     (Buffer       : access Source_Buffer_Record;
      Start_Line   : Editable_Line_Type;
      Start_Column : Character_Offset_Type;
      End_Line     : Editable_Line_Type;
      End_Column   : Character_Offset_Type);
   --  Select the given region

   procedure Select_Region
     (Buffer       : access Source_Buffer_Record;
      Start_Line   : Editable_Line_Type;
      Start_Column : Visible_Column_Type;
      End_Line     : Editable_Line_Type;
      End_Column   : Visible_Column_Type);
   --  Same as above, working with user columns

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
   --  Refill selected text or the current paragraph if no selection is active.
   --
   --  The text is wrapped depending on the right margin defined by the column
   --  highlight preference. This routine handles simple text and comments. The
   --  indentation and comment detection is done according to the first line
   --  selected.

   procedure Newline_And_Indent
     (Buffer : access Source_Buffer_Record;
      As_Is : Boolean);
   --  Insert a newline and indent atomically

   function Should_Indent (Buffer : Source_Buffer) return Boolean;
   --  Return true if auto-indentation is supported for this buffer, and if
   --  the user has activated it.

   type Action_Type is
     (No_Action,      --  No action
      Insert_Text,    --  User is inserting non-blank graphical characters
      Insert_Spaces,  --  User is inserting spaces or tabs
      Insert_Line,    --  User is inserting lines
      Delete_Text,    --  User is deleting non-blank graphical characters
      Delete_Spaces,  --  User is deleting spaces or tabs
      Delete_Line,    --  User is deleting lines
      External        --  External action, not done by user
     );

   procedure Enqueue
     (Buffer      : access Source_Buffer_Record;
      Command     : Command_Access;
      User_Action : Action_Type);
   --  Enqueue an action in the Buffer queue
   --  User_Action represents the action that the user is doing, and must be
   --  set to No_Action if the command is not caused by user action.

   procedure Notify_Text_Drag_N_Drop
     (Buffer : not null access Source_Buffer_Record);
   --  Used to notify the buffer that the user performed a text drag n drop in
   --  the view.
   --  In this case, we should group the insertion and deletion commands that
   --  constitute the drag n drop action so that we revert both when undoing
   --  a text drag n drop.

   function Get_Kernel
     (Buffer : access Source_Buffer_Record) return GPS.Kernel.Kernel_Handle;
   --  Return the kernel associated to Buffer

   function Get_Filename
     (Buffer : access Source_Buffer_Record) return GNATCOLL.VFS.Virtual_File;
   --  Return the name of the file associated with Buffer.
   --  WARNING: For buffer corresponding to unnamed files VFS.No_File is
   --  returned. Use Get_File_Identifier in that case if you need to retrieve
   --  the virtual file associated with the buffer.

   procedure Set_Filename
     (Buffer : access Source_Buffer_Record;
      Name   : GNATCOLL.VFS.Virtual_File);
   --  Set the name of the file associated with Buffer to Name

   procedure Set_Title
      (Buffer : not null access Source_Buffer_Record;
       Title  : String);
   function Get_Title
      (Buffer : not null access Source_Buffer_Record) return String;
   --  Force the MDI tabs to use another name than the filename

   procedure Set_Initial_Dir
     (Buffer : access Source_Buffer_Record;
      Name   : GNATCOLL.VFS.Virtual_File);
   --  Set the directory in which we are going to create the file. This is
   --  useful only when creating new files, and Name is used to determine
   --  the directory shown in the file selector.

   function Get_Initial_Dir
     (Buffer : access Source_Buffer_Record) return GNATCOLL.VFS.Virtual_File;
   --  Return the directory specified in Set_Initial_Dir

   procedure Filename_Changed
     (Buffer : access Source_Buffer_Record'Class);
   --  Emit the "filename_changed" signal

   function Get_File_Identifier
     (Buffer : access Source_Buffer_Record) return GNATCOLL.VFS.Virtual_File;
   --  Return the identifier of the file associated with Buffer

   procedure Set_File_Identifier
     (Buffer : access Source_Buffer_Record;
      Name   : GNATCOLL.VFS.Virtual_File);
   --  Set the file identifier for Buffer. This identifier is used for
   --  unnamed files, so that they can be uniquely identified.
   --
   --  ??? This is not really a file name, although we use it as such in
   --  various contexts

   procedure Source_Lines_Folded
     (Buffer     : access Source_Buffer_Record;
      Start_Line : Editable_Line_Type;
      End_Line   : Editable_Line_Type);
   --  Emit the signal informing that source lines have been folded

   procedure Source_Lines_Unfolded
     (Buffer     : access Source_Buffer_Record;
      Start_Line : Editable_Line_Type;
      End_Line   : Editable_Line_Type);
   --  Emit the signal informing that source lines have been unfolded

   procedure Add_Controls (Buffer : access Source_Buffer_Record);
   --  Connect the Undo/Redo buttons to the queue containing the buffer
   --  commands. This MUST be called every time that the Buffer.Queue
   --  pointer is modified.

   procedure Remove_Controls (Buffer : access Source_Buffer_Record);
   --  Disconnect the Undo/Redo buttons from the queue containing the buffer
   --  commands. This MUST be called every time that the Buffer.Queue
   --  pointer is modified and the controls actually refer to Queue.

   procedure Register_View
     (Buffer : access Source_Buffer_Record; Add : Boolean);
   --  Register or Unregister a view for the buffer

   function Avoid_Cursor_Move_On_Changes
     (Buffer : access Source_Buffer_Record) return Boolean;
   --  When this return true, moving the text cursor should be avoided when
   --  doing e.g. insert and delete operations. This is particulary usefull
   --  when batching changes, e.g. doing a replace all.

   procedure Set_Avoid_Cursor_Move_On_Changes
     (Buffer : access Source_Buffer_Record; Value : Boolean);
   --  Set wether we should avoid to do cursor modifications in case of
   --  additions / deletions.

   type Source_Buffer_Array is array (Natural range <>) of Source_Buffer;
   function Buffer_List
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
      return Source_Buffer_Array;
   --  Return the list of all buffers currently edited. Each buffer appears
   --  only once even if multiple views exist.

   function Is_Inserting_Internally
     (Buffer  : access Source_Buffer_Record) return Boolean;
   --  Predicate to know if the buffer is in the middle of an internal
   --  insertion.

   procedure Add_Listener_Factory
     (Factory : Editor_Listener_Factory_Access);

   ---------------------
   -- Automatic saves --
   ---------------------

   function Is_Auto_Save (File : GNATCOLL.VFS.Virtual_File) return Boolean;
   --  Return True if File is an autosave file

   -------------------
   -- Buffer Status --
   -------------------

   subtype Status_Type is GPS.Kernel.File_Status;

   function Get_Status
     (Buffer : access Source_Buffer_Record) return Status_Type;
   --  Return the status of the buffer.
   --  Calculate the status from the queue position.

   procedure Status_Changed
     (Buffer : access Source_Buffer_Record'Class);
   --  Emit the "status_changed" signal

   procedure Set_Last_Status
     (Buffer : access Source_Buffer_Record'Class;
      Status : Status_Type);
   --  Set the last calculated status.

   function Needs_To_Be_Saved
     (Buffer : access Source_Buffer_Record'Class) return Boolean;
   --  Return True if the buffer needs to be saved

   function Has_Been_Saved
     (Buffer : access Source_Buffer_Record'Class) return Boolean;
   --  Return True if the buffer has been saved sucessfully on disk

   function Get_Tree
     (Buffer : access Source_Buffer_Record) return Semantic_Tree'Class;

   function Blocks_Are_Exact
     (Buffer : access Source_Buffer_Record) return Boolean;
   --  Return True iff the blocks information computed in the buffer

   -----------------------
   -- Extra Information --
   -----------------------

   type Line_Information_Access is access Line_Information_Record;
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Line_Information_Record, Line_Information_Access);

   procedure Free (Info : in out Line_Information_Access);
   --  Free memory associated with Info

   --  The following is related to extra information associated to the buffer,
   --  such as VCS status of the file.

   type Extra_Information_Record is record
      Identifier : GNAT.Strings.String_Access;
      Info       : Line_Information_Access;
      Tooltip    : GNAT.Strings.String_Access;
      Icon       : GNAT.Strings.String_Access;
   end record;
   type Extra_Information_Access is access Extra_Information_Record;

   procedure Free (Info : in out Extra_Information_Access);

   type Extra_Information_Array is
     array (Natural range <>) of Extra_Information_Access;
   type Extra_Information_Array_Access is access Extra_Information_Array;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Extra_Information_Array, Extra_Information_Array_Access);

   function Get_Extra_Information
     (Buffer : Source_Buffer) return Extra_Information_Array_Access;
   --  Return the extra information associated with the buffer

   package Message_Reference_List is new Ada.Containers.Doubly_Linked_Lists
     (Message_Reference);

   --  The following is related to information to be put in the side column

   type Line_Info_Width is record
      Messages : Message_Reference_List.List;
      Action   : Line_Information_Access;
      Set      : Boolean := False;
   end record;

   type Line_Info_Width_Array is array (Natural range <>) of Line_Info_Width;
   type Line_Info_Width_Array_Access is access Line_Info_Width_Array;
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
    (Line_Info_Width_Array, Line_Info_Width_Array_Access);

   type Line_Info_Display_Record is record
      Identifier : GNAT.Strings.String_Access;
      --  This identifies the column

      Starting_X : Integer;
      --  The pixel distance between the left border of the column and
      --  the left border of the left window.

      Width      : Integer;
      --  The pixel width of the column

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
   --  Return True if Line needs to be refreshed

   function Get_Total_Column_Width
     (Buffer : access Source_Buffer_Record) return Natural;
   --  Return the size of the total column width, in pixels

   type Highlight_Location is (Highlight_Speedbar, Highlight_Editor);
   type Highlight_Location_Array is array (Highlight_Location) of Boolean;
   pragma Pack (Highlight_Location_Array);
   --  The various locations where a highlight can be made visible

   procedure Set_Line_Highlighting
     (Editor       : access Source_Buffer_Record;
      Line         : Buffer_Line_Type;
      Style        : not null Style_Access;
      Set          : Boolean;
      Highlight_In : Highlight_Location_Array);
   --  Common function for [Add|Remove]_Line_Highlighting

   procedure Add_Line_Highlighting
     (Editor       : access Source_Buffer_Record;
      Line         : Editable_Line_Type;
      Style        : not null Style_Access;
      Highlight_In : Highlight_Location_Array);
   --  Enable the highlighting of Line using colors defined in category
   --  corresponding to Id.
   --  See Src_Editor_Box.Add_Line_Highlighting.

   procedure Remove_Line_Highlighting
     (Editor : access Source_Buffer_Record;
      Line   : Editable_Line_Type;
      Style  : not null Style_Access);
   --  Disable the highlighting of Line using colors defined in category
   --  corresponding to Id.
   --  See Src_Editor_Box.Remove_Line_Highlighting.

   function Get_Highlight_Color
     (Editor  : access Source_Buffer_Record;
      Line    : Buffer_Line_Type;
      Context : Highlight_Location) return Gdk.RGBA.Gdk_RGBA;
   pragma Inline (Get_Highlight_Color);
   --  Return the current highlighting for Line, or null if no highlighting
   --  is set.

   type Block_Record is record
      Indentation_Level : Integer := 0;
      --  Represent the indentation level of the block

      Offset_Start      : Integer := 0;
      --  The indentation offset of the first line of the block, in characters

      Stored_Offset     : Integer := 0;
      --  Stores the last calculated offset of this block. Mainly used to be
      --  able to draw the blocks almost correctly even as we are editing the
      --  text. Zero means that value is not calculated.

      First_Line        : Editable_Line_Type := 0;
      Last_Line         : Editable_Line_Type := 0;
      --  Indicate the lines that bound the block

      Name              : GNATCOLL.Symbols.Symbol;
      --  The name of the block, this is the subprogram or package name. This
      --  pointer is null for a block where name has no meaning.

      Block_Type        : Language.Language_Category := Language.Cat_Unknown;
      --  Indicates the type of the block, if Indentation_Level /= 0

      Color             : Gdk.RGBA.Gdk_RGBA := Gdk.RGBA.Null_RGBA;
      --  The color to use when highlighting this block

      Tree_Node         : Sem_Node_Holders.Holder;
      --  The iterator representing the current construct. This can only used
      --  until the file is modified, so should never be stored outside of this
      --  block_record
   end record;

   function Get_Block
     (Editor             : access Source_Buffer_Record;
      Line               : Editable_Line_Type;
      Update_Immediately : Boolean;
      Filter             : Language.Tree.Category_Array :=
        Language.Tree.Null_Category_Array;
      Column             : Visible_Column_Type := 1) return Block_Record;
   --  Return the block information associated with Line.
   --  If Update_Immediately is True, update the constructs information before
   --  returning the block.

   function Get_Subprogram_Block
     (Editor : access Source_Buffer_Record;
      Line   : Editable_Line_Type;
      Update_Tree : Boolean := False) return Block_Record;
   --  Same as above, with a filter that only selects blocks like subprograms
   --  and packages.

   function Has_Block_Information
     (Editor : access Source_Buffer_Record) return Boolean;
   --  Returh whether the buffer has relevant block information

   type Src_Editor_Action_Context is new GPS.Kernel.Action_Filter_Record
      with null record;
   overriding function Filter_Matches_Primitive
     (Context : access Src_Editor_Action_Context;
      Ctxt    : GPS.Kernel.Selection_Context) return Boolean;
   --  A key context that matches if the current widget is a source editor

   type Writable_Src_Editor_Action_Context
   is new GPS.Kernel.Action_Filter_Record
      with null record;
   overriding function Filter_Matches_Primitive
     (Context : access Writable_Src_Editor_Action_Context;
      Ctxt    : GPS.Kernel.Selection_Context) return Boolean;
   --  A key context that matches if the current widget is a writable
   --  source editor

   type Last_Editor_Action_Context is new GPS.Kernel.Action_Filter_Record
      with null record;
   overriding function Filter_Matches_Primitive
     (Context : access Last_Editor_Action_Context;
      Ctxt    : GPS.Kernel.Selection_Context) return Boolean;
   --  A key context that matches if the last source editor is available

   --------------
   -- Contexts --
   --------------
   --  Every time the cursor changes position, the editor recomputes the
   --  current context to update various pieces of information like the
   --  toolbar buttons. This can however be expensive when doing lots of
   --  manipulation, so it is possible to temporary freeze the context.

   procedure Freeze_Context
     (Self : not null access Source_Buffer_Record'Class)
     with Inline;
   procedure Thaw_Context
     (Self : not null access Source_Buffer_Record'Class)
     with Inline;
   --  Stop refreshing the GPS context every time the cursor moves.
   --  The number of calls to Thaw should match the number of calls to Freeze

   function Context_Is_Frozen
     (Self  : not null access Source_Buffer_Record'Class)
      return Boolean
     with Inline;
   --  Whether the context should be refreshed when the cursor position
   --  changes.

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
   --  - "filename_changed"
   --    procedure Handler (Buffer : Gtk_Object_Record'Class);
   --    Emitted when the filename has been changed.
   --
   --  - "closed"
   --    procedure Handler (Buffer : Gtk_Object_Record'Class);
   --    Emitted when the buffer is closing
   --
   --  </signals>

   Signal_Cursor_Position_Changed           : constant Signal_Name :=
                                                "cursor_position_changed";
   Signal_Side_Column_Changed               : constant Signal_Name :=
                                                "side_column_changed";
   Signal_Side_Column_Configuration_Changed : constant Signal_Name :=
                                          "side_column_configuration_changed";
   Signal_Line_Highlights_Changed           : constant Signal_Name :=
                                                "line_highlights_changed";
   Signal_Buffer_Information_Changed        : constant Signal_Name :=
                                                "buffer_information_changed";
   Signal_Status_Changed                    : constant Signal_Name :=
                                                "status_changed";
   Signal_Filename_Changed                  : constant Signal_Name :=
                                                "filename_changed";
   Signal_Closed                            : constant Signal_Name :=
                                                "closed";

   function Get_Buffer_Line
     (Buffer : access Source_Buffer_Record;
      Line   : Editable_Line_Type) return Buffer_Line_Type;
   pragma Inline (Get_Buffer_Line);
   --  Get the buffer line corresponding to Line.
   --  Return 0 if no buffer line was found.
   --  Note: Buffer lines are indexes in Buffer.Line_Data, ie are equal to
   --  lines in the actual Gtk_Text_Buffer, plus 1.

   function Get_String
     (Buffer : access Source_Buffer_Record'Class)
      return GNAT.Strings.String_Access;
   --  Return the entire editable string, encoded in UTF-8
   --  The caller is responsible for freeing the returned value.

   function Get_Buffer_Lines
     (Buffer     : access Source_Buffer_Record'Class;
      Start_Line : Editable_Line_Type;
      End_Line   : Editable_Line_Type) return GNAT.Strings.String_Access;
   --  Return the text from Start_Line to End_Line, included

   function Get_Byte_Index
     (Iter : Gtk.Text_Iter.Gtk_Text_Iter) return Natural;
   --  Return the byte index of the iterator given in parameter - as opposed
   --  to the character index (e.g. some UTF8 character are coded on more than
   --  one byte).

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
   --  Refresh the side columns in Buffer

   procedure End_Action (Buffer : access Source_Buffer_Record'Class);
   --  This procedure should be called every time that an internal
   --  event should cancel the current user action: focus switching
   --  to another window, cursor moved, etc.

   function In_Destruction
     (Buffer : access Source_Buffer_Record'Class) return Boolean;
   --  Similar to Gtk.Widget.In_Destruction

   function Get_Command_Queue
     (Buffer : access Source_Buffer_Record'Class) return Command_Queue;
   --  Return the command queue associated to Buffer

   procedure Prevent_CR_Insertion
     (Buffer  : access Source_Buffer_Record'Class;
      Prevent : Boolean := True);
   --  Whether the buffer should monitor the next text insertion and strip any
   --  CRs.

   procedure Get_Iter_At_Screen_Position
     (Buffer : access Source_Buffer_Record;
      Iter   : out Gtk.Text_Iter.Gtk_Text_Iter;
      Line   : Editable_Line_Type;
      Column : Visible_Column_Type);
   --  Return the iter at position (Line, Column), tab expansion included.
   --  If Line is not in the text, return the Iter at beginning of text.

   procedure Get_Iter_At_Screen_Position
     (Buffer : access Source_Buffer_Record;
      Iter   : out Gtk.Text_Iter.Gtk_Text_Iter;
      Line   : Editable_Line_Type;
      Column : Character_Offset_Type);
   --  Return the iter at position (Line, Column), tab expansion included.
   --  If Line is not in the text, return the Iter at beginning of text.

   ----------------
   -- Src_String --
   ----------------

   type Src_String is record
      Contents  : Unchecked_String_Access;
      Length    : Natural := 0;
      Read_Only : Boolean := False;
   end record;
   --  Special purpose string type to avoid extra copies and string allocation
   --  as much as possible.
   --  The actual contents of a Src_String is represented by
   --  Contents (1 .. Length) (as utf8-encoded string)
   --  Never use Free (Contents) directly, use the Free procedure below.

   function To_String (S : Src_String) return String;
   --  Return the string in Src_String, and the empty string if S is null

   procedure Free (S : in out Src_String);
   --  Free the memory associated with S

   function Get_String
     (Buffer : Source_Buffer;
      Line   : Editable_Line_Type) return Src_String;
   --  Return the string at line Line, without the line terminator.
   --  Return null if the Line is not a valid line or there is no contents
   --  associated with the line.
   --  The caller is responsible for freeing the returned value..
   --  The returned string is UTF8-encoded

   procedure Set_In_Completion
     (Buffer        : Source_Buffer;
      In_Completion : Boolean);
   function In_Completion (Buffer : Source_Buffer) return Boolean;
   --  Get/set the flag that indicates whether we are currently in a completion

   --------------------
   -- Casing support --
   --------------------

   procedure Add_Typed_Char
     (Buffer : access Source_Buffer_Record'Class;
      C      : Gunichar);
   --  Add a character into the as-typed buffer

   procedure Delete_Last_Typed_Char
     (Buffer : access Source_Buffer_Record'Class);
   --  Delete last typed character

   procedure Clear_Typed_Chars (Buffer : access Source_Buffer_Record'Class);
   pragma Inline (Clear_Typed_Chars);
   --  Clear the whole buffer

   function Get_Typed_Chars
     (Buffer : access Source_Buffer_Record'Class;
      N      : Positive) return Basic_Types.UTF8_String;
   --  Returns the N last typed characters

   function Get_Version
     (Buffer : access Source_Buffer_Record'Class) return Integer;
   --  Return the version of the buffer. Version is an integer that is
   --  incremented every time the buffer is modified

   procedure Start_Undo_Group (Buffer : access Source_Buffer_Record'Class);
   procedure Finish_Undo_Group (Buffer : access Source_Buffer_Record'Class);
   --  Start / Finish an undo group on this buffer.
   --  This shouldn't be called except by GNATbench and by the Python
   --  interfacing in GPS. From the Ada code in GPS, call Current_Undo_Group
   --  and New_Undo_Group.

   function Current_Undo_Group
     (Buffer : access Source_Buffer_Record'Class) return Group_Block;
   --  Enter the current undo/redo group: as long as we are in the scope
   --  where Group_Block is valid, editor commands will go to the current
   --  undo group.

   function New_Undo_Group
     (Buffer : access Source_Buffer_Record'Class) return Group_Block;
   --  Enter a new undo/redo group: as long as we are in the scope
   --  where Group_Block is valid, editor commands will go to a new
   --  undo group, separate from the previous one and the following one.

   procedure Enable_Highlighting (Buffer : access Source_Buffer_Record'Class);
   procedure Disable_Highlighting (Buffer : access Source_Buffer_Record'Class);
   --  Suppress highlighting in the Buffer for a while

   package Listener_Factory_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Editor_Listener_Factory_Access);

   Listener_Factories : Listener_Factory_Lists.List;

   type Editor_Buffer_Access is access all GPS.Editors.Editor_Buffer'Class;

   function Get_Editor_Buffer
     (Buffer : access Source_Buffer_Record'Class) return Editor_Buffer_Access;
   --  Get the abstract instance of Editor_Buffer corresponding to this
   --  Source_Buffer_Record

   function Get_Global_Editor_Buffer_Factory
     return access GPS.Editors.Editor_Buffer_Factory'Class;
   --  Get the global Editor_Buffer_Factory. Useful to access buffers in an
   --  abstract way (for example, opening new buffers)

private

   procedure Set_Cursor_Position
     (Buffer    : access Source_Buffer_Record;
      Line      : Gint;
      Column    : Gint;
      Internal  : Boolean;
      Extend_Selection : Boolean := False);
   --  Move the insert cursor to the given position.
   --
   --  The validity of the cursor position must be verified before invoking
   --  this procedure. An incorrect position will cause an Assertion_Failure
   --  when compiled with assertion checks, or an undefined behavior otherwise.
   --  This is obsolete, Set_Cursor_Position above should be called.
   --
   --  Internal should be set to True if the call is due to internal
   --  mechanics of GPS (ie, implementation of editor commands), and False if
   --  it is due to something external (ie, through python/xml commands).
   --

   --  If Extend_Selection is True, extend the selection from the current
   --  bound to the given position.

   procedure Find_Current_Comment_Paragraph
     (Buffer : not null access Source_Buffer_Record;
      Line   : Editable_Line_Type;
      Start_Line, End_Line : out Editable_Line_Type);
   --  Find the bounds of the current comment paragraph that includes Line.
   --  * If Line is not a comment, returns the bounds for that line.
   --  * Otherwise, search backward and forward for either the first blank
   --    comment line, or the first line not in a comment, and use those bounds
   --
   --  This computation relies on syntax highlighting and will always only
   --  return the current line if syntax highlighting has not been activated

   function Is_In_Comment
     (Buffer : Source_Buffer;
      Iter   : Gtk.Text_Iter.Gtk_Text_Iter) return Boolean;
   --  Returns true if Iter is in a comment. This relies on syntax coloring and
   --  will return False if the syntax coloring has not been computed for Iter.

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

   procedure Buffer_Information_Changed
     (Buffer : access Source_Buffer_Record'Class);
   --  Emit the "buffer_information_changed" signal

   procedure Line_Highlights_Changed
     (Buffer : access Source_Buffer_Record'Class);
   --  Emit the "Line_Highlights_Changed" signal

   procedure Register_Edit_Timeout
     (Buffer : access Source_Buffer_Record'Class);
   --  Indicate that the text has been edited, and that a timeout should be
   --  registered to call the corresponding "after-timeout" hook.

   procedure Emit_New_Cursor_Position
     (Buffer : access Source_Buffer_Record'Class);
   --  Signal the new cursor position by emitting the "cursor_position_changed"
   --  signal.

   ---------------
   -- Line data --
   ---------------

   type Block_Access is access Block_Record;
   type Boolean_Array is array (Natural range <>) of Boolean;
   type Boolean_Array_Access is access Boolean_Array;

   type Highlighting_Data_Record is record
      Enabled : Boolean_Array_Access;
      --  This array corresponds to the categories in Source_Editor_Module_Id.
      --  If an item is set to True, that means that line highlighting is
      --  enabled for that categories.
      --  For simplicity, the range of this array should match the range of
      --  the array of categories in the cache.

      Active  : Natural;
      --  This is the category to use for highlighting
   end record;

   type Highlighting_Data_Array is
     array (Highlight_Location) of Highlighting_Data_Record;

   type Line_Data_Record is record
      Side_Info_Data : Line_Info_Width_Array_Access;
      --  The array corresponding to information to be displayed in columns,
      --  indexed on columns.

      Editable_Line      : Editable_Line_Type;
      --  The line in the real buffer

      Line_Mark          : Gtk.Text_Mark.Gtk_Text_Mark;
      --  The mark used for referencing special lines, for example.
      --  -1 if there is no marker for this line.

      File_Line          : File_Line_Type;
      --  The corresponding line in the file corresponding to Buffer.
      --  0 if the line is not in the file.

      Highlighting : Highlighting_Data_Array;
      --  Highlighting information.
   end record;

   -----------------------
   -- Line highlighting --
   -----------------------

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Boolean_Array, Boolean_Array_Access);

   New_Block : constant Block_Record :=
     (0, 0, 0, 0, 0, GNATCOLL.Symbols.No_Symbol, Language.Cat_Unknown,
      Gdk.RGBA.Null_RGBA, Sem_Node_Holders.Empty_Holder);

   procedure Create_Side_Info
     (Buffer : access Source_Buffer_Record;
      Line   : Buffer_Line_Type);
   --  Create blank Side_Info_Data

   New_Line_Data : constant Line_Data_Record :=
     (null, 0, null, 0, (others => (null, 0)));

   type Line_Data_Array is array (Buffer_Line_Type range <>) of
     Line_Data_Record;
   type Line_Data_Array_Access is access Line_Data_Array;

   procedure Reset_Blocks_Info (Buffer : access Source_Buffer_Record'Class);
   --  Reset block information used by Data

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Line_Data_Array, Line_Data_Array_Access);

   type Line_Terminator_Style is (Unknown, LF, CR, CR_LF);
   --  The line terminator style of the given buffer

   procedure Free
     (Buffer        : access Source_Buffer_Record;
      X             : in out Line_Info_Width;
      Free_Messages : Boolean);
   --  Free memory associated to X
   --  If Free_Messages, then remove the messages from the buffer.

   --------------------------
   -- Recursion protection --
   --------------------------

   function Inserting
     (Buffer : access Source_Buffer_Record'Class) return Boolean;
   --  Return True if we are inserting internally

   procedure Start_Inserting
     (Buffer : access Source_Buffer_Record'Class);
   --  Call this to notify the buffer that we are starting to insert internally

   procedure End_Inserting
     (Buffer : access Source_Buffer_Record'Class);
   --  Call this to notify the buffer that we have stopped inserting internally

   --------------------
   -- Universal line --
   --------------------

   --  An universal line can store either a special line or an editable line

   type Line_Nature is (Editable, Special);

   type Universal_Line is record
      Nature : Line_Nature;
      Data   : Line_Data_Record;

      Text               : GNAT.Strings.String_Access;
      --  The text contained in the original special line

      Line_Mark          : Gtk.Text_Mark.Gtk_Text_Mark;
      --  The mark used for referencing special lines, for example

      --  ??? Need to store the line category
   end record;

   package Lines_List is new Ada.Containers.Doubly_Linked_Lists
     (Universal_Line);

   type Universal_Line_Access is access Universal_Line;
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Universal_Line, Universal_Line_Access);

   --------------------
   -- Editable lines --
   --------------------

   type Line_Location_Type is (In_Buffer, In_Mark);

   type Editable_Line_Data (Where : Line_Location_Type := In_Buffer) is record
      Stored_Lines   : Lines_List.List;
      --  The list of stored lines

      Stored_Editable_Lines : Natural := 0;
      --  Caches the number of editable lines stored in Stored_Lines,
      --  recursively.

      case Where is
         when In_Buffer =>
            Buffer_Line : Buffer_Line_Type;

         when In_Mark =>
            Text : GNAT.Strings.String_Access := null;
            --  ??? This string is UTF-8, it should be marked as so!

            UL   : Universal_Line_Access := null;
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

   Max_Typed_Chars : constant := 100;
   --  100 is the maximum length of the identifier that will be handled by the
   --  conservative casing circuitry. Past this length the casing (as-typed)
   --  won't be kept. Note that this needs to be long enough as we do not want
   --  to cover only the keywords but also identifier casing exceptions.

   type Last_Typed_Chars is array (1 .. Max_Typed_Chars) of Gunichar;
   --  The array to store the last typed characters in a buffer

   ------------------
   -- Slave_Cursor --
   ------------------

   type Slave_Cursor is record
      Id                       : Integer := -1;
      Mark                     : Gtk.Text_Mark.Gtk_Text_Mark;
      Sel_Mark                 : Gtk.Text_Mark.Gtk_Text_Mark;
      Current_Command          : Command_Access;
      Column_Memory            : Gint := 0;
      Clipboard                : Ada.Strings.Unbounded.Unbounded_String :=
        Ada.Strings.Unbounded.To_Unbounded_String ("");
   end record;
   --  Represents the information we have to store about each multi-cursor
   --  The Mark field is the mark representing the multi cursor in the buffer
   --  The Current_Command field is the last command relative to this mark, so
   --  that we can perform command aggregation (see multiple insertions as one
   --  for example)

   package Slave_Cursors_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Slave_Cursor);

   type Cursors_Sync_Type (Mode : Cursor_Sync_Mode_Type := Auto) is record
      case Mode is
         when Manual_Slave => MC : Slave_Cursor_Access;
         when others => null;
      end case;
   end record;

   package Listener_Lists is
     new Ada.Containers.Doubly_Linked_Lists (Editor_Listener_Access);

   type Line_Position_Kind is (At_Begin, At_End, Other);
   --  In what place of line is text inserted or deleted

   --------------------------
   -- Source_Buffer_Record --
   --------------------------

   type Source_Buffer_Record is new Gtkada_Text_Buffer_Record with record
      Kernel          : GPS.Kernel.Kernel_Handle;
      Filename        : GNATCOLL.VFS.Virtual_File;
      File_Identifier : GNATCOLL.VFS.Virtual_File;
      --  This identifier is used to identify buffers for untitled files

      Initial_Dir     : GNATCOLL.VFS.Virtual_File;
      --  Where to save the file initially, for editors for empty files

      Forced_Title  : GNAT.Strings.String_Access;
      --  If set, this title is used for the window, instead of the file name

      Lang          : Language.Language_Access;
      Syntax_Tags   : Src_Highlighting.Highlighting_Tags;
      Delimiter_Tag : Gtk.Text_Tag.Gtk_Text_Tag;
      --  A tag used when highlighting delimiters (e.g. parens)

      Non_Editable_Tag : Gtk.Text_Tag.Gtk_Text_Tag;
      --  A tag for text that cannot be interactively deleted

      Extend_Existing_Selection : Boolean := False;
      --  See Set_Extend_Existing_Selection

      In_Text_Drag_N_Drop : Boolean := False;
      --  Whether we are performing a drag n drop. Used to group the insertion
      --  and deletion commands that belong to the drag n drop user action.

      Insert_Mark      : Gtk.Text_Mark.Gtk_Text_Mark;
      --  This is a copy of the "insert" mark.
      --  This could be easily looked-up when needed, but having a copy is
      --  helping performance-wise, since a  lot of subprograms use it.
      --  This must always be a valid text mark.

      Inserting_Count : Natural := 0;
      --  If >= 1, this means we are modifying text due to internal operation
      --  Used to avoid recursion, when using commands.
      --  Do not reference directly, see section "Recursion protection" above

      Inserting_Position : Line_Position_Kind := At_Begin;
      --  Holds place where new text is inserting

      No_Cursor_Move_On_Changes : Boolean := False;
      --  When this is true, we'll avoid moving the cursor when performing
      --  changes (e.g. addition or deletions).

      Do_Not_Move_Cursor : Boolean := False;
      --  Used to disable functions moving the cursor or emit the
      --  "cursor_position_changed" signal when we know we are going to
      --  move the cursor a lot.

      Queue           : Command_Queue;
      --  Contains the queue of editor commands for this editor

      Saved_Position  : Integer := 0;
      --  The saved position in the command queue

      Current_Command : Command_Access := null;
      --  The current editor command. Belongs to Queue, defined above

      Current_Status  : GPS.Kernel.File_Status := GPS.Kernel.Unmodified;
      --  The current buffer status

      Number_Of_Views : Integer := 0;
      --  The number of objects viewing the buffer

      Modified_Auto : Boolean := False;
      --  Whether the buffer has been modified since last auto save

      Line_Terminator : Line_Terminator_Style := Unknown;

      Strip_Trailing_Blanks : Boolean := True;
      --  Whether the buffer should strip trailing spaces

      Strip_Trailing_Lines : Boolean := True;
      --  Whether the buffer should strip empty trailing lines

      Timeout_Id         : Glib.Main.G_Source_Id := 0;
      Timeout_Registered : Boolean := False;
      --  Whether Timeout corresponds to a registered timeout

      Setting_Mark : Boolean := False;
      --  Used to prevent recursion when creating text marks

      Has_Delimiters_Highlight   : Boolean := False;
      --  Whether delimiters are currently highlighted

      Start_Delimiters_Highlight : Gtk.Text_Mark.Gtk_Text_Mark;
      End_Delimiters_Highlight   : Gtk.Text_Mark.Gtk_Text_Mark;
      --  Bounds for the parenthesis highlighting

      --  The following is related to information regarding
      --  the side column information.

      Editable_Line_Info_Columns : Columns_Config_Access;
      --  The information concerning columns of data that should be displayed
      --  in the left window.
      --  Must never be null.

      Editable_Lines : Editable_Line_Array_Access;
      --  Reference array for editable lines

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
      --  The number of lines in the file on disk

      Total_Column_Width : Natural := 0;
      --  Width of the Left Window, in pixels

      Line_Numbers_Width : Natural := 0;
      --  Width allocated to line numbers in the Left Window, in pixels

      Original_Text_Inserted : Boolean := False;

      Extra_Information : Extra_Information_Array_Access;
      --  Extra information concerning the buffer

      First_Removed_Line, Last_Removed_Line : Buffer_Line_Type;
      --  These line indicate the lines that have just been removed in the
      --  editor. If First_Removed_Line = 0, then no lines have been removed.

      Parse_Blocks : Boolean := False;
      --  Whether the block information should be parsed on-the-fly whenever
      --  the text is edited.

      Blocks_Timeout_Registered : Boolean := False;
      --  Whether the blocks need to be recomputed

      Blocks_Timeout : Glib.Main.G_Source_Id := Glib.Main.No_Source_Id;
      --  A timeout handling the refresh of the timeouts

      Blocks_Request_Timestamp : Ada.Calendar.Time;
      --  The last time the blocks refresh was requested

      Cursor_Timeout : Glib.Main.G_Source_Id;
      --  A timeout handling the refresh of the timeouts

      Blank_Lines : Natural := 0;
      --  The number of blank lines in the buffer

      Hidden_Lines : Natural := 0;
      --  The number of hidden lines in the buffer

      Block_Folding : Boolean := False;
      --  Whether the editor buffer should allow block folding

      Block_Highlighting : Boolean := False;
      --  Whether the editor buffer should allow block highlighting

      Block_Highlighting_Column : Integer := -1;
      --  The column (index in Buffer_Line_Info_Columns) that contains the
      --  block information. Set to a negative value if the column does not
      --  exist.

      Modifying_Real_Lines : Boolean := False;
      --  Set to True when we are currently modifying the range of the real
      --  lines (ie when we are folding/unfolding text or adding/removing
      --  blank lines.

      --  The following information are used for idle buffer highlighting

      First_Highlight_Mark : Gtk.Text_Mark.Gtk_Text_Mark;
      Last_Highlight_Mark  : Gtk.Text_Mark.Gtk_Text_Mark;
      --  Those marks indicate the minimum area that need to be highlighted.
      --  They must be valid marks at all times.

      Highlight_Needed : Boolean := False;
      --  Whether the text should be re-highlighted

      Highlight_Enabled : Boolean := True;
      --  Endable/Disable immediate highlighting after each insert/delete

      Auto_Syntax_Check : Boolean := False;
      --  Whether the syntax should be checked automatically

      Highlight_Delimiters : Boolean := False;
      --  Cache corresponding preference

      Tab_Width : Positive := 8;
      --  Width of a Tab character

      In_Destruction : Boolean := False;
      --  Indicates whether the buffer is currently being destroyed

      Charset : GNAT.Strings.String_Access;
      --  The charset associated with the buffer

      Save_Complete : Boolean := True;
      --  Whether the buffer was saved successfully during the last attempt.
      --  This can be False if only an approximate save was done, for
      --  instance if characters have been lost in the conversion from UTF8.

      Blocks_Exact : Boolean := False;
      --  Whether the blocks information is exact

      Writable : Boolean := True;
      --  Whether the buffer is currently writable or read-only

      Explicit_Writable_Set : Boolean := False;
      --  Whether the user has manually toggled the editor read-only
      --  or writable.

      Prevent_CR_Insertion : Boolean := False;
      --  Whether the buffer should monitor every text inserted and strip it
      --  of potential CRs.

      In_Completion        : Boolean := False;
      --  Whether we are in an autocompletion loop

      Last_User_Action     : Action_Type := No_Action;

      Typed_Chars          : Last_Typed_Chars;
      Index                : Natural := 0;
      --  Records last typed chars to help auto-casing restore as much as
      --  possible the user's original casing.

      Hyper_Mode                            : Boolean := False;
      --  Whether we are currently in Hyper mode

      Hyper_Mode_Tag                        : Gtk.Text_Tag.Gtk_Text_Tag;
      --  The tag used for highlighting hyper mode items

      Hyper_Mode_Has_Highlight              : Boolean := False;
      --  Whether Hyper Mode is currently highlighting a section of the text

      Hyper_Mode_Current_Action             : Subprogram_Type := null;
      --  Indicates the current primary action for the highlighted text

      Hyper_Mode_Current_Alternate          : Subprogram_Type := null;
      --  Indicates the current alternate action for the highlighted text

      Hyper_Mode_Highlight_Begin            : Gtk.Text_Mark.Gtk_Text_Mark;
      Hyper_Mode_Highlight_End              : Gtk.Text_Mark.Gtk_Text_Mark;
      --  The begin and end of the highlighted section

      Has_MC_Clipboard                      : Boolean := False;
      --  Has there been a cut/copy with multi cursors active ?

      Slave_Cursors_List                    : Slave_Cursors_Lists.List;
      --  The list of all active multi cursors

      Slave_Cursors_Last_Alive_Id           : Natural := 0;
      Slave_Cursors_Next_Id                 : Natural := 1;
      --  Unique id for the next multi cursor. Incremented at multi cursor
      --  creation

      Cursors_Delete_Offset                 : Gint := 0;
      --  Internal field used between before and after delete events handlers
      --  Represents a simple deletion. +5 means delete 5 chars forward.
      --  -5 means delete 5 chars backward. 0 means do nothing.

      Cursors_Sync                          : Cursors_Sync_Type;
      --  The sync mode of the buffer. The operating mode is detailed precisely
      --  in the public procedures related to sync, in
      --  Src_Editor_Buffer.Cursors.

      Cursor_Column_Memory                  : Gint := 0;
      --  Memory for the horizontal of the cursor when moving from line to line

      Editor_Buffer : Editor_Buffer_Access;
      Listeners : Listener_Lists.List;

      Last_Checked_Version : Integer := -1;
      Version              : Integer := -1;

      Context_Frozen       : Integer := 0;
      --  GPS context is refreshed every time the cursor position changes and
      --  this variable is set to 0. See Freeze_Context and Thaw_Context.
   end record;

   procedure Emit_By_Name
     (Object : System.Address;
      Name   : Signal_Name);
   pragma Import (C, Emit_By_Name, "ada_g_signal_emit_by_name");

   Default_Column : constant String := "Block Information";
   --  Identifier for the block information column

end Src_Editor_Buffer;
