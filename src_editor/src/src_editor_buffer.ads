-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                        Copyright (C) 2001                         --
--                            ACT-Europe                             --
--                                                                   --
-- GLIDE is free software; you can redistribute it and/or modify  it --
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

--  <description>
--  This package provides a text editor buffer with lots of extensions
--  to support many functionalities related to source code editing.
--  This includes for example syntax highlighting, auto-indent, cross-
--  reference support, etc.
--  </description>

with Glib;             use Glib;
with Gtk;
with Gtk.Text_Buffer;
with Gtk.Text_Iter;
with Gtk.Text_Mark;
with Gtk.Text_Tag;
with Gtkada.Types;

with Language;
with Src_Highlighting;

package Src_Editor_Buffer is

   type Source_Buffer_Record is new Gtk.Text_Buffer.Gtk_Text_Buffer_Record
     with private;
   type Source_Buffer is access all Source_Buffer_Record'Class;

   procedure Gtk_New
     (Buffer : out Source_Buffer;
      Lang   : Language.Language_Access := null);
   --  Create a new Source_Buffer with the given Language.

   procedure Initialize
     (Buffer : access Source_Buffer_Record'Class;
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

   procedure Clear (Buffer : access Source_Buffer_Record);
   --  Delete all characters from the given buffer, leaving an empty buffer.

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

   procedure Get_Cursor_Position
     (Buffer : access Source_Buffer_Record;
      Line   : out Gint;
      Column : out Gint);
   --  Return the current cursor position

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
      End_Line     : Gint;
      End_Column   : Gint) return String;
   --  Return the text located between (Start_Line, Start_Column) and
   --  (End_Line, End_Column).
   --
   --  The validity of both start and end positions must be verified before
   --  invoking this function. An incorrect position will cause an
   --  Assertion_Failure when compiled with assertion checks, or an undefined
   --  behavior otherwise.

   function Get_Slice
     (Buffer       : access Source_Buffer_Record;
      Start_Line   : Gint;
      Start_Column : Gint;
      End_Line     : Gint;
      End_Column   : Gint) return Gtkada.Types.Chars_Ptr;
   --  Same as above but return the C pointer directly for efficiency.
   --  The caller is responsible for freeing the memory (with g_free).

   procedure Insert
     (Buffer  : access Source_Buffer_Record;
      Line    : Gint;
      Column  : Gint;
      Text    : String);
   --  Insert the given text in at the specified position.
   --
   --  The validity of the given position must be verified before invoking this
   --  procedure. An incorrect position  will cause an Assertion_Failure when
   --  compiled with assertion checks, or an undefined behavior otherwise.

   procedure Replace_Slice
     (Buffer       : access Source_Buffer_Record;
      Start_Line   : Gint;
      Start_Column : Gint;
      End_Line     : Gint;
      End_Column   : Gint;
      Text         : String);
   --  Replace the text between the start and end positions by Text.
   --
   --  The validity of the given positions must be verified before invoking
   --  this procedure. An incorrect position  will cause an Assertion_Failure
   --  when compiled with assertion checks, or an undefined behavior otherwise.

   procedure Delete_Slice
     (Buffer       : access Source_Buffer_Record;
      Start_Line   : Gint;
      Start_Column : Gint;
      End_Line     : Gint;
      End_Column   : Gint);
   --  Replace the text between the start and end positions by Text.
   --
   --  The validity of the given positions must be verified before invoking
   --  this procedure. An incorrect position  will cause an Assertion_Failure
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

   procedure Highlight_Region
     (Buffer : access Source_Buffer_Record;
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

private

   type Source_Buffer_Record is new Gtk.Text_Buffer.Gtk_Text_Buffer_Record with
   record
      Lang          : Language.Language_Access;
      Syntax_Tags   : Src_Highlighting.Highlighting_Tags;
      HL_Line_Tag   : Gtk.Text_Tag.Gtk_Text_Tag;
      --  A tag used when highlighting lines

      HL_Region_Tag : Gtk.Text_Tag.Gtk_Text_Tag;
      --  A tag used when highlighting regions

      Insert_Mark   : Gtk.Text_Mark.Gtk_Text_Mark;
      --  This is a copy of the "insert" mark. This could be easily looked-up
      --  when needed, but having a copy is helping performance-wise, since a
      --  lot of subprograms use it.

      Inserting     : Boolean := False;
   end record;

end Src_Editor_Buffer;
