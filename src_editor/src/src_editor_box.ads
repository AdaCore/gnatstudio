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

--  <description>
--  This package provides an object that represents the source editor.
--  The source editor is composed of the following entities:
--    - A Source_View, with vertical and horizontal scrollbars
--    - A status bar at the bottom containing
--        - the current file name
--        - the line and column number of the insert cursor
--  </description>

with Gtk.Box;
with Gtk.Container;
with Gtk.Label;

with Language;
with GNAT.OS_Lib;         use GNAT.OS_Lib;
with Glide_Kernel;
with Src_Editor_Buffer;
with Src_Editor_View;
with Src_Info;

package Src_Editor_Box is

   type Source_Editor_Box_Record is private;
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
      Source : access Source_Editor_Box_Record);
   --  Create a new view of the given box.
   --  ??? Do we want to copy the font attributes as well, or do we want
   --  ??? to add another parameter?

   procedure Destroy (Box : in out Source_Editor_Box);
   --  Destroy the given Source_Editor_Box, then set it to null.

   procedure Attach
     (Box    : access Source_Editor_Box_Record;
      Parent : access Gtk.Container.Gtk_Container_Record'Class);
   --  Attach Box to the given Parent, if possible.

   procedure Detach
     (Box    : access Source_Editor_Box_Record);
   --  Detach Box of its Parent, if possible.

   ------------------------------------
   -- Source_Buffer related services --
   ------------------------------------

   function Get_Kernel
     (Editor : access Source_Editor_Box_Record)
      return Glide_Kernel.Kernel_Handle;
   --  ??? Temporary function for testing purposes... Should be removed.

   procedure Set_Filename
     (Editor   : access Source_Editor_Box_Record;
      Filename : String);
   --  Change the filename of the given editor to be Filename.

   function Get_Filename
     (Editor : access Source_Editor_Box_Record) return String;
   --  Return the filename associated the given Editor. Return the empty
   --  string if Editor does not have any filename.

   procedure Load_File
     (Editor          : access Source_Editor_Box_Record;
      Filename        : String;
      Lang_Autodetect : Boolean := True;
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

   procedure Save_To_File
     (Editor   : access Source_Editor_Box_Record;
      Filename : String := "";
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

   procedure Set_Cursor_Location
     (Editor : access Source_Editor_Box_Record;
      Line   : Positive;
      Column : Positive := 1);
   --  Move the insert cursor to the given location. Success is set to False
   --  if the position is outside of the buffer.
   --
   --  The position must be verified before invoking this procedure. An invalid
   --  position leads to an Assertion_Failure when compiled with assertion
   --  checks, or to an undefined behavior otherwise.

   procedure Get_Cursor_Location
     (Editor  : access Source_Editor_Box_Record;
      Line    : out Positive;
      Column  : out Positive);
   --  Get the current cursor position.

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

   function Get_Selection
     (Editor : access Source_Editor_Box_Record) return String;
   --  If a portion of the buffer is currently selected, then return this
   --  portion. Otherwise, return the empty string.
   --
   --  This procedure is faster than the Get_Selection_Bounds + Get_Slice
   --  sequence because it does not work with (line, column) locations but
   --  directly with buffer iterators.

   procedure Search
     (Editor             : access Source_Editor_Box_Record;
      Pattern            : String;
      Case_Sensitive     : Boolean := True;
      Whole_Word         : Boolean := False;
      Search_Forward     : Boolean := True;
      From_Line          : Positive := 1;
      From_Column        : Positive := 1;
      Found              : out Boolean;
      Match_Start_Line   : out Positive;
      Match_Start_Column : out Positive;
      Match_End_Line     : out Positive;
      Match_End_Column   : out Positive);
   --  Search function. Regular expressions for Pattern are not supported.
   --  If the pattern is found, then Found is set to True and the positions
   --  of the begining and of the end of the matching portion are returned.
   --
   --  The validity of the start position must be verified before invoking
   --  this function. An incorrect position will cause an Assertion_Failure
   --  when compiled with assertion checks, or an undefined behavior otherwise.

   function Get_Slice
     (Editor       : access Source_Editor_Box_Record;
      Start_Line   : Positive;
      Start_Column : Positive;
      End_Line     : Positive;
      End_Column   : Positive) return String;
   --  Return the text located between (Start_Line, Start_Column) and
   --  (End_Line, End_Column).
   --
   --  The validity of both locations should be verified before invoking this
   --  function. An incorrect location will cause an Assertion_Failure when
   --  compiled with assertion checks, or an undefined behavior otherwise.

   procedure Insert
     (Editor  : access Source_Editor_Box_Record;
      Line    : Positive;
      Column  : Positive;
      Text    : String);
   --  Insert the given text in at the specified location.
   --
   --  The validity of the given location must be verified before invoking this
   --  procedure. An incorrect location  will cause an Assertion_Failure when
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

   procedure Delete_Slice
     (Editor       : access Source_Editor_Box_Record;
      Start_Line   : Positive;
      Start_Column : Positive;
      End_Line     : Positive;
      End_Column   : Positive);
   --  Replace the text between the start and end locations by Text.
   --
   --  The validity of the given locations must be verified before invoking
   --  this procedure. An incorrect location  will cause an Assertion_Failure
   --  when compiled with assertion checks, or an undefined behavior otherwise.

   procedure Select_All (Editor : access Source_Editor_Box_Record);
   --  Set the selection bounds from the begining to the end of the buffer.

   procedure Cut_Clipboard (Editor : access Source_Editor_Box_Record);
   --  Copy the currently-selected text to the clipboard and then delete it.

   procedure Copy_Clipboard (Editor : access Source_Editor_Box_Record);
   --  Copy the currently-selected text to the clipboard.

   procedure Paste_Clipboard (Editor : access Source_Editor_Box_Record);
   --  Paste the contents of the clipboard.

   procedure Paste_Primary (Editor : access Source_Editor_Box_Record);
   --  Paste the contents of the primary selection.
   --  ??? The primary selection is the X11 cut/copy/paste buffer I think.
   --  ??? what is the relationship with the clipboard?

   procedure Highlight_Line
     (Editor  : access Source_Editor_Box_Record;
      Line    : Positive);
   --  Highlight the given line number. If another line was previously
   --  highlighted, then restore this line unhighlighted.
   --
   --  The line number must be verified before invoking this procedure. An
   --  invalid line leads to an Assertion_Failure when compiled with assertion
   --  checks, or to an undefined behavior otherwise.

   procedure Unhighlight_Line
     (Editor  : access Source_Editor_Box_Record;
      Line    : Positive);
   --  Restore the given line unhighlighted.
   --
   --  The line number must be verified before invoking this procedure. An
   --  invalid line leads to an Assertion_Failure when compiled with assertion
   --  checks, or to an undefined behavior otherwise.

   procedure Cancel_Highlight_Line
     (Editor : access Source_Editor_Box_Record);
   --  If a line in the given buffer is highlighted (from using
   --  Highlight_Line), then restores this line un-highlighted.

   procedure Highlight_Region
     (Editor : access Source_Editor_Box_Record;
      Start_Line   : Positive;
      Start_Column : Positive;
      End_Line     : Positive;
      End_Column   : Positive);
   --  Highlight the given region. The color used in this case is different
   --  from the color used for highlighting lines.
   --
   --  Both start and end positions must be verified before calling this
   --  procedure. An incorrect position will cause an Assertion_Failure
   --  when compiled with assertion checks, or an undefined behavior otherwise.

   procedure Unhighlight_Region
     (Editor : access Source_Editor_Box_Record;
      Start_Line   : Positive;
      Start_Column : Positive;
      End_Line     : Positive;
      End_Column   : Positive);
   --  Restore the given region unhighlighted.
   --
   --  Both start and end positions must be verified before calling this
   --  procedure. An incorrect position will cause an Assertion_Failure
   --  when compiled with assertion checks, or an undefined behavior otherwise.

   procedure Unhighlight_All (Editor : access Source_Editor_Box_Record);
   --  Restore all highlighted regions to unhighlighted. Only the region
   --  highlighting is canceled, the other potential highlightings (line,
   --  syntax) are preserved.

   ----------------------------------
   -- Source_View related services --
   ----------------------------------

   procedure Set_Show_Line_Numbers
     (Editor       : access Source_Editor_Box_Record;
      Show_Numbers : Boolean := True);
   --  Set whether the line numbers should be displayed or not.

   function Get_Show_Line_Numbers
     (Editor : access Source_Editor_Box_Record) return Boolean;
   --  Returns True if the line numbers are displayed.

   ------------------
   -- XRef Support --
   ------------------

   procedure Find_Declaration_Or_Body
     (Editor       : access Source_Editor_Box_Record;
      Line         : Natural := 0;
      Column       : Natural := 0;
      Lib_Info     : Src_Info.LI_File_Ptr;
      Filename     : out GNAT.OS_Lib.String_Access;
      Start_Line   : out Positive;
      Start_Column : out Positive;
      End_Line     : out Positive;
      End_Column   : out Positive);
   --  Return the location of the declaration of the entity located at the
   --  given position. If either Line or Column is equal to 0, then the current
   --  insert cursor position is used instead. If the position is already
   --  pointing at the declaration, then return the location of the body
   --  associated to the entity. If no reference to the entity could be found,
   --  then Filename_Found returned is null, and the values of Start_Line,
   --  Start_Column, End_Line, End_Column are undefined..
   --
   --  The memory allocated for Filename_Found must be deallocated after use.
   --
   --  Note that this routine assumes that the given Lib_Info is up-to-date.
   --  It also assumes that the given cursor position is valid; otherwise, the
   --  result is undefined.

private

   type Source_Editor_Box_Record is record
      Root_Container      : Gtk.Box.Gtk_Box;
      Never_Attached      : Boolean := True;
      Kernel              : Glide_Kernel.Kernel_Handle;
      Source_View         : Src_Editor_View.Source_View;
      Source_Buffer       : Src_Editor_Buffer.Source_Buffer;
      --  The status bar
      Filename_Label      : Gtk.Label.Gtk_Label;
      Cursor_Line_Label   : Gtk.Label.Gtk_Label;
      Cursor_Column_Label : Gtk.Label.Gtk_Label;
      --  The non graphical attributes
      Filename            : String_Access;
   end record;
   --  Note that it is straightforward to retrieve the Source_Buffer from
   --  the Source_View, thus making the Source_View field not absolutely
   --  necessary. But it is kept nonetheless for performance reasons, since
   --  we have to retrieve the buffer for lots of operation...
   --  ??? Is the latter true?

end Src_Editor_Box;
