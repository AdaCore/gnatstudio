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
with Src_Editor_Buffer;
with Src_Editor_View;

package Src_Editor_Box is

   type Source_Editor_Box_Record is private;
   type Source_Editor_Box is access all Source_Editor_Box_Record;

   procedure Gtk_New
     (Box  : out Source_Editor_Box;
      Lang : Language.Language_Access := null);
   --  Create a new Source_Editor_Box. It must be destroyed after use
   --  (see procedure Destroy below).

   procedure Initialize
     (Box  : access Source_Editor_Box_Record;
      Lang : Language.Language_Access);
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

   procedure Load_File
     (Editor          : access Source_Editor_Box_Record;
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
     (Editor   : access Source_Editor_Box_Record;
      Filename : String;
      Success  : out Boolean);
   --  Save the buffer to the given file. Success is set to false if the
   --  buffer could not be saved.

   procedure Set_Language
     (Editor : access Source_Editor_Box_Record;
      Lang   : Language.Language_Access := null);
   --  Change the language of the source editor. If the new language is
   --  not null, then causes the syntax-highlighting to be recomputed.

   function Get_Language
     (Editor : access Source_Editor_Box_Record)
      return Language.Language_Access;
   --  Return the current language.

   procedure Set_Cursor_Location
     (Editor  : access Source_Editor_Box_Record;
      Line    : Positive;
      Column  : Positive := 1;
      Success : out Boolean);
   --  Move the insert cursor to the given location. Success is set to False
   --  if the position is outside of the buffer.

   procedure Get_Cursor_Location
     (Editor  : access Source_Editor_Box_Record;
      Line    : out Positive;
      Column  : out Positive);
   --  Get the current cursor position.

   procedure Highlight_Line
     (Editor  : access Source_Editor_Box_Record;
      Line    : Positive;
      Success : out Boolean);
   --  Highlight the given line number. If another line was previously
   --  highlighted, then restore this line unhighlighted.
   --
   --  If Line exceeds the number of lines in the buffer, no action is
   --  performed and Success is set to False.

   procedure Unhighlight_Line
     (Editor  : access Source_Editor_Box_Record;
      Line    : Positive;
      Success : out Boolean);
   --  Restore the given line unhighlighted. If line exceeds the number
   --  of lines in the buffer, no action is performed and Success is set
   --  to False.

   procedure Cancel_Highlight_Line
     (Editor : access Source_Editor_Box_Record);
   --  If a line in the given buffer is highlighted (from using
   --  Highlight_Line), then restores this line un-highlighted.

private

   type Source_Editor_Box_Record is record
      Root_Container      : Gtk.Box.Gtk_Box;
      Never_Attached      : Boolean := True;
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
