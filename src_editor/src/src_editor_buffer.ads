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
--  This package provides a text editor buffer with lots of extensions
--  to support many functionalities related to source code editing.
--  This includes for example syntax highlighting, auto-indent, cross-
--  reference support, etc.
--  </description>

with Glib;             use Glib;
with Gtk;
with Gtk.Text_Buffer;
with Gtk.Text_Mark;
with Gtk.Text_Tag;

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

   procedure Set_Cursor_Position
     (Buffer  : access Source_Buffer_Record;
      Line    : Gint;
      Column  : Gint;
      Success : out Boolean);
   --  Move the insert cursor to the given position. If the (Line, Column)
   --  position is not valid, the cursor is not moved and Success is set
   --  to False.

   procedure Get_Cursor_Position
     (Buffer : access Source_Buffer_Record;
      Line   : out Gint;
      Column : out Gint);
   --  Return the current cursor position

   procedure Highlight_Line
     (Buffer  : access Source_Buffer_Record;
      Line    : Gint;
      Success : out Boolean);
   --  Highlight the given line number. If another line was previously
   --  highlighted, then restore this line unhighlighted.
   --
   --  If Line exceeds the number of lines in the buffer, no action is
   --  performed and Success is set to False.

   procedure Unhighlight_Line
     (Buffer  : access Source_Buffer_Record;
      Line    : Gint;
      Success : out Boolean);
   --  Restore the given line unhighlighted. If line exceeds the number
   --  of lines in the buffer, no action is performed and Success is set
   --  to False.

   procedure Cancel_Highlight_Line
     (Buffer : access Source_Buffer_Record);
   --  If a line in the given buffer is highlighted (from using
   --  Highlight_Line), then restores this line un-highlighted.

private

   type Source_Buffer_Record is
     new Gtk.Text_Buffer.Gtk_Text_Buffer_Record with
   record
      Lang          : Language.Language_Access;
      Syntax_Tags   : Src_Highlighting.Highlighting_Tags;
      Highlight_Tag : Gtk.Text_Tag.Gtk_Text_Tag;
      Insert_Mark   : Gtk.Text_Mark.Gtk_Text_Mark;
   end record;

end Src_Editor_Buffer;
