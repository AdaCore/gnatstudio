-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                       Copyright (C) 2003                          --
--                            ACT-Europe                             --
--                                                                   --
-- GPS is free  software; you can  redistribute it and/or modify  it --
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

--  This package handles the customizable information in the buffer,
--  such as information added to the sides of lines, or VCS information.

with Pango.Layout; use Pango.Layout;

with Glib; use Glib;

with Gdk.Pixmap;
with Gdk.Pixbuf; use Gdk.Pixbuf;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Text_View; use Gtk.Text_View;

package Src_Editor_Buffer.Line_Information is

   procedure Create_Line_Information_Column
     (Buffer        : access Source_Buffer_Record'Class;
      Identifier    : String;
      Stick_To_Data : Boolean;
      Every_Line    : Boolean);
   --  Add a column corresponding to Identifier in Buffer.

   procedure Remove_Line_Information_Column
     (Buffer     : access Source_Buffer_Record'Class;
      Identifier : String);
   --  Remove a column from the side information in Buffer.

   procedure Add_File_Information
     (Buffer     : access Source_Buffer_Record'Class;
      Identifier : String;
      Box        : Gtk_Widget;
      Info       : Glide_Kernel.Modules.Line_Information_Data);
   --  Add the line information to the Buffer.
   --  User must not free Info.

   procedure Draw_Line_Info
     (Editor      : access Source_Buffer_Record'Class;
      Top_Line    : Buffer_Line_Type;
      Bottom_Line : Buffer_Line_Type;
      View        : Gtk_Text_View;
      GC          : Gdk.GC.Gdk_GC;
      Layout      : in out Pango_Layout;
      Drawable    : in out Gdk.Pixmap.Gdk_Pixmap);
   --  Draw side info from Top_Line to Bottom_Line on Drawable.
   --  Layout should be used to draw text.

   procedure On_Click
     (Editor : access Source_Buffer_Record'Class;
      Line   : Buffer_Line_Type;
      Offset : Gint);
   --  Perform a click in the side column for line Line, offset Offset.

   function Add_Blank_Lines
     (Editor : access Source_Buffer_Record'Class;
      Line   : Editable_Line_Type;
      GC     : Gdk.GC.Gdk_GC;
      Text   : String;
      Number : Positive) return Gtk.Text_Mark.Gtk_Text_Mark;
   --  Add Number blank lines at line Line.
   --  Blank lines cannot be edited, and are not saved on disk.

   function Create_Mark
     (Buffer : access Source_Buffer_Record'Class;
      Line   : Editable_Line_Type;
      Column : Positive) return Gtk.Text_Mark.Gtk_Text_Mark;
   --  Create mark at Line, Column.

   procedure Add_Lines
     (Buffer : access Source_Buffer_Record'Class;
      Start  : Buffer_Line_Type;
      Number : Buffer_Line_Type);
   --  Add Number blank lines to the column info and line highlights,
   --  after Start.

   procedure Remove_Lines
     (Buffer     : access Source_Buffer_Record'Class;
      Start_Line : Buffer_Line_Type;
      End_Line   : Buffer_Line_Type);
   --  Remove lines from the column info and line highlights.

   procedure Remove_Blank_Lines
     (Buffer : access Source_Buffer_Record'Class;
      Mark   : Gtk.Text_Mark.Gtk_Text_Mark;
      Number : Natural);
   --  Remove Number blank lines associated with Mark.

   procedure Hide_Lines
     (Buffer     : access Source_Buffer_Record'Class;
      Mark       : Gtk.Text_Mark.Gtk_Text_Mark;
      Number     : Editable_Line_Type);
   --  Hide Number editable lines from Mark.

   procedure Unhide_Lines
     (Buffer     : access Source_Buffer_Record'Class;
      Mark       : Gtk.Text_Mark.Gtk_Text_Mark);
   --  Unhide editable lines that are hidden between the line that Mark is on
   --  and the following line.

   procedure Add_Block_Command
     (Buffer      : access Source_Buffer_Record'Class;
      Buffer_Line : Buffer_Line_Type;
      Command     : Command_Access;
      Image       : Gdk_Pixbuf);
   --  Add a command in the block column information.

   procedure Fold_All (Buffer : access Source_Buffer_Record'Class);
   --  Fold all top-level foldable blocks.

   procedure Unfold_All (Buffer : access Source_Buffer_Record'Class);
   --  Unfold all top-level foldable blocks.

   procedure Unfold_Line
     (Buffer : access Source_Buffer_Record'Class;
      Line   : Editable_Line_Type);
   --  Unfold the block(s) containing Line.

   function Lines_Are_Real
     (Buffer : access Source_Buffer_Record'Class) return Boolean;
   pragma Inline (Lines_Are_Real);
   --  Return True if all editable lines are visible and all visible lines
   --  are editable lines.

   procedure Remove_Block_Folding_Commands
     (Buffer                 : access Source_Buffer_Record'Class;
      Remove_Unfold_Commands : Boolean := True);
   --  Remove the commands corresponding to block folding/unfolding from the
   --  side column.
   --  If Remove_Unfold_Commands is False, will only remove block folding
   --  command.

   procedure Highlight_Range
     (Buffer    : access Source_Buffer_Record'Class;
      Category  : String;
      Line      : Natural;
      Start_Col : Integer;
      End_Col   : Integer;
      Remove    : Boolean := False);
   --  Highlight the given range of text with category Category.
   --  If Start_Col <= 0, start at the beginning of line.
   --  If End_Col <= 0, end at end of line.
   --  If Remove is True, remove the highlighting instead of adding it.
   --  If Line = 0, (un)highlight the whole buffer.

   function Get_Line
     (Buffer   : access Source_Buffer_Record'Class;
      Position : Gtk.Text_Mark.Gtk_Text_Mark) return Editable_Line_Type;
   --  Return the line of Position.

   function Get_Column
     (Buffer   : access Source_Buffer_Record'Class;
      Position : Gtk.Text_Mark.Gtk_Text_Mark) return Positive;
   --  Return the column of Position.

end Src_Editor_Buffer.Line_Information;
