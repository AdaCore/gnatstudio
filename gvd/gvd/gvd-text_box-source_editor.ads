-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                      Copyright (C) 2000-2001                      --
--                              ACT-Europe                           --
--                                                                   --
-- GVD is free  software;  you can redistribute it and/or modify  it --
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

with Gtk.Widget;
with Gtk.Container;
with Language;
with GVD.Types;

package GVD.Text_Box.Source_Editor is

   type Source_Editor_Record is abstract tagged private;
   type Source_Editor is access all Source_Editor_Record'Class;

   procedure Attach
     (Editor : access Source_Editor_Record;
      Parent : access Gtk.Container.Gtk_Container_Record'Class) is abstract;
   --  Attach Editor to the specified parent, if possible.

   procedure Detach (Editor : access Source_Editor_Record) is abstract;
   --  Detach Editor from its parent, if possible.

   procedure Set_Current_Language
     (Editor : access Source_Editor_Record;
      Lang   : Language.Language_Access);
   --  Change the current language for the editor.
   --  The text already present in the editor is not re-highlighted for the
   --  new language, this only influences future addition to the editor.
   --
   --  If Lang is null, then no color highlighting will be performed.

   procedure Highlight_Word
     (Editor   : access Source_Editor_Record;
      Line     : Natural;
      Column   : Natural;
      Position : GVD.Types.Position_Type) is abstract;
   --  Highlight the word that starts at the given position in the file
   --  associated with the editor (ie ignoring the line numbers that could
   --  be displayed).

   procedure Update_Breakpoints
     (Editor : access Source_Editor_Record;
      Br     : GVD.Types.Breakpoint_Array) is abstract;
   --  Change the list of breakpoints to highlight in the editor.
   --  All the breakpoints that previously existed are removed from the screen,
   --  and replaced by the new ones.
   --  The breakpoints that do not apply to the current file are ignored.

   function Get_Current_File
     (Editor : access Source_Editor_Record) return String;
   --  See GVD.Code_Editors for more information

   procedure Load_File
     (Editor      : access Source_Editor_Record;
      File_Name   : String;
      Set_Current : Boolean := True) is abstract;
   --  Load and append a file in the editor.
   --  The contents is highlighted based on the current language.
   --  Debugger is used to calculate which lines should get icons on the side,
   --  through calls to Line_Contains_Code.
   --  If Set_Current is True, then File_Name becomes the current file for the
   --  debugger (ie the one that contains the current execution line).

   procedure Highlight_Current_Line
     (Editor : access Source_Editor_Record) is abstract;
   --  Highlight the current line in the editor, if required by the user.
   --  If the edited file is not the one that contains the current line,
   --  this procedure does nothing.

   procedure Preferences_Changed
     (Editor : access Source_Editor_Record) is abstract;
   --  Called when the preferences have changed, and the editor should be
   --  redisplayed with the new setup.

   procedure Set_Line
     (Editor      : access Source_Editor_Record;
      Line        : Natural;
      Set_Current : Boolean := True) is abstract;
   --  Set the current line (and draw the button on the side).
   --  If Set_Current is True, then the line becomes the current line (ie the
   --  one on which the debugger is stopped). Otherwise, Line is simply the
   --  line that we want to display in the editor.

   function Get_Line
     (Editor : access Source_Editor_Record) return Natural is abstract;
   --  Return the current line.

   function Get_Widget
     (Editor : access Source_Editor_Record) return Gtk.Widget.Gtk_Widget;
   --  Return the widget associated with Editor.
   --  WARNING: do not add or remove this widget in a container.
   --  Instead, use the Attach/Detach routines provided in this package.

private
   type Source_Editor_Record is abstract tagged record
      Widget : Gtk.Widget.Gtk_Widget;
      Current_File : GVD.Types.String_Access;

      Debugger_Current_File : GVD.Types.String_Access;
      --  The file/line on which the debugger is stopped (ie these were set
      --  when the Set_Current parameter is True for Set_line and Load_File)

      Lang : Language.Language_Access;
   end record;

end GVD.Text_Box.Source_Editor;
