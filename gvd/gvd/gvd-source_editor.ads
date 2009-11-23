-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2000-2005                       --
--                              AdaCore                              --
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

with Glib.Object;
with Gtk.Widget;
with GVD.Types;
with GNATCOLL.VFS;

package GVD.Source_Editor is

   type Source_Editor_Record is abstract tagged private;
   type Source_Editor is access all Source_Editor_Record'Class;

   procedure Update_Breakpoints
     (Editor  : access Source_Editor_Record;
      Br      : GVD.Types.Breakpoint_Array;
      Process : Glib.Object.GObject) is abstract;
   --  Change the list of breakpoints to highlight in the editor.
   --  All the breakpoints that previously existed are removed from the screen,
   --  and replaced by the new ones.
   --  The breakpoints that do not apply to the current file are ignored.
   --  Process is the Visual_Debugger which corresponds to the debugger
   --  that emitted the request.

   function Get_Current_File
     (Editor : access Source_Editor_Record) return GNATCOLL.VFS.Virtual_File;
   --  Return the name of the file at which the debugger is stopped.
   --  "" is returned if there is no current debugger file.

   procedure Show_Message
     (Editor      : access Source_Editor_Record;
      Message     : String) is abstract;
   --  Display a message in the editor.
   --  This deletes the currently displayed file.

   procedure Load_File
     (Editor    : access Source_Editor_Record;
      File_Name : GNATCOLL.VFS.Virtual_File) is abstract;
   --  Load and append a file in the editor.
   --  The contents is highlighted based on the current language.
   --  Debugger is used to calculate which lines should get icons on the side.
   --  File_Name becomes the current file for the
   --  debugger (ie the one that contains the current execution line).

   procedure Highlight_Current_Line
     (Editor : access Source_Editor_Record) is abstract;
   --  Highlight the current line in the editor, if required by the user.
   --  If the edited file is not the one that contains the current line,
   --  this procedure does nothing.

   procedure Unhighlight_Current_Line
     (Editor  : access Source_Editor_Record;
      Process : Glib.Object.GObject) is abstract;
   --  Unhighlight the current line in the editor if it is highlighted.
   --  Do nothing otherwise.

   procedure Preferences_Changed
     (Editor : access Source_Editor_Record) is abstract;
   --  Called when the preferences have changed, and the editor should be
   --  redisplayed with the new setup.

   procedure Set_Line
     (Editor  : access Source_Editor_Record;
      Line    : Natural;
      Process : Glib.Object.GObject) is abstract;
   --  Set the current line (and draw the button on the side).
   --  Process is the Visual_Debugger which corresponds to the debugger
   --  that is stopped.

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
      Widget       : Gtk.Widget.Gtk_Widget;
      Current_File : GNATCOLL.VFS.Virtual_File;
      --  The file/line on which the debugger is stopped (ie these were set
      --  when the Set_Current parameter is True for Set_line and Load_File)
   end record;

end GVD.Source_Editor;
