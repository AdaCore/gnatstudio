------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2000-2016, AdaCore                     --
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

with Glib.Object;
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

private
   type Source_Editor_Record is abstract tagged record
      Current_File : GNATCOLL.VFS.Virtual_File;
      --  The file/line on which the debugger is stopped (ie these were set
      --  when the Set_Current parameter is True for Set_line and Load_File)
   end record;

end GVD.Source_Editor;
