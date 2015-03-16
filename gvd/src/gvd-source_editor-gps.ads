------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2015, AdaCore                     --
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
with GPS.Main_Window; use GPS.Main_Window;
with GPS.Kernel.Style_Manager;    use GPS.Kernel.Style_Manager;

with String_List_Utils;

--  This package provides an implementation of Source_Editor based on
--  the GPS Editor.

package GVD.Source_Editor.GPS is

   type GEdit_Record is new Source_Editor_Record with private;
   type GEdit is access all GEdit_Record'Class;

   procedure Gtk_New
     (Editor : out GEdit;
      Window : access GPS_Window_Record'Class);
   --  Create a new source editor.

   procedure Initialize
     (Editor : access GEdit_Record'Class;
      Window : access GPS_Window_Record'Class);
   --  Internal initialization procedure.

   overriding procedure Update_Breakpoints
     (Editor  : access GEdit_Record;
      Br      : GVD.Types.Breakpoint_Array;
      Process : Glib.Object.GObject);
   --  See GVD.Text_Box.Source_Editor for more information.

   overriding procedure Show_Message
     (Editor  : access GEdit_Record;
      Message : String);
   --  Display a message in the editor.
   --  This deletes the currently displayed file.

   overriding procedure Load_File
     (Editor    : access GEdit_Record;
      File_Name : GNATCOLL.VFS.Virtual_File);
   --  See GVD.Text_Box.Source_Editor for more information.

   overriding procedure Highlight_Current_Line (Editor : access GEdit_Record);
   --  See GVD.Text_Box.Source_Editor for more information.

   overriding procedure Unhighlight_Current_Line
     (Editor  : access GEdit_Record;
      Process : Glib.Object.GObject);
   --  See GVD.Text_Box.Source_Editor for more information.

   overriding procedure Preferences_Changed (Editor : access GEdit_Record);
   --  See GVD.Text_Box.Source_Editor for more information.

   overriding procedure Set_Line
     (Editor  : access GEdit_Record;
      Line    : Natural;
      Process : Glib.Object.GObject);
   --  See GVD.Text_Box.Source_Editor for more information.

   overriding function Get_Line (Editor : access GEdit_Record) return Natural;
   --  See GVD.Text_Box.Source_Editor for more information.

   procedure Free_Debug_Info (Editor : access GEdit_Record);
   --  Free dynamic memory associated to Editor, relative to debugger
   --  information.

private

   type GEdit_Record is new Source_Editor_Record with record
      Window : GPS_Window;
      Line   : Natural := 0;

      Current_Breakpoints : GVD.Types.Breakpoint_Array_Ptr;
      --  This array contains the currently set breakpoints in the
      --  File.
      --  Entries in this array may be set to 0, in order to avoid
      --  necessity to reallocate it every time the set of breakpoints
      --  is updated.

      Current_Line_Style : Style_Access;
      --  style used for highlighting the current line

      Highlighted_Files : String_List_Utils.String_List.List;
      --  The list of files for which a location is currently highlighted.
   end record;

end GVD.Source_Editor.GPS;
