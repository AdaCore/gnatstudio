-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
--                            ACT-Europe                             --
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

with Gtk.Container;
with Basic_Types;
with GVD.Types;
with GVD.Main_Window;
with Gtk.Widget;

with String_List_Utils;

--  This package provides an implementation of Source_Editor based on
--  the Glide Editor.

package GVD.Text_Box.Source_Editor.Glide is

   type GEdit_Record is new Source_Editor_Record with private;
   type GEdit is access all GEdit_Record'Class;

   procedure Gtk_New
     (Editor : out GEdit;
      Window : access GVD.Main_Window.GVD_Main_Window_Record'Class);
   --  Create a new source editor.

   procedure Initialize
     (Editor : access GEdit_Record'Class;
      Window : access GVD.Main_Window.GVD_Main_Window_Record'Class);
   --  Internal initialization procedure.

   procedure Attach
     (Editor : access GEdit_Record;
      Parent : access Gtk.Container.Gtk_Container_Record'Class);
   --  See GVD.Text_Box.Source_Editor for more information.

   procedure Detach (Editor : access GEdit_Record);
   --  See GVD.Text_Box.Source_Editor for more information.

   procedure Highlight_Word
     (Editor   : access GEdit_Record;
      Line     : Natural;
      Column   : Natural;
      Position : Basic_Types.Position_Type);
   --  See GVD.Text_Box.Source_Editor for more information.

   procedure Update_Breakpoints
     (Editor  : access GEdit_Record;
      Br      : GVD.Types.Breakpoint_Array;
      Process : Gtk.Widget.Gtk_Widget);
   --  See GVD.Text_Box.Source_Editor for more information.

   procedure Show_Message
     (Editor      : access GEdit_Record;
      Message     : String);
   --  Display a message in the editor.
   --  This deletes the currently displayed file.

   procedure Load_File
     (Editor      : access GEdit_Record;
      File_Name   : String;
      Set_Current : Boolean := True;
      Force       : Boolean := False);
   --  See GVD.Text_Box.Source_Editor for more information.

   procedure Highlight_Current_Line (Editor : access GEdit_Record);
   --  See GVD.Text_Box.Source_Editor for more information.

   procedure Preferences_Changed (Editor : access GEdit_Record);
   --  See GVD.Text_Box.Source_Editor for more information.

   procedure Set_Line
     (Editor      : access GEdit_Record;
      Line        : Natural;
      Set_Current : Boolean := True;
      Process     : Gtk.Widget.Gtk_Widget);
   --  See GVD.Text_Box.Source_Editor for more information.

   function Get_Line (Editor : access GEdit_Record) return Natural;
   --  See GVD.Text_Box.Source_Editor for more information.

   procedure Free_Debug_Info (Editor : access GEdit_Record);
   --  Free dynamic memory associated to Editor, relative to debugger
   --  information.

private

   type GEdit_Record is new Source_Editor_Record with record
      Window : GVD.Main_Window.GVD_Main_Window;
      Line   : Natural := 0;

      Current_Breakpoints : GVD.Types.Breakpoint_Array_Ptr;
      --  This array contains the currently set breakpoints in the
      --  File.
      --  Entries in this array may be set to 0, in order to avoid
      --  necessity to reallocate it every time the set of breakpoints
      --  is updated.

      Highlighted_Files : String_List_Utils.String_List.List;
      --  The list of files for which a location is currently highlighted.
   end record;

end GVD.Text_Box.Source_Editor.Glide;
