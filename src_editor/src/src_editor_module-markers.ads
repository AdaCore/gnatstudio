-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                  Copyright (C) 2005-2007, AdaCore                 --
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

with Glib.Xml_Int;
with Gtk.Text_Mark;

with GPS.Kernel;
with VFS;

package Src_Editor_Module.Markers is

   type File_Marker_Record
     is new GPS.Kernel.Location_Marker_Record with private;
   type File_Marker is access all File_Marker_Record'Class;
   pragma No_Strict_Aliasing (File_Marker);

   function Create_File_Marker
     (Kernel : access Kernel_Handle_Record'Class;
      File   : VFS.Virtual_File;
      Line   : Editable_Line_Type;
      Column : Visible_Column_Type;
      Length : Natural := 0) return File_Marker;
   --  Create a new marker that represents a position inside a file. It isn't
   --  related to a specific editor. The mark will always indicate the same
   --  position in the file, even if the file is closed, reopened or modified.

   function Create_File_Marker
     (Kernel : access Kernel_Handle_Record'Class;
      File   : VFS.Virtual_File;
      Mark   : Gtk.Text_Mark.Gtk_Text_Mark) return File_Marker;
   --  Create a new marker from an existing text mark. The mark will always
   --  indicate the same position in the file.

   procedure Push_Current_Editor_Location_In_History
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Push the location in the current editor in the history of locations.
   --  This should be called before jumping to a new location on a user's
   --  request, so that he can easily choose to go back to the previous
   --  location.

   function Load
     (Kernel   : access Kernel_Handle_Record'Class;
      From_XML : Glib.Xml_Int.Node_Ptr := null) return Location_Marker;
   --  Create a new marker either from From_XML or from the current context

   function Get_File
     (Marker : access File_Marker_Record'Class) return VFS.Virtual_File;
   --  Return the file in which Marker is set

   function Get_Line
     (Marker : access File_Marker_Record'Class) return Editable_Line_Type;
   function Get_Column
     (Marker : access File_Marker_Record'Class) return Visible_Column_Type;
   function Get_Mark
     (Marker : access File_Marker_Record'Class)
      return Gtk.Text_Mark.Gtk_Text_Mark;
   function Get_Id
     (Marker : access File_Marker_Record'Class) return Integer;
   pragma Inline (Get_File, Get_Line, Get_Column, Get_Mark, Get_Id);
   --  Return the coordinates of the marker

   function Find_Mark (Id : Natural) return File_Marker;
   --  Find the mark corresponding to Id, or return null

   procedure Reset_Markers_For_File
     (Kernel : access Kernel_Handle_Record'Class;
      File   : VFS.Virtual_File);
   --  Rests all markers that were set for File, so that we recreate the
   --  text_marks associated with them.

private
   type File_Marker_Record
     is new GPS.Kernel.Location_Marker_Record with
      record
         Id     : Natural;   --  Needed only for the shell API
         File   : VFS.Virtual_File;
         Line   : Editable_Line_Type;
         Column : Visible_Column_Type;
         Length : Natural := 1;
         Mark   : Gtk.Text_Mark.Gtk_Text_Mark;
         Buffer : Gtk.Text_Buffer.Gtk_Text_Buffer;
         Kernel : Kernel_Handle;
      end record;

   procedure Destroy (Marker : in out File_Marker_Record);
   function Go_To
     (Marker : access File_Marker_Record;
      Kernel : access GPS.Kernel.Kernel_Handle_Record'Class) return Boolean;
   function To_String
     (Marker : access File_Marker_Record) return String;
   function Save
     (Marker : access File_Marker_Record) return Glib.Xml_Int.Node_Ptr;
   --  See inherited documentation

end Src_Editor_Module.Markers;
