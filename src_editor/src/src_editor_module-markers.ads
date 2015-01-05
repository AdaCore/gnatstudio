------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2005-2015, AdaCore                     --
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

with XML_Utils;
with Gtk.Handlers;
with Gtk.Text_Mark;

with GPS.Kernel;
with GNATCOLL.Projects;
with GNATCOLL.VFS;

package Src_Editor_Module.Markers is

   type File_Marker_Record
     is new GPS.Kernel.Location_Marker_Record with private;
   type File_Marker is access all File_Marker_Record'Class;
   pragma No_Strict_Aliasing (File_Marker);

   function Create_File_Marker
     (Kernel  : access Kernel_Handle_Record'Class;
      File    : GNATCOLL.VFS.Virtual_File;
      Project : GNATCOLL.Projects.Project_Type;
      Line    : Editable_Line_Type;
      Column  : Visible_Column_Type;
      Length  : Natural := 0) return File_Marker;
   --  Create a new marker that represents a position inside a file. It isn't
   --  related to a specific editor. The mark will always indicate the same
   --  position in the file, even if the file is closed, reopened or modified.

   function Create_File_Marker
     (Kernel  : access Kernel_Handle_Record'Class;
      File    : GNATCOLL.VFS.Virtual_File;
      Project : GNATCOLL.Projects.Project_Type;
      Mark    : Gtk.Text_Mark.Gtk_Text_Mark) return File_Marker;
   --  Create a new marker from an existing text mark. The mark will always
   --  indicate the same position in the file.

   procedure Move
     (Marker : access File_Marker_Record'Class;
      Mark   : Gtk.Text_Mark.Gtk_Text_Mark);
   --  Move the marker to a new location within an open editor

   procedure Push_Current_Editor_Location_In_History
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Push the location in the current editor in the history of locations.
   --  This should be called before jumping to a new location on a user's
   --  request, so that he can easily choose to go back to the previous
   --  location.

   function Load
     (Kernel   : access Kernel_Handle_Record'Class;
      From_XML : XML_Utils.Node_Ptr := null) return Location_Marker;
   --  Create a new marker either from From_XML or from the current context

   function Get_File
     (Marker : access File_Marker_Record'Class)
      return GNATCOLL.VFS.Virtual_File;
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
   function Get_Kernel
     (Marker : access File_Marker_Record'Class) return Kernel_Handle;
   pragma Inline
     (Get_File, Get_Line, Get_Column, Get_Mark, Get_Id, Get_Kernel);
   --  Return the coordinates of the marker

   function Find_Mark (Id : Natural) return File_Marker;
   --  Find the mark corresponding to Id, or return null

   procedure Reset_Markers_For_File
     (Kernel : access Kernel_Handle_Record'Class;
      File   : GNATCOLL.VFS.Virtual_File);
   --  Rests all markers that were set for File, so that we recreate the
   --  text_marks associated with them.

private
   type File_Marker_Record
     is new GPS.Kernel.Location_Marker_Record with
      record
         Id     : Natural;   --  Needed only for the shell API
         File   : GNATCOLL.VFS.Virtual_File;
         Project : GNATCOLL.Projects.Project_Type;
         Line   : Editable_Line_Type;
         Column : Visible_Column_Type;
         Length : Natural := 1;
         Mark   : Gtk.Text_Mark.Gtk_Text_Mark;
         Buffer : Gtk.Text_Buffer.Gtk_Text_Buffer;
         Kernel : Kernel_Handle;
         Cid    : Gtk.Handlers.Handler_Id;
      end record;

   overriding procedure Destroy (Marker : in out File_Marker_Record);
   overriding function Go_To
     (Marker : access File_Marker_Record;
      Kernel : access GPS.Kernel.Kernel_Handle_Record'Class) return Boolean;
   overriding function To_String
     (Marker : access File_Marker_Record) return String;
   overriding function Clone
     (Marker : access File_Marker_Record) return Location_Marker;
   overriding function Save
     (Marker : access File_Marker_Record) return XML_Utils.Node_Ptr;
   overriding function Similar
     (Left  : access File_Marker_Record;
      Right : access Location_Marker_Record'Class) return Boolean;
   overriding function Distance
     (Left  : access File_Marker_Record;
      Right : access Location_Marker_Record'Class) return Integer;
   --  See inherited documentation

end Src_Editor_Module.Markers;
