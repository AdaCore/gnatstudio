------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2005-2019, AdaCore                     --
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

with GPS.Editors;          use GPS.Editors;
with GPS.Kernel;
with GPS.Markers;          use GPS.Markers;
with GPS.Scripts;          use GPS.Scripts;
with GNATCOLL.Projects;
with GNATCOLL.Scripts;     use GNATCOLL.Scripts;
with GNATCOLL.VFS;

package Src_Editor_Module.Markers is

   type File_Marker_Data is new Abstract_File_Marker_Data with private;
   type File_Marker is access all File_Marker_Data'Class;

   function Create_File_Marker
     (Kernel  : access Kernel_Handle_Record'Class;
      File    : GNATCOLL.VFS.Virtual_File;
      Project : GNATCOLL.Projects.Project_Type;
      Line    : Editable_Line_Type;
      Column  : Visible_Column_Type;
      Length  : Natural := 0) return Location_Marker;
   --  Create a new marker that represents a position inside a file. It isn't
   --  related to a specific editor. The mark will always indicate the same
   --  position in the file, even if the file is closed, reopened or modified.

   function Create_File_Marker
     (Kernel  : access Kernel_Handle_Record'Class;
      File    : GNATCOLL.VFS.Virtual_File;
      Project : GNATCOLL.Projects.Project_Type;
      Mark    : Gtk.Text_Mark.Gtk_Text_Mark) return Location_Marker;
   --  Create a new marker from an existing text mark. The mark will always
   --  indicate the same position in the file.

   procedure Move
     (Marker : not null access File_Marker_Data'Class;
      Mark   : Gtk.Text_Mark.Gtk_Text_Mark);
   --  Move the marker to a new location within an open editor

   procedure Delete (Self : not null access File_Marker_Data'Class);
   --  Delete underling low level marker.

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

   overriding function Get_File
     (Marker : not null access File_Marker_Data)
      return GNATCOLL.VFS.Virtual_File with Inline;
   --  Return the file in which Marker is set
   overriding function Get_Line
     (Marker : not null access File_Marker_Data)
      return Editable_Line_Type with Inline;
   overriding function Get_Column
     (Marker : not null access File_Marker_Data)
      return Visible_Column_Type with Inline;
   function Get_Mark
     (Marker : not null access File_Marker_Data'Class)
      return Gtk.Text_Mark.Gtk_Text_Mark with Inline;
   --  Return the coordinates of the marker

   procedure Reset_Markers_For_File
     (Kernel : access Kernel_Handle_Record'Class;
      File   : GNATCOLL.VFS.Virtual_File);
   --  Rests all markers that were set for File, so that we recreate the
   --  text_marks associated with them.

   ------------
   -- Script --
   ------------

   function Get_Or_Create_Instance
     (Self            : Location_Marker;
      Script          : not null access Scripting_Language_Record'Class)
     return Class_Instance;
   --  If Self was already associated with a class instance in the given
   --  scripting language, returns that same instance.
   --  Otherwise, create a new instance to wrap Self.

   procedure Unset_Marker_In_Instance (Self : Class_Instance);
   --  Unset the marker stored in Self.

   function From_Instance (Inst : Class_Instance) return Location_Marker;
   --  Return the location stored in Inst

private
   type File_Marker_Proxy is new Script_Proxy with null record;
   overriding function Class_Name (Self : File_Marker_Proxy) return String
     is ("EditorMark");

   type File_Marker_Data is new Abstract_File_Marker_Data with record
      File     : GNATCOLL.VFS.Virtual_File;
      Project  : GNATCOLL.Projects.Project_Type;
      Line     : Editable_Line_Type;
      Column   : Visible_Column_Type;
      Length   : Natural := 1;
      Mark     : Gtk.Text_Mark.Gtk_Text_Mark;
      Buffer   : Gtk.Text_Buffer.Gtk_Text_Buffer;
      Kernel   : Kernel_Handle;
      Cid      : Gtk.Handlers.Handler_Id;
      Instances : File_Marker_Proxy;
   end record;
   overriding procedure Destroy (Marker : in out File_Marker_Data);
   overriding function Go_To
     (Marker : not null access File_Marker_Data) return Boolean;
   overriding function To_String
     (Marker : not null access File_Marker_Data) return String;
   overriding function Save
     (Marker : not null access File_Marker_Data) return XML_Utils.Node_Ptr;
   overriding function Similar
     (Left  : not null access File_Marker_Data;
      Right : not null access Location_Marker_Data'Class) return Boolean;
   overriding function Distance
     (Left  : not null access File_Marker_Data;
      Right : not null access Location_Marker_Data'Class) return Integer;
   --  See inherited documentation

   package Proxies is new Script_Proxies
     (Element_Type => Location_Marker,
      Proxy        => File_Marker_Proxy);

end Src_Editor_Module.Markers;
