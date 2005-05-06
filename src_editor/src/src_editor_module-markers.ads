-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2005                            --
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

with GPS.Kernel;
with VFS;

package Src_Editor_Module.Markers is

   type File_Marker_Record
     is new GPS.Kernel.Location_Marker_Record with private;
   type File_Marker is access all File_Marker_Record'Class;

   function Create_File_Marker
     (File   : VFS.Virtual_File;
      Line   : Natural;
      Column : Natural) return File_Marker;
   --  Create a new marker that represents a position inside a file. It isn't
   --  related to a specific editor

private
   type File_Marker_Record
     is new GPS.Kernel.Location_Marker_Record with
      record
         File   : VFS.Virtual_File;
         Line   : Natural;
         Column : Natural;
      end record;

   function Go_To
     (Marker : access File_Marker_Record;
      Kernel : access GPS.Kernel.Kernel_Handle_Record'Class) return Boolean;
   function To_String
     (Marker : access File_Marker_Record) return String;
   --  See inherited documentation

end Src_Editor_Module.Markers;
