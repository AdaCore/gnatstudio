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

with GPS.Kernel;                use GPS.Kernel;
with GPS.Kernel.Standard_Hooks; use GPS.Kernel.Standard_Hooks;
with VFS;                       use VFS;
with Src_Editor_Box;            use Src_Editor_Box;
with Glib.Xml_Int;              use Glib.Xml_Int;

package body Src_Editor_Module.Markers is

   ------------------------
   -- Create_File_Marker --
   ------------------------

   function Create_File_Marker
     (File   : VFS.Virtual_File;
      Line   : Natural;
      Column : Natural) return File_Marker
   is
   begin
      --  ??? If an editor is open, we should put a mark there, and keep an
      --  eye if the file is modified, so that the File_Marker is updated as
      --  appropriate.
      --  We should also pay attention whether the file will be opened later,
      --  and put a mark at that time. Harder to do, since this means we need
      --  to keep a ref to that marker somewhere...
      return new File_Marker_Record'
        (Location_Marker_Record with
         File   => File,
         Line   => Line,
         Column => Column);
   end Create_File_Marker;

   ---------------------------------------------
   -- Push_Current_Editor_Location_In_History --
   ---------------------------------------------

   procedure Push_Current_Editor_Location_In_History
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Child : constant MDI_Child := Find_Current_Editor (Kernel);
      Box : constant Source_Editor_Box := Get_Source_Box_From_MDI (Child);
      Line, Column : Integer;
   begin
      if Box /= null then
         Get_Cursor_Location (Box, Line, Column);
         Push_Marker_In_History
           (Kernel  => Kernel,
            Marker  => Create_File_Marker
              (File   => Get_Filename (Box),
               Line   => Line,
               Column => Column));
      end if;
   end Push_Current_Editor_Location_In_History;

   -----------
   -- Go_To --
   -----------

   function Go_To
     (Marker : access File_Marker_Record;
      Kernel : access GPS.Kernel.Kernel_Handle_Record'Class) return Boolean is
   begin
      Open_File_Editor
        (Kernel,
         Marker.File,
         Marker.Line,
         Marker.Column,
         Enable_Navigation => False,
         New_File          => False);
      return True;
   end Go_To;

   ---------------
   -- To_String --
   ---------------

   function To_String
     (Marker : access File_Marker_Record) return String is
   begin
      return Base_Name (Marker.File)
        & " at line" & Integer'Image (Marker.Line)
        & " column"  & Integer'Image (Marker.Column);
   end To_String;

   ----------
   -- Save --
   ----------

   function Save
     (Marker : access File_Marker_Record) return Glib.Xml_Int.Node_Ptr
   is
      Node : constant Node_Ptr := new Glib.Xml_Int.Node;
   begin
      Node.Tag := new String'("file_marker");
      Set_Attribute (Node, "file", Full_Name (Marker.File).all);
      Set_Attribute (Node, "line", Integer'Image (Marker.Line));
      Set_Attribute (Node, "column", Integer'Image (Marker.Column));
      return Node;
   end Save;

end Src_Editor_Module.Markers;
