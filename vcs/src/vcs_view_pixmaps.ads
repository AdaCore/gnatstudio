-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                        Copyright (C) 2001                         --
--                            ACT-Europe                             --
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

with Gdk.Pixbuf;      use Gdk.Pixbuf;
with Basic_Types;     use Basic_Types;

package VCS_View_Pixmaps is

   Status_Unknown_Pixbuf        : Gdk_Pixbuf := Null_Pixbuf;
   Status_Not_Registered_Pixbuf : Gdk_Pixbuf := Null_Pixbuf;
   Status_Up_To_Date_Pixbuf     : Gdk_Pixbuf := Null_Pixbuf;
   Status_Removed_Pixbuf        : Gdk_Pixbuf := Null_Pixbuf;
   Status_Modified_Pixbuf       : Gdk_Pixbuf := Null_Pixbuf;
   Status_Needs_Merge_Pixbuf    : Gdk_Pixbuf := Null_Pixbuf;
   Status_Needs_Update_Pixbuf   : Gdk_Pixbuf := Null_Pixbuf;

   procedure Init_Graphics;
   --  Initialize the pixbufs.

private

   Unknown_File_Xpm : aliased Pixmap_Array;
   pragma Import (C, Unknown_File_Xpm, "unknown_file_xpm");

   Removed_Xpm : aliased Pixmap_Array;
   pragma Import (C, Removed_Xpm, "unknown_file_xpm");

   Not_Registered_Xpm : aliased Pixmap_Array;
   pragma Import (C, Not_Registered_Xpm, "unknown_file_xpm");

   Modified_Xpm : aliased Pixmap_Array;
   pragma Import (C, Modified_Xpm, "modified_xpm");

   Up_To_Date_Xpm : aliased Pixmap_Array;
   pragma Import (C, Up_To_Date_Xpm, "up_to_date_xpm");

   Needs_Merge_Xpm : aliased Pixmap_Array;
   pragma Import (C, Needs_Merge_Xpm, "needs_merge_xpm");

   Needs_Update_Xpm : aliased Pixmap_Array;
   pragma Import (C, Needs_Update_Xpm, "needs_update_xpm");

end VCS_View_Pixmaps;
