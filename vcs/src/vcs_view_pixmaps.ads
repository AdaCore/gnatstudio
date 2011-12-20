------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2012, AdaCore                     --
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

with Gdk.Pixbuf;  use Gdk.Pixbuf;
with Basic_Types; use Basic_Types;

package VCS_View_Pixmaps is

   Status_Unknown_Pixbuf        : Gdk_Pixbuf := Null_Pixbuf;
   Status_Not_Registered_Pixbuf : Gdk_Pixbuf := Null_Pixbuf;
   Status_Up_To_Date_Pixbuf     : Gdk_Pixbuf := Null_Pixbuf;
   Status_Removed_Pixbuf        : Gdk_Pixbuf := Null_Pixbuf;
   Status_Added_Pixbuf          : Gdk_Pixbuf := Null_Pixbuf;
   Status_Modified_Pixbuf       : Gdk_Pixbuf := Null_Pixbuf;
   Status_Needs_Merge_Pixbuf    : Gdk_Pixbuf := Null_Pixbuf;
   Status_Needs_Update_Pixbuf   : Gdk_Pixbuf := Null_Pixbuf;

   procedure Init_Graphics;
   --  Initialize the pixbufs.

private

   Unknown_File_Xpm : aliased Pixmap_Array;
   pragma Import (C, Unknown_File_Xpm, "unknown_file_xpm");

   Removed_Xpm : aliased Pixmap_Array;
   pragma Import (C, Removed_Xpm, "removed_file_xpm");

   Not_Registered_Xpm : aliased Pixmap_Array;
   pragma Import (C, Not_Registered_Xpm, "not_registered_xpm");

   Modified_Xpm : aliased Pixmap_Array;
   pragma Import (C, Modified_Xpm, "modified_xpm");

   Up_To_Date_Xpm : aliased Pixmap_Array;
   pragma Import (C, Up_To_Date_Xpm, "up_to_date_xpm");

   Needs_Merge_Xpm : aliased Pixmap_Array;
   pragma Import (C, Needs_Merge_Xpm, "needs_merge_xpm");

   Needs_Update_Xpm : aliased Pixmap_Array;
   pragma Import (C, Needs_Update_Xpm, "needs_update_xpm");

   Added_Xpm : aliased Pixmap_Array;
   pragma Import (C, Added_Xpm, "added_file_xpm");

end VCS_View_Pixmaps;
