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

package body VCS_View_Pixmaps is

   -------------------
   -- Init_Graphics --
   -------------------

   procedure Init_Graphics is
   begin
      if Status_Unknown_Pixbuf = Null_Pixbuf then
         Status_Unknown_Pixbuf
           := Gdk_New_From_Xpm_Data (Unknown_File_Xpm);
         Status_Up_To_Date_Pixbuf
           := Gdk_New_From_Xpm_Data (Up_To_Date_Xpm);
         Status_Modified_Pixbuf
           := Gdk_New_From_Xpm_Data (Modified_Xpm);
         Status_Needs_Merge_Pixbuf
           := Gdk_New_From_Xpm_Data (Needs_Merge_Xpm);
         Status_Needs_Update_Pixbuf
           := Gdk_New_From_Xpm_Data (Needs_Update_Xpm);
         Status_Removed_Pixbuf
           := Gdk_New_From_Xpm_Data (Removed_Xpm);
         Status_Not_Registered_Pixbuf
           := Gdk_New_From_Xpm_Data (Not_Registered_Xpm);
      end if;
   end Init_Graphics;

end VCS_View_Pixmaps;
