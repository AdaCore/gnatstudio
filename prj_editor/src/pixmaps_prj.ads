-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                   Copyright (C) 2001-2003                         --
--                            ACT-Europe                             --
--                                                                   --
-- GPS is free  software; you can  redistribute it and/or modify  it --
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

with Gtkada.Types; use Gtkada.Types;

package Pixmaps_Prj is

   --  Note that the following pixmaps are in alphabetical order.
   --  The actual size of the pixmaps is only known by GtkAda, which is
   --  enough for our needs and avoid having to maintain this information here.

   logo_xpm                    : aliased Chars_Ptr_Array (0 .. 0);
   project_closed_xpm          : aliased Chars_Ptr_Array (0 .. 0);
   project_modified_closed_xpm : aliased Chars_Ptr_Array (0 .. 0);
   project_xpm                 : aliased Chars_Ptr_Array (0 .. 0);
   project_modified_xpm        : aliased Chars_Ptr_Array (0 .. 0);
   project_ext_xpm             : aliased Chars_Ptr_Array (0 .. 0);
   project_ext_closed_xpm      : aliased Chars_Ptr_Array (0 .. 0);
   mini_folder_object_xpm      : aliased Chars_Ptr_Array (0 .. 0);
   mini_folder_exec_xpm        : aliased Chars_Ptr_Array (0 .. 0);

private

   pragma Import (C, logo_xpm);
   pragma Import (C, project_closed_xpm);
   pragma Import (C, project_xpm);
   pragma Import (C, project_ext_xpm);
   pragma Import (C, project_ext_closed_xpm);
   pragma Import (C, mini_folder_object_xpm, "mini_folder_object_xpm");
   pragma Import (C, mini_folder_exec_xpm, "mini_folder_exec_xpm");
   pragma Import (C, project_modified_xpm);
   pragma Import (C, project_modified_closed_xpm);

end Pixmaps_Prj;
