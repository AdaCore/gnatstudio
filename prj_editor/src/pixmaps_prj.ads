-----------------------------------------------------------------------
--                          GLIDE II                                 --
--                     Copyright (C) 2001                            --
--                          ACT-Europe                               --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

with Gtkada.Types; use Gtkada.Types;

package Pixmaps_Prj is

   --  Note that the following pixmaps are in alphabetical order.
   --  The actual size of the pixmaps is only known by GtkAda, which is
   --  enough for our needs and avoid having to maintain this information here.

   logo_xpm            : aliased Chars_Ptr_Array (0 .. 0);
   delete_var_xpm      : aliased Chars_Ptr_Array (0 .. 0);
   project_closed_xpm  : aliased Chars_Ptr_Array (0 .. 0);
   project_xpm         : aliased Chars_Ptr_Array (0 .. 0);

private
   pragma Import (C, logo_xpm);
   pragma Import (C, delete_var_xpm);
   pragma Import (C, project_closed_xpm);
   pragma Import (C, project_xpm);

end Pixmaps_Prj;
