-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                         Copyright (C) 2006                        --
--                             AdaCore                               --
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
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

--  This package describes the icons associated to language constructs.

with Gdk.Pixbuf;
with Gtk.Widget;

package Language.Icons is

   type Cat_Array is array (Language_Category) of Gdk.Pixbuf.Gdk_Pixbuf;

   type Pixbuf_Entity_Array is array
     (Boolean,              --  True for entities that are declarations
      Construct_Visibility) --  The visibility of entities
   of Cat_Array;

   Entity_Icons : Pixbuf_Entity_Array;
   --  The icons to be associated with entities.

   procedure Init_Graphics (Widget : Gtk.Widget.Gtk_Widget);
   --  Initialize the graphics. Widgets serves as a resource for the default
   --  styles and GCs.

end Language.Icons;
