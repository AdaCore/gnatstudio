-----------------------------------------------------------------------
--                 Odd - The Other Display Debugger                  --
--                                                                   --
--                         Copyright (C) 2000                        --
--                 Emmanuel Briot and Arnaud Charlet                 --
--                                                                   --
-- Odd is free  software;  you can redistribute it and/or modify  it --
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

with Gtk.Ctree; use Gtk.Ctree;
with Gtk.Widget; use Gtk.Widget;
with Language;

package Odd.Explorer is

   type Position_Type is record
      Line, Column, Index : Natural;
   end record;

   type Explorer_Handler is access
     procedure
       (Widget   : access Gtk_Widget_Record'Class;
        Position : Position_Type);
   --  Handler called when an item is selected in the tree.
   --  Index is the position in the buffer where the selected entity
   --  starts.
   --  Widget is the Window parameter given to Explore below.

   function Explore
     (Window  : access Gtk_Widget_Record'Class;
      Buffer  : String;
      Lang    : Language.Language_Access;
      Handler : Explorer_Handler := null) return Gtk_Ctree;
   --  Parse the entities present in buffer.
   --  Return a grapihcal tree representing these entities.
   --  See Explorer_Handler above for a description of Handler.

end Odd.Explorer;
