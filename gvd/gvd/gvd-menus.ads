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

--  <description>
--  This package implements all the contextual menus available throughout
--  odd. All the menus are created in this package, and then stored as user
--  data in the appropriate widgets, so that we don't need to recreate them
--  every time.
--  </description>

with Gtk.Menu;
with Odd.Canvas;
with Glib;

with Odd.Code_Editors;
with Display_Items;
with Items;
with Odd.Process;

package Odd.Menus is

   function Contextual_Background_Menu
     (Canvas : access Odd.Canvas.Odd_Canvas_Record'Class)
     return Gtk.Menu.Gtk_Menu;
   --  Create (if necessary) and reset the contextual menu used when the user
   --  clicks in the background of the canvas.

   function Item_Contextual_Menu
     (Canvas    : access Odd.Canvas.Odd_Canvas_Record'Class;
      Item      : access Display_Items.Display_Item_Record'Class;
      Component : Items.Generic_Type_Access)
     return Gtk.Menu.Gtk_Menu;
   --  Create (if necessary) and reset the contextual menu used when a specific
   --  component in an item is selected.
   --  Note that Component can be null if the user has clicked for instance
   --  on the title bar.

   function Editor_Contextual_Menu
     (Editor   : access Odd.Code_Editors.Code_Editor_Record'Class;
      Line     : Glib.Gint;
      Entity   : String)
     return Gtk.Menu.Gtk_Menu;
   --  Create (if necessary) and reset the contextual menu used when a specific
   --  entity is selected in the code editor.

   function Debugger_Contextual_Menu
     (Process  : access Odd.Process.Debugger_Process_Tab_Record'Class)
     return Gtk.Menu.Gtk_Menu;
   --  Create (if necessary) and reset the contextual menu used in the
   --  debugger command window.
   --  ??? Should we pass a Task_Dialog_Record as well, for the current
   --  thread/task ?

end Odd.Menus;
