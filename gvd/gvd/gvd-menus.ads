-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                         Copyright (C) 2000                        --
--                 Emmanuel Briot and Arnaud Charlet                 --
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

--  <description>
--  This package implements all the contextual menus available throughout
--  odd. All the menus are created in this package, and then stored as user
--  data in the appropriate widgets, so that we don't need to recreate them
--  every time.
--  </description>

with Gtk.Menu;
with GVD.Canvas;
with Gtk.Object;

with Display_Items;
with Items;
with GVD.Process;

package GVD.Menus is

   package Menu_User_Data is new Gtk.Object.User_Data (Gtk.Menu.Gtk_Menu);

   function Contextual_Background_Menu
     (Canvas : access GVD.Canvas.GVD_Canvas_Record'Class)
      return Gtk.Menu.Gtk_Menu;
   --  Create (if necessary) and reset the contextual menu used when the user
   --  clicks in the background of the canvas.

   function Item_Contextual_Menu
     (Canvas         : access GVD.Canvas.GVD_Canvas_Record'Class;
      Item           : access Display_Items.Display_Item_Record'Class;
      Component      : Items.Generic_Type_Access;
      Component_Name : String) return Gtk.Menu.Gtk_Menu;
   --  Create (if necessary) and reset the contextual menu used when a specific
   --  component in an item is selected.
   --  Note that Component can be null if the user has clicked for instance
   --  on the title bar.

   function Debugger_Contextual_Menu
     (Process  : access GVD.Process.Debugger_Process_Tab_Record'Class)
      return Gtk.Menu.Gtk_Menu;
   --  Create (if necessary) and reset the contextual menu used in the
   --  debugger command window.
   --  ??? Should we pass a Task_Dialog_Record as well, for the current
   --  thread/task ?

end GVD.Menus;
