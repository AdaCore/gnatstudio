-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                         Copyright (C) 2005                        --
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

with Glib.Object; use Glib.Object;
with Gtk.Menu;    use Gtk.Menu;

with GPS.Kernel;  use GPS.Kernel;

package VCS_Activities_View_API is

   procedure Open_Activities_Explorer
     (Kernel  : Kernel_Handle;
      Context : Selection_Context_Access);
   --  If the VCS Activities Explorer is not displayed, display it

   procedure Query_Status
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle);
   --  List all files as part of an activity

   procedure Query_Activities_Files
     (Kernel     : Kernel_Handle;
      Real_Query : Boolean);
   --  Query status for activities files

   procedure VCS_Activities_Contextual_Menu
     (Kernel  : Kernel_Handle;
      Context : Selection_Context_Access;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class);
   --  Complete Menu with the commands related to the VCS Activities,
   --  according to the information in Context.
   --  If Show_Everything is True, add insensitive menus for items that do not
   --  correspond to the context.

   procedure On_Menu_Add_To_Activity
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);
   --  Add a file to an activity menu callback

end VCS_Activities_View_API;
