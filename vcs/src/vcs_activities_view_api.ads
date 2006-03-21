-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2005-2006                      --
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

with Glib.Object;         use Glib.Object;
with Gtk.Menu;            use Gtk.Menu;

with GPS.Kernel;          use GPS.Kernel;
with VCS_View.Activities; use VCS_View.Activities;

package VCS_Activities_View_API is

   procedure Open_Activities_Explorer
     (Kernel  : Kernel_Handle;
      Context : Selection_Context);
   --  If the VCS Activities Explorer is not displayed, display it

   procedure Query_Status
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle);
   --  List all files as part of an activity

   procedure Query_Activities_Files
     (Kernel      : Kernel_Handle;
      Real_Query  : Boolean);
   --  Query status for activities files

   procedure Query_Activities_Files
     (Explorer   : VCS_Activities_View_Access;
      Kernel     : Kernel_Handle;
      Real_Query : Boolean);
   --  Query/List the status of files belonging to activities.
   --  If Real_Query is True, a real VCS query will be made, otherwise
   --  the files will simply be listed.
   --  Calling this does NOT open the VCS Explorer.

   procedure VCS_Activities_Contextual_Menu
     (Kernel  : Kernel_Handle;
      Context : Selection_Context;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class);
   --  Complete Menu with the commands related to the VCS Activities,
   --  according to the information in Context.

   procedure On_Menu_Add_To_Activity
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context);
   --  Add a file to an activity menu callback

   procedure On_Menu_Close_Open_Activity
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context);
   --  Change Activity's status to/from closed/opened

end VCS_Activities_View_API;
