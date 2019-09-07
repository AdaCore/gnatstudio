------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2005-2019, AdaCore                     --
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

with GNATCOLL.Scripts;    use GNATCOLL.Scripts;

with Glib.Object;         use Glib.Object;
with Gtk.Menu;            use Gtk.Menu;

with GPS.Kernel;          use GPS.Kernel;
with VCS_View.Activities; use VCS_View.Activities;

package VCS_Activities_View_API is

   procedure Open_Activities_Explorer
     (Kernel  : not null access Kernel_Handle_Record'Class;
      Context : Selection_Context);
   --  If the VCS Activities Explorer is not displayed, display it

   procedure Query_Status
     (Widget : access GObject_Record'Class;
      Kernel : not null access Kernel_Handle_Record'Class);
   --  List all files as part of an activity

   procedure Query_Activities_Files
     (Kernel     : Kernel_Handle;
      Real_Query : Boolean);
   --  Query status for activities files

   procedure Query_Activities_Files
     (Explorer   : VCS_Activities_View_Access;
      Kernel     : not null access Kernel_Handle_Record'Class;
      Real_Query : Boolean);
   --  Query/List the status of files belonging to activities.
   --  If Real_Query is True, a real VCS query will be made, otherwise
   --  the files will simply be listed.
   --  Calling this does NOT open the VCS Explorer.

   procedure VCS_Activities_Contextual_Menu
     (Context : Selection_Context;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class);
   --  Complete Menu with the commands related to the VCS Activities,
   --  according to the information in Context.

   procedure On_Menu_Add_To_Activity
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context);
   --  Add a file to an activity menu callback

   procedure On_Menu_Remove_From_Activity
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context);
   --  Remove a file from an activity menu callback

   procedure On_Menu_Commit_As_Activity
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context);
   --  Add selected files into an anonymous activity ready to group-commit

   procedure On_Menu_Close_Open_Activity
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context);
   --  Change Activity's status to/from closed/opened

   procedure VCS_Activities_Command_Handler
     (Data    : in out GNATCOLL.Scripts.Callback_Data'Class;
      Command : String);
   --  VCS Activities class command handler

end VCS_Activities_View_API;
