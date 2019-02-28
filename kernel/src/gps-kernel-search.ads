------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2013-2019, AdaCore                     --
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

--  The root for all the search providers in GPS

with GPS.Search;
with Glib.Object;
with Gtk.Box;
with Gtk.Widget;

with GNATCOLL.Projects;
with Histories;

package GPS.Kernel.Search is

   type Kernel_Search_Result is abstract new GPS.Search.Search_Result
   with record
      Kernel : GPS.Kernel.Kernel_Handle;
   end record;

   function Full
      (Self : not null access Kernel_Search_Result)
      return Gtk.Widget.Gtk_Widget is (null);
   --  Returns the full description for the result. This description might be
   --  displayed in a separate pane in the search popup. In most cases, GPS
   --  will not query or display this information at all.

   type Kernel_Search_Provider is abstract new GPS.Search.Search_Provider
   with record
      Kernel : GPS.Kernel.Kernel_Handle;
   end record;
   type Kernel_Search_Provider_Access is
     access all Kernel_Search_Provider'Class;

   type On_Settings_Changed_Callback is access procedure
     (Data : access Glib.Object.GObject_Record'Class);

   procedure Register_Module (Self : not null access Kernel_Search_Provider)
   is null;
   --  Register the module corresponding to the given search provider.
   --  This procedure does nothing by default: override it if the inherited
   --  search provider needs to store some global data in a module.

   procedure Edit_Settings
     (Self : not null access Kernel_Search_Provider;
      Box  : not null access Gtk.Box.Gtk_Box_Record'Class;
      Data : not null access Glib.Object.GObject_Record'Class;
      On_Change : On_Settings_Changed_Callback) is null;
   --  Add settings edition widgets to the box. Any change to the settings
   --  should result in a call to On_Change and pass Data as a parameter.
   --  For instance, each widget would connect its change callback to this, to
   --  ensure proper refresh of the completion entry.

   procedure Adjust_Score
      (Self   : not null access Kernel_Search_Provider;
       Result : not null access GPS.Search.Search_Result'Class);
   --  Adjust the score of Result, using various criteria. Among other
   --  things, this uses the list of most recent items selected for this
   --  provider so that they appear first

   overriding procedure On_Result_Executed
      (Self   : not null access Kernel_Search_Provider;
       Result : not null access GPS.Search.Search_Result'Class);
   --  Change the list of recent items, after Result has been selected
   --  by the user.

   Provider_Filenames   : constant String := "File names";
   Provider_Actions     : constant String := "Actions";
   Provider_Builds      : constant String := "Build";
   Provider_Opened_Win  : constant String := "Opened";
   Provider_Entities    : constant String := "Entities";
   Provider_Sources     : constant String := "Sources";
   Provider_Bookmarks   : constant String := "Bookmarks";
   Provider_Plugins     : constant String := "Plugins";
   Provider_Preferences : constant String := "Preferences";

   --  The names must be synchronized with the search.py plugin

   Action_Name_Prefix : constant String := "Global Search in context: ";
   --  prefix for the actions, which should be followed by one of the provider
   --  ids.

   Key_Search_Displays_Relative_Paths : constant Histories.History_Key :=
     "search-displays-relative-paths";
   --  Whether to display paths of project's sources as a relative
   --  to the project's file

   function Path_And_Name
     (Kernel  : Kernel_Handle;
      File    : Virtual_File;
      Project : GNATCOLL.Projects.Project_Type)
      return String;
   --  Return name and relative path of file if
   --  Key_Search_Displays_Relative_Paths is set and absolute path and name
   --  in other case.

end GPS.Kernel.Search;
