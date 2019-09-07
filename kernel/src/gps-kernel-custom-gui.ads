------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2006-2019, AdaCore                     --
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

--  This package provides a GUI to edit the list of startup scripts, and
--  possible initialization strings. A large part of the work is done in
--  GPS.Kernel.Modules;

with GNAT.Strings;            use GNAT.Strings;
with GNATCOLL.VFS;

with Gtk.Notebook;
with Gtk.Tree_Store;
with Gtk.Tree_View;
with Gtk.Widget;

with Default_Preferences;     use Default_Preferences;
with Default_Preferences.GUI; use Default_Preferences.GUI;
with GPS.Kernel;
with GPS.Kernel.Modules;      use GPS.Kernel.Modules;

package GPS.Kernel.Custom.GUI is

   type Startup_Module_ID_Record is new Module_ID_Record with private;
   type Startup_Module_ID is access all Startup_Module_ID_Record'Class;

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register the module into the list

   type Root_Plugins_Preferences_Page_Record is new Preferences_Page_Record
   with null record;
   type Root_Plugins_Preferences_Page is
     access all Root_Plugins_Preferences_Page_Record'Class;
   --  Type used to represent the root preferences page for all plugins.

   overriding function Get_Widget
     (Self    : not null access Root_Plugins_Preferences_Page_Record;
      Manager : not null Preferences_Manager)
      return Gtk.Widget.Gtk_Widget;
   --  See inherited documentation.

   type Plugins_Preferences_Assistant_Page_Record is
     new Root_Plugins_Preferences_Page_Record with null record;

   overriding function Get_Widget
     (Self    : not null access Plugins_Preferences_Assistant_Page_Record;
      Manager : not null Preferences_Manager)
      return Gtk.Widget.Gtk_Widget;
   --  See inherited documentation.

   type Plugin_Preferences_Page_Record is new Preferences_Page_Record with
      private;
   type Plugin_Preferences_Page is
     access all Plugin_Preferences_Page_Record'Class;
   --  Type used for plugin preferences pages.

   overriding procedure Register_Group
     (Self             : not null access Plugin_Preferences_Page_Record;
      Name             : String;
      Group            : not null Preferences_Group;
      Priority         : Integer := -1;
      Replace_If_Exist : Boolean := False);
   --  See inherited documentation.

   overriding function Get_Widget
     (Self    : not null access Plugin_Preferences_Page_Record;
      Manager : not null Preferences_Manager)
      return Gtk.Widget.Gtk_Widget;
   --  See inherited documentation.

   overriding procedure Free (Self : in out Plugin_Preferences_Page_Record);
   --  See inherited documentation.

   function Get_Documentation
     (Self : not null access Plugin_Preferences_Page_Record) return String;
   --  Return the plugin page documentation.

   function Get_Plugin_Name
     (Self : not null access Plugin_Preferences_Page_Record) return String;
   --  Return the name of the plugin associated to this page

   function Get_Plugin_Label
     (Self : not null access Plugin_Preferences_Page_Record) return String;
   --  Return a suitable label for the plugin associated with this page

   type Startup_Editor_Page_View_Record is new Preferences_Page_View_Record
   with private;
   type Startup_Editor is access all Startup_Editor_Page_View_Record'Class;
   --  Type of the 'Plugins' page view in the preferences editor dialog.

   overriding procedure Display_Subpage
     (Self         : not null access Startup_Editor_Page_View_Record;
      Subpage_Name : String);
   --  See inherited documentation.

   overriding procedure Set_Pref_Highlighted
     (Self      : not null access Startup_Editor_Page_View_Record;
      Pref      : not null access Preference_Record'Class;
      Highlight : Boolean);
   --  See inherited documentation.

private

   type Plugin_Preferences_Page_Record is new Preferences_Page_Record with
      record
         Plugin_Name       : GNAT.Strings.String_Access;
         --  Name of the plugin associated to this page.

         Plugin_Label      : GNAT.Strings.String_Access;
         --  Suitable label used to identify the plugin in the 'Plugins' page
         --  of the Preferences Editor (or Preferences Assistant).

         File              : GNATCOLL.VFS.Virtual_File;
         --  Plugin file associated to this preferences page.

         Explicit          : Boolean;
         --  Used to know if the plugin has been loaded at startup or not
         --  because of an explicit user setting.

         Doc               : GNAT.Strings.String_Access;
         --  Plugin documentation.
      end record;

   type Startup_Editor_Page_View_Record is new Preferences_Page_View_Record
   with record
      Tree                : Gtk.Tree_View.Gtk_Tree_View;
      Model               : Gtk.Tree_Store.Gtk_Tree_Store;
      Plugins_Notebook    : Gtk.Notebook.Gtk_Notebook;

      Show_Restart_Dialog : Boolean := True;
      --  Used to know if a dialog asking if the user wants to restart GPS
      --  to take into account the modifications in the list of startup scripts
      --  should be displayed or not.
   end record;

   type Startup_Module_ID_Record is new Module_ID_Record with null record;

end GPS.Kernel.Custom.GUI;
