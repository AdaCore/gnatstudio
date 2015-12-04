------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2006-2015, AdaCore                     --
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

with GNAT.Strings;
with GNATCOLL.VFS;

with Gtk.Widget;

with Default_Preferences;       use Default_Preferences;
with GPS.Kernel;
with GPS.Kernel.Modules;        use GPS.Kernel.Modules;

package Startup_Module is

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

   type Plugin_Preferences_Page_Record is new Preferences_Page_Record with
      private;
   type Plugin_Preferences_Page is
     access all Plugin_Preferences_Page_Record'Class;
   --  Type used for plugin preferences pages.

   overriding procedure Add_Pref
     (Self : not null access Plugin_Preferences_Page_Record;
      Pref : not null Preference);
   --  See inherited documentation.

   overriding function Get_Widget
     (Self    : not null access Plugin_Preferences_Page_Record;
      Manager : not null Preferences_Manager)
      return Gtk.Widget.Gtk_Widget;
   --  See inherited documentation.

   overriding procedure Free (Self : in out Plugin_Preferences_Page_Record);

private

   type Plugin_Preferences_Page_Record is new Preferences_Page_Record with
      record
         Plugin_Name       : GNAT.Strings.String_Access;
         --  Name of the plugin associated to this page.

         File              : GNATCOLL.VFS.Virtual_File;
         --  Plugin file associated to this preferences page.

         Loaded_At_Startup : Boolean;
         --  Used to know if the corresponding plugin is set to be loaded
         --  at startup or not.

         Explicit          : Boolean;
         --  Used to know if the plugin has been loaded at startup or not
         --  because of an explicit user setting.

         Doc               : GNAT.Strings.String_Access;
         --  Plugin documentation.
      end record;

   type Startup_Module_ID_Record is new Module_ID_Record with record
      Has_Changed : Boolean := False;
      --  Boolean used to indicate if the set of startup scripts to load has
      --  been modified since GPS started.
   end record;

end Startup_Module;
