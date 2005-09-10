-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2003-2005                       --
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

--  This package provides an interface to the customization files.
--  This files are written in XML, and can be found in several places:
--     - system wide directory
--     - project specific directory (not implemented yet)
--     - user specific directory
--  None of these files is specific to any module. They can mix menu additions,
--  new aliases, new languages,... freely, so that the user can organize the
--  files as he sees fit. This also makes it more convenient to add support for
--  new modules that need to extend several other modules.
--
--  All these files are parsed at startup time by GPS, which create an internal
--  repository which can be queries by each module on demand.
--
--  Modules which are interested in customization must provide a callback
--  in their call to Register_Module. This callback will be called at startup
--  once all the modules have been registered. For dynamically loaded modules,
--  this callback is called just after they have been registered.
--  If a module adds a hard coded customization string, all modules will be
--  signaled.
--
--  See GPS.Kernel.Modules.Module_Customization_Handler.

--  Future plans:
--    - Handling of dynamic modules: they need to be aware, when loaded, of
--      the current customizations concerning them. This means either keep in
--      memory the contents of the custom files, or reparse them every time a
--      module is loaded. Likewise for strings executed through
--      Add_Customization_String.
--    - Dynamic modules need to be loaded when the menus they have registered
--      are called initially.

with Glib;
with Glib.Xml_Int;

package GPS.Kernel.Custom is

   -----------------------------------------
   -- Loading and defining customizations --
   -----------------------------------------

   procedure Load_All_Custom_Files
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Find and parse all the customization files (system, project or user).
   --  This also parses any XML string that has been registered previously
   --  through Add_Customization_String.
   --  This updates the internal directory, and calls all the callback
   --  registered by the modules.

   function Add_Customization_String
     (Kernel        : access GPS.Kernel.Kernel_Handle_Record'Class;
      Customization : UTF8_String;
      From_File     : String;
      Start_Line    : Positive := 1) return String;
   --  Add a new customization string, as if it had been parsed from a custom
   --  file. Customization should contain one or more top-level XML tags, as in
   --      "<Alias ...></Alias>  <Language ..></Language>
   --  If the customization files have already been loaded through
   --  Load_All_Custom_Files, this function immediately calls all the
   --  callback which the modules have registered.
   --  (From_File, Start_Line) indicate where the customization string was read
   --  from, so that error messages are properly localized.
   --  This function returns the error message, if any, from the XML parser if
   --  the string couldn't be parsed

   procedure Execute_Customization_String
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      File   : VFS.Virtual_File;
      Node   : Glib.Xml_Int.Node_Ptr;
      Level  : GPS.Kernel.Customization_Level);
   --  Send a signal to all registered modules to indicate a new customization
   --  string.

   function Get_Custom_Path return String;
   --  Returns the custom path as set by GPS_CUSTOM_PATH

end GPS.Kernel.Custom;
