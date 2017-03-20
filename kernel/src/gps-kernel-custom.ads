------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2003-2017, AdaCore                     --
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

with GNATCOLL.VFS_Utils;
with String_Hash;

package GPS.Kernel.Custom is

   -----------------------------------------
   -- Loading and defining customizations --
   -----------------------------------------

   procedure Load_System_Custom_Files
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Find and parse all the customization files (system, project).
   --  This also parses any XML string that has been registered previously
   --  through Add_Customization_String.
   --  This updates the internal directory, and calls all the callback
   --  registered by the modules.

   procedure Load_User_Custom_Files
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  As above but for user's plugins

   function Add_Customization_String
     (Kernel        : access GPS.Kernel.Kernel_Handle_Record'Class;
      Customization : Glib.UTF8_String;
      From_File     : Filesystem_String;
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

   -------------------------------
   -- Customization directories --
   -------------------------------

   function Autoload_System_Dir
     (Kernel : access Kernel_Handle_Record'Class) return Virtual_File;
   --  Return the system directory for automatically loaded scripts.
   --  This is $prefix/share/gps/plug-ins

   function No_Autoload_System_Dir
     (Kernel : access Kernel_Handle_Record'Class) return Virtual_File;
   --  Return the system directory for scripts that are not automatically
   --  loaded by default.
   --  This is $prefix/share/gps/libraries

   function Autoload_User_Dir
     (Kernel : access Kernel_Handle_Record'Class) return Virtual_File;
   --  Return the user directory for automatically loaded scripts
   --  This is ~/.gps/plug-ins.
   --  The directory is created if it doesn't exist yet.

   function Support_Core_Dir
     (Kernel : access Kernel_Handle_Record'Class) return Virtual_File;
   function Support_UI_Dir
     (Kernel : access Kernel_Handle_Record'Class) return Virtual_File;
   function Support_No_Autoload_Dir
     (Kernel : access Kernel_Handle_Record'Class) return Virtual_File;
   --  The two directories that contain support python files to be loaded at
   --  startup, but not visible to users in the Plugins dialog.

   function Get_Custom_Path return File_Array;
   --  Return a list of directories in which the user might
   --  have put custom scripts to autoload.

   -----------------------------------------------
   -- Customization files and automatic loading --
   -----------------------------------------------

   procedure Parse_Startup_Scripts_List
     (Kernel : access Kernel_Handle_Record'Class);
   --  Parse the list of scripts that should be automatically loaded at
   --  startup, and their initialization commands.

   procedure Save_Startup_Scripts_List
     (Kernel : access Kernel_Handle_Record'Class);
   --  Save the file ~/.gps/startup.xml

   procedure Override_Startup_Script
     (Kernel         : access Kernel_Handle_Record'Class;
      Base_Name      : String;
      Load           : Boolean);
   --  Override the attributes of startup.xml for this specific script. This
   --  new value will automatically be saved in startup.xml at the end of the
   --  session.
   --  This mustn't be called while you are iterating over all known scripts.
   --  Initialization shouldn't be freed by the caller, and becomes the
   --  ownership of Kernel

   function Load_File_At_Startup
     (Kernel         : access Kernel_Handle_Record'Class;
      File           : GNATCOLL.VFS.Virtual_File;
      Default        : Boolean) return Boolean;
   --  Whether File should be loaded at startup, based on the contents of
   --  the file ~/.gps/startup.xml
   --  This function also registers File as a startup script, so that GPS can
   --  list them to the user later on.
   --  For python modules, the File should be the name of the directory.

   procedure For_All_Startup_Scripts
     (Kernel : access Kernel_Handle_Record'Class;
      Callback : not null access procedure
        (Name     : String;
         File     : GNATCOLL.VFS.Virtual_File;
         Loaded   : Boolean;
         Explicit : Boolean));
   --  Iterate over all known startup scripts.
   --  This only include those scripts that user can choose to explicitly
   --  enable or disable, not the mandatory support scripts.
   --  It also only return the scripts for which an actual source was found,
   --  not those that were only mentioned in startup.xml
   --
   --  File is the full file name of the startup script.
   --
   --  Loaded is True if the script have been loaded on startup. This boolean
   --  might either have been specified in startup.xml, or found explicitly
   --  from the location of the startup script. Note that it is possible for
   --  python scripts to be loaded by other scripts (and not at GPS startup),
   --  but in this case they would return False here.
   --
   --  Explicit is True if the script was loaded (or not) because of an
   --  explicit user settings.

private
   type Load_Mode is
     (Automatic,     --  depending on directory
      Explicit_On,   --  explicitly enabled
      Explicit_Off); --  explicitly disabled

   type Script_Description is record
      Mode           : Load_Mode := Automatic;  --  Whether to load this plugin
      Loaded         : Boolean := False;
      File           : GNATCOLL.VFS.Virtual_File;
   end record;
   type Script_Description_Access is access all Script_Description;
   --  File is set to No_File to indicate that no source file was found for the
   --  plugin. If No_File, the plugin is not displayed in the dialog.
   --
   --  Loaded indicates whether the script was loaded

   function Get_Script_From_Base_Name
     (Kernel    : not null access Kernel_Handle_Record'Class;
      Base_Name : String) return Script_Description_Access;
   --  Return the script description for the given Base_Name, or null if not
   --  Found.

   procedure Free (File : in out Script_Description_Access);
   --  Free the memory occupied by File

   package Scripts_Hash is new String_Hash
     (Data_Type      => Script_Description_Access,
      Free_Data      => Free,
      Null_Ptr       => null,
      Case_Sensitive => GNATCOLL.VFS_Utils.Local_Host_Is_Case_Sensitive);

end GPS.Kernel.Custom;
