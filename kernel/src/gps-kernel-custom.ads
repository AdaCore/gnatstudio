-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2003-2006                      --
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
with Commands.Custom;
with File_Utils;
with Remote;

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
   --  As above but for user's plug-ins

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

   -------------------------------
   -- Customization directories --
   -------------------------------

   function Autoload_System_Dir
     (Kernel : access Kernel_Handle_Record'Class) return String;
   --  Return the system directory for automatically loaded scripts.
   --  This is $prefix/share/gps/plug-ins

   function No_Autoload_System_Dir
     (Kernel : access Kernel_Handle_Record'Class) return String;
   --  Return the system directory for scripts that are not automatically
   --  loaded by default.
   --  This is $prefix/share/gps/libraries

   function Autoload_User_Dir
     (Kernel : access Kernel_Handle_Record'Class) return String;
   --  Return the user directory for automatically loaded scripts
   --  This is ~/.gps/plug-ins.
   --  The directory is created if it doesn't exist yet.

   function Get_Custom_Path return String;
   --  Return a colon-separated list of directories in which the user might
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
      Load           : Boolean;
      Initialization : Glib.Xml_Int.Node_Ptr);
   --  Override the attributes of startup.xml for this specific script. This
   --  new value will automatically be saved in startup.xml at the end of the
   --  session.
   --  This mustn't be called while you are iterating over all known scripts.
   --  Initialization shouldn't be freed by the caller, and becomes the
   --  ownership of Kernel

   function Load_File_At_Startup
     (Kernel  : access Kernel_Handle_Record'Class;
      File    : VFS.Virtual_File;
      Default : Boolean) return Boolean;
   --  Whether File should be loaded at startup, based on the contents of
   --  the file ~/.gps/startup.xml
   --  This function also registers File as a startup script, so that GPS can
   --  list them to the user later on.

   function Initialization_Command
     (Kernel  : access Kernel_Handle_Record'Class;
      File    : VFS.Virtual_File)
      return Commands.Custom.Custom_Command_Access;
   --  Return the command to execute to initialize this module. This is
   --  null if no initialization command was provided. These are read from
   --  the file ~/.gps/startup.xml.
   --  The user must free the returned value.

   type Script_Iterator is private;
   type Script_Description is private;

   procedure Get_First_Startup_Script
     (Kernel : access Kernel_Handle_Record'Class;
      Iter   : out Script_Iterator);
   procedure Next  (Iter : in out Script_Iterator);
   function At_End (Iter : Script_Iterator) return Boolean;
   function Get    (Iter : Script_Iterator) return Script_Description;
   function Get_Script (Iter : Script_Iterator) return String;
   --  Iterate over all known startup

   function Get_Full_File (Desc : Script_Description) return VFS.Virtual_File;
   --  Return the full file name of the startup script. This is different from
   --  Get_Script above.
   --  The latter returns the base name of the script, as found in startup.xml.
   --  The script can thus be found anywhere in the GPS directories.
   --  Get_Full_File will return the actual location where the script was
   --  found, or No_File if it was not found when GPS started.

   function Get_Load (Desc : Script_Description) return Boolean;
   --  Whether this script should be loaded on startup. This boolean might
   --  either have been specified in startup.xml, or found explicitly from the
   --  location of the startup script

   function Get_Explicit (Desc : Script_Description) return Boolean;
   --  Whether the current loading status of the file was given by startup.xml
   --  (if True is returned), or found automatically

   function Get_Init (Descr : Script_Description) return Glib.Xml_Int.Node_Ptr;
   --  The initialization commands that should be performed after the script
   --  has been loaded. You mustn't free the returned value, which points to
   --  internal data.

private
   type Script_Description is record
      Initialization : Glib.Xml_Int.Node_Ptr; --  from startup.xml
      Load           : Boolean;               --  from startup.xml
      Explicit       : Boolean;               --  whether it was in startup.xml
      File           : VFS.Virtual_File;      --  from reading the directories
   end record;
   type Script_Description_Access is access all Script_Description;

   procedure Free (File : in out Script_Description_Access);
   --  Free the memory occupied by File

   package Scripts_Hash is new String_Hash
     (Data_Type      => Script_Description_Access,
      Free_Data      => Free,
      Null_Ptr       => null,
      Case_Sensitive =>
         File_Utils.Is_Case_Sensitive (Server => Remote.GPS_Server));

   type Script_Iterator is record
      Kernel : Kernel_Handle;
      Iter   : Scripts_Hash.String_Hash_Table.Iterator;
   end record;

end GPS.Kernel.Custom;
