-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2006                         --
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

with GNAT.Expect;
pragma Warnings (Off);
with GNAT.Expect.TTY.Remote;     use GNAT.Expect.TTY.Remote;
pragma Warnings (On);
with GNAT.OS_Lib;

with GPS.Intl;             use GPS.Intl;
with GPS.Kernel.Hooks;     use GPS.Kernel.Hooks;
with GPS.Kernel.Scripts;
with Filesystem;           use Filesystem;
with Interactive_Consoles;
with VFS;

package GPS.Kernel.Remote is

   Remote_Menu_Path : constant String
     := "/_" & (-"Tools") & "/_" & (-"Remote") & "/";
   --  Defines where the path for the remote menu

   type Server_Type is
     (GPS_Server,
      Build_Server,
      Execution_Server,
      Debug_Server);

   subtype Remote_Server_Type is Server_Type
     range Build_Server .. Debug_Server;

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register the module into the list

   function To_Remote_Possible
     (Path : String;
      To   : String) return Boolean;
   --  Tells if path equivalence on To server exists

   function To_Local_Possible
     (Path : String;
      From : String) return Boolean;
   --  Tells if From'path equivalence on local server exists

   function To_Remote
     (Path       : String;
      To         : Server_Type;
      Unix_Style : Boolean := False) return String;
   --  Translate a local file/directory path to server 'To'
   --  if Unix_Style is set, the translated path will have a unix style.

   function To_Remote
     (Path       : String;
      To         : String;
      Unix_Style : Boolean := False) return String;
   --  Same as above, using To's nickname instead of Server_Type

   function To_Local
     (Path : String;
      From : Server_Type) return String;
   --  Translate a remote file/directory path from server 'From' to local path.

   function To_Local
     (Path : String;
      From : String) return String;
   --  Same as above, using From's nickname instead of Server_Type.

   function To_Unix_Path
     (Path       : String;
      Server     : Server_Type;
      Use_Cygwin : Boolean := False) return String;
   --  Transform a remote path into unix path style.
   --  Use_Cygwin forces cygwin style path if filesystem of server is
   --  windows fs

   procedure Synchronize
     (Kernel       : Kernel_Handle;
      From         : Server_Type;
      To           : Server_Type;
      Queue_Id     : String;
      Sync_Deleted : Boolean);
   --  Forces a file system synchronisation between the two servers.
   --  If Queue_Id is not an empty string, then the synchronisation is
   --   launched as an asynchronous command using the queue_id. Else, it is
   --   launched synchronously.
   --  If Sync_Deleted is set, then deleted files in src server will be
   --   deleted on To server. Else, an update is performed (only newer files
   --   are copied).

   ---------------------------------
   -- Error display when spawning --
   ---------------------------------

   type Error_Display_Record is abstract tagged null record;
   type Error_Display is access all Error_Display_Record'Class;

   procedure On_Error
     (Manager : access Error_Display_Record;
      Message : String) is abstract;

   type Default_Error_Display_Record is new Error_Display_Record with record
      Kernel : Kernel_Handle;
   end record;
   --  Displays error on the messages console

   procedure On_Error
     (Manager : access Default_Error_Display_Record;
      Message : String);

   procedure Spawn
     (Kernel           : Kernel_Handle := null;
      Arguments        : GNAT.OS_Lib.Argument_List;
      Server           : Server_Type;
      Pd               : out GNAT.Expect.Process_Descriptor_Access;
      Success          : out Boolean;
      Use_Ext_Terminal : Boolean := False;
      Console          : Interactive_Consoles.Interactive_Console := null;
      Show_Command     : Boolean := True;
      Directory        : String := "";
      Error_Manager    : Error_Display := null);
   --  Launch given arguments on Server. Returns a valid Process
   --  descriptor and success set to true upon success.
   --  If Use_Ext_Terminal is not null, then the program is executed in a
   --  separate terminal.
   --  If Console is not null, and Show_Command is set, outputs the command
   --  line to the console.
   --  If directory is set, change default dir to Directory before launching
   --  the command. Returns to current dir after spawning.

   --------------------------
   -- Synchronization Hook --
   --------------------------

   Rsync_Action_Hook : constant String := "rsync_action_hooks";

   Rsync_Hook_Type : constant String := "rsync_action_hook";

   type Rsync_Hooks_Args
     (Tool_Name_Length, Src_Name_Length, Dest_Name_Length, Queue_Id_Length,
      Src_Path_Length, Dest_Path_Length : Natural)
   is new Hooks_Data with record
      Sync_Deleted : Boolean;
      --  Delete dest files if local files were deleted
      Synchronous  : Boolean;
      --  Tells if the synchronisation call shall be performed synchronously
      Tool_Name    : String (1 .. Tool_Name_Length);
      --  What hook function shall perform the action
      Src_Name     : String (1 .. Src_Name_Length);
      --  Source server nickname
      Dest_Name    : String (1 .. Dest_Name_Length);
      --  Destination server nickname
      Queue_Id     : String (1 .. Queue_Id_Length);
      --  Queue_Id used to enqueue the sync command
      Src_Path     : String (1 .. Src_Path_Length);
      --  Source path
      Dest_Path    : String (1 .. Dest_Path_Length);
      --  Destination path
   end record;

   function Create_Callback_Data
     (Script    : access GPS.Kernel.Scripts.Scripting_Language_Record'Class;
      Hook_Name : String;
      Data      : access Rsync_Hooks_Args)
      return GPS.Kernel.Scripts.Callback_Data_Access;
   --  See inherited for documentation

   --------------------------------
   -- Server Config Changed Hook --
   --------------------------------

   Server_Config_Changed_Hook : constant String := "srv_config_hooks";

   Server_Config_Changed_Hook_Type : constant String := "srv_config_hook";

   type Server_Config_Changed_Hooks_Args
     (Nickname_Length : Natural) is new Hooks_Data with
   record
      Server   : Server_Type;
      --  The server type modified
      Nickname : String (1 .. Nickname_Length);
      --  The new server nickname attached to it
   end record;

   function Create_Callback_Data
     (Script    : access GPS.Kernel.Scripts.Scripting_Language_Record'Class;
      Hook_Name : String;
      Data      : access Server_Config_Changed_Hooks_Args)
      return GPS.Kernel.Scripts.Callback_Data_Access;
   --  See inherited for documentation

   ------------------------------
   -- Server List Changed Hook --
   ------------------------------

   Server_List_Changed_Hook : constant String := "srv_list_hooks";
   --  No data hook

   ----------------------------
   --  Servers configuration --
   ----------------------------

   procedure Configure_Server_List
     (Kernel : GPS.Kernel.Kernel_Handle);
   --  Runs the server list editor dialog

   procedure Assign
     (Kernel     : Kernel_Handle;
      Server     : Server_Type;
      Nickname   : String;
      Prj_File   : VFS.Virtual_File := VFS.No_File;
      Reload_Prj : Boolean := False);
   --  Assigns a Server to a configuration
   --  Prj_File allows to select to which project file this configuration is
   --   assigned to. If No_File, it is assigned to the current project.
   --  Reload_Prj, if set, recomputes the view if the build_server changed.

   function Get_Nickname (Server : Server_Type) return String;
   --  Gets the nickname of a server

   function Get_Printable_Nickname (Server : Server_Type) return String;
   --  Gets the nickname of a server. If server is local, Local_Nickname is
   --  returned

   function Get_Network_Name (Server : Server_Type) return String;
   --  Gets the network name of a server

   function Get_Filesystem
     (Server : Server_Type) return Filesystem_Record'Class;
   --  Get the filesystem of the specified server

   function Is_Local (Server : Server_Type) return Boolean;
   --  Tells if the server is the local server or is remote

end GPS.Kernel.Remote;
