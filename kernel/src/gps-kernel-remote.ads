-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2006-2008, AdaCore           --
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

with GPS.Kernel.Hooks;     use GPS.Kernel.Hooks;
with GNAT.Scripts;
with Filesystem;           use Filesystem;
with Interactive_Consoles;
with Remote;               use Remote;
with VFS;

package GPS.Kernel.Remote is

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register the module into the list

   procedure Synchronize
     (Kernel         : Kernel_Handle;
      From           : Server_Type;
      To             : Server_Type;
      Blocking       : Boolean;
      Print_Command  : Boolean;
      Print_Output   : Boolean;
      Sync_Once_Dirs : Boolean;
      Queue_Id       : String  := "");
   --  Perform a file system synchronisation between From and To.
   --  If Blocking is set, the call is synchronous. Else an asynchronous
   --  command is used.
   --  If Print_Command is set, then asynchronous command will print the rsync
   --  command line on the Messages console.
   --  If Print_Output is set, then asynchronous command will print rsync's
   --  output on the Messages console.
   --  If Sync_Once_Dirs is set, then mirror paths marked as 'sync once' will
   --  also be rsynced
   --  If Blocking is not set and queue_id is not an empty string, then the
   --  specified queue id will be used for the command.

   procedure Synchronize
     (Kernel         : Kernel_Handle;
      From           : String;
      To             : String;
      Blocking       : Boolean;
      Print_Command  : Boolean;
      Print_Output   : Boolean;
      Sync_Once_Dirs : Boolean;
      Queue_Id       : String  := "");
   --  Same as above, with From and To servers identified by their nickname

   procedure Spawn
     (Kernel           : Kernel_Handle;
      Arguments        : GNAT.OS_Lib.Argument_List;
      Server           : Server_Type;
      Pd               : out GNAT.Expect.Process_Descriptor_Access;
      Success          : out Boolean;
      Use_Ext_Terminal : Boolean := False;
      Console          : Interactive_Consoles.Interactive_Console := null;
      Show_Command     : Boolean := True;
      Directory        : String := "";
      Use_Pipes        : Boolean := True);
   --  Launch given arguments on Server. Returns a valid Process
   --  descriptor and success set to true upon success.
   --  If Use_Ext_Terminal is not null, then the program is executed in a
   --  separate terminal.
   --  If Console is not null, and Show_Command is set, outputs the command
   --  line to the console.
   --  If directory is set, change default dir to Directory before launching
   --  the command. Returns to current dir after spawning.
   --  Use_Pipes tells wether we are using pipes when spawning the process, or
   --  consoles (on Windows).

   ---------------------
   -- Connection Hook --
   ---------------------

   Build_Server_Connected_Hook : constant String :=
                                   "build_server_connected_hook";
   --  No data hook

   --------------------------
   -- Synchronization Hook --
   --------------------------

   Rsync_Action_Hook : constant String := "rsync_action_hook";

   Rsync_Hook_Type : constant String := "rsync_action_hook_t";

   type Rsync_Hooks_Args
     (Tool_Name_Length, Src_Name_Length, Dest_Name_Length, Queue_Id_Length,
      Src_Path_Length, Dest_Path_Length : Natural)
   is new Hooks_Data with record
      Synchronous   : Boolean;
      --  Tells if the synchronisation call shall be performed synchronously
      Print_Output  : Boolean;
      --  Tells if rsync output is displayed on the Messages window.
      Print_Command : Boolean;
      --  Tells if rsync command is displayed on thee Messages window.
      Tool_Name     : String (1 .. Tool_Name_Length);
      --  What hook function shall perform the action
      Src_Name      : String (1 .. Src_Name_Length);
      --  Source server nickname
      Dest_Name     : String (1 .. Dest_Name_Length);
      --  Destination server nickname
      Queue_Id      : String (1 .. Queue_Id_Length);
      --  Queue_Id used to enqueue the sync command
      Src_Path      : String (1 .. Src_Path_Length);
      --  Source path
      Dest_Path     : String (1 .. Dest_Path_Length);
      --  Destination path
   end record;

   function Create_Callback_Data
     (Script    : access GNAT.Scripts.Scripting_Language_Record'Class;
      Hook_Name : String;
      Data      : access Rsync_Hooks_Args)
      return GNAT.Scripts.Callback_Data_Access;
   --  See inherited for documentation

   --------------------------------
   -- Server Config Changed Hook --
   --------------------------------

   Server_Config_Changed_Hook : constant String := "server_config_hook";

   Server_Config_Changed_Hook_Type : constant String :=
                                       "server_config_hook_t";

   type Server_Config_Changed_Hooks_Args
     (Nickname_Length : Natural) is new Hooks_Data with
   record
      Server   : Server_Type;
      --  The server type modified
      Nickname : String (1 .. Nickname_Length);
      --  The new server nickname attached to it
   end record;

   function Create_Callback_Data
     (Script    : access GNAT.Scripts.Scripting_Language_Record'Class;
      Hook_Name : String;
      Data      : access Server_Config_Changed_Hooks_Args)
      return GNAT.Scripts.Callback_Data_Access;
   --  See inherited for documentation

   ------------------------------
   -- Server List Changed Hook --
   ------------------------------

   Server_List_Changed_Hook : constant String := "server_list_hook";
   --  No data hook

   ----------------------------
   --  Servers configuration --
   ----------------------------

   procedure Configure_Server_List
     (Kernel         : GPS.Kernel.Kernel_Handle;
      Default_Server : String := "");
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

   function Get_Filesystem
     (Server : Server_Type) return Filesystem_Record'Class;
   --  Get the filesystem of the specified server

   function Is_Default_Remote_Setting return Boolean;
   --  Tell if the servers assigned are default for the current project

   procedure Set_Default_Remote_Settings;
   --  Set the current remote settings as default for the current project

end GPS.Kernel.Remote;
