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

with GNAT.Expect;
pragma Warnings (Off);
with GNAT.Expect.TTY.Remote; use GNAT.Expect.TTY.Remote;

pragma Warnings (On);

with GNATCOLL.Scripts;
with GPS.Kernel.Hooks;       use GPS.Kernel.Hooks;
with Interactive_Consoles;
with Remote;                 use Remote;
with GNATCOLL.VFS;

with GNATCOLL.Arg_Lists; use GNATCOLL.Arg_Lists;

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
      Force          : Boolean;
      Queue_Id       : String  := "";
      File           : GNATCOLL.VFS.Virtual_File := GNATCOLL.VFS.No_File);
   --  Perform a file system synchronisation between From and To.
   --  If Blocking is set, the call is synchronous. Else an asynchronous
   --  command is used.
   --  If Print_Command is set, then asynchronous command will print the rsync
   --  command line on the Messages console.
   --  If Print_Output is set, then asynchronous command will print rsync's
   --  output on the Messages console.
   --  If Force is set, then mirror paths marked as 'sync once' will
   --  also be rsynced
   --  If Blocking is not set and queue_id is not an empty string, then the
   --  specified queue id will be used for the command.
   --  If File is an actual file, then only this file will be synchronized

   procedure Synchronize
     (Kernel         : Kernel_Handle;
      From           : String;
      To             : String;
      Blocking       : Boolean;
      Print_Command  : Boolean;
      Print_Output   : Boolean;
      Force          : Boolean;
      Queue_Id       : String  := "";
      File           : GNATCOLL.VFS.Virtual_File := GNATCOLL.VFS.No_File);
   --  Same as above, with From and To servers identified by their nickname

   procedure Spawn
     (Kernel            : Kernel_Handle;
      Arguments         : Arg_List;
      Server            : Server_Type;
      Pd                : out GNAT.Expect.Process_Descriptor_Access;
      Success           : out Boolean;
      Use_Ext_Terminal  : Boolean := False;
      Console           : Interactive_Consoles.Interactive_Console := null;
      Show_Command      : Boolean := True;
      Directory         : GNATCOLL.VFS.Virtual_File := GNATCOLL.VFS.No_File;
      Use_Pipes         : Boolean := True);
   --  Launch given arguments on Server. Returns a valid Process
   --  descriptor and success set to true upon success.
   --  See remote.ads for explanations on Server values, in particular
   --  the Tools_Server and its relationship with the dual compilation module.
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

   Build_Server_Connected_Hook : constant Hook_Name :=
                                   To_Hook_Name
       ("build_server_connected_hook");
   --  No data hook

   --------------------------
   -- Synchronization Hook --
   --------------------------

   Rsync_Action_Hook : constant Hook_Name :=
                         To_Hook_Name ("rsync_action_hook");

   Rsync_Finished_Hook : constant Hook_Name := To_Hook_Name ("rsync_finished");

   Rsync_Hook_Type : constant Hook_Type := "rsync_action_hook_t";

   type Rsync_Hooks_Args
     (Tool_Name_Length, Host_Name_Length, Queue_Id_Length : Natural)
   is new Hooks_Data with record
      Synchronous   : Boolean;
      --  Tells if the synchronisation call shall be performed synchronously
      Force         : Boolean;
      --  Tells if we want to force the synchronisation.
      To_Remote     : Boolean;
      --  Tells if sync so from host to remote (Set to true) or to host from
      --  remote
      Print_Output  : Boolean;
      --  Tells if rsync output is displayed on the Messages window
      Print_Command : Boolean;
      --  Tells if rsync command is displayed on thee Messages window
      Tool_Name     : String (1 .. Tool_Name_Length);
      --  Source server nickname
      Host_Name     : String (1 .. Host_Name_Length);
      --  The host we want to synchronise with
      Queue_Id      : String (1 .. Queue_Id_Length);
      --  Queue_Id used to enqueue the sync command
      File          : GNATCOLL.VFS.Virtual_File;
   end record;

   overriding function Create_Callback_Data
     (Script : access GNATCOLL.Scripts.Scripting_Language_Record'Class;
      Hook   : Hook_Name;
      Data   : access Rsync_Hooks_Args)
      return GNATCOLL.Scripts.Callback_Data_Access;
   --  See inherited for documentation

   --------------------------------
   -- Server Config Changed Hook --
   --------------------------------

   Server_Config_Changed_Hook : constant Hook_Name :=
                                  To_Hook_Name ("server_config_hook");

   Server_Config_Changed_Hook_Type : constant Hook_Type :=
                                       "server_config_hook_t";

   type Server_Config_Changed_Hooks_Args
     (Nickname_Length : Natural) is new Hooks_Data with
   record
      Server   : Distant_Server_Type;
      --  The server type modified
      Nickname : String (1 .. Nickname_Length);
      --  The new server nickname attached to it
   end record;

   overriding function Create_Callback_Data
     (Script : access GNATCOLL.Scripts.Scripting_Language_Record'Class;
      Hook   : Hook_Name;
      Data   : access Server_Config_Changed_Hooks_Args)
      return GNATCOLL.Scripts.Callback_Data_Access;
   --  See inherited for documentation

   ----------------------------
   --  Servers configuration --
   ----------------------------

   procedure Assign
     (Kernel     : Kernel_Handle;
      Server     : Distant_Server_Type;
      Nickname   : String;
      Prj_File   : GNATCOLL.VFS.Virtual_File := GNATCOLL.VFS.No_File;
      Reload_Prj : Boolean := False);
   --  Assigns a Server to a configuration
   --  Prj_File allows to select to which project file this configuration is
   --   assigned to. If No_File, it is assigned to the current project.
   --  Reload_Prj, if set, recomputes the view if the build_server changed.

   function Is_Default_Remote_Setting return Boolean;
   --  Tell if the servers assigned are default for the current project

   procedure Set_Default_Remote_Settings;
   --  Set the current remote settings as default for the current project

end GPS.Kernel.Remote;
