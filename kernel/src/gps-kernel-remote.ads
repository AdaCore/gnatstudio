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

with GPS.Kernel.Hooks;     use GPS.Kernel.Hooks;
with GPS.Kernel.Scripts;
with Filesystem;           use Filesystem;
with Interactive_Consoles;

package GPS.Kernel.Remote is

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

   function To_Remote (Path       : String;
                       To         : Server_Type;
                       Unix_Style : Boolean := False) return String;
   --  Translate a local file/directory path to server 'To'
   --  if Unix_Style is set, the translated path will have a unix style.

   function To_Unix_Path (Path             : String;
                          Server           : Server_Type) return String;
   --  Transform a remote path into unix path style.

   procedure Synchronize (Kernel        : Kernel_Handle;
                          From          : Server_Type;
                          To            : Server_Type;
                          Queue_Id      : String);
   --  Forces a file system synchronisation between the two servers.

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
     (Queue_Id_Length, Src_Path_Length, Dest_Path_Length : Natural)
     is new Hooks_Data with record
        Src       : Server_Type;
        Dest      : Server_Type;
        Queue_Id  : String (1 .. Queue_Id_Length);
        Src_Path  : String (1 .. Src_Path_Length);
        Dest_Path : String (1 .. Dest_Path_Length);
     end record;

   function Create_Callback_Data
     (Script    : access GPS.Kernel.Scripts.Scripting_Language_Record'Class;
      Hook_Name : String;
      Data      : access Rsync_Hooks_Args)
      return GPS.Kernel.Scripts.Callback_Data_Access;

   --------------------------------
   -- Server Config Changed Hook --
   --------------------------------

   Server_Config_Changed_Hook : constant String := "srv_config_hooks";

   Server_Config_Changed_Hook_Type : constant String := "srv_config_hook";

   type Server_Config_Changed_Hooks_Args
     (Nickname_Length : Natural)
     is new Hooks_Data with record
        Server   : Server_Type;
        Nickname : String (1 .. Nickname_Length);
     end record;

   function Create_Callback_Data
     (Script    : access GPS.Kernel.Scripts.Scripting_Language_Record'Class;
      Hook_Name : String;
      Data      : access Server_Config_Changed_Hooks_Args)
      return GPS.Kernel.Scripts.Callback_Data_Access;

   ----------------------------
   --  Servers configuration --
   ----------------------------

   type Mirror_Attribute is
     (System_Defined,
      User_Defined,
      Project_Specific);

   procedure Assign
     (Kernel   : Kernel_Handle;
      Server   : Server_Type;
      Nickname : String);
   --  Assigns a Server to a configuration

   function Get_Nickname (Server : Server_Type) return String;
   --  Gets the nickname of a server

   function Get_Network_Name (Server : Server_Type) return String;
   --  Gets the network name of a server

   function Get_Filesystem (Server : Server_Type)
                            return Filesystem_Record'Class;
   --  Get the filesystem of the specified server

   function Is_Local (Server : Server_Type) return Boolean;
   --  Tells if the server is the local server or is remote

   procedure Add_Mirror_Path
     (Nickname    : String;
      GPS_Ref     : String;
      Remote_Path : String;
      Need_Sync   : Boolean := False;
      Attribute   : Mirror_Attribute := Project_Specific);
   --  Nickname of the server whose fs is mirrored
   --  Path in GPS referential
   --  Path in Remote server referential
   --  If Need_Sync is set, a synchronisation between the two path
   --   will be performed when needed. Else, it is supposed that the
   --   two filesystems are shared.

end GPS.Kernel.Remote;
