------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2009-2019, AdaCore                     --
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

--  This package implements all remote configuration interfaces defined in
--  GNATCOLL.Remote and Gexpect. It defines the main repository for
--  handling the remote configuration

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;
with GNAT.Expect;          use GNAT.Expect;
with GNAT.Strings;         use GNAT.Strings;
with GNATCOLL.Remote.Db;   use GNATCOLL.Remote;
with GNATCOLL.VFS;         use GNATCOLL.VFS;
with GNATCOLL.VFS_Types;   use GNATCOLL.VFS_Types;
with GPS.Kernel;           use GPS.Kernel;
with Gexpect.Db;           use Gexpect;
with XML_Utils;

package Remote.Db is

   Invalid_Remote_Configuration : exception;

   type Synchronisation_Type is
     (Never,
      On_Request,
      Always,
      To_Remote,
      To_Local);

   type Mount_Point is record
      Local_Root : Virtual_File;
      Remote_Root : Virtual_File;
      Sync        : Synchronisation_Type;
   end record;
   type Mount_Point_Array is array (Natural range <>) of Mount_Point;

   --  Remote Server definition.

   type Machine_Type is new Gexpect.Machine_Type
     and GNATCOLL.Remote.Server_Record with private;
   type Machine_Access is access all Machine_Type'Class;

   overriding procedure Ref (Machine : in out Machine_Type);
   overriding procedure Unref (Machine : access Machine_Type);

   function New_Machine
     (Kernel   : access Kernel_Handle_Record'Class;
      Nickname : String) return Machine_Type;
   overriding function Nickname
     (Machine : Machine_Type) return String;
   overriding function Network_Name
     (Machine : Machine_Type) return String;
   overriding function Access_Tool
     (Machine : Machine_Type) return String;
   overriding function Shell
     (Machine : Machine_Type) return String;
   overriding function Sync_Tool
     (Machine : Machine_Type) return String;
   overriding function Sync_Tool_Args
     (Machine : Machine_Type) return String_List;
   overriding function Extra_Init_Commands
     (Machine : Machine_Type) return String_List;
   overriding function User_Name
     (Machine : Machine_Type) return String;
   overriding function Max_Nb_Connections
     (Machine : Machine_Type) return Natural;
   overriding function Timeout
     (Machine : Machine_Type) return Natural;
   overriding function Cr_Lf
     (Machine : Machine_Type) return Cr_Lf_Handling;
   overriding function Use_Dbg
     (Machine : Machine_Type) return Boolean;
   overriding procedure Dbg
     (Machine : access Machine_Type;
      Str     : String;
      Mode    : Mode_Type);
   procedure Set_Network_Name
     (Machine      : in out Machine_Type;
      Network_Name : String);
   procedure Set_Access_Tool
     (Machine      : in out Machine_Type;
      Access_Tool  : String);
   procedure Set_Shell
     (Machine : in out Machine_Type;
      Shell   : String);
   procedure Set_Sync_Tool
     (Machine   : in out Machine_Type;
      Sync_Func : String);
   procedure Set_Extra_Init_Commands
     (Machine : in out Machine_Type;
      Cmds    : String_List);
   overriding procedure Set_User_Name
     (Machine   : in out Machine_Type;
      User_Name : String);
   procedure Set_Max_Nb_Connections
     (Machine : in out Machine_Type;
      Nb      : Natural);
   procedure Set_Timeout
     (Machine  : in out Machine_Type;
      MSeconds : Natural);
   procedure Set_Cr_Lf
     (Machine : in out Machine_Type;
      Cr_Lf   : Cr_Lf_Handling);
   procedure Set_Use_Dbg
     (Machine : in out Machine_Type;
      Dbg     : Boolean);

   --  Machine's shell properties
   overriding function Shell_Command
     (Machine : Machine_Type) return String;
   overriding function Shell_Generic_Prompt
     (Machine : Machine_Type) return Pattern_Matcher_Access;
   overriding function Shell_Configured_Prompt
     (Machine : Machine_Type) return Pattern_Matcher_Access;
   overriding function Shell_FS
     (Machine : Machine_Type) return FS_Type;
   overriding function Shell_No_Echo_Cmd
     (Machine : Machine_Type) return String;
   overriding function Shell_Init_Cmds
     (Machine : Machine_Type) return String_List;
   overriding function Shell_Exit_Cmds
     (Machine : Machine_Type) return String_List;
   overriding function Shell_Cd_Cmd
     (Machine : Machine_Type) return String;
   overriding function Shell_Get_Status_Cmd
     (Machine : Machine_Type) return String;
   overriding function Shell_Get_Status_Pattern
     (Machine : Machine_Type) return Pattern_Matcher_Access;

   --  Machine's access tool properties

   overriding function Access_Tool_Command
     (Machine : Machine_Type) return String;
   overriding function Access_Tool_Common_Args
     (Machine : Machine_Type) return String_List;
   overriding function Access_Tool_User_Args
     (Machine : Machine_Type) return String_List;
   overriding function Access_Tool_Send_Interrupt
     (Machine : Machine_Type) return String;
   overriding function Access_Tool_User_Prompt_Ptrn
     (Machine : Machine_Type) return Pattern_Matcher_Access;
   overriding function Access_Tool_Password_Prompt_Ptrn
     (Machine : Machine_Type) return Pattern_Matcher_Access;
   overriding function Access_Tool_Passphrase_Prompt_Ptrn
     (Machine : Machine_Type) return Pattern_Matcher_Access;
   overriding function Access_Tool_Extra_Prompts
     (Machine : Machine_Type) return Extra_Prompt_Array;
   overriding function Access_Tool_Use_Pipes
     (Machine : Machine_Type) return Boolean;

   --  GNAT.Expect.TTY.Remote private datas

   overriding procedure Set_Data
     (Machine : in out Machine_Type;
      Data    : Machine_User_Data_Access);
   overriding function Get_Data
     (Machine : Machine_Type) return Machine_User_Data_Access;

   --  Exec methods used by GNATCOLL.VFS

   overriding procedure Execute_Remotely
     (Server              : access Machine_Type;
      Args                : GNAT.Strings.String_List;
      Status              : out Boolean;
      Execution_Directory : FS_String := "");
   overriding procedure Execute_Remotely
     (Server              : access Machine_Type;
      Args                : GNAT.Strings.String_List;
      Result              : out GNAT.Strings.String_Access;
      Status              : out Boolean;
      Execution_Directory : FS_String := "");
   overriding procedure Spawn_Remotely
     (Server              : access Machine_Type;
      Descriptor          : out GNAT.Expect.Process_Descriptor_Access;
      Args                : GNAT.Strings.String_List);

   --  Remote configuration database

   type Remote_Db_Type is
     new Gexpect.Db.Machine_Db_Interface
     and GNATCOLL.Remote.Db.Remote_Db_Interface with private;
   type Remote_Db_Type_Access is access all Remote_Db_Type'Class;

   function Initialize_Database return Remote_Db_Type_Access;

   procedure Free (DB : in out Remote_Db_Type_Access);

   procedure Read_From_XML
     (Db        : access Remote_Db_Type;
      Kernel    : access Kernel_Handle_Record'Class;
      Node      : XML_Utils.Node_Ptr;
      Is_System : Boolean);

   procedure Save_To_XML
     (Db      : access Remote_Db_Type;
      Node    : XML_Utils.Node_Ptr);

   overriding function Is_Configured
     (Db       : Remote_Db_Type;
      Nickname : String) return Boolean;
   --  Tell if Machine is configured.

   overriding function Get_Servers
     (Db       : Remote_Db_Type) return String_List;
   --  Get the list of all configured machines.
   --  DO NOT FREE THE STRINGS IN THIS LIST ...

   function Get_Shells
     (Db       : Remote_Db_Type) return String_List;
   --  Get the list of all configured shells.

   function Get_Access_Tools
     (Db       : Remote_Db_Type) return String_List;
   --  Get the list of all configured access tools.

   function Get_Sync_Tools
     (Db       : Remote_Db_Type) return String_List;
   --  Get the list of all configured sync tools.
   --  Caller must free the result value.

   overriding function Get_Server
     (Db       : Remote_Db_Type;
      Nickname : String) return Gexpect.Machine_Access;
   --  Return the Gexpect.Machine_Access

   overriding function Get_Server
     (Config   : Remote_Db_Type;
      Nickname : String) return GNATCOLL.Remote.Server_Access;
   --  Return the GNATCOLL.Remote.Server_Access

   function Get_Machine
     (Db       : Remote_Db_Type;
      Nickname : String) return Machine_Access;
   --  Get the Machine according to its nickname.

   overriding function Nb_Mount_Points
     (Config   : Remote_Db_Type;
      Nickname : String) return Natural;
   --  Get the number of mount points defined for the server

   overriding function Get_Mount_Point_Local_Root
     (Config   : Remote_Db_Type;
      Nickname : String;
      Index    : Natural) return FS_String;
   --  Get the local mount point

   overriding function Get_Mount_Point_Host_Root
     (Config   : Remote_Db_Type;
      Nickname : String;
      Index    : Natural) return FS_String;

   function Get_Mount_Points
     (Config   : Remote_Db_Type;
      Nickname : String) return Mount_Point_Array;

   procedure Set_Mount_Points
     (Config       : access Remote_Db_Type;
      Nickname     : String;
      Mount_Points : Mount_Point_Array);
   function Is_Sys_Default
     (Config   : Remote_Db_Type;
      Nickname : String) return Boolean;
   function Has_Sys_Default
     (Config   : Remote_Db_Type;
      Nickname : String) return Boolean;
   function Get_Sys_Default
     (Config   : Remote_Db_Type;
      Nickname : String) return Machine_Access;
   procedure Add_Or_Replace
     (Config    : access Remote_Db_Type;
      Machine   : Machine_Access;
      Is_System : Boolean := False);
   procedure Remove
     (Config   : access Remote_Db_Type;
      Nickname : String);

private

   type Shell_Record is record
      Name             : String_Access          := null;
      Filesystem       : FS_Type                := FS_Unknown;
      Start_Cmd        : String_Access          := null;
      No_Echo_Cmd      : String_Access          := null;
      Init_Cmds        : String_List_Access     := null;
      Exit_Cmds        : String_List_Access     := null;
      Cd_Cmd           : String_Access          := null;
      Get_Status_Cmd   : String_Access          := null;
      Get_Status_Ptrn  : Pattern_Matcher_Access := null;
      Generic_Prompt   : Pattern_Matcher_Access := null;
      Prompt           : Pattern_Matcher_Access := null;
   end record;
   type Shell_Access is access all Shell_Record;

   type Extra_Prompt_Array_Access is access all Extra_Prompt_Array;

   procedure Free (Prompt : in out Extra_Prompt);
   procedure Free (Prompts : in out Extra_Prompt_Array_Access);
   --  Free memory used by Prompt

   type Access_Tool_Record is record
      Name                   : String_Access             := null;
      Start_Cmd              : String_Access             := null;
      Start_Cmd_Common_Args  : String_List_Access        := null;
      Start_Cmd_User_Args    : String_List_Access        := null;
      Send_Interrupt         : String_Access             := null;
      User_Prompt_Ptrn       : Pattern_Matcher_Access    := null;
      Password_Prompt_Ptrn   : Pattern_Matcher_Access    := null;
      Passphrase_Prompt_Ptrn : Pattern_Matcher_Access    := null;
      Extra_Prompts          : Extra_Prompt_Array_Access := null;
      Use_Pipes              : Boolean                   := False;
      Max_Password_Prompt    : Natural                   := 3;
   end record;
   type Access_Tool_Access is access all Access_Tool_Record;

   type Sync_Tool_Record is record
      Name  : String_Access      := null;
      Args  : String_List_Access := null;
   end record;
   type Sync_Tool_Access is access all Sync_Tool_Record;

   type Machine_Type is new Gexpect.Machine_Type
     and GNATCOLL.Remote.Server_Record
   with record
      Kernel              : access Kernel_Handle_Record'Class;
      --  The GPS Kernel
      Nickname            : String_Access;
      --  Identifier of the machine
      Network_Name        : String_Access;
      --  Used to access the Machine using the network
      Access_Tool_Name    : String_Access;
      Access_Tool         : Access_Tool_Access := null;
      --  Tool used to remotely access the Machine
      Shell_Name          : String_Access;
      Shell               : Shell_Access := null;
      --  Shell used on the remote Machine
      Extra_Init_Commands : String_List_Access := null;
      --  User specific init commands
      User_Name           : String_Access;
      --  User name used for connection
      Max_Nb_Connections  : Natural := 3;
      --  Maximum number of simultaneous connections on the machine
      Timeout             : Natural := 5000;
      --  Timeout value used when connecting to the machine (in ms)
      Cr_Lf               : Cr_Lf_Handling := Auto;
      --  Whether we should send LF, CR/LF or determine it automatically
      Use_Dbg             : Boolean := False;
      --  Connection debug console.
      Sync_Tool_Name     : String_Access := null;
      Sync_Tool          : Sync_Tool_Access;
      --  The remote sync tool used for file synchronisation
      User_Data           : Machine_User_Data_Access;
      --  User Data
      Ref_Counter         : Natural := 1;
   end record;

   package Shell_Db is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => Shell_Access,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => Standard."=");
   package Access_Tools_Db is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => Access_Tool_Access,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => Standard."=");
   package Machine_Db is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => Machine_Access,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => Standard."=");
   package Mount_Points_Db is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => Mount_Point_Array,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => Standard."=");
   package Sync_Tools_Db is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => Sync_Tool_Access,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => Standard."=");

   type Remote_Db_Type is
     new Gexpect.Db.Machine_Db_Interface
     and GNATCOLL.Remote.Db.Remote_Db_Interface
   with record
      Shells           : Shell_Db.Map;
      Access_Tools     : Access_Tools_Db.Map;
      Sync_Tools       : Sync_Tools_Db.Map;
      Machines         : Machine_Db.Map;
      Sys_Machines     : Machine_Db.Map;
      Mount_Points     : Mount_Points_Db.Map;
      Resolved         : Boolean := True;
   end record;
   type Remote_Db_Access is access all Remote_Db_Type;

end Remote.Db;
