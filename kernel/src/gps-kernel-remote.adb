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

with Ada.Exceptions;             use Ada, Ada.Exceptions;
with Ada.Unchecked_Deallocation;

with GNAT.Expect;                use GNAT.Expect;
pragma Warnings (Off);
with GNAT.Expect.TTY;            use GNAT.Expect.TTY;
pragma Warnings (On);
with GNAT.OS_Lib;                use GNAT.OS_Lib;

with GNATCOLL.Scripts;           use GNATCOLL.Scripts;
with GNATCOLL.Traces;            use GNATCOLL.Traces;
with GNATCOLL.Projects;          use GNATCOLL.Projects;
with GNATCOLL.VFS;               use GNATCOLL.VFS;

with Glib;                       use Glib;
with Glib.Main;                  use Glib.Main;
with Glib.Object;                use Glib.Object;
with XML_Utils;                  use XML_Utils;

with GPS.Intl;                   use GPS.Intl;
with GPS.Properties;             use GPS.Properties;
with GPS.Kernel.Modules;         use GPS.Kernel.Modules;
with GPS.Kernel.Preferences;     use GPS.Kernel.Preferences;
with GPS.Kernel.Project;         use GPS.Kernel.Project;
with GPS.Kernel.Properties;      use GPS.Kernel.Properties;
with GPS.Kernel.Scripts;         use GPS.Kernel.Scripts;
with GPS.Kernel.Standard_Hooks;  use GPS.Kernel.Standard_Hooks;

with Interactive_Consoles;       use Interactive_Consoles;
with Toolchains_Old;             use Toolchains_Old;
with UTF8_Utils;                 use UTF8_Utils;

with Gexpect.Db;                 use Gexpect, Gexpect.Db;

package body GPS.Kernel.Remote is

   ------------
   -- Module --
   ------------

   Me : constant Trace_Handle := Create ("GPS.Kernel.Remote");

   type Remote_Module_Record is new Module_ID_Record with record
      Kernel            : Kernel_Handle;
      Project_Reloading : Boolean := False;
   end record;
   type Remote_Module_ID is access all Remote_Module_Record'Class;

   Remote_Module : Remote_Module_ID;

   Id : Natural := 0;
   function Get_New_Queue_Id return String;
   --  Returns a new unique queue id

   -----------------------
   -- Server Assignment --
   -----------------------

   type Server_Config is record
      Is_Local : Boolean := True;
      --  Is_Local Tells if the server is the local machine or not
      Nickname : GNAT.OS_Lib.String_Access;
      --  Identifier of the server
   end record;
   type Servers_Config is array (Distant_Server_Type) of Server_Config;

   type Servers_Property is new Property_Record with record
      Servers : Servers_Config;
   end record;

   overriding procedure Save
     (Property : access Servers_Property;
      Node     : in out XML_Utils.Node_Ptr);
   overriding procedure Load
     (Property : in out Servers_Property; From : XML_Utils.Node_Ptr);
   overriding procedure Destroy
     (Property : in out Servers_Property);

   ----------------------------
   -- Project load utilities --
   ----------------------------

   type Reload_Callback_Data is record
      File   : GNATCOLL.VFS.Virtual_File;
      Kernel : Kernel_Handle;
   end record;

   package Reload_Timeout is new Glib.Main.Generic_Sources
     (Reload_Callback_Data);

   function Reload_Prj_Cb (Data : Reload_Callback_Data) return Boolean;
   --  Callback used to reload the project when build_server changed

   ---------------------
   -- Utility methods --
   ---------------------

   procedure Simple_Free is new Ada.Unchecked_Deallocation
     (Object => Argument_List, Name => Argument_List_Access);
   --  Frees the pointer without freeing internal strings

   ---------------
   -- Callbacks --
   ---------------

   procedure On_Project_Changing
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class);
   --  Called when project is about to change

   function From_Callback_Data_Sync_Hook
     (Data : Callback_Data'Class) return Hooks_Data'Class;
   --  retrieve hook data from callback data

   function From_Callback_Data_Server_Config_Changed_Hook
     (Data : Callback_Data'Class) return Hooks_Data'Class;
   --  retrieve hook data from callback data

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy (Property : in out Servers_Property) is
   begin
      for C in Property.Servers'Range loop
         Free (Property.Servers (C).Nickname);
      end loop;
   end Destroy;

   ----------------------------------
   -- From_Callback_Data_Sync_Hook --
   ----------------------------------

   function From_Callback_Data_Sync_Hook
     (Data : Callback_Data'Class) return Hooks_Data'Class is
   begin
      declare
         Tool_Name     : constant String       := Nth_Arg (Data, 2);
         Host_Name     : constant String       := Nth_Arg (Data, 3);
         To_Remote     : constant Boolean      := Nth_Arg (Data, 4);
         Queue_Id      : constant String       := Nth_Arg (Data, 5);
         Synchronous   : constant Boolean      := Nth_Arg (Data, 6);
         Print_Output  : constant Boolean      := Nth_Arg (Data, 7);
         Print_Command : constant Boolean      := Nth_Arg (Data, 8);
         Force         : constant Boolean      := Nth_Arg (Data, 9);
         File          : constant Virtual_File := Nth_Arg (Data, 10);

      begin
         return Rsync_Hooks_Args'
           (Hooks_Data with
            Tool_Name_Length => Tool_Name'Length,
            Host_Name_Length => Host_Name'Length,
            Queue_Id_Length  => Queue_Id'Length,
            Tool_Name        => Tool_Name,
            Host_Name        => Host_Name,
            To_Remote        => To_Remote,
            Queue_Id         => Queue_Id,
            Synchronous      => Synchronous,
            Print_Output     => Print_Output,
            Print_Command    => Print_Command,
            Force            => Force,
            File             => File);
      end;
   end From_Callback_Data_Sync_Hook;

   --------------------------
   -- Create_Callback_Data --
   --------------------------

   overriding function Create_Callback_Data
     (Script : access Scripting_Language_Record'Class;
      Hook   : Hook_Name;
      Data   : access Rsync_Hooks_Args)
      return Callback_Data_Access
   is
      D : constant Callback_Data_Access :=
            new Callback_Data'Class'(Create (Script, 11));
   begin
      Set_Nth_Arg (D.all,  1, To_String (Hook));
      Set_Nth_Arg (D.all,  2, Data.Tool_Name);
      Set_Nth_Arg (D.all,  3, Data.Host_Name);
      Set_Nth_Arg (D.all,  4, Data.To_Remote);
      Set_Nth_Arg (D.all,  5, Data.Queue_Id);
      Set_Nth_Arg (D.all,  6, Data.Synchronous);
      Set_Nth_Arg (D.all,  7, Data.Print_Output);
      Set_Nth_Arg (D.all,  8, Data.Print_Command);
      Set_Nth_Arg (D.all,  9, Data.Force);
      Set_Nth_Arg (D.all, 10, Data.File);
      return D;
   end Create_Callback_Data;

   ----------------------------------------------
   -- From_Callback_Server_Config_Changed_Hook --
   ----------------------------------------------

   function From_Callback_Data_Server_Config_Changed_Hook
     (Data : Callback_Data'Class) return Hooks_Data'Class
   is
      Server : Distant_Server_Type;
   begin
      Server  := Distant_Server_Type'Value (String'(Nth_Arg (Data, 2)));

      declare
         Nickname  : constant String := Nth_Arg (Data, 3);
      begin
         return Server_Config_Changed_Hooks_Args'
           (Hooks_Data with
            Nickname_Length => Nickname'Length,
            Server          => Server,
            Nickname        => Nickname);
      end;
   end From_Callback_Data_Server_Config_Changed_Hook;

   ---------------------------
   -- Create_Callbackc_Data --
   ---------------------------

   overriding function Create_Callback_Data
     (Script : access Scripting_Language_Record'Class;
      Hook   : Hook_Name;
      Data   : access Server_Config_Changed_Hooks_Args)
      return Callback_Data_Access
   is
      D : constant Callback_Data_Access :=
            new Callback_Data'Class'(Create (Script, 3));
   begin
      Set_Nth_Arg (D.all, 1, To_String (Hook));
      Set_Nth_Arg (D.all, 2, Distant_Server_Type'Image (Data.Server));
      Set_Nth_Arg (D.all, 3, Data.Nickname);
      return D;
   end Create_Callback_Data;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class) is
   begin
      --  Register synchronisation hook
      Register_Hook_Data_Type
        (Kernel, Rsync_Hook_Type,
         Args_Creator => From_Callback_Data_Sync_Hook'Access);
      Register_Hook_Return_Boolean
        (Kernel, Rsync_Action_Hook, Rsync_Hook_Type);

      Register_Hook_No_Args (Kernel, Rsync_Finished_Hook);

      --  Register server config changed hook
      Register_Hook_Data_Type
        (Kernel, Server_Config_Changed_Hook_Type,
         Args_Creator => From_Callback_Data_Server_Config_Changed_Hook'Access);
      Register_Hook_No_Return
        (Kernel, Server_Config_Changed_Hook, Server_Config_Changed_Hook_Type);

      --  Register build server connected hook
      Register_Hook_No_Args
        (Kernel, Build_Server_Connected_Hook);

      --  Register the module
      Remote_Module := new Remote_Module_Record;
      Remote_Module.Kernel := Kernel_Handle (Kernel);
      Remote_Module.Project_Reloading := False;
      Register_Module (Remote_Module, Kernel, "remote");

      --  Connect to project_changing hook
      Add_Hook
        (Kernel, Project_Changing_Hook,
         Wrapper (On_Project_Changing'Access), "gps.kernel.remote");
   end Register_Module;

   ----------
   -- Save --
   ----------

   overriding procedure Save
     (Property : access Servers_Property;
      Node     : in out XML_Utils.Node_Ptr)
   is
      Srv : Node_Ptr;
   begin
      Trace (Me, "Saving remote property");

      for J in Property.Servers'Range loop
         Srv := new XML_Utils.Node;
         Srv.Tag := new String'(Server_Type'Image (J));
         Srv.Value := new String'(Property.Servers (J).Nickname.all);
         Add_Child (Node, Srv);
      end loop;
   end Save;

   ----------
   -- Load --
   ----------

   overriding procedure Load
     (Property : in out Servers_Property; From : XML_Utils.Node_Ptr)
   is
      Srv : Node_Ptr;
   begin
      Trace (Me, "Loading remote property");

      if From.Child /= null then
         for J in Property.Servers'Range loop
            Srv := Find_Tag (From.Child, Server_Type'Image (J));

            Free (Property.Servers (J).Nickname);

            if Srv /= null
              and then Gexpect.Db.Is_Configured (Srv.Value.all)
              and then Srv.Value.all /= ""
            then
               Property.Servers (J) :=
                 (Is_Local => False,
                  Nickname => new String'(Srv.Value.all));
            else
               Property.Servers (J) :=
                 (Is_Local => True, Nickname => new String'(Local_Nickname));
            end if;
         end loop;

      else
         for J in Property.Servers'Range loop
            Free (Property.Servers (J).Nickname);
            Property.Servers (J) :=
              (Is_Local => True,
               Nickname => new String'(Local_Nickname));
         end loop;
      end if;
   end Load;

   ------------------------
   -- On_Project_Changed --
   ------------------------

   procedure On_Project_Changing
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class)
   is
      D          : constant File_Hooks_Args := File_Hooks_Args (Data.all);
      Property   : Servers_Property;
      Prop       : Property_Access;
      Success    : Boolean;
      Local_File : GNATCOLL.VFS.Virtual_File;

   begin
      --  This module reloaded the project: remote config is OK
      if Remote_Module.Project_Reloading then
         return;
      end if;

      --  Get local file equivalence for project

      if not Is_Local (D.File) then
         Local_File := To_Local (D.File);
      else
         Local_File := D.File;
      end if;

      --  Get servers associated with this file

      Trace (Me, "Loading servers_config property for file " &
             Local_File.Display_Full_Name);
      Get_Property
        (Property, Local_File,
         Name => "servers_config", Found => Success);

      --  If no previous property exist, create it
      if not Success then
         Trace (Me, "Property servers_config does not exist. Create it");

         if not Is_Local (D.File) then
            for J in Property.Servers'Range loop
               Property.Servers (J) :=
                 (Is_Local => False,
                  Nickname => new String'(Get_Host (D.File)));
            end loop;

         else
            for J in Property.Servers'Range loop
               Property.Servers (J) :=
                 (Is_Local => True,
                  Nickname => new String'(Local_Nickname));
            end loop;
         end if;

         --  Set the property for loaded project
         Prop := new Servers_Property'(Property);
         Set_Property (Kernel,
                       Local_File, "servers_config", Prop,
                       Persistent => True);
      end if;

      --  Assign servers following property values

      for J in Property.Servers'Range loop
         --  If current server is not local, and we're assigning the local
         --  server.
         if not Is_Local (J)
           and then Property.Servers (J).Nickname.all = Local_Nickname
         then
            declare
               Hook_Data : aliased Server_Config_Changed_Hooks_Args :=
                 (Hooks_Data with
                  Nickname_Length => Local_Nickname'Length,
                  Server          => J,
                  Nickname        => Local_Nickname);
            begin
               Assign (J, Local_Nickname);
               Run_Hook (Kernel, Server_Config_Changed_Hook,
                         Hook_Data'Unchecked_Access);
            end;

         elsif Property.Servers (J).Nickname.all /= Get_Nickname (J) then
            declare
               Nickname  : constant String :=
                             Property.Servers (J).Nickname.all;
               Hook_Data : aliased Server_Config_Changed_Hooks_Args :=
                             (Hooks_Data with
                              Nickname_Length => Nickname'Length,
                              Server          => J,
                              Nickname        => Nickname);

            begin
               Assign (J, Nickname);
               Run_Hook (Kernel, Server_Config_Changed_Hook,
                         Hook_Data'Unchecked_Access);
            end;
         end if;
      end loop;

      --  If Project is loaded from a distant host, force build_server as
      --  distant host.

      if not Is_Local (D.File)
        and then Get_Host (D.File) /= Get_Nickname (Build_Server)
      then
         Trace (Me, "Assign build server: project loaded from remote host");
         Assign (Kernel_Handle (Kernel),
                 Build_Server,
                 Get_Host (D.File),
                 Local_File,
                 Reload_Prj => False);
      end if;

      --  If project is loaded from distant host then synchronize all dirs to
      --  local machine

      if not Is_Local (D.File) then
         Trace (Me, "Start synchronization of build_server");
         Synchronize
           (Kernel_Handle (Kernel), Build_Server, GPS_Server,
            Blocking      => True,
            Print_Command => False,
            Print_Output  => False,
            Force         => True);
      end if;

   exception
      when E : others => Trace (Me, E);
   end On_Project_Changing;

   -------------------
   -- Reload_Prj_Cb --
   -------------------

   function Reload_Prj_Cb (Data : Reload_Callback_Data) return Boolean is
   begin
      Trace (Me, "Reloading the project");
      Remote_Module.Project_Reloading := True;
      Load_Project (Data.Kernel, Get_Project (Data.Kernel).Project_Path);
      Remote_Module.Project_Reloading := False;
      return False;

   exception
      when E : others => Trace (Me, E);
         return False;
   end Reload_Prj_Cb;

   -------------------------------
   -- Is_Default_Remote_Setting --
   -------------------------------

   function Is_Default_Remote_Setting return Boolean is
      The_File : GNATCOLL.VFS.Virtual_File;
      Property : Servers_Property;
      Found    : Boolean;

   begin
      if Get_Registry (Remote_Module.Kernel).Tree.Status /= From_File then
         return True;
      end if;

      The_File := Get_Project (Remote_Module.Kernel).Project_Path;
      Get_Property (Property, The_File, "servers_config", Found);

      if not Found then
         --  Check that all servers are local
         for J in Distant_Server_Type'Range loop
            if not Is_Local (J) then
               Trace (Me, "server " & Server_Type'Image (J) & " not local");
               return False;
            end if;
         end loop;

      else
         for J in Distant_Server_Type'Range loop
            if  Property.Servers (J).Nickname.all /= Get_Printable_Nickname (J)
              and then Property.Servers (J).Nickname.all /= Get_Nickname (J)
            then
               Trace
                 (Me, "server " & Server_Type'Image (J) &
                  " is changed from property");
               return False;
            end if;
         end loop;
      end if;

      return True;
   end Is_Default_Remote_Setting;

   ---------------------------------
   -- Set_Default_Remote_Settings --
   ---------------------------------

   procedure Set_Default_Remote_Settings is
      The_File : GNATCOLL.VFS.Virtual_File;
      Property : Servers_Property;
      Prop     : Property_Access;
      Found    : Boolean;
      Set_Prop : Boolean;

   begin
      if Get_Registry (Remote_Module.Kernel).Tree.Status /= From_File then
         --  ??? do we want to be able to set a default config for the default
         --  project ?
         return;
      end if;

      The_File := Get_Project (Remote_Module.Kernel).Project_Path;
      Get_Property (Property, The_File, "servers_config", Found);

      if Found then
         Remove_Property (Remote_Module.Kernel, The_File, "servers_config");
      end if;

      Set_Prop := False;
      for J in Distant_Server_Type'Range loop
         if not Is_Local (J) then
            Set_Prop := True;
            exit;
         end if;
      end loop;

      if not Set_Prop then
         --  No need to save: default setting is 'all servers set to local'
         return;
      end if;

      for J in Property.Servers'Range loop
         Property.Servers (J) :=
           (Is_Local => Is_Local (J),
            Nickname => new String'(Get_Nickname (J)));
      end loop;

      Prop := new Servers_Property'(Property);
      Set_Property
        (Remote_Module.Kernel,
         The_File, "servers_config", Prop, Persistent => True);
   end Set_Default_Remote_Settings;

   ------------
   -- Assign --
   ------------

   procedure Assign
     (Kernel     : Kernel_Handle;
      Server     : Distant_Server_Type;
      Nickname   : String;
      Prj_File   : GNATCOLL.VFS.Virtual_File := GNATCOLL.VFS.No_File;
      Reload_Prj : Boolean := False)
   is
      Data       : aliased Server_Config_Changed_Hooks_Args :=
                     (Hooks_Data with
                      Nickname_Length => Nickname'Length,
                      Server          => Server,
                      Nickname        => Nickname);
      Timeout    : constant Guint := 50;
      Id         : G_Source_Id;
      Load_Data  : Reload_Callback_Data;
      Old_Server : constant String := Get_Nickname (Server);
      pragma Unreferenced (Id);

   begin
      if Old_Server = Nickname
        or else Get_Printable_Nickname (Server) = Nickname
      then
         return;
      end if;

      if not Is_Configured (Nickname) then
         Insert
           (Kernel,
            -"Error: The Server " & Nickname & (-" is not configured, and " &
              "cannot be used as remote server for ") & Server'Img,
            Mode => Error);
         return;
      end if;

      Assign (Server, Nickname);

      --  Reload project if Build_Server has been assigned

      if Server = Build_Server and then Reload_Prj then
         Load_Data.Kernel := Kernel;

         if Prj_File = GNATCOLL.VFS.No_File then
            Load_Data.File := To_Remote
              (Get_Project (Kernel).Project_Path,
               Get_Nickname (Build_Server));
         else
            Load_Data.File := To_Remote
              (Prj_File,
               Get_Nickname (Build_Server));
         end if;

         if Get_Host (Load_Data.File) /= Get_Nickname (Build_Server) then
            Insert
              (Kernel,
               -"Error: the project " &
               Display_Full_Name (Load_Data.File) &
               (-" has no path equivalence on remote machine ") &
               Get_Nickname (Build_Server) & ASCII.LF &
               (-("Verify that the 'Path translation' setting is configured " &
               ("in your Server setting dialog"))));
         else
            Trace (Me, "Asking project reload");
            Id := Reload_Timeout.Timeout_Add
              (Timeout, Reload_Prj_Cb'Access, Load_Data);
         end if;
      end if;

      if Active (Me) then
         Trace (Me, "run server_changed hook for " &
                Server_Type'Image (Server) & " => " & Data.Nickname);
      end if;

      Run_Hook (Kernel, Server_Config_Changed_Hook, Data'Unchecked_Access);
   end Assign;

   ----------------------
   -- Get_New_Queue_Id --
   ----------------------

   function Get_New_Queue_Id return String is
      Str_Id : constant String := Natural'Image (Id);
   begin
      Id := Id + 1;
      return "gps-kernel-remote-sync" & Str_Id;
   end Get_New_Queue_Id;

   -----------------
   -- Synchronize --
   -----------------

   procedure Synchronize
     (Kernel        : Kernel_Handle;
      From          : String;
      To            : String;
      Blocking      : Boolean;
      Print_Command : Boolean;
      Print_Output  : Boolean;
      Force         : Boolean;
      Queue_Id      : String  := "";
      File          : Virtual_File := No_File)
   is
      Machine        : Machine_Access;
      Remote_Is_Dest : Boolean;

      function Get_Queue_Id return String;
      --  Get a new queue id if needed

      ------------------
      -- Get_Queue_Id --
      ------------------

      function Get_Queue_Id return String is
      begin
         if Queue_Id /= "" then
            return Queue_Id;
         elsif not Blocking then
            return Get_New_Queue_Id;
         else
            return "";
         end if;
      end Get_Queue_Id;

      The_Queue_Id : constant String := Get_Queue_Id;
   begin
      if From = Local_Nickname then
         if To = Local_Nickname then
            --  Both are local ...
            return;
         end if;

         if not Gexpect.Db.Is_Configured (To) then
            Kernel.Insert
              (-"Sync files failure: " &
               To & (-" is not correctly configured"),
               Mode => Error);
            return;
         end if;

         Remote_Is_Dest := True;
         Machine := Get_Server (To);

      elsif To = Local_Nickname then
         if not Gexpect.Db.Is_Configured (From) then
            Kernel.Insert
              (-"Sync files failure: " &
               From & (-" is not correctly configured"),
               Mode => Error);
            return;
         end if;

         Remote_Is_Dest := False;
         Machine := Get_Server (From);

      else
         Trace (Me, "ERROR: cannot synchronize two remote servers: " &
                From & " - " & To);
         return;
      end if;

      if not Remote_Is_Dest then
         Trace (Me, "Synchronizing paths from " & From);
      else
         Trace (Me, "Synchronizing paths to " & To);
      end if;

      declare
         Data : aliased Rsync_Hooks_Args :=
                  (Hooks_Data with
                   Tool_Name_Length => Machine.Rsync_Func'Length,
                   Host_Name_Length => Machine.Nickname'Length,
                   Queue_Id_Length  => The_Queue_Id'Length,
                   Tool_Name        => Machine.Rsync_Func,
                   Host_Name        => Machine.Nickname,
                   To_Remote        => Remote_Is_Dest,
                   Queue_Id         => The_Queue_Id,
                   Synchronous      => Blocking,
                   Print_Output     => Print_Output,
                   Print_Command    => Print_Command,
                   Force            => Force,
                   File             => File);

      begin
         Trace
           (Me, "run sync hook for " & Data.Host_Name);

         if not Run_Hook_Until_Success
           (Kernel, Rsync_Action_Hook, Data'Unchecked_Access)
         then
            Kernel.Insert
              (Machine.Rsync_Func & (-" failure: ") &
               (-"Please verify your network configuration"),
               Mode => Error);

            Trace (Me, "No remote sync was registered or errors during" &
                   " calls");
            return;
         end if;

      exception
         when E : others => Trace (Me, E);
      end;
   end Synchronize;

   -----------------
   -- Synchronize --
   -----------------

   procedure Synchronize
     (Kernel        : Kernel_Handle;
      From          : Server_Type;
      To            : Server_Type;
      Blocking      : Boolean;
      Print_Command : Boolean;
      Print_Output  : Boolean;
      Force         : Boolean;
      Queue_Id      : String  := "";
      File          : Virtual_File := No_File) is
   begin
      Synchronize
        (Kernel        => Kernel,
         From          => Get_Nickname (From),
         To            => Get_Nickname (To),
         Blocking      => Blocking,
         Print_Command => Print_Command,
         Print_Output  => Print_Output,
         Force         => Force,
         Queue_Id      => Queue_Id,
         File          => File);
   end Synchronize;

   -----------
   -- Spawn --
   -----------

   procedure Spawn
     (Kernel           : Kernel_Handle;
      Arguments        : Arg_List;
      Server           : Server_Type;
      Pd               : out GNAT.Expect.Process_Descriptor_Access;
      Success          : out Boolean;
      Use_Ext_Terminal : Boolean := False;
      Console          : Interactive_Consoles.Interactive_Console := null;
      Show_Command     : Boolean := True;
      Directory        : Virtual_File := No_File;
      Use_Pipes        : Boolean := True)
   is
      Exec    : GNAT.OS_Lib.String_Access;
      Old_Dir : Virtual_File;
      Args    : Argument_List_Access;

      function Check_Exec
        (Exec : Filesystem_String) return Filesystem_String;
      --  Check that executable is on the path, and return the full path if
      --  found, return null otherwise.

      procedure On_New_Connection (Server_Name : String);
      --  Executed when a new connection is performed

      ----------------
      -- Check_Exec --
      ----------------

      function Check_Exec
        (Exec : Filesystem_String) return Filesystem_String
      is
         Full_Exec : Virtual_File;
      begin
         if Server = Build_Server then
            Full_Exec := Locate_Compiler_Executable (Exec);
         else
            Full_Exec := Locate_Tool_Executable (Exec);
         end if;

         if Full_Exec = No_File then
            Insert
              (Kernel,
               -"Could not locate executable on path: " &
               Unknown_To_UTF8 (+Exec),
               Mode => Error);
            return +"";
         end if;

         --  use Normalize_Pathname to prevent relative paths like
         --  ./my_prog. See F322-010 concerning problem with gnatls in this
         --  particular case.
         return Full_Exec.Full_Name (True);
      end Check_Exec;

      -----------------------
      -- On_New_Connection --
      -----------------------

      procedure On_New_Connection (Server_Name : String) is
      begin
         if Server_Name = Get_Nickname (Build_Server) then
            Run_Hook (Kernel, Build_Server_Connected_Hook);
         end if;
      end On_New_Connection;

   begin
      Success := False;

      --  First verify the executable to be launched

      if Is_Local (Server) then
         declare
            E : constant Filesystem_String :=
                  Check_Exec (+Get_Command (Arguments));
         begin
            if E'Length = 0 then
               return;
            end if;

            Exec := new String'(+E);
         end;

         Args := new Argument_List'
           ((1 => Exec) &
             To_List (Arguments, Include_Command => False));
      else
         Args := new Argument_List'(To_List (Arguments, True));
      end if;

      if Console /= null and then Show_Command then
         if Is_Local (Server) then
            Insert_With_Links
              (Console,
               To_Display_String (Arguments),
               Add_LF => True);
         else
            Insert_With_Links
              (Console,
               Get_Nickname (Server) & "> " &
                 To_Display_String (Arguments),
               Add_LF => True);
         end if;
      end if;

      if Is_Local (Server) then
         Pd := new GNAT.Expect.TTY.TTY_Process_Descriptor;
         Set_Use_Pipes (TTY_Process_Descriptor (Pd.all), Use_Pipes);

         --  If using an external terminal, use Execute_Command preference
         --  ??? incompatible with gnat.expect.tty.remote...

         if Use_Ext_Terminal then
            declare
               Tmp_Args_1 : Argument_List_Access := Args;
               Tmp_Args_2 : Argument_List_Access;
            begin
               Tmp_Args_2 := Argument_String_To_List
                 (GPS.Kernel.Preferences.Execute_Command.Get_Pref);
               Args := new Argument_List'(Tmp_Args_2.all & Tmp_Args_1.all);
               Simple_Free (Tmp_Args_1);
               Simple_Free (Tmp_Args_2);
            end;
         end if;

         if Directory /= No_File then
            Old_Dir := Get_Current_Dir;
            Change_Dir (Directory);
         end if;

         if Active (Me) then
            Trace (Me, "Spawning");

            if Args /= null then
               for J in Args'Range loop
                  if Args (J) /= null then
                     Trace (Me, "Arg: """ & Args (J).all & """");
                  end if;
               end loop;
            end if;
         end if;

         declare
            Env     : GNAT.OS_Lib.String_Access := Getenv ("PATH");
            Oldpath : constant File_Array := From_Path (+Env.all);
         begin
            --  Modify the PATH environment variable just before spawning the
            --  compiler, so that the correct one is picked up.
            if Is_Toolchains_Active then
               if Server = Build_Server then
                  Setenv
                    ("PATH",
                     +To_Path ((1 => Get_Compiler_Search_Path) & Oldpath));
               else
                  Setenv
                    ("PATH",
                     +To_Path ((1 => Get_Tool_Search_Path) & Oldpath));
               end if;

               if Active (Me) then
                  Trace (Me, Getenv ("PATH").all);
               end if;
            end if;

            --  Set buffer_size to 0 for dynamically allocated buffer (prevents
            --  possible overflow)

            Non_Blocking_Spawn
              (Pd.all,
               Args (Args'First).all,
               Args (Args'First + 1 .. Args'Last),
               Buffer_Size => 0,
               Err_To_Out  => True);

            if Is_Toolchains_Active then
               Setenv ("PATH", +To_Path (Oldpath));
            end if;

            Free (Env);

         exception
            when Invalid_Process =>
               raise Invalid_Process with "not an executable";
         end;

         if Directory /= No_File then
            Change_Dir (Old_Dir);
         end if;

      else
         if Active (Me) then
            Trace
              (Me, "Remote Spawning " & Argument_List_To_String (Args.all));
         end if;

         if Directory = No_File then
            Old_Dir := To_Remote (Get_Current_Dir, Get_Nickname (Server));
         elsif Get_Host (Directory) /= Get_Nickname (Server) then
            Old_Dir := To_Remote (Directory, Get_Nickname (Server));
         else
            Old_Dir := Directory;
         end if;

         Remote_Spawn
           (Pd,
            Target_Nickname     => Get_Nickname (Server),
            Args                => Args.all,
            Execution_Directory => Old_Dir.Full_Name,
            Err_To_Out          => True,
            On_New_Connection   => On_New_Connection'Access);
      end if;

      Success := True;

      Free (Args);

   exception
      when E : Invalid_Process | Process_Died =>
         Success := False;
         Insert
           (Kernel,
            -"Error while trying to execute " & Args (Args'First).all & ": " &
            Exception_Message (E),
            Mode => Error);

      when E : others =>
         Success := False;
         Insert
           (Kernel,
            -"Remote configuration error: " & Exception_Message (E),
            Mode => Error);
   end Spawn;

end GPS.Kernel.Remote;
