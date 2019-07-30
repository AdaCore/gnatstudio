------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2006-2019, AdaCore                     --
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
with GNAT.Expect.TTY;            use GNAT.Expect.TTY;
pragma Warnings (Off, ".*is an internal GNAT unit");
with GNAT.Expect.TTY.Remote;     use GNAT.Expect.TTY.Remote;
pragma Warnings (On, ".*is an internal GNAT unit");
with GNAT.OS_Lib;                use GNAT.OS_Lib;

with GNATCOLL.JSON;
with GNATCOLL.Traces;            use GNATCOLL.Traces;
with GNATCOLL.Projects;          use GNATCOLL.Projects;
with GNATCOLL.VFS;               use GNATCOLL.VFS;

with Glib;                       use Glib;
with Glib.Main;                  use Glib.Main;
with Glib.Object;                use Glib.Object;

with GPS.Intl;                   use GPS.Intl;
with GPS.Properties;             use GPS.Properties;
with GPS.Kernel.Hooks;           use GPS.Kernel.Hooks;
with GPS.Kernel.Modules;         use GPS.Kernel.Modules;
with GPS.Kernel.Preferences;     use GPS.Kernel.Preferences;
with GPS.Kernel.Project;         use GPS.Kernel.Project;
with GPS.Kernel.Properties;      use GPS.Kernel.Properties;

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
      Value    : in out GNATCOLL.JSON.JSON_Value);
   overriding procedure Load
     (Property : in out Servers_Property;
      Value    : GNATCOLL.JSON.JSON_Value);
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

   type On_Project_Changing is new File_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_Project_Changing;
      Kernel : not null access Kernel_Handle_Record'Class;
      File   : GNATCOLL.VFS.Virtual_File);
   --  Called when project is about to change

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy (Property : in out Servers_Property) is
   begin
      for C in Property.Servers'Range loop
         Free (Property.Servers (C).Nickname);
      end loop;
   end Destroy;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class) is
   begin
      --  Register the module
      Remote_Module := new Remote_Module_Record;
      Remote_Module.Kernel := Kernel_Handle (Kernel);
      Remote_Module.Project_Reloading := False;
      Register_Module (Remote_Module, Kernel, "remote");

      --  Connect to project_changing hook
      Project_Changing_Hook.Add (new On_Project_Changing);
   end Register_Module;

   ----------
   -- Save --
   ----------

   overriding procedure Save
     (Property : access Servers_Property;
      Value    : in out GNATCOLL.JSON.JSON_Value)
   is
      use GNATCOLL.JSON;

      Values : JSON_Array;
   begin
      Trace (Me, "Saving remote property");

      for J in Property.Servers'Range loop
         declare
            Value : constant JSON_Value := Create_Object;
         begin
            Value.Set_Field ("name", Server_Type'Image (J));
            Value.Set_Field ("value", Property.Servers (J).Nickname.all);
            Append (Values, Value);
         end;
      end loop;
      Value.Set_Field ("value", Values);
      Trace (Me, "Saving remote property");
   end Save;

   ----------
   -- Load --
   ----------

   overriding procedure Load
     (Property : in out Servers_Property;
      Value    : GNATCOLL.JSON.JSON_Value)
   is
      use GNATCOLL.JSON;

      Values : JSON_Array;
      Item   : JSON_Value;

   begin
      Trace (Me, "Loading remote property");

      Values := Value.Get ("value");

      if Length (Values) = 0 then
         for J in Property.Servers'Range loop
            Free (Property.Servers (J).Nickname);
            Property.Servers (J) :=
              (Is_Local => True,
               Nickname => new String'(Local_Nickname));
         end loop;
      else
         for J in Property.Servers'Range loop
            Item := JSON_Null;
            for Index in 1 .. Length (Values) loop
               Item := Get (Values, Index);
               exit when Item.Get ("name") = Server_Type'Image (J);
            end loop;

            Free (Property.Servers (J).Nickname);

            if Item /= JSON_Null
              and then Gexpect.Db.Is_Configured (Item.Get ("value"))
              and then String'(Item.Get ("value")) /= ""
            then
               Property.Servers (J) :=
                 (Is_Local => False,
                  Nickname => new String'(Item.Get ("value")));
            else
               Property.Servers (J) :=
                 (Is_Local => True, Nickname => new String'(Local_Nickname));
            end if;
         end loop;
      end if;
   end Load;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Project_Changing;
      Kernel : not null access Kernel_Handle_Record'Class;
      File   : GNATCOLL.VFS.Virtual_File)
   is
      pragma Unreferenced (Self);
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

      if not Is_Local (File) then
         Local_File := To_Local (File);
      else
         Local_File := File;
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

         if not Is_Local (File) then
            for J in Property.Servers'Range loop
               Property.Servers (J) :=
                 (Is_Local => False,
                  Nickname => new String'(Get_Host (File)));
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
            Assign (J, Local_Nickname);
            Server_Config_Hook.Run
               (Kernel   => Kernel,
                Server   => J,
                Nickname => Local_Nickname);

         elsif Property.Servers (J).Nickname.all /= Get_Nickname (J) then
            declare
               Nickname  : constant String :=
                             Property.Servers (J).Nickname.all;
            begin
               Assign (J, Nickname);
               Server_Config_Hook.Run
                  (Kernel   => Kernel,
                   Server   => J,
                   Nickname => Nickname);
            end;
         end if;
      end loop;

      --  If Project is loaded from a distant host, force build_server as
      --  distant host.

      if not Is_Local (File)
        and then Get_Host (File) /= Get_Nickname (Build_Server)
      then
         Trace (Me, "Assign build server: project loaded from remote host");
         Assign (Kernel_Handle (Kernel),
                 Build_Server,
                 Get_Host (File),
                 Local_File,
                 Reload_Prj => False);
      end if;

      --  If project is loaded from distant host then synchronize all dirs to
      --  local machine

      if not Is_Local (File) then
         Trace (Me, "Start synchronization of build_server");
         Synchronize
           (Kernel_Handle (Kernel), Build_Server, GPS_Server,
            Blocking      => True,
            Print_Command => False,
            Print_Output  => False,
            Force         => True);
      end if;
   end Execute;

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
                Server_Type'Image (Server) & " => " & Nickname);
      end if;

      Server_Config_Hook.Run
         (Kernel   => Kernel,
          Server   => Server,
          Nickname => Nickname);
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

      Trace (Me, "run sync hook for " & Machine.Nickname);

      if not Rsync_Action_Hook.Run
         (Kernel        => Kernel,
          Tool_Name     => Machine.Sync_Tool,
          Host_Name     => Machine.Nickname,
          To_Remote     => Remote_Is_Dest,
          Queue_Id      => The_Queue_Id,
          Synchronous   => Blocking,
          Print_Output  => Print_Output,
          Print_Command => Print_Command,
          Force         => Force,
          File          => File)
      then
         Kernel.Insert
           (Machine.Sync_Tool & (-" failure: ") &
            (-"Please verify your network configuration"),
            Mode => Error);

         Trace (Me, "No remote sync was registered or errors during" &
                " calls");
         return;
      end if;
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
            Build_Server_Connected_Hook.Run (Kernel);
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
            Trace (Me, "Switching to directory "
                   & Directory.Display_Full_Name);
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
