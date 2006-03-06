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

with Ada.Exceptions;
with Ada.Unchecked_Deallocation;

with GNAT.Directory_Operations;  use GNAT.Directory_Operations;
with GNAT.Expect;                use GNAT.Expect;
pragma Warnings (Off);
with GNAT.Expect.TTY;            use GNAT.Expect.TTY;
pragma Warnings (On);
with GNAT.OS_Lib;                use GNAT.OS_Lib;

with Glib.Xml_Int;               use Glib.Xml_Int;

with GPS.Intl;                   use GPS.Intl;
with GPS.Kernel.Console;         use GPS.Kernel.Console;
with GPS.Kernel.Modules;         use GPS.Kernel.Modules;
with GPS.Kernel.Preferences;     use GPS.Kernel.Preferences;
with GPS.Kernel.Project;         use GPS.Kernel.Project;
with GPS.Kernel.Properties;      use GPS.Kernel.Properties;
with GPS.Kernel.Scripts;         use GPS.Kernel.Scripts;

with Config;                     use Config;
with Filesystem.Unix;            use Filesystem.Unix;
with Filesystem.Windows;         use Filesystem.Windows;
with Interactive_Consoles;       use Interactive_Consoles;
with String_Utils;               use String_Utils;
with Traces;                     use Traces;

package body GPS.Kernel.Remote is

   ------------
   -- Module --
   ------------

   Me : constant Debug_Handle := Create ("GPS.Kernel.Remote");

   type Remote_Module_Record is new Module_ID_Record with record
      Kernel : Kernel_Handle;
   end record;
   type Remote_Module_ID is access all Remote_Module_Record'Class;

   procedure Customize
     (Module : access Remote_Module_Record;
      File   : VFS.Virtual_File;
      Node   : Node_Ptr;
      Level  : Customization_Level);
   procedure Destroy (Module : in out Remote_Module_Record);
   --  See doc for inherited subprogram

   Remote_Module : Remote_Module_ID;

   ------------------
   -- Mirror_Paths --
   ------------------

   type Mirror_Path_Record;
   type Mirror_Path is access all Mirror_Path_Record;

   type Mirror_Path_Record is record
      Nickname    : String_Ptr       := null;
      Local_Path  : String_Ptr       := null;
      Remote_Path : String_Ptr       := null;
      Need_Sync   : Boolean          := False;
      Next        : Mirror_Path      := null;
      Attribute   : Mirror_Attribute := Project_Specific;
   end record;

   Mirror_Path_List : Mirror_Path := null;

   --------------------------
   -- Server Configuration --
   --------------------------

   type Server_Config is record
      Is_Local : Boolean := True;
      --  Is_Local Tells if the server is the local machine or not
      Nickname : String_Ptr;
      --  Identifier of the server
   end record;

   Servers : array (Server_Type) of Server_Config
     := (others => (Is_Local => True,
                    Nickname => new String'(Local_Nickname)));
   --  Servers currently used. Default is the localhost.

   type Servers_Property is new Property_Record with null record;

   procedure Save
     (Property : access Servers_Property;
      Node     : in out Glib.Xml_Int.Node_Ptr);

   procedure Load
     (Property : in out Servers_Property; From : Glib.Xml_Int.Node_Ptr);

   ---------------------
   -- Utility methods --
   ---------------------

   procedure Simple_Free is new Ada.Unchecked_Deallocation
     (Object => Argument_List, Name => Argument_List_Access);
   --  Frees the pointer without freeing internal strings

   ---------------
   -- Callbacks --
   ---------------

   procedure On_Project_Changed
     (Kernel : access Kernel_Handle_Record'Class);
   --  Called when project changed

   function From_Callback_Data_Sync_Hook
     (Data : Callback_Data'Class) return Hooks_Data'Class;
   --  retrieve hook data from callback data

   function From_Callback_Data_Server_Config_Changed_Hook
     (Data : Callback_Data'Class) return Hooks_Data'Class;
   --  retrieve hook data from callback data

   ----------------------------------
   -- From_Callback_Data_Sync_Hook --
   ----------------------------------

   function From_Callback_Data_Sync_Hook
     (Data : Callback_Data'Class) return Hooks_Data'Class
   is
      Src_Server  : Server_Type;
      Dest_Server : Server_Type;
   begin
      Src_Server  := Server_Type'Value (String'(Nth_Arg (Data, 2)));
      Dest_Server := Server_Type'Value (String'(Nth_Arg (Data, 3)));
      declare
         Queue_Id  : constant String := Nth_Arg (Data, 4);
         Src_Path  : constant String := Nth_Arg (Data, 5);
         Dest_Path : constant String := Nth_Arg (Data, 6);
      begin
         return Rsync_Hooks_Args'
           (Hooks_Data with
            Queue_Id_Length  => Queue_Id'Length,
            Src_Path_Length  => Src_Path'Length,
            Dest_Path_Length => Dest_Path'Length,
            Src              => Src_Server,
            Dest             => Dest_Server,
            Queue_Id         => Queue_Id,
            Src_Path         => Src_Path,
            Dest_Path        => Dest_Path);
      end;
   end From_Callback_Data_Sync_Hook;

   --------------------------
   -- Create_Callback_Data --
   --------------------------

   function Create_Callback_Data
     (Script    : access GPS.Kernel.Scripts.Scripting_Language_Record'Class;
      Hook_Name : String;
      Data      : access Rsync_Hooks_Args)
      return GPS.Kernel.Scripts.Callback_Data_Access
   is
      D : constant Callback_Data_Access :=
        new Callback_Data'Class'(Create (Script, 6));
   begin
      Set_Nth_Arg (D.all, 1, Hook_Name);
      Set_Nth_Arg (D.all, 2, Server_Type'Image (Data.Src));
      Set_Nth_Arg (D.all, 3, Server_Type'Image (Data.Dest));
      Set_Nth_Arg (D.all, 4, Data.Queue_Id);
      Set_Nth_Arg (D.all, 5, Data.Src_Path);
      Set_Nth_Arg (D.all, 6, Data.Dest_Path);
      return D;
   end Create_Callback_Data;

   ----------------------------------------------
   -- From_Callback_Server_Config_Changed_Hook --
   ----------------------------------------------

   function From_Callback_Data_Server_Config_Changed_Hook
     (Data : Callback_Data'Class) return Hooks_Data'Class
   is
      Server  : Server_Type;
   begin
      Server  := Server_Type'Value (String'(Nth_Arg (Data, 2)));
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

   function Create_Callback_Data
     (Script    : access GPS.Kernel.Scripts.Scripting_Language_Record'Class;
      Hook_Name : String;
      Data      : access Server_Config_Changed_Hooks_Args)
      return GPS.Kernel.Scripts.Callback_Data_Access
   is
      D : constant Callback_Data_Access :=
        new Callback_Data'Class'(Create (Script, 3));
   begin
      Set_Nth_Arg (D.all, 1, Hook_Name);
      Set_Nth_Arg (D.all, 2, Server_Type'Image (Data.Server));
      Set_Nth_Arg (D.all, 3, Data.Nickname);
      return D;
   end Create_Callback_Data;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Unix_FS    : Unix_Filesystem_Record;
      Windows_FS : Windows_Filesystem_Record;
   begin
      --  Register synchronisation hook
      Register_Hook_Data_Type
        (Kernel, Rsync_Hook_Type,
         Args_Creator => From_Callback_Data_Sync_Hook'Access);
      Register_Hook_Return_Boolean
        (Kernel, Rsync_Action_Hook, Rsync_Hook_Type);

      --  Register server config changed hook
      Register_Hook_Data_Type
        (Kernel, Server_Config_Changed_Hook_Type,
         Args_Creator => From_Callback_Data_Server_Config_Changed_Hook'Access);
      Register_Hook_No_Return
        (Kernel, Server_Config_Changed_Hook, Server_Config_Changed_Hook_Type);

      --  Register the module
      Remote_Module := new Remote_Module_Record;
      Remote_Module.Kernel := Kernel_Handle (Kernel);
      Register_Module
        (Remote_Module, Kernel, "remote");

      Initialize_Module (Unix_FS);
      Initialize_Module (Windows_FS);

      --  Connect to project_changed hook
      Add_Hook (Kernel, Project_Changed_Hook,
                Wrapper (On_Project_Changed'Access), "gps.kernel.remote");
   end Register_Module;

   ----------
   -- Save --
   ----------

   procedure Save
     (Property : access Servers_Property;
      Node     : in out Glib.Xml_Int.Node_Ptr)
   is
      Srv : Node_Ptr;
      pragma Unreferenced (Property);
   begin
      for J in Servers'Range loop
         if not Servers (J).Is_Local then
            Srv := new Glib.Xml_Int.Node;
            Srv.Tag := new String'(Server_Type'Image (J));
            Srv.Value := new String'(Servers (J).Nickname.all);
            Add_Child (Node, Srv);
         end if;
      end loop;
   end Save;

   ----------
   -- Load --
   ----------

   procedure Load
     (Property : in out Servers_Property; From : Glib.Xml_Int.Node_Ptr)
   is
      Srv :  Node_Ptr;
      pragma Unreferenced (Property);
   begin
      for J in Servers'Range loop
         if From.Child /= null then
            Srv := Find_Tag (From.Child, Server_Type'Image (J));
            if Srv /= null
              and then Is_Configured (Srv.Value.all)
            then
               Servers (J) := (Is_Local => False,
                               Nickname => new String'(Srv.Value.all));
            else
               Servers (J) := (Is_Local => True,
                               Nickname => new String'(Local_Nickname));
            end if;
         end if;
      end loop;
   end Load;

   ------------------------
   -- On_Project_Changed --
   ------------------------

   procedure On_Project_Changed
     (Kernel : access Kernel_Handle_Record'Class)
   is
      Property : Servers_Property;
      Success  : Boolean;
   begin
      Get_Property
        (Property, Get_Project (Kernel),
         Name => "servers_config", Found => Success);
   end On_Project_Changed;

   ---------------
   -- Customize --
   ---------------

   procedure Customize
     (Module : access Remote_Module_Record;
      File   : VFS.Virtual_File;
      Node   : Glib.Xml_Int.Node_Ptr;
      Level  : Customization_Level)
   is
      pragma Unreferenced (File, Level);
      Name                      : String_Ptr;
      Start_Command             : String_Ptr;
      Start_Command_Common_Args : String_List_Access;
      Start_Command_User_Args   : String_List_Access;
      User_Prompt_Ptrn          : String_Ptr;
      Password_Prompt_Ptrn      : String_Ptr;

   begin
      if Node.Tag.all = "remote_connection_config" then
         Trace (Me, "Initialize_Remote_Config : 'remote_connection_config'");

         Name := new String'(Get_Attribute (Node, "name"));

         if Name.all = "" then
            Console.Insert
              (Module.Kernel,
               " XML Error: remote_connection_config tags shall" &
               " have a name attribute",
               Add_LF => True, Mode => Error);
            return;
         end if;

         Start_Command := Get_Field (Node, "start_command");

         if Start_Command = null then
            Console.Insert
              (Module.Kernel,
               " XML Error: remote_connection_config tags shall" &
               " have a start_command field",
               Add_LF => True, Mode => Error);
            return;
         end if;

         Start_Command_Common_Args := Argument_String_To_List
           (Get_Field (Node, "start_command_common_args").all);
         Start_Command_User_Args := Argument_String_To_List
           (Get_Field (Node, "start_command_user_args").all);
         User_Prompt_Ptrn := Get_Field (Node, "user_prompt_ptrn");
         Password_Prompt_Ptrn := Get_Field (Node, "password_prompt_ptrn");
         Add_Remote_Access_Descriptor
           (Name                      => Name.all,
            Start_Command             => Start_Command.all,
            Start_Command_Common_Args => Start_Command_Common_Args.all,
            Start_Command_User_Args   => Start_Command_User_Args.all,
            User_Prompt_Ptrn          => User_Prompt_Ptrn.all,
            Password_Prompt_Ptrn      => Password_Prompt_Ptrn.all);
         Glib.Xml_Int.Free (Name);
      end if;
   end Customize;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Module : in out Remote_Module_Record) is
      pragma Unreferenced (Module);
   begin
      Remote_Module := null;
      --  ???: save machines configuration in .gps/remote.xml
   end Destroy;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize;
   procedure Initialize is
   begin
      --  ??? Note that the whole Initialize procedure here shall go away
      --  with proper configuration load at startup

      Add_Mirror_Path
        (Nickname           => "cardiff",
         GPS_Ref            => "h:\",
         Remote_Path        => "/alexandria.a/home/lambourg/");
   end Initialize;

   -------------
   -- Convert --
   -------------

   function To_Remote
     (Path       : String;
      To         : Server_Type;
      Unix_Style : Boolean := False) return String
   is
      Path_From      : String_Ptr;
      Path_To        : String_Ptr;
      Mirror         : Mirror_Path;

   begin
      --  If From and To are the same machine (and no unix path translation is
      --  needed), just return Path
      if Is_Local (To) then
         if Unix_Style then
            return To_Unix (Get_Local_Filesystem, Path);
         else
            return Path;
         end if;
      end if;

      --  Search for mirror path in 'To' config
      Mirror := Mirror_Path_List;
      while Mirror /= null loop
         if Mirror.Nickname.all = Servers (To).Nickname.all
           and then Is_Subtree (Get_Local_Filesystem,
                                Mirror.Local_Path.all,
                                Path)
         then
            Path_From := Mirror.Local_Path;
            Path_To   := Mirror.Remote_Path;
            exit;
         end if;

         Mirror := Mirror.Next;
      end loop;

      if Path_From = null or Path_To = null then
         --  Not configured mirror path.
         return Path;
      end if;

      --  At this point, we have the from and to moint points. Let's translate
      --  the path
      declare
         To_Filesystem : Filesystem_Record'Class := Get_Filesystem (To);
         U_Path     : constant String := To_Unix (Get_Local_Filesystem,
                                                  Path);
         --  The input path in unix style
         U_Frompath : constant String := To_Unix (Get_Local_Filesystem,
                                                  Path_From.all);
         --  The local root dir, in unix style
         U_Subpath  : constant String
           := U_Path (U_Path'First + U_Frompath'Length .. U_Path'Last);
      begin
         if Unix_Style then
            return To_Unix (To_Filesystem, Path_To.all) & U_Subpath;
         else
            return Concat (To_Filesystem,
                           Path_To.all,
                           From_Unix (To_Filesystem, U_Subpath));
         end if;
      end;
   end To_Remote;

   ------------------
   -- To_Unix_Path --
   ------------------

   function To_Unix_Path
     (Path             : String;
      Server           : Server_Type) return String
   is
   begin
      return To_Unix (Get_Filesystem (Server), Path);
   end To_Unix_Path;

   -----------------
   -- Synchronize --
   -----------------

   procedure Synchronize
     (Kernel   : Kernel_Handle;
      From     : Server_Type;
      To       : Server_Type;
      Queue_Id : String)
   is
      Server    : Server_Type;
      Mirror    : Mirror_Path;
      From_Path : String_Ptr;
      To_Path   : String_Ptr;

   begin
      Trace (Me, "Synchronizing paths");

      if not Is_Local (From) then
         Server := From;
      elsif not Is_Local (To) then
         Server := To;
      else
         --  Both servers local. No need to synchronize.
         return;
      end if;

      --  Synchronize with remote host
      Mirror := Mirror_Path_List;

      while Mirror /= null loop
         if Mirror.Nickname.all = Servers (Server).Nickname.all
           and then Mirror.Need_Sync
         then
            if Is_Local (From) then
               From_Path := Mirror.Local_Path;
               To_Path   := Mirror.Remote_Path;
            else
               From_Path := Mirror.Remote_Path;
               To_Path   := Mirror.Local_Path;
            end if;

            declare
               Data : aliased Rsync_Hooks_Args
                 := (Hooks_Data with
                     Queue_Id_Length  => Queue_Id'Length,
                     Src_Path_Length  => From_Path'Length,
                     Dest_Path_Length => To_Path'Length,
                     Src              => From,
                     Dest             => To,
                     Queue_Id         => Queue_Id,
                     Src_Path         => From_Path.all,
                     Dest_Path        => To_Path.all);

            begin
               Trace (Me, "run sync hook for " & Data.Src_Path);

               if not Run_Hook_Until_Success
                 (Kernel, Rsync_Action_Hook, Data'Unchecked_Access)
               then
                  Trace (Me, "No remote sync was registered");
               end if;
            end;
         end if;

         Mirror := Mirror.Next;
      end loop;
   end Synchronize;

   --------------
   -- On_Error --
   --------------

   procedure On_Error
     (Manager : access Default_Error_Display_Record;
      Message : String) is
   begin
      if Manager.Kernel /= null then
         Insert (Manager.Kernel,
                 Message,
                 Mode => Error);
      end if;
   end On_Error;

   -----------
   -- Spawn --
   -----------

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
      Error_Manager    : Error_Display := null)
   is
      Exec         : String_Access;
      Old_Dir      : String_Access;
      Args         : Argument_List_Access;
--        New_Args     : Argument_List_Access;
      L_Args       : Argument_List_Access := null;
      Default_Error_Manager : aliased Default_Error_Display_Record;
      In_Use_Error_Manager  : Error_Display;

      function Check_Exec (Exec : String) return String_Access;
      --  Check that executable is on the path, and return the full path if
      --  found, return null otherwise.

      ----------------
      -- Check_Exec --
      ----------------

      function Check_Exec (Exec : String) return String_Access is
         Full_Exec : String_Access;
      begin
         Full_Exec := Locate_Exec_On_Path (Exec);

         if Full_Exec = null then
            On_Error (In_Use_Error_Manager,
                      -"Could not locate executable on path: " & Exec);
            return null;
         end if;

         return Full_Exec;
      end Check_Exec;

      Request_User : Request_User_Object
        := (Main_Window => null);

   begin
      Success := False;

      --  Set the error display manager

      if Error_Manager = null then
         Default_Error_Manager.Kernel := Kernel;
         In_Use_Error_Manager := Default_Error_Manager'Unchecked_Access;
      else
         In_Use_Error_Manager := Error_Manager;
      end if;

      if Kernel /= null then
         Request_User.Main_Window := Get_Main_Window (Kernel);
      end if;

      --  First verify the executable to be launched

      if Is_Local (Server) then
--           Filesystem := Get_Local_Filesystem;
         Exec := Check_Exec (Arguments (Arguments'First).all);

         if Exec = null then
            return;
         end if;

         Args := new Argument_List'
           ((1 => Exec) &
            Clone (Arguments (Arguments'First + 1 .. Arguments'Last)));
      else
--           Filesystem := Get_Filesystem (Servers (Server).Nickname.all);
         Args := new Argument_List'(Clone (Arguments));
      end if;

      if Console /= null
        and then Show_Command
        and then not Is_Local (Server)
      then
         if Is_Local (Server) then
            Insert (Console,
                    Argument_List_To_String (Arguments),
                    Add_LF => True);
         else
            Insert (Console,
                    Get_Nickname (Server) & "> " &
                    Argument_List_To_String (Arguments),
                    Add_LF => True);
         end if;
      end if;

      if Servers (Server).Is_Local then
         Pd := new GNAT.Expect.TTY.TTY_Process_Descriptor;

         if Host = Config.Windows then
            --  Windows commands are launched using "cmd /c the_command"
            L_Args :=
              new Argument_List'((new String'("cmd"), new String'("/c")));
         end if;

         --  If using an external terminal, use Execute_Command preference
         --  ??? incompatible with gnat.expect.tty.remote... needs to be fixed
         if Use_Ext_Terminal then
            declare
               Tmp_Args_1 : Argument_List_Access := Args;
               Tmp_Args_2 : Argument_List_Access;
            begin
               Tmp_Args_2 := Argument_String_To_List
                 (Get_Pref (GPS.Kernel.Preferences.Execute_Command));
               Args := new Argument_List'(Tmp_Args_2.all & Tmp_Args_1.all);
               Simple_Free (Tmp_Args_1);
               Simple_Free (Tmp_Args_2);
            end;
         end if;

         if Directory /= "" then
            Old_Dir := new String'(Get_Current_Dir);
            Change_Dir (Directory);
         end if;

         if Active (Me) then
            Trace (Me, "Spawning " & Argument_List_To_String (Args.all));
         end if;

         --  Set buffer_size to 0 for dynamically allocated buffer (prevents
         --  possible overflow)

         if L_Args /= null then
            Non_Blocking_Spawn (Pd.all,
                                L_Args (L_Args'First).all,
                                L_Args (L_Args'First + 1 .. L_Args'Last) &
                                Args.all,
                                Buffer_Size => 0,
                                Err_To_Out  => True);
            Free (L_Args);
         else
            Non_Blocking_Spawn (Pd.all,
                                Args (Args'First).all,
                                Args (Args'First + 1 .. Args'Last),
                                Buffer_Size => 0,
                                Err_To_Out  => True);
         end if;

         if Directory /= "" then
            Change_Dir (Old_Dir.all);
            Free (Old_Dir);
         end if;

      else
         if Active (Me) then
            Trace (Me, "Remote Spawning " &
                   Argument_List_To_String (Args.all));
         end if;

         if Directory = "" then
            Old_Dir := new String'
              (To_Remote (Get_Current_Dir, Server));
         else
            Old_Dir := new String'(Directory);
         end if;

         --  Set buffer_size to 0 for dynamically allocated buffer
         --  (prevents possible overflow)
         Remote_Spawn
           (Pd,
            Target_Nickname       => Servers (Server).Nickname.all,
            Args                  => Args.all,
            Execution_Directory   => Old_Dir.all,
            Err_To_Out            => True,
            Request_User_Instance => Request_User);
         Free (Old_Dir);
      end if;

      Success := True;

      Free (Args);

   exception
      when E : Invalid_Process | Process_Died =>
         Success := False;

         On_Error (In_Use_Error_Manager,
                   -"Invalid command (" &
                   Ada.Exceptions.Exception_Message (E) &
                   ")");
   end Spawn;

   ---------------------
   -- Add_Mirror_Path --
   ---------------------

   procedure Add_Mirror_Path
     (Nickname    : String;
      GPS_Ref     : String;
      Remote_Path : String;
      Need_Sync   : Boolean := False;
      Attribute   : Mirror_Attribute := Project_Specific)
   is
      Mirror : Mirror_Path;
   begin
      Mirror := new Mirror_Path_Record'
        (Nickname    => new String'(Nickname),
         Local_Path  => new String'(GPS_Ref),
         Remote_Path => new String'(Remote_Path),
         Need_Sync   => Need_Sync,
         Next        => Mirror_Path_List,
         Attribute   => Attribute);
      Mirror_Path_List := Mirror;
   end Add_Mirror_Path;

   ------------
   -- Assign --
   ------------

   procedure Assign
     (Kernel   : Kernel_Handle;
      Server   : Server_Type;
      Nickname : String)
   is
      Data : aliased Server_Config_Changed_Hooks_Args
        := (Hooks_Data with
            Nickname_Length => Nickname'Length,
            Server          => Server,
            Nickname        => Nickname);
      Prop : Property_Access;
   begin
      Glib.Free (Servers (Server).Nickname);

      Servers (Server) := (Is_Local => Nickname = Local_Nickname,
                           Nickname => new String'(Nickname));

      Prop := new Servers_Property;
      Set_Property
        (Get_Project (Kernel),
         Name       => "servers_config",
         Property   => Prop,
         Persistent => True);

      if Active (Me) then
         Trace (Me, "run server_changed hook for " &
                Server_Type'Image (Server) & " => " & Data.Nickname);
      end if;

      Run_Hook (Kernel, Server_Config_Changed_Hook, Data'Unchecked_Access);
   end Assign;

   ------------------
   -- Get_Nickname --
   ------------------

   function Get_Nickname (Server : Server_Type) return String is
   begin
      return Servers (Server).Nickname.all;
   end Get_Nickname;

   ----------------------
   -- Get_Network_Name --
   ----------------------

   function Get_Network_Name (Server : Server_Type) return String is
   begin
      if Is_Local (Server) then
         return "localhost";
      else
         return Get_Network_Name (Get_Nickname (Server));
      end if;
   end Get_Network_Name;

   --------------------
   -- Get_Filesystem --
   --------------------

   function Get_Filesystem (Server : Server_Type)
                            return Filesystem_Record'Class is
   begin
      if Is_Local (Server) then
         return Get_Local_Filesystem;
      else
         return Get_Filesystem (Get_Nickname (Server));
      end if;
   end Get_Filesystem;

   --------------
   -- Is_Local --
   --------------

   function Is_Local (Server : Server_Type) return Boolean is
   begin
      return Servers (Server).Is_Local;
   end Is_Local;

begin
   Initialize;
   --  ??? Remove once the Initialize procedure is actually called at
   --  config time.

end GPS.Kernel.Remote;
