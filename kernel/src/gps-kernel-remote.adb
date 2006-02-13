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
with GNAT.Expect.TTY.Remote;     use GNAT.Expect.TTY.Remote;
pragma Warnings (On);
with GNAT.OS_Lib;                use GNAT.OS_Lib;

with Glib.Object;                use Glib.Object;

with GPS.Intl;                   use GPS.Intl;
with GPS.Kernel.Console;         use GPS.Kernel.Console;
with GPS.Kernel.Modules;         use GPS.Kernel.Modules;
with GPS.Kernel.Preferences;     use GPS.Kernel.Preferences;
with GPS.Kernel.Scripts;         use GPS.Kernel.Scripts;

with Config;                     use Config;
with Interactive_Consoles;       use Interactive_Consoles;
with String_Utils;               use String_Utils;
with Traces;                     use Traces;

package body GPS.Kernel.Remote is

   Me : constant Debug_Handle := Create ("GPS.Kernel.Remote");

   ------------------
   -- Mirror_Paths --
   ------------------

   type Mirror_Path_Record;
   type Mirror_Path is access all Mirror_Path_Record;

   type Mirror_Path_Record is record
      Local_Path  : String_Ptr  := null;
      Remote_Path : String_Ptr  := null;
      Need_Sync   : Boolean     := False;
      Next        : Mirror_Path := null;
   end record;

   --------------------------
   -- Server Configuration --
   --------------------------

   type Server_Config_Record;
   type Server_Config is access all Server_Config_Record;

   type Server_Config_Record is record
      Is_Local         : Boolean := True;
      --  teells if the server is the local machine or not
      Filesystem       : Filesystem_Type;
      --  the filesystem on the remote or local machine
      Nickname         : String_Ptr  := null;
      --  nickname : informative only
      Network_Name     : String_Ptr  := null;
      --  used to access the server using the network
      Remote_Access    : String_Ptr  := null;
      --  utility used to remotely access the server
      Remote_Shell     : String_Ptr  := null;
      --  shell used on the remote server
      User_Name        : String_Ptr  := null;
      --  user name used for connection
      Timeout          : Natural;
      --  timeout value for shell operations
      Mirror_Path_List : Mirror_Path := null;
      --  list of remote paths
      Next             : Server_Config := null;
      --  next item in the list
   end record;

   function Get_Local_Filesystem return Filesystem_Type;
   --  Retrieves the local filesystem

   --------------------------
   -- Get_Local_Filesystem --
   --------------------------

   function Get_Local_Filesystem return Filesystem_Type is
   begin
      if Host = Windows then
         return Windows;
      else
         --  unix and windows support only
         return Unix;
      end if;
   end Get_Local_Filesystem;

   Config_List : constant Server_Config := new Server_Config_Record'
     (Is_Local         => True,
      Filesystem       => Get_Local_Filesystem,
      Nickname         => new String'("(local)"),
      Network_Name     => null,
      Remote_Access    => null,
      Remote_Shell     => null,
      User_Name        => null,
      Timeout          => 0,
      Mirror_Path_List => null,
      Next             => null);
   --  Filesystem is dummy at this point. Initialized at elaboration
   --  (see end of package).

   Nb_Servers : Server_Id := 1;
   --  Total number of defined servers

   Servers : array (Server_Type) of Server_Config
           := (others => Config_List);
   --  Servers currently used. Default to first Config List item (localhost)

   ---------------------
   -- Utility methods --
   ---------------------

   function Match (Mirror, Path : String;
                   Case_Sensitive : Boolean) return Boolean;
   --  returns true if path starts with mirror;

   procedure Simple_Free is new Ada.Unchecked_Deallocation
     (Object => Argument_List, Name => Argument_List_Access);
   --  frees the pointer without freeing internal strings

   function VMS_Wrapper (Cmd : String) return String;
   --  VMS command line sender

   -----------
   -- Match --
   -----------

   function Match (Mirror, Path : String;
                   Case_Sensitive : Boolean) return Boolean is
   begin
      --  Path length shall be greater or equal to mirror length
      if Mirror'Length > Path'Length then
         return False;
      end if;

      --  Do not try to compare last character: on VMS, you will compare
      --  a closing bracket with a dot (disk:[path] with disk:[path.subpath])
      return Equal (Path (Path'First .. Path'First + Mirror'Length - 2),
                    Mirror (Mirror'First .. Mirror'Last - 1),
                    Case_Sensitive);
   end Match;

   -----------------
   -- VMS_Wrapper --
   -----------------

   function VMS_Wrapper (Cmd : String) return String is
   begin
      return Cmd & ASCII.CR;
   end VMS_Wrapper;

   function Shell_Selector (Target_Identifier : String)
                            return String;
   function Remote_Access_Selector (Target_Identifier : String)
                                    return String;
   --  Configuration selectors

   ---------------
   -- Callbacks --
   ---------------

   procedure On_Servers_Configure
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle);
   --  Remote->Servers configuration

   procedure On_Mirror_Path_Configure
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle);
   --  Remote->File system configuration

   function From_Callback_Data_Sync_Hook
     (Data : Callback_Data'Class) return Hooks_Data'Class;
   --  retrieve hook data from callback data

   ----------------------------
   -- Remote_Access_Selector --
   ----------------------------

   function Remote_Access_Selector (Target_Identifier : String)
                                    return String
   is
      Srv_Type : constant Server_Type := Server_Type'Value (Target_Identifier);
   begin
      return Servers (Srv_Type).Remote_Access.all;
   end Remote_Access_Selector;

   --------------------
   -- Shell_Selector --
   --------------------

   function Shell_Selector (Target_Identifier : String)
                            return String
   is
      Srv_Type : constant Server_Type := Server_Type'Value (Target_Identifier);
   begin
      return Servers (Srv_Type).Remote_Shell.all;
   end Shell_Selector;

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
        new Callback_Data'Class'(Create (Script, 5));
   begin
      Set_Nth_Arg (D.all, 1, Hook_Name);
      Set_Nth_Arg (D.all, 2, Server_Type'Image (Data.Src));
      Set_Nth_Arg (D.all, 3, Server_Type'Image (Data.Dest));
      Set_Nth_Arg (D.all, 4, Data.Queue_Id);
      Set_Nth_Arg (D.all, 5, Data.Src_Path);
      Set_Nth_Arg (D.all, 6, Data.Dest_Path);
      return D;
   end Create_Callback_Data;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Remote : constant String := "/_" & (-"Remote") & '/';
   begin
      Register_Menu
        (Kernel, Remote, -"Servers _configuration", "",
         On_Servers_Configure'Access);
      Register_Menu
        (Kernel, Remote, -"_File system configuration", "",
         On_Mirror_Path_Configure'Access);
      --  Register synchronisation hook
      Register_Hook_Data_Type
        (Kernel, Rsync_Hook_Type,
         Args_Creator => From_Callback_Data_Sync_Hook'Access);
      Register_Hook_Return_Boolean
        (Kernel, Rsync_Action_Hook, Rsync_Hook_Type);
   end Initialize;

   --------------------------
   -- On_Servers_Configure --
   --------------------------

   procedure On_Servers_Configure
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget, Kernel);
   begin
      null;
   end On_Servers_Configure;

   ------------------------------
   -- On_Mirror_Path_Configure --
   ------------------------------

   procedure On_Mirror_Path_Configure
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget, Kernel);
   begin
      null;
   end On_Mirror_Path_Configure;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
      Success : Boolean;
   begin
      Set_Config_Selector
        (Remote_Access_Selector => Remote_Access_Selector'Access,
         Shell_Selector         => Shell_Selector'Access);

      Add_Remote_Access_Descriptor
        (Name                      => "ssh",
         Start_Command             => "ssh",
         Start_Command_Common_Args => (new String'("-Y"),
                                       new String'("-t"),
                                       new String'("-t"),
                                       new String'("%h")),
         Start_Command_User_Args   => (new String'("-l"),
                                       new String'("%u")),
         User_Prompt_Ptrn          => "(.*ogin|Name \([^\)]*\)): *$",
         Password_Prompt_Ptrn      => "^.*(password|passphrase.*): *$");

      Add_Shell_Descriptor
        ("bash", "bash",
         Generic_Prompt      => "^[^#$>\n]*[#$%>] *$",
         Configured_Prompt   => "^---GPSPROMPT--#.*$",
         Init_Commands       => (new String'("unalias ls"),
                                 new String'("export COLUMNS=2048"),
                                 new String'("export PS1=---GPSPROMPT--#")),
         Exit_Commands       => (1 => new String'("exit")),
         Cd_Command          => "cd %d",
         Get_Status_Command  => "echo $?",
         Get_Status_Ptrn     => "^([0-9]*)$",
         Echoing             => False);
      Add_Shell_Descriptor
        ("vms", "",
         Generic_Prompt      => "\0[^#$>\n]*[#$%>] *$",
         Configured_Prompt   => "\0---GPSPROMPT--#.*$",
         Init_Commands       =>
           (new String'("SET TERMINAL/WIDTH=255"),
            new String'("SET TERMINAL/NOWRAP"),
            new String'("USEINSTALL5"), --  ??? AdaCore specific
            new String'("SET PROMPT=---GPSPROMPT--#")),
         Exit_Commands       => (1 => new String'("logout")),
         Cd_Command          => "SET DEFAULT %d",
         Get_Status_Command  => "WRITE TERMINAL $STATUS",
         Get_Status_Ptrn     => "^%X([0-9]*) *$",
         Wrapper             => VMS_Wrapper'Access,
         Echoing             => True);
--        Add_Program_Descriptor
--          ("windshae-windows", "windsh",
--           Start_Command_Args => (1 => new String'("%h")),
--           Start_Timeout      => 300000,
--           Prompt             => "^\[vxKernel\] -> ",
--           Echoing            => True,
--           Output_Processor   => Windsh_Output_Processor'Access);
      Add_Or_Replace_Server_Config
        (Nickname      => "cardiff (x86 linux)",
         Old_Nickname  => "",
         Network_Name  => "cardiff.act-europe.fr",
         Filesystem    => Unix,
         Remote_Access => "ssh",
         Remote_Shell  => "bash",
         Timeout       => 5000,
         Success       => Success);
--        Add_Mirror_Path
--          (Nickname    => "cardiff (x86 linux)",
--           GPS_Ref     => "/home/lambourg/cardiff.a/remote/sdc/",
--           Remote_Path => "/alexandria.a/home/lambourg/remote/sdc/",
--           Need_Sync   => True);
      Add_Mirror_Path
        (Nickname    => "cardiff (x86 linux)",
         GPS_Ref     => "/home/lambourg/cardiff.a/build/",
         Remote_Path => "/cardiff.a/lambourg/build/",
         Need_Sync   => True);
      Add_Mirror_Path
        (Nickname    => "cardiff (x86 linux)",
         GPS_Ref     => "/home/lambourg/cardiff.a/install-cardiff/",
         Remote_Path => "/cardiff.a/lambourg/install-cardiff/",
         Need_Sync   => True);
      Add_Or_Replace_Server_Config
        (Nickname      => "bars (VMS)",
         Old_Nickname  => "",
         Network_Name  => "bars",
         User_Name     => "gnatmail",
         Filesystem    => VMS,
         Remote_Access => "ssh",
         Remote_Shell  => "vms",
         Timeout       => 10000,
         Success       => Success);
      Add_Mirror_Path
        (Nickname    => "bars (VMS)",
         GPS_Ref     => "/alexandria.a/home/lambourg/",
         Remote_Path => "DNFS1:[HOME.LAMBOURG]");
   end Initialize;

   -------------
   -- Convert --
   -------------

   function Convert (Path       : String;
                     From       : Server_Type;
                     To         : Server_Type;
                     Unix_Style : Boolean := False) return String
   is
      Path_From      : String_Ptr;
      Path_To        : String_Ptr;
      Mirror         : Mirror_Path;
      Case_Sensitive : Boolean;

      function Is_Case_Sensitive (Filesystem : Filesystem_Type) return Boolean;
      --  Tells if a specified filesystem is case sensitive

      -----------------------
      -- Is_Case_Sensitive --
      -----------------------

      function Is_Case_Sensitive (Filesystem : Filesystem_Type) return Boolean
      is
      begin
         case Filesystem is
         when Windows | Windows_Cygwin | VMS =>
            return False;
         when Unix =>
            return True;
         end case;
      end Is_Case_Sensitive;

   begin
      --  if From and To are the same machine (and no unix path translation is
      --  needed), just return Path
      if Servers (From) = Servers (To) then
         if Unix_Style then
            return To_Unix_Path (Path, From);
         else
            return Path;
         end if;
      end if;

      --  set case sensitivity
      Case_Sensitive := Is_Case_Sensitive (Servers (From).Filesystem);

      if Servers (From).Is_Local then
         --  search for mirror path in 'To' config
         Mirror := Servers (To).Mirror_Path_List;
         while Mirror /= null loop
            if Match (Mirror.Local_Path.all, Path, Case_Sensitive) then
               Path_From := Mirror.Local_Path;
               Path_To   := Mirror.Remote_Path;
               exit;
            end if;
            Mirror := Mirror.Next;
         end loop;

      else
         --  search for mirror path in 'From' config
         Mirror := Servers (From).Mirror_Path_List;
         while Mirror /= null loop
            if Match (Mirror.Remote_Path.all, Path, Case_Sensitive) then
               Path_From := Mirror.Remote_Path;
               Path_To   := Mirror.Local_Path;
            end if;
            Mirror := Mirror.Next;
         end loop;

         --  everything's fine as long as To is not a remote server
         if not Servers (To).Is_Local then
            --  Path_To points to local GPS path.
            --  Translate it to remote To path
            declare
               GPS_Path : constant String_Ptr := Path_To;
            begin
               Path_To := null;
               Mirror := Servers (To).Mirror_Path_List;
               while Mirror /= null loop
                  --  match in a case insensitive manner as insensitive
                  --  paths are stored lowercase
                  if Match (Mirror.Local_Path.all, GPS_Path.all, False) then
                     Path_To := Mirror.Remote_Path;
                     exit;
                  end if;
                  Mirror := Mirror.Next;
               end loop;
            end;
         end if;
      end if;

      if Path_From = null or Path_To = null then
         return Path;
         --  ??? return error ?
      end if;

      --  At this point, we have the from and to moint points. Let's translate
      --  the path

      declare
         Subpath : String := Path
           (Path'First + Path_From'Length .. Path'Last);
         First_Slash : Boolean;
      begin
         --  convert subpath to unix style
         case Servers (From).Filesystem is
         when Unix | Windows_Cygwin =>
            null;
         when Windows =>
            for J in Subpath'Range loop
               if Subpath (J) = '\' then
                  Subpath (J) := '/';
               end if;
            end loop;
         when VMS =>
            for J in Subpath'Range loop
               if Subpath (J) = '.' then
                  Subpath (J) := '/';
               elsif Subpath (J) = ']' then
                  Subpath (J) := '/';
                  exit;
               end if;
            end loop;
         end case;

         --  convert unix style subpath to target style
         if not Unix_Style then
            case Servers (To).Filesystem is
            when Unix | Windows_Cygwin =>
               null;
            when Windows =>
               for J in Subpath'Range loop
                  if Subpath (J) = '/' then
                     Subpath (J) := '\';
                  end if;
               end loop;
            when VMS =>
               First_Slash := True;
               for J in reverse Subpath'Range loop
                  if Subpath (J) = '/' then
                     if First_Slash then
                        First_Slash := False;
                        Subpath (J) := ']';
                     else
                        Subpath (J) := '.';
                     end if;
                  end if;
               end loop;
            end case;
         end if;
         --  now, we have subpath in target style.
         --  Check, if unix style is used, the from path

         if not Unix_Style then
            declare
               Path : String := Path_To.all & Subpath;
            begin
               --  If we have a VMS path, at this point we get
               --  device:[foo.foo2]bar]file
               --  replace the first closing bracket by a dot
               if Servers (To).Filesystem = VMS then
                  for J in Path'Range loop
                     if Path (J) = ']' then
                        Path (J) := '.';
                        exit;
                     end if;
                  end loop;
                  return Path;
               else
                  return Path;
               end if;
            end;
         else
            --  Unix style case
            case Servers (To).Filesystem is
               when Unix | Windows_Cygwin =>
                  return Path_To.all & Subpath;
               when Windows =>
                  declare
                     Base_Dir : String := Path_To.all;
                  begin
                     for J in Base_Dir'Range loop
                        if Base_Dir (J) = '\' then
                           Base_Dir (J) := '/';
                        end if;
                     end loop;
                     Base_Dir (Base_Dir'First .. Base_Dir'First + 1)
                       := "/" & Base_Dir (Base_Dir'First);
                     return Base_Dir & Subpath;
                  end;
               when VMS =>
                  declare
                     Base_Dir : String := Path_To.all;
                     Dir_Index : Natural := 0;
                  begin
                     for J in Base_Dir'Range loop
                        --  transform device:[foo by /device/foo
                        if Dir_Index = 0 and Base_Dir (J) = ':' then
                           Base_Dir (Base_Dir'First .. J + 1) :=
                             "/" & Base_Dir (Base_Dir'First .. J - 1) & "/";
                           Dir_Index := J - 1;
                        end if;
                        if Base_Dir (J) = '.' or Base_Dir (J) = ']' then
                           Base_Dir (J) := '/';
                        end if;
                     end loop;
                     return Base_Dir & Subpath;
                  end;
            end case;
         end if;
      end;
   end Convert;

   ------------------
   -- To_Unix_Path --
   ------------------

   function To_Unix_Path (Path   : String;
                          Server : Server_Type) return String
   is
      The_Path : String := Path;
      Device_Found : Boolean := False;
   begin
      case Servers (Server).Filesystem is
         when Unix | Windows_Cygwin =>
            null;

         when VMS =>
            for J in The_Path'Range loop
               if not Device_Found and then The_Path (J) = ':' then
                  The_Path (The_Path'First .. J + 1) :=
                    '/' & The_Path (The_Path'First .. J - 1) & '/';
                  Device_Found := True;
               elsif The_Path (J) = '.' or The_Path (J) = ']' then
                  The_Path (J) := '/';
               end if;
            end loop;

         when Windows =>
            for J in The_Path'Range loop
               if not Device_Found and then The_Path (J) = ':' then
                  The_Path (The_Path'First .. J + 1) :=
                    '/' & The_Path (The_Path'First .. J - 1) & '/';
                  Device_Found := True;
               elsif The_Path (J) = '\' then
                  The_Path (J) := '/';
               end if;
            end loop;

      end case;

      return The_Path;
   end To_Unix_Path;

   -----------------
   -- Synchronize --
   -----------------

   procedure Synchronize (Kernel   : Kernel_Handle;
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
         --  both servers local. No need to synchronize.
         return;
      end if;

      --  synchronize with remote host
      Mirror := Servers (Server).Mirror_Path_List;
      while Mirror /= null loop
         if Mirror.Need_Sync then
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
               Trace (Me, "run sync hook for " &
                      Data.Src_Path);
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

   -----------
   -- Spawn --
   -----------

   procedure Spawn
     (Kernel           : Kernel_Handle;
      Arguments        : GNAT.OS_Lib.Argument_List;
      Server           : Server_Type;
      Pd               : out GNAT.Expect.Process_Descriptor_Access;
      Success          : out Boolean;
      Use_Ext_Terminal : Boolean := False;
      Console          : Interactive_Consoles.Interactive_Console := null;
      Show_Command     : Boolean := True;
      Directory        : String := "")
   is
      Exec         : String_Access;
      Old_Dir      : String_Access;
      Args         : Argument_List_Access;
      New_Args     : Argument_List_Access;
      L_Args       : Argument_List_Access := null;
      function Check_Exec (Exec : String) return String_Access;
      --  checks that executable is on the path, and return the full path if
      --  found, else null is returned

      ----------------
      -- Check_Exec --
      ----------------
      function Check_Exec (Exec : String) return String_Access
      is
         Full_Exec : String_Access;
      begin
         Full_Exec := Locate_Exec_On_Path (Exec);
         if Full_Exec = null then
            Insert
              (Kernel,
               -"Could not locate executable on path: " & Exec,
               Mode => Error);
            return null;
         end if;
         return Full_Exec;
      end Check_Exec;

   begin
      --  first verify the executable to be launched
      if Is_Local (Server) then
         Exec := Check_Exec (Arguments (Arguments'First).all);
         if Exec = null then
            return;
         end if;
         Args := new Argument_List'
           ((1 => Exec) &
            Clone (Arguments (Arguments'First + 1 .. Arguments'Last)));
      else
         Args := new Argument_List'(Clone (Arguments));
      end if;

      if Servers (Server).Filesystem = VMS then
         --  in VMS case, -X arguments shall be quoted. Perform a manual deep
         --  copy.

         --  ??? in some cases (user-defined images), we need to launch MCR
         --  to execute the built program... need to find a way to
         --  automatically do this
         New_Args := new Argument_List'(Args.all);
         for I in New_Args'Range loop
            if New_Args (I) (New_Args (I)'First) = '-' then
               New_Args (I) := new String'("""" &
                                           New_Args (I).all & """");
            else
               New_Args (I) := new String'(New_Args (I).all);
            end if;
         end loop;
         Free (Args);
         Args := New_Args;
      end if;

      if Servers (GPS_Server).Filesystem = Windows then
         --  Windows commands are launched using "cmd /c the_command"
         L_Args := new Argument_List'(
           (new String'("cmd"),
            new String'("/c")));
      end if;

      if Console /= null and then Show_Command then
         Insert (Console,
                 Get_Nickname (Server) & "> " &
                 Argument_List_To_String (Arguments),
                 Add_LF => True);
      end if;

      if Servers (Server).Is_Local then
         Pd := new GNAT.Expect.TTY.TTY_Process_Descriptor;

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

         Pd := new Remote_Process_Descriptor;
         if Active (Me) then
            Trace (Me, "Remote Spawning " &
                   Argument_List_To_String (Args.all));
         end if;

         if Directory = "" then
            declare
               R_Dir : constant String
                 := Convert (Get_Current_Dir,
                             GPS_Server,
                             Server);
            begin
               --  if current_dir matches a remote² directory let's
               --  set it as working directory on remote machine
               if R_Dir /= Get_Current_Dir then
                  Old_Dir := new String'(R_Dir);
               else
                  Old_Dir := new String'("");
               end if;
            end;
         else
            Old_Dir := new String'(Directory);
         end if;

         --  Set buffer_size to 0 for dynamically allocated buffer
         --  (prevents possible overflow)
         if L_Args /= null then
            Remote_Spawn
              (Remote_Process_Descriptor (Pd.all),
               Target_Name         => Servers (Server).Network_Name.all,
               Target_Identifier   => Server_Type'Image (Server),
               Args                => Args.all,
               Local_Args          => L_Args.all,
               Execution_Directory => Old_Dir.all,
               User_Name           => Servers (Server).User_Name.all,
               Launch_Timeout      => Servers (Server).Timeout,
               Err_To_Out          => True);
            Free (L_Args);
         else
            Remote_Spawn
              (Remote_Process_Descriptor (Pd.all),
               Target_Name         => Servers (Server).Network_Name.all,
               Target_Identifier   => Server_Type'Image (Server),
               Args                => Args.all,
               Local_Args          => (1 .. 0 => null),
               Execution_Directory => Old_Dir.all,
               User_Name           => Servers (Server).User_Name.all,
               Launch_Timeout      => Servers (Server).Timeout,
               Err_To_Out          => True);
         end if;
         Free (Old_Dir);
      end if;

      Success := True;

      Free (Args);

   exception
      when E : Invalid_Process | Process_Died =>
         Success := False;
         GPS.Kernel.Console.Insert
           (Kernel, -"Invalid command (" &
            Ada.Exceptions.Exception_Message (E) &
            ")", Mode => Error);
   end Spawn;

   ----------------
   -- Add_Server --
   ----------------

   procedure Add_Or_Replace_Server_Config
     (Nickname      : String;
      Old_Nickname  : String;
      Network_Name  : String;
      Filesystem    : Filesystem_Type;
      Remote_Access : String;
      Remote_Shell  : String;
      User_Name     : String := "";
      Timeout       : Natural := 5000;
      Success       : out Boolean)
   is
      Item : Server_Config := null;
   begin
      Success := True;
      --  ??? We should first check that Remote_Access exists and return
      --  False in this case

      --  Check if the server is already defined
      if Old_Nickname /= "" then
         Item := Config_List.Next;
         --  Skip the first item which is the local server
         while Item /= null loop
            exit when Item.Nickname.all = Old_Nickname;
            Item := Item.Next;
         end loop;

         --  If a server with specified old_nickname could not be found,
         --  return here with appropriate status.
         if Item = null then
            Success := False;
            return;
         end if;
      end if;

      --  If Server is null, then it is not already defined
      --  We create a new Server_Item then and add it at the end of the
      --  servers list.
      if Item = null then
         Item := Config_List;
         while Item.Next /= null loop
            Item := Item.Next;
         end loop;
         Item.Next := new Server_Config_Record;
         Nb_Servers  := Nb_Servers + 1;
         Item := Item.Next;
      else
         --  Replace Item's values... free previous ones
         Free (Item.Nickname);
         Free (Item.Network_Name);
         Free (Item.Remote_Access);
         Free (Item.User_Name);
      end if;

      --  Let's set the new values of the server
      Item.Is_Local      := False;
      Item.Filesystem    := Filesystem;
      Item.Timeout       := Timeout;
      Item.Nickname      := new String'(Nickname);
      Item.Network_Name  := new String'(Network_Name);
      Item.Remote_Access := new String'(Remote_Access);
      Item.Remote_Shell  := new String'(Remote_Shell);
      Item.User_Name     := new String'(User_Name);

      --  ??? Add Hook concerning the servers list that changed...
   end Add_Or_Replace_Server_Config;

   ---------------------
   -- Add_Mirror_Path --
   ---------------------

   procedure Add_Mirror_Path
     (Nickname    : String;
      GPS_Ref     : String;
      Remote_Path : String;
      Need_Sync   : Boolean := False)
   is
      Item : Server_Config := Config_List.Next;
      Mirror : Mirror_Path;
   begin
      while Item /= null loop
         if Item.Nickname.all = Nickname then
            Mirror := Item.Mirror_Path_List;
            Item.Mirror_Path_List := new Mirror_Path_Record'
              (Local_Path  => new String'(GPS_Ref),
               Remote_Path => new String'(Remote_Path),
               Need_Sync   => Need_Sync,
               Next        => Mirror);
            exit;
         end if;
         Item := Item.Next;
      end loop;
   end Add_Mirror_Path;

   ------------
   -- Assign --
   ------------

   procedure Assign
     (Server   : Server_Type;
      Nickname : String)
   is
      Item : Server_Config := Config_List;
   begin
      while Item /= null loop
         if Item.Nickname.all = Nickname then
            Trace (Me, "Server " & Server_Type'Image (Server) &
                   " assigned to config " & Nickname);
            Servers (Server) := Item;
            return;
         end if;
         Item := Item.Next;
      end loop;
   end Assign;

   ---------------------------------
   -- Get_Number_Of_Server_Config --
   ---------------------------------

   function Get_Number_Of_Server_Config return Server_Id is
   begin
      return Nb_Servers;
   end Get_Number_Of_Server_Config;

   ------------------
   -- Get_Nickname --
   ------------------

   function Get_Nickname (Id : Server_Id) return String
   is
      Config : Server_Config := Config_List;
   begin
      for J in 2 .. Id loop
         Config := Config.Next;
         exit when Config = null;
      end loop;
      if Config = null then
         return "";
      else
         return Config.Nickname.all;
      end if;
   end Get_Nickname;

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
      return Servers (Server).Network_Name.all;
   end Get_Network_Name;

   --------------
   -- Is_Local --
   --------------

   function Is_Local (Server : Server_Type) return Boolean is
   begin
      return Servers (Server).Is_Local;
   end Is_Local;

begin

   Initialize;
   --  ??? remove once the Initialize procedure is actually called at
   --  config time.

end GPS.Kernel.Remote;
