------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2001-2026, AdaCore                     --
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

with Ada.Containers.Vectors;

with Glib.Main;

with GNATCOLL.Arg_Lists;
with GNATCOLL.OS.Process;
with GNATCOLL.Projects;  use GNATCOLL.Projects;
with GNATCOLL.Traces;    use GNATCOLL.Traces;
with GNATCOLL.Templates; use GNATCOLL.Templates;
with GNATCOLL.VFS;       use GNATCOLL.VFS;

with Spawn.Environments;

with Basic_Types;         use Basic_Types;
with Commands.Interactive;
use Commands, Commands.Interactive;
with Default_Preferences.Enums;
use Default_Preferences;
with GPS.Intl;            use GPS.Intl;
with GPS.Kernel.Actions;  use GPS.Kernel.Actions;
with GPS.Kernel.Contexts; use GPS.Kernel.Contexts;
with GPS.Kernel.Modules;  use GPS.Kernel.Modules;
with GPS.Kernel.Project;  use GPS.Kernel.Project;
with GPS.Kernel;          use GPS.Kernel;
with Toolchains_Old;      use Toolchains_Old;

package body External_Editor_Module is

   Me : constant Trace_Handle :=
     Create ("GPS.SOURCE_EDITOR.EXTERNAL_EDITOR_MODULE");

   Timeout : constant Glib.Guint := 500;
   --  Timeout in millisecond to check the external editor processes.
   --  This is intentionnaly very big currently, since all we want is to
   --  properly handle the death of an external process, to remove zombie
   --  processes, but we do not process the actual output.

   -----------------
   -- Preferences --
   -----------------

   type Supported_Clients is
     (Auto, Gnuclient, Emacsclient, Emacs, Vim, Vi, Custom);
   package Supported_Client_Preferences is new
     Default_Preferences.Enums.Generics (Supported_Clients);
   --  The list of supported external editors.

   Default_External_Editor : Supported_Client_Preferences.Preference;
   Custom_Editor           : String_Preference;

   type Constant_String_Access is access constant String;

   --------------
   -- Commands --
   --------------

   type Edit_With_External_Command is new Interactive_Command with null record;
   overriding
   function Execute
     (Command : access Edit_With_External_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

   -----------
   -- Types --
   -----------

   type External_Client is record
      Command_Name : Constant_String_Access;
      --  The command to load a file.
      --  The following substitutions are provided:
      --     %l = line to display
      --     %c = column to display
      --     %f = file to display
      --     %e = extended lisp command
      --     %p = top level project file name
      --     %% = percent sign ('%')

      Lisp_Command_Name : Constant_String_Access;
      --  The command to use to emit any lisp command, null if this is not
      --  possible. Same substitutions as above

      Server_Start_Command : Constant_String_Access;
      --  The lisp command to provide to Emacs to start the server.
      --  This code will not attempt to start a server if this is
      --  null. Otherwise, if Command_Name fails, then Emacs is started with
      --  the lisp command Server_Start_Command, and then Command_Name is
      --  attempted again.
      --  If this is null, then Command_Name is started in the background, so
      --  as not to block the current process.

      Extra_Test : Constant_String_Access;
      --  Name of an extra executable to look for on the executable path to
      --  test whether this client is available. This should be used in cases
      --  where Command_Name starts a terminal that contains the editor
   end record;

   type External_Clients is array (Supported_Clients) of External_Client;

   type External_Server is record
      Command_Name : Constant_String_Access;
      --  The command to start a server.
      --  The following substitutions are provided:
      --     %e = lisp command to use to start the server. This is provided
      --          automatically by the client
      --     %% = percent sign ('%')
   end record;

   type Supported_Servers is (Glide, Emacs);
   type External_Servers is array (Supported_Servers) of External_Server;

   --  We use constant strings, to avoid allocating memory below, and thus
   --  avoid memory leaks.
   Gnuclient_Command : aliased constant String := "gnuclient -q +%l %f";
   Gnuclient_Lisp    : aliased constant String := "gnudoit -q %e";
   Gnuclient_Server  : aliased constant String :=
     "(progn (load-library ""gnuserv"") (gnuserv-start)"
     & "(require 'ada-mode)(ada-reread-prj-file ""%p""))";
   --  Load the project file right away. Doing it with gnuclient isn't doable,
   --  and it would be less efficient to always gnudoit even for simply loading
   --  a file.

   Emacsclient_Command : aliased constant String := "emacsclient -n +%l:%c %f";
   Emacsclient_Server  : aliased constant String :=
     "(progn (server-start) (require 'ada-mode)"
     & "(ada-reread-prj-file ""%p""))";
   --  Try to load the project file right away, since we won't be able to do
   --  that later with emacsclient. This means that the server needs to be
   --  restarted if the project file changes

   Emacs_Command : aliased constant String := "emacs +%l %f";

   Vim_Command : aliased constant String :=
     "xterm -geometry 80x50 -e vim +%l %f";
   Vim_Extra   : aliased constant String := "vim";

   Vi_Command : aliased constant String :=
     "xterm -geometry 80x50 -e vi +%l %f";
   Vi_Extra   : aliased constant String := "vi";

   Custom_Command : aliased constant String := "<custom>";

   Clients : constant External_Clients :=
     (Auto        => (others => null),
      Gnuclient   =>
        (Command_Name         => Gnuclient_Command'Access,
         Lisp_Command_Name    => Gnuclient_Lisp'Access,
         Server_Start_Command => Gnuclient_Server'Access,
         others               => null),
      Emacsclient =>
        (Command_Name         => Emacsclient_Command'Access,
         Server_Start_Command => Emacsclient_Server'Access,
         others               => null),
      Emacs       => (Command_Name => Emacs_Command'Access, others => null),
      Vim         =>
        (Command_Name => Vim_Command'Access,
         Extra_Test   => Vim_Extra'Access,
         others       => null),
      Vi          =>
        (Command_Name => Vi_Command'Access,
         Extra_Test   => Vi_Extra'Access,
         others       => null),
      Custom      => (Command_Name => Custom_Command'Access, others => null));

   Glide_Command        : aliased constant String :=
     "glide -emacs --eval -emacs %e";
   Emacs_Server_Command : aliased constant String := "emacs --eval %e";

   Servers : constant External_Servers :=
     (Glide => (Command_Name => Glide_Command'Access),
      Emacs => (Command_Name => Emacs_Server_Command'Access));

   subtype Process_Handle is GNATCOLL.OS.Process.Process_Handle;
   package Process_Handle_Vectors is new
     Ada.Containers.Vectors
       (Index_Type   => Positive,
        Element_Type => Process_Handle,
        "="          => GNATCOLL.OS.Process."=");
   subtype Process_Handle_List is Process_Handle_Vectors.Vector;

   subtype Arg_List is GNATCOLL.Arg_Lists.Arg_List;

   ------------------------
   -- Module description --
   ------------------------

   type External_Editor_Module_Record is new Module_ID_Record with record
      Client : Supported_Clients := Auto;
      --  The index of the client we are currently using

      Command_Args : aliased Arg_List := GNATCOLL.Arg_Lists.Empty_Command_Line;
      --  Command for current process

      Environment : GNATCOLL.OS.Process.Environment_Dict;
      --  User environment captured at module registration, before spawning.

      Processes : Process_Handle_List := Process_Handle_Vectors.Empty_Vector;
      --  The list of external processes that have been started.

      Timeout_Id : Glib.Main.G_Source_Id := 0;
      --  Timeout handles asynchronous polling
   end record;
   type External_Editor_Module_Record_Id is
     access all External_Editor_Module_Record'Class;

   External_Editor_Module_Id   : External_Editor_Module_Record_Id;
   External_Editor_Module_Name : constant String := "External_Editor";

   -----------------
   -- Subprograms --
   -----------------

   function Editor_Environment
     (Kernel : access Kernel_Handle_Record'Class)
      return GNATCOLL.OS.Process.Environment_Dict;
   --  Convert the saved user environment without inheriting IDE overrides.

   function To_Argument_List
     (Args : Arg_List) return GNATCOLL.OS.Process.Argument_List;
   --  Use GNATCOLL.Args_List.Arg_List when building commands,
   --  convert to Argument_List for GNATCOLL.OS.Process spawns

   function Tool_Is_In_Path
     (Command : String;
      Args    : out Arg_List;
      Result  : out GNATCOLL.VFS.Virtual_File) return Boolean;
   --  True if the tool invoked in Command is available in PATH
   --  Writes parsed command to Args and tool path to Result

   procedure Spawn_Server
     (Kernel : access Kernel_Handle_Record'Class; Success : out Boolean);
   --  Start Emacs and the server, so that a client can connect to it.
   --  False is returned if the server could not be started.

   function Client_Command
     (Kernel        : access Kernel_Handle_Record'Class;
      File          : Virtual_File := No_File;
      Line          : Natural := 1;
      Column        : Visible_Column_Type := 1;
      Extended_Lisp : String := "") return Boolean;
   --  Calls the client with the appropriate parameters

   procedure Select_Client (Kernel : access Kernel_Handle_Record'Class);
   --  Select the appropriate external editor to use.
   --  Editors are checked in the order given by Clients,
   --  and selected when found on the PATH.
   --  If Lisp_Command_Name is specified, this must also be on PATH
   --  in order to select the editor.

   procedure Substitute (Args : in out Arg_List; F, C, L, E, P : String := "");
   --  Does all the substitutions in Args for %f, %c, %l, %e and %p.

   function External_Timeout return Boolean;
   --  Timeout callback to monitor and reap processes

   procedure Spawn_New_Process
     (Process_Args : in out Arg_List; Result : out Boolean);
   --  Spawn a new process, and store it in External_Clients, so that we can
   --  properly handle its termination.

   function Blocking_Spawn (Args : Arg_List) return Integer;
   --  Spawn a new process, and waits for its termination.
   --  It hides both its standard output and standard error.
   --  As the call is blocking, the process handle is never stored

   ------------------------
   -- Editor_Environment --
   ------------------------

   function Editor_Environment
     (Kernel : access Kernel_Handle_Record'Class)
      return GNATCOLL.OS.Process.Environment_Dict
   is
      Original : constant Spawn.Environments.Process_Environment :=
        Kernel.Get_Original_Environment;
      Result   : GNATCOLL.OS.Process.Environment_Dict;
   begin
      for Name of Original.Keys loop
         Result.Insert (Name, Original.Value (Name));
      end loop;
      return Result;
   end Editor_Environment;

   ----------------------
   -- To_Argument_List --
   ----------------------

   function To_Argument_List
     (Args : Arg_List) return GNATCOLL.OS.Process.Argument_List
   is
      use GNATCOLL.Arg_Lists;

      Result : GNATCOLL.OS.Process.Argument_List;
   begin
      if Args.Args_Length < 0 then
         return Result;
      end if;
      for I in 0 .. Args.Args_Length loop
         Result.Append (Nth_Arg (Args, I));
      end loop;

      return Result;
   end To_Argument_List;

   ---------------------
   -- Tool_Is_In_Path --
   ---------------------

   function Tool_Is_In_Path
     (Command : String;
      Args    : out Arg_List;
      Result  : out GNATCOLL.VFS.Virtual_File) return Boolean
   is
      use GNATCOLL.Arg_Lists;
   begin
      Args := Parse_String (Command, Separate_Args);
      if Args.Args_Length < 0 then
         return False;
      end if;
      Result := Locate_Tool_Executable (+Get_Command (Args));
      return Result /= GNATCOLL.VFS.No_File;
   end Tool_Is_In_Path;

   -------------------
   -- Select_Client --
   -------------------

   procedure Select_Client (Kernel : access Kernel_Handle_Record'Class) is
      Default_Client : constant Supported_Clients :=
        Supported_Clients'Val (Default_External_Editor.Get_Pref);
      Match          : Boolean := False;
      Args           : GNATCOLL.Arg_Lists.Arg_List;
      Tool           : GNATCOLL.VFS.Virtual_File;

      Editor_Args renames External_Editor_Module_Id.Command_Args;

      function Full_Cmd (Client : Supported_Clients) return String
      is ((if Client in Custom
           then Get_Pref (Custom_Editor)
           else Clients (Client).Command_Name.all));
      --  Select one of predefined editor command, or user-configured command

      function Is_Client_Available (C : Supported_Clients) return Boolean;
      --  Parse the client command and check if the executable is on the PATH.
      --  Also check this for additional commands or tools,
      --  e.g. lisp commands or terminal utilities

      -------------------------
      -- Is_Client_Available --
      -------------------------

      function Is_Client_Available (C : Supported_Clients) return Boolean is
         Client    : constant External_Client := Clients (C);
         Cmd_Str   : constant String := Full_Cmd (C);
         Lisp_Args : Arg_List;
         Lisp_Tool : Virtual_File;
      begin
         --  Invalid editor configuration
         if Cmd_Str = "" then
            Insert
              (Kernel,
               "Invalid external editor configuration for " & C'Img,
               Mode => Error);
            return False;
         end if;
         if not Tool_Is_In_Path (Cmd_Str, Args, Tool) then
            return False;
         end if;
         if Client.Lisp_Command_Name /= null
           and then
             not Tool_Is_In_Path
                   (Client.Lisp_Command_Name.all, Lisp_Args, Lisp_Tool)
         then
            return False;
         end if;
         if Client.Extra_Test /= null
           and then Locate_Tool_Executable (+Client.Extra_Test.all) in No_File
         then
            return False;
         end if;
         return True;
      end Is_Client_Available;
   begin
      --  If the user has specified a default client, use that one.
      --  If selected client cannot be found, fail early
      if Default_Client /= Auto then
         if Is_Client_Available (Default_Client) then
            Trace (Me, "User configured client: " & Full_Cmd (Default_Client));
            External_Editor_Module_Id.Client := Default_Client;
            Args.Set_Nth_Arg (0, Tool.Display_Full_Name);
            Editor_Args := Args;
         else
            Trace (Me, Args.Get_Command & " not available.");
            Insert
              (Kernel,
               """"
               & Full_Cmd (Default_Client)
               & """ not available. Check for tools in PATH.",
               Mode => Error);
            Editor_Args := GNATCOLL.Arg_Lists.Empty_Command_Line;
         end if;
         return;
      end if;

      --  Otherwise, loop through clients until one is available in PATH
      External_Editor_Module_Id.Client := Auto;
      Args := GNATCOLL.Arg_Lists.Empty_Command_Line;

      for C in Clients'Range when Clients (C).Command_Name /= null loop
         Match := Is_Client_Available (C);
         --  Expand tool name to full path
         if Match then
            External_Editor_Module_Id.Client := C;
            Args.Set_Nth_Arg (0, Tool.Display_Full_Name);
            Editor_Args := Args;
            exit;
         else
            --  Reset temporary args
            Args := GNATCOLL.Arg_Lists.Empty_Command_Line;
         end if;
      end loop;

      if External_Editor_Module_Id.Client /= Auto
        and then Editor_Args not in GNATCOLL.Arg_Lists.Empty_Command_Line
      then
         Trace
           (Me,
            "Current client is "
            & Clients (External_Editor_Module_Id.Client).Command_Name.all);
      else
         Trace (Me, "No available client");
      end if;
   end Select_Client;

   ----------------
   -- Substitute --
   ----------------

   procedure Substitute (Args : in out Arg_List; F, C, L, E, P : String := "")
   is
      Substrings : Substitution_Array :=
        (1 => (Name => new String'("f"), Value => new String'(F)),
         2 => (Name => new String'("c"), Value => new String'(C)),
         3 => (Name => new String'("l"), Value => new String'(L)),
         4 => (Name => new String'("e"), Value => new String'(E)),
         5 => (Name => new String'("p"), Value => new String'(P)),
         6 => (Name => new String'("%"), Value => new String'("%")));
   begin
      --  Args_Length is the highest index, so 0 means no args, only command
      for A in 1 .. Args.Args_Length loop
         declare
            Arg_Str  : constant String := Args.Nth_Arg (A);
            Expanded : constant String :=
              Substitute
                (Str        => Arg_Str,
                 Delimiter  => '%',
                 Substrings => Substrings,
                 Recursive  => True);
         begin
            if Expanded /= Arg_Str then
               Args.Set_Nth_Arg (A, Expanded);
            end if;
         end;
      end loop;

      Free (Substrings);
   end Substitute;

   ----------------------
   -- External_Timeout --
   ----------------------

   function External_Timeout return Boolean is
      use Process_Handle_Vectors;
      use GNATCOLL.OS.Process;
      Editor_Pids renames External_Editor_Module_Id.Processes;

      State  : Process_State;
      Handle : Process_Handle;
      Code   : Integer;
   begin
      for C in reverse Editor_Pids.First_Index .. Editor_Pids.Last_Index loop
         Handle := Editor_Pids (C);
         State := Handle.State;

         --  Remove process handles once complete
         --  Some editors return immediately after spawning
         --  as they delegate to a child thread
         --  while others only return once the editor is closed
         case State is
            when WAITABLE   =>
               --  Wait is non-blocking here
               Code := Handle.Wait;
               Editor_Pids.Delete (C);
               if Code = 127 then
                  Trace (Me, "Editor process spawned but command failed.");
               else
                  Trace (Me, "Editor process returned: " & Code'Img);
               end if;

            when TERMINATED =>
               Editor_Pids.Delete (C);
               Trace (Me, "Editor process terminated");

            when RUNNING    =>
               null;
         end case;

      end loop;

      if External_Editor_Module_Id.Processes.Is_Empty then
         Trace (Me, "Last external editor returned");
         External_Editor_Module_Id.Timeout_Id := 0;
         return False;
      else
         return True;
      end if;
   end External_Timeout;

   -----------------------
   -- Spawn_New_Process --
   -----------------------

   procedure Spawn_New_Process
     (Process_Args : in out Arg_List; Result : out Boolean)
   is
      Handle : Process_Handle;
   begin
      if Active (Me) then
         Trace
           (Me,
            "Spawn: "
            & Process_Args.To_Display_String (Include_Command => True));
      end if;

      Handle :=
        GNATCOLL.OS.Process.Start
          (Args        => To_Argument_List (Process_Args),
           Env         => External_Editor_Module_Id.Environment,
           Inherit_Env => False);
      External_Editor_Module_Id.Processes.Append (Handle);

      if External_Editor_Module_Id.Timeout_Id in Glib.Main.No_Source_Id then
         External_Editor_Module_Id.Timeout_Id :=
           Glib.Main.Timeout_Add
             (Interval => Timeout, Func => External_Timeout'Access);
      end if;

      Result := True;
      return;

   exception
      when others =>
         Result := False;
         return;
   end Spawn_New_Process;

   ------------------
   -- Spawn_Server --
   ------------------

   procedure Spawn_Server
     (Kernel : access Kernel_Handle_Record'Class; Success : out Boolean)
   is
      Path        : Virtual_File;
      Client_Cmd  : constant Constant_String_Access :=
        Clients (External_Editor_Module_Id.Client).Server_Start_Command;
      Server_Args : aliased GNATCOLL.Arg_Lists.Arg_List;
   begin
      Success := False;
      if Client_Cmd = null then
         Trace (Me, "No server start command specified");
         return;
      end if;

      for S in Servers'Range loop
         if Tool_Is_In_Path
              (Command => Servers (S).Command_Name.all,
               Args    => Server_Args,
               Result  => Path)
         then
            if Server_Args.Args_Length > 0 then
               Substitute
                 (Args => Server_Args,
                  P    => Get_Project (Kernel).Name,
                  E    => Client_Cmd.all);
            end if;

            Trace (Me, "Spawning server: " & Server_Args.Get_Command);
            Server_Args.Set_Nth_Arg (0, Path.Display_Full_Name);
            Spawn_New_Process (Server_Args, Success);
            exit;
         end if;
      end loop;

      if not Success then
         Trace (Me, "Failed to spawn server.");
      end if;

      return;
   end Spawn_Server;

   --------------------
   -- Blocking_Spawn --
   --------------------

   function Blocking_Spawn (Args : Arg_List) return Integer is
   begin
      return
        GNATCOLL.OS.Process.Run
          (Args        => To_Argument_List (Args),
           Env         => External_Editor_Module_Id.Environment,
           Inherit_Env => False);
   exception
      when others =>
         return 1;
   end Blocking_Spawn;

   --------------------
   -- Client_Command --
   --------------------

   function Client_Command
     (Kernel        : access Kernel_Handle_Record'Class;
      File          : Virtual_File := No_File;
      Line          : Natural := 1;
      Column        : Visible_Column_Type := 1;
      Extended_Lisp : String := "") return Boolean
   is
      Editor_Args renames External_Editor_Module_Id.Command_Args;

      Max_Tries : constant := 5;
      Line_Str  : constant String := Natural'Image (Line);
      Col_Str   : constant String := Visible_Column_Type'Image (Column);
      Result    : Integer := 0;
      Success   : Boolean;
   begin

      Substitute
        (Editor_Args,
         P => Get_Project (Kernel).Name,
         F => +File.Full_Name,
         C => Col_Str (Col_Str'First + 1 .. Col_Str'Last),
         L => Line_Str (Line_Str'First + 1 .. Line_Str'Last),
         E => Extended_Lisp);

      if Clients (External_Editor_Module_Id.Client).Server_Start_Command
        /= null
      then
         Result := Blocking_Spawn (Editor_Args);
         --  If the command failed, it's likely that Emacs wasn't started,
         --  at least not with the server.
         if Result /= 0 then
            Spawn_Server (Kernel, Success);

            if Success then
               --  Give Emacs some time to start the server, and try again
               delay 1.0;

               for Try in 1 .. Max_Tries loop
                  Result := Blocking_Spawn (Editor_Args);
                  exit when Result = 0;
                  delay 0.5;
               end loop;
            end if;
         end if;
         Success := Result = 0;
      --  Success if the client spawns instantly,
      --  or the server spawns eventually

      else
         Spawn_New_Process (Editor_Args, Success);
      end if;

      return Success;
   end Client_Command;

   -------------
   -- Execute --
   -------------

   overriding
   function Execute
     (Command : access Edit_With_External_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);

      Editor_Args renames External_Editor_Module_Id.Command_Args;

      Line   : Integer := 1;
      Column : Visible_Column_Type := 1;
   begin
      Editor_Args := GNATCOLL.Arg_Lists.Empty_Command_Line;
      Trace
        (Me,
         "Edit file with external editor "
         & Display_Full_Name (File_Information (Context.Context)));

      if Has_Entity_Column_Information (Context.Context) then
         Line := Line_Information (Context.Context);
         Column := Entity_Column_Information (Context.Context);
      end if;

      --  Find executable and parse args
      Select_Client (Get_Kernel (Context.Context));

      if Editor_Args in GNATCOLL.Arg_Lists.Empty_Command_Line then
         Trace (Me, "Could not resolve external client.");
         return Commands.Failure;
      end if;

      if Client_Command
           (Kernel => Get_Kernel (Context.Context),
            File   => File_Information (Context.Context),
            Line   => Line,
            Column => Column)
      then
         return Commands.Success;
      else
         return Commands.Failure;
      end if;
   exception
      when E : others =>
         Trace (Me, E);
         return Commands.Failure;
   end Execute;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Manager : constant Preferences_Manager := Kernel.Get_Preferences;
   begin
      External_Editor_Module_Id := new External_Editor_Module_Record;
      --  Load user's original environment
      External_Editor_Module_Id.Environment := Editor_Environment (Kernel);

      --  Create the preferences

      Default_External_Editor :=
        Supported_Client_Preferences.Create
          (Manager => Manager,
           Path    => "Editor:External Editors",
           Name    => "External-Editor-Default-Editor",
           Label   => -"External editor",
           Doc     => -"The default external editor to use",
           Default => Gnuclient);

      Custom_Editor :=
        Create
          (Manager => Manager,
           Path    => -"Editor:External Editors",
           Name    => "External-Editor-Custom-Command",
           Label   => -"Custom editor command",
           Doc     => -"Command to use for launching a custom editor",
           Default => "emacs +%l %f");

      Register_Action
        (Kernel,
         "edit with external editor",
         new Edit_With_External_Command,
         Description =>
           -("Edit the file with an external editor, as configured in the"
             & " preferences"),
         Filter      => Lookup_Filter (Kernel, "File"));

      Register_Module
        (Module      => Module_ID (External_Editor_Module_Id),
         Kernel      => Kernel,
         Module_Name => External_Editor_Module_Name,
         Priority    => Default_Priority + 1);
   end Register_Module;

end External_Editor_Module;
