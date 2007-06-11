-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2007                       --
--                             AdaCore                               --
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

with Unchecked_Deallocation;

with GNAT.Expect;               use GNAT.Expect;
pragma Warnings (Off);
with GNAT.Expect.TTY;           use GNAT.Expect.TTY;
pragma Warnings (On);
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with GNAT.Strings;

with Glib;                      use Glib;
with Glib.Properties.Creation;  use Glib.Properties.Creation;
with Glib.Generic_Properties;   use Glib.Generic_Properties;
with Gtk.Main;                  use Gtk.Main;

with Basic_Types;               use Basic_Types;
with Commands.Interactive;      use Commands, Commands.Interactive;
with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel.Console;        use GPS.Kernel.Console;
with GPS.Kernel.Contexts;       use GPS.Kernel.Contexts;
with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;
with GPS.Kernel.Modules;        use GPS.Kernel.Modules;
with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;
with GPS.Kernel.Project;        use GPS.Kernel.Project;
with GPS.Kernel;                use GPS.Kernel;
with GPS.Kernel.Timeout;        use GPS.Kernel.Timeout;
with GPS.Kernel.Standard_Hooks; use GPS.Kernel.Standard_Hooks;
with Traces;                    use Traces;
with Projects;                  use Projects;
with String_Utils;              use String_Utils;
with VFS;                       use VFS;

package body External_Editor_Module is

   Me : constant Debug_Handle := Create ("External_Editor_Module");

   Timeout : constant Guint32 := 500;
   --  Timeout in millisecond to check the external editor processes.
   --  This is intentionnaly very big currently, since all we want is to
   --  properly handle the death of an external process, to remove zombie
   --  processes, but we do not process the actual output.

   -----------------
   -- Preferences --
   -----------------

   type Supported_Clients is
     (Auto, Gnuclient, Emacsclient, Emacs, Vim, Vi, Custom);
   for Supported_Clients'Size use Gint'Size;
   pragma Convention (C, Supported_Clients);
   --  The list of supported external editors.

   package Supported_Client_Properties is new Generic_Enumeration_Property
     ("Supported_Clients", Supported_Clients);

   Default_External_Editor    : Param_Spec_Enum;
   Custom_Editor              : Param_Spec_String;
   Always_Use_External_Editor : Param_Spec_Boolean;

   type Constant_String_Access is access constant String;

   --------------
   -- Commands --
   --------------

   type Edit_With_External_Command is new Interactive_Command with null record;
   function Execute
     (Command : access Edit_With_External_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

   -----------
   -- Types --
   -----------

   type External_Client is record
      Command_Name      : Constant_String_Access;
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

   Emacs_Command       : aliased constant String := "emacs +%l %f";

   Vim_Command         : aliased constant String :=
     "xterm -geometry 80x50 -exec vim +%l %f";
   Vim_Extra           : aliased constant String := "vim";

   Vi_Command          : aliased constant String :=
     "xterm -geometry 80x50 -exec vi +%l %f";
   Vi_Extra            : aliased constant String := "vi";

   Custom_Command      : aliased constant String := "<custom>";

   Clients : constant External_Clients :=
     (Auto      => (null, null, null, null),
      Gnuclient =>
        (Command_Name         => Gnuclient_Command'Access,
         Lisp_Command_Name    => Gnuclient_Lisp'Access,
         Server_Start_Command => Gnuclient_Server'Access,
         Extra_Test           => null),

      Emacsclient =>
        (Command_Name         => Emacsclient_Command'Access,
         Lisp_Command_Name    => null,
         Server_Start_Command => Emacsclient_Server'Access,
         Extra_Test           => null),

      Emacs =>
        (Command_Name         => Emacs_Command'Access,
         Lisp_Command_Name    => null,
         Server_Start_Command => null,
         Extra_Test           => null),

      Vim =>
        (Command_Name         => Vim_Command'Access,
         Lisp_Command_Name    => null,
         Server_Start_Command => null,
         Extra_Test           => Vim_Extra'Access),

      Vi =>
        (Command_Name         => Vi_Command'Access,
         Lisp_Command_Name    => null,
         Server_Start_Command => null,
         Extra_Test           => Vi_Extra'Access),

      Custom =>
        (Command_Name         => Custom_Command'Access,
         Lisp_Command_Name    => null,
         Server_Start_Command => null,
         Extra_Test           => null));

   Glide_Command : aliased constant String := "glide -emacs --eval -emacs %e";
   Emacs_Server_Command : aliased constant String := "emacs --eval %e";

   Servers : constant External_Servers :=
     (Glide => (Command_Name => Glide_Command'Access),
      Emacs => (Command_Name => Emacs_Server_Command'Access));

   type Process_Descriptor_Array is array (Positive range <>)
     of Process_Descriptor_Access;
   type Process_Descriptor_Array_Access is access
     Process_Descriptor_Array;

   ------------------------
   -- Module description --
   ------------------------

   type External_Editor_Module_Record is new Module_ID_Record with record
      Client : Supported_Clients := Auto;
      --  The index of the client we are currently using

      Processes : Process_Descriptor_Array_Access;
      --  The list of external processes that have been started.

      Timeout_Id : Gtk.Main.Timeout_Handler_Id := 0;
      --  The timeout loop that takes care of all the spawned external
      --  editors.
   end record;
   type External_Editor_Module_Record_Id is access all
     External_Editor_Module_Record'Class;

   External_Editor_Module_Id : External_Editor_Module_Record_Id;
   External_Editor_Module_Name : constant String := "External_Editor";

   -----------------
   -- Subprograms --
   -----------------

   procedure Unchecked_Free is new Unchecked_Deallocation
     (Process_Descriptor_Array, Process_Descriptor_Array_Access);

   procedure Spawn_Server
     (Kernel  : access Kernel_Handle_Record'Class;
      Success : out Boolean);
   --  Start Emacs and the server, so that a client can connect to it.
   --  False is returned if the server could not be started.

   procedure Client_Command
     (Kernel        : access Kernel_Handle_Record'Class;
      File          : String := "";
      Line          : Natural := 1;
      Column        : Visible_Column_Type := 1;
      Extended_Lisp : String := "");
   --  Calls the client with the appropriate parameters

   procedure Select_Client;
   --  Select the appropriate external editor to use. They are tested in the
   --  order given in Clients, and are selected if both Command_Name and
   --  Lisp_Command_Name are found on the path.

   procedure Substitute
     (Args : Argument_List_Access; F, C, L, E, P  : String := "");
   --  Does all the substitutions in Args for %f, %c, %l, %e and %%.

   function Open_File_Hook
     (Kernel    : access Kernel_Handle_Record'Class;
      Data      : access Hooks_Data'Class) return Boolean;
   --  Handle an edition request

   procedure Spawn_New_Process
     (Kernel  : access Kernel_Handle_Record'Class;
      Command : String;
      Args    : GNAT.OS_Lib.Argument_List;
      Result  : out Boolean);
   --  Spawn a new process, and store it in External_Clients, so that we can
   --  properly handle its termination.

   function External_Timeout (D : Process_Data) return Boolean;
   --  Timeout used to monitor the external editors. It is also used to make
   --  sure that no zombie process is left on Unix machines.

   procedure External_Timeout_Destroy (D : in out Process_Data);
   --  Called when the timeout for the external editors is terminated.

   function Blocking_Spawn
     (Command : String; Args : Argument_List) return Integer;
   --  Spawn a new process, and waits for its termination. It hides both its
   --  standard output and standard error.

   procedure Preferences_Changed (Kernel : access Kernel_Handle_Record'Class);
   --  Called when the preferences have changed.

   -------------------
   -- Select_Client --
   -------------------

   procedure Select_Client is
      Path           : GNAT.Strings.String_Access;
      Args           : Argument_List_Access;
      Match          : Boolean;
      Default_Client : constant Supported_Clients := Supported_Clients'Val
        (Get_Pref (Default_External_Editor));
   begin
      --  If the user has specified a default client, use that one.
      if Default_Client /= Auto then
         External_Editor_Module_Id.Client := Default_Client;
         return;
      end if;

      External_Editor_Module_Id.Client := Auto;

      for C in Clients'Range loop
         Match := False;

         if Clients (C).Command_Name /= null then
            declare
               Command : constant String := Clients (C).Command_Name.all;
            begin
               if Command = Custom_Command then
                  Args := Argument_String_To_List
                    (Get_Pref (Custom_Editor));

               else
                  Args := Argument_String_To_List (Command);
               end if;
            end;

            if Args'Length /= 0 then
               Path := Locate_Exec_On_Path (Args (Args'First).all);

               if Path /= null then
                  Free (Path);
                  Match := True;
               end if;
            end if;

            Free (Args.all);
            Unchecked_Free (Args);
         end if;

         if Match and then Clients (C).Lisp_Command_Name /= null then
            Args := Argument_String_To_List
              (Clients (C).Lisp_Command_Name.all);

            Path := Locate_Exec_On_Path (Args (Args'First).all);
            if Path /= null then
               Free (Path);
            else
               Match := False;
            end if;

            Free (Args.all);
            Unchecked_Free (Args);
         end if;

         if Match and then Clients (C).Extra_Test /= null then
            Path := Locate_Exec_On_Path (Clients (C).Extra_Test.all);
            if Path /= null then
               Free (Path);
            else
               Match := False;
            end if;
         end if;

         if Match then
            External_Editor_Module_Id.Client := C;
            exit;
         end if;
      end loop;

      if External_Editor_Module_Id.Client /= Auto then
         Trace (Me, "Current client is "
                & Clients (External_Editor_Module_Id.Client).Command_Name.all);
      else
         Trace (Me, "No available client");
      end if;

      --  ??? Should we check that DISPLAY is defined if the command doesn't
      --  need a terminal ? It is necessary when we spawn Emacs, but not if
      --  we simply use emacsclient or gnuclient.
   end Select_Client;

   ----------------
   -- Substitute --
   ----------------

   procedure Substitute
     (Args : Argument_List_Access; F, C, L, E, P  : String := "")
   is
      Substrings : Substitution_Array :=
        (1 => (Name => new String'("f"), Value => new String'(F)),
         2 => (Name => new String'("c"), Value => new String'(C)),
         3 => (Name => new String'("l"), Value => new String'(L)),
         4 => (Name => new String'("e"), Value => new String'(E)),
         5 => (Name => new String'("p"), Value => new String'(P)),
         6 => (Name => new String'("%"), Value => new String'("%")));
   begin
      for A in Args'Range loop
         declare
            S : constant String := Substitute
              (Str               => Args (A).all,
               Substitution_Char => '%',
               Substrings        => Substrings,
               Recursive         => True);
         begin
            if S /= Args (A).all then
               Free (Args (A));
               Args (A) := new String'(S);
            end if;
         end;
      end loop;

      Free (Substrings);
   end Substitute;

   ----------------------
   -- External_Timeout --
   ----------------------

   function External_Timeout (D : Process_Data) return Boolean is
      pragma Unreferenced (D);
      Result : Expect_Match;
      Old    : Process_Descriptor_Array_Access;
      J      : Integer := External_Editor_Module_Id.Processes'First;
   begin
      while External_Editor_Module_Id.Processes /= null
        and then J <= External_Editor_Module_Id.Processes'Last
      loop
         begin
            Expect
              (Descriptor => External_Editor_Module_Id.Processes (J).all,
               Result     => Result,
               Regexp     => ".+",
               Timeout    => 1);

            J := J + 1;

         exception
            when Process_Died =>
               Trace (Me, "External editor died");
               Close (External_Editor_Module_Id.Processes (J).all);

               if External_Editor_Module_Id.Processes'Length = 1 then
                  Unchecked_Free (External_Editor_Module_Id.Processes);
               else
                  Old := External_Editor_Module_Id.Processes;
                  External_Editor_Module_Id.Processes :=
                    new Process_Descriptor_Array (1 .. Old'Length - 1);
                  External_Editor_Module_Id.Processes (1 .. J - 1) :=
                    Old (1 .. J - 1);
                  External_Editor_Module_Id.Processes
                    (J .. External_Editor_Module_Id.Processes'Last) :=
                    Old (J + 1 .. Old'Last);
                  Unchecked_Free (Old);
               end if;

            when others =>
               null;
         end;
      end loop;

      return External_Editor_Module_Id.Processes /= null;
   end External_Timeout;

   ----------------------
   -- External_Timeout --
   ----------------------

   procedure External_Timeout_Destroy (D : in out Process_Data) is
      pragma Unreferenced (D);
   begin
      Trace (Me, "Last external editor was killed");
      External_Editor_Module_Id.Timeout_Id := 0;
   end External_Timeout_Destroy;

   -----------------------
   -- Spawn_New_Process --
   -----------------------

   procedure Spawn_New_Process
     (Kernel  : access Kernel_Handle_Record'Class;
      Command : String;
      Args    : GNAT.OS_Lib.Argument_List;
      Result  : out Boolean)
   is
      Old     : Process_Descriptor_Array_Access;
      Process : TTY_Process_Descriptor;
   begin
      if Active (Me) then
         Trace (Me, "Spawn: " & Command &
                " " & Argument_List_To_String (Args));
      end if;

      Non_Blocking_Spawn
        (Descriptor  => Process,
         Command     => Command,
         Args        => Args,
         Buffer_Size => 0,
         Err_To_Out  => True);

      if External_Editor_Module_Id.Processes = null then
         External_Editor_Module_Id.Processes :=
           new Process_Descriptor_Array (1 .. 1);
      else
         Old := External_Editor_Module_Id.Processes;
         External_Editor_Module_Id.Processes := new Process_Descriptor_Array
           (1 .. Old'Length + 1);
         External_Editor_Module_Id.Processes (1 .. Old'Length) := Old.all;
         Unchecked_Free (Old);
      end if;

      External_Editor_Module_Id.Processes
        (External_Editor_Module_Id.Processes'Last) :=
        new TTY_Process_Descriptor'(Process);

      if External_Editor_Module_Id.Timeout_Id = 0 then
         External_Editor_Module_Id.Timeout_Id := Process_Timeout.Add
           (Interval => Timeout,
            Func     => External_Timeout'Access,
            D        => Process_Data'
              (Kernel_Handle (Kernel), null, null, null, null, null, False),
           Destroy   => External_Timeout_Destroy'Access);
      end if;

      Result := True;
      return;

   exception
      when Invalid_Process =>
         Result := False;
         return;
   end Spawn_New_Process;

   ------------------
   -- Spawn_Server --
   ------------------

   procedure Spawn_Server
     (Kernel  : access Kernel_Handle_Record'Class;
      Success : out Boolean)
   is
      Args : Argument_List_Access;
      Path : GNAT.Strings.String_Access;

   begin
      Success := False;
      if Clients (External_Editor_Module_Id.Client).Server_Start_Command
        = null
      then
         Trace (Me, "No server start command specified");
         return;
      end if;

      for S in Servers'Range loop
         Args := Argument_String_To_List (Servers (S).Command_Name.all);
         Path := Locate_Exec_On_Path (Args (Args'First).all);

         if Path /= null then
            Substitute
              (Args,
               P => Project_Name (Get_Project (Kernel)),
               E => Clients
                 (External_Editor_Module_Id.Client).Server_Start_Command.all);

            Spawn_New_Process
              (Kernel, Path.all, Args.all, Success);
            Free (Path);
            Free (Args.all);
            Unchecked_Free (Args);
            exit;
         end if;

         Free (Args.all);
         Unchecked_Free (Args);
      end loop;

      if not Success then
         Trace (Me, "Couldn't spawn emacs or the server");
      end if;

      return;
   end Spawn_Server;

   --------------------
   -- Blocking_Spawn --
   --------------------

   function Blocking_Spawn
     (Command : String; Args : Argument_List) return Integer
   is
      Status : Integer;
      Result : Expect_Match;
      Pd     : TTY_Process_Descriptor;
   begin
      Non_Blocking_Spawn
        (Pd, Command, Args, Buffer_Size => 0, Err_To_Out => True);

      loop
         Expect (Pd, Result, "@@@");
      end loop;

      return -1;

   exception
      when Process_Died =>
         Close (Pd, Status);
         return Status;
   end Blocking_Spawn;

   --------------------
   -- Client_Command --
   --------------------

   procedure Client_Command
     (Kernel        : access Kernel_Handle_Record'Class;
      File          : String := "";
      Line          : Natural := 1;
      Column        : Visible_Column_Type := 1;
      Extended_Lisp : String := "")
   is
      Max_Tries : constant := 40;
      Line_Str  : constant String := Natural'Image (Line);
      Col_Str   : constant String := Visible_Column_Type'Image (Column);
      Result    : Integer := 0;
      Success   : Boolean;
      Path      : GNAT.Strings.String_Access;
      Args      : Argument_List_Access;

   begin
      if Extended_Lisp /= ""
        and then Clients (External_Editor_Module_Id.Client).Lisp_Command_Name
          /= null
      then
         Args := Argument_String_To_List
           (Clients (External_Editor_Module_Id.Client).Lisp_Command_Name.all);

      elsif Extended_Lisp = ""
        and then Clients
          (External_Editor_Module_Id.Client).Command_Name /= null
      then
         declare
            Command : constant String :=
              Clients (External_Editor_Module_Id.Client).Command_Name.all;
         begin
            if Command = Custom_Command then
               Args := Argument_String_To_List
                 (Get_Pref (Custom_Editor));
            else
               Args := Argument_String_To_List (Command);
            end if;
         end;

      else
         Trace (Me, "No client available");
         return;
      end if;

      Substitute
        (Args,
         P => Project_Name (Get_Project (Kernel)),
         F => File,
         C => Col_Str (Col_Str'First + 1 .. Col_Str'Last),
         L => Line_Str (Line_Str'First + 1 .. Line_Str'Last),
         E => Extended_Lisp);

      if Args'Length /= 0 then
         Path := Locate_Exec_On_Path (Args (Args'First).all);
      else
         Insert (Kernel, """" & Get_Pref (Custom_Editor)
                    & """ is not a valid external editor",
                 Mode => Error);
         return;
      end if;

      if Path = null then
         Insert (Kernel, Args (Args'First).all & " not found on PATH",
                 Mode => Error);
         Free (Args.all);
         Unchecked_Free (Args);
         return;
      end if;

      if Clients (External_Editor_Module_Id.Client).Server_Start_Command
        /= null
      then
         Result := Blocking_Spawn
           (Path.all, Args (Args'First + 1 .. Args'Last));
      else
         Spawn_New_Process
           (Kernel, Path.all, Args (Args'First + 1 .. Args'Last), Success);
      end if;

      --  If we couldn't send the command, it probably means that Emacs wasn't
      --  started, at least not with the server.
      if Result /= 0
        and then Clients
          (External_Editor_Module_Id.Client).Server_Start_Command /= null
      then
         Spawn_Server (Kernel, Success);

         if Success then
            --  Give Emacs some time to start the server, and try a number of
            --  times.
            delay 1.0;

            for Try in 1 .. Max_Tries loop
               Result := Blocking_Spawn
                 (Path.all, Args (Args'First + 1 .. Args'Last));
               exit when Result = 0;
               delay 0.5;
            end loop;
         end if;
      end if;

      Free (Path);
      Free (Args.all);
      Unchecked_Free (Args);
   end Client_Command;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Edit_With_External_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Line   : Integer := 1;
      Column : Visible_Column_Type := 1;
   begin
      Push_State (Get_Kernel (Context.Context), Busy);
      Trace (Me, "Edit file with external editor "
             & Full_Name (File_Information (Context.Context)).all);

      if Has_Entity_Column_Information (Context.Context) then
         Line := Line_Information (Context.Context);
         Column := Entity_Column_Information (Context.Context);
      end if;

      Client_Command
        (Kernel => Get_Kernel (Context.Context),
         File   => Full_Name (File_Information (Context.Context)).all,
         Line   => Line,
         Column => Column);
      Pop_State (Get_Kernel (Context.Context));
      return Commands.Success;

   exception
      when E : others =>
         Trace (Exception_Handle, E);
         Pop_State (Get_Kernel (Context.Context));
         return Commands.Failure;
   end Execute;

   --------------------
   -- Open_File_Hook --
   --------------------

   function Open_File_Hook
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class) return Boolean
   is
      D : constant Source_File_Hooks_Args := Source_File_Hooks_Args (Data.all);
   begin
      if External_Editor_Module_Id.Client /= Auto
        and then Get_Pref (Always_Use_External_Editor)
      then
         if Is_Regular_File (D.File) then
            Push_State (Kernel_Handle (Kernel), Processing);
            --  ??? Incorrect handling of remote files
            Client_Command
              (Kernel => Kernel,
               File   => Full_Name (D.File).all,
               Line   => Natural (D.Line),
               Column => D.Column);
            Pop_State (Kernel_Handle (Kernel));

            return True;
         end if;
      end if;

      return False;
   end Open_File_Hook;

   -------------------------
   -- Preferences_Changed --
   -------------------------

   procedure Preferences_Changed
     (Kernel : access Kernel_Handle_Record'Class)
   is
      pragma Unreferenced (Kernel);
   begin
      Select_Client;
   end Preferences_Changed;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Command : Interactive_Command_Access;
   begin
      External_Editor_Module_Id := new External_Editor_Module_Record;

      --  Create the preferences

      Default_External_Editor :=
        Param_Spec_Enum (Supported_Client_Properties.Gnew_Enum
        (Name    => "External-Editor-Default-Editor",
         Nick    => -"External editor",
         Blurb   => -"The default external editor to use",
         Default => Gnuclient));
      Register_Property
        (Kernel, Param_Spec (Default_External_Editor),
         Page => -"Editor");

      Custom_Editor := Param_Spec_String (Gnew_String
        (Name    => "External-Editor-Custom-Command",
         Nick    => -"Custom editor command",
         Blurb   => -"Command to use for launching a custom editor",
         Default => "emacs +%l %f"));
      Register_Property
        (Kernel, Param_Spec (Custom_Editor), -"Editor");

      Always_Use_External_Editor := Param_Spec_Boolean (Gnew_Boolean
        (Name    => "External-Editor-Always-Use-External-Editor",
         Default => False,
         Blurb   => -("True if all editions should be done with the external"
                      & " editor. This will deactivate completely the"
                      & " internal editor. False if the external editor"
                      & " needs to be explicitely called by the user."),
         Nick    => -"Always use external editor"));
      Register_Property
        (Kernel, Param_Spec (Always_Use_External_Editor), -"Editor");

      Command := new Edit_With_External_Command;
      Register_Contextual_Menu
        (Kernel, "Edit with external editor",
         Action => Command,
         Filter => Lookup_Filter (Kernel, "File"));

      Register_Module
        (Module                  => Module_ID (External_Editor_Module_Id),
         Kernel                  => Kernel,
         Module_Name             => External_Editor_Module_Name,
         Priority                => Default_Priority + 1);
      Add_Hook (Kernel, Open_File_Action_Hook,
                Wrapper (Open_File_Hook'Access),
                Name => "external_editor.open_file");

      Add_Hook (Kernel, Preferences_Changed_Hook,
                Wrapper (Preferences_Changed'Access),
                Name => "external_editor.preferences_changed");
   end Register_Module;

end External_Editor_Module;
