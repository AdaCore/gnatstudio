------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2018, AdaCore                     --
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

with Unchecked_Deallocation;

with GNAT.Expect;               use GNAT.Expect;
pragma Warnings (Off);
with GNAT.Expect.TTY;           use GNAT.Expect.TTY;
pragma Warnings (On);
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with GNATCOLL.Templates;        use GNATCOLL.Templates;
with GNATCOLL.Utils;            use GNATCOLL.Utils;
with GNATCOLL.Arg_Lists;        use GNATCOLL.Arg_Lists;
with GNATCOLL.Projects;         use GNATCOLL.Projects;
with GNATCOLL.Traces;           use GNATCOLL.Traces;
with GNATCOLL.Xref;             use GNATCOLL.Xref;
with GNATCOLL.VFS;              use GNATCOLL.VFS;

with Glib;                      use Glib;
with Glib.Main;                 use Glib.Main;

with Basic_Types;               use Basic_Types;
with Commands.Interactive;      use Commands, Commands.Interactive;
with Default_Preferences.Enums; use Default_Preferences;
with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel.Actions;        use GPS.Kernel.Actions;
with GPS.Kernel.Contexts;       use GPS.Kernel.Contexts;
with GPS.Kernel.Modules;        use GPS.Kernel.Modules;
with GPS.Kernel.Project;        use GPS.Kernel.Project;
with GPS.Kernel;                use GPS.Kernel;
with Toolchains_Old;            use Toolchains_Old;

package body External_Editor_Module is

   Me : constant Trace_Handle := Create
     ("GPS.SOURCE_EDITOR.EXTERNAL_EDITOR_MODULE");

   Timeout : constant Guint := 500;
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

   Default_External_Editor    : Supported_Client_Preferences.Preference;
   Custom_Editor              : String_Preference;

   type Constant_String_Access is access constant String;

   --------------
   -- Commands --
   --------------

   type Edit_With_External_Command is new Interactive_Command with null record;
   overriding function Execute
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

      Timeout_Id : Glib.Main.G_Source_Id := 0;
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
      File          : Virtual_File := No_File;
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

   procedure Spawn_New_Process
     (Command : Filesystem_String;
      Args    : GNAT.OS_Lib.Argument_List;
      Result  : out Boolean);
   --  Spawn a new process, and store it in External_Clients, so that we can
   --  properly handle its termination.

   function External_Timeout return Boolean;
   --  Timeout used to monitor the external editors. It is also used to make
   --  sure that no zombie process is left on Unix machines.

   function Blocking_Spawn
     (Command : Filesystem_String; Args : Argument_List) return Integer;
   --  Spawn a new process, and waits for its termination. It hides both its
   --  standard output and standard error.

   -------------------
   -- Select_Client --
   -------------------

   procedure Select_Client is
      Path           : Virtual_File;
      Args           : Argument_List_Access;
      Match          : Boolean;
      Default_Client : constant Supported_Clients := Supported_Clients'Val
        (Default_External_Editor.Get_Pref);
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
               Path := Locate_Tool_Executable (+Args (Args'First).all);

               if Path /= No_File then
                  Match := True;
               end if;
            end if;

            Free (Args.all);
            Unchecked_Free (Args);
         end if;

         if Match and then Clients (C).Lisp_Command_Name /= null then
            Args := Argument_String_To_List
              (Clients (C).Lisp_Command_Name.all);

            Path := Locate_Tool_Executable (+Args (Args'First).all);
            if Path = No_File then
               Match := False;
            end if;

            Free (Args.all);
            Unchecked_Free (Args);
         end if;

         if Match and then Clients (C).Extra_Test /= null then
            Path := Locate_Tool_Executable (+Clients (C).Extra_Test.all);
            if Path = No_File then
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
               Delimiter         => '%',
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

   function External_Timeout return Boolean is
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

      if External_Editor_Module_Id.Processes /= null then
         return True;
      else
         Trace (Me, "Last external editor was killed");
         External_Editor_Module_Id.Timeout_Id := 0;
         return False;
      end if;
   end External_Timeout;

   -----------------------
   -- Spawn_New_Process --
   -----------------------

   procedure Spawn_New_Process
     (Command : Filesystem_String;
      Args    : GNAT.OS_Lib.Argument_List;
      Result  : out Boolean)
   is
      Old     : Process_Descriptor_Array_Access;
      Process : TTY_Process_Descriptor;
   begin
      if Active (Me) then
         Trace (Me, "Spawn: " & (+Command) &
                " " & Argument_List_To_String (Args));
      end if;

      Non_Blocking_Spawn
        (Descriptor  => Process,
         Command     => +Command,
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
         --  ??? Should we just run through the task manager and use the
         --  timeout there instead ?
         External_Editor_Module_Id.Timeout_Id := Glib.Main.Timeout_Add
           (Interval => Timeout,
            Func     => External_Timeout'Access);
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
      Path : Virtual_File;

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
         Path := Locate_Tool_Executable (+Args (Args'First).all);

         if Path /= No_File then
            Substitute
              (Args,
               P => Get_Project (Kernel).Name,
               E => Clients
                 (External_Editor_Module_Id.Client).Server_Start_Command.all);

            Spawn_New_Process
              (Path.Full_Name, Args.all, Success);
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
     (Command : Filesystem_String; Args : Argument_List) return Integer
   is
      Status : Integer;
      Result : Expect_Match;
      Pd     : TTY_Process_Descriptor;
   begin
      Non_Blocking_Spawn
        (Pd, +Command, Args, Buffer_Size => 0, Err_To_Out => True);

      loop
         Expect (Pd, Result, "@@@");
      end loop;

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
      File          : Virtual_File := No_File;
      Line          : Natural := 1;
      Column        : Visible_Column_Type := 1;
      Extended_Lisp : String := "")
   is
      Max_Tries : constant := 40;
      Line_Str  : constant String := Natural'Image (Line);
      Col_Str   : constant String := Visible_Column_Type'Image (Column);
      Result    : Integer := 0;
      Success   : Boolean;
      Path      : Virtual_File;
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
         P => Get_Project (Kernel).Name,
         F => +File.Full_Name,
         C => Col_Str (Col_Str'First + 1 .. Col_Str'Last),
         L => Line_Str (Line_Str'First + 1 .. Line_Str'Last),
         E => Extended_Lisp);

      if Args'Length /= 0 then
         Path := Locate_Tool_Executable (+Args (Args'First).all);
      else
         Insert (Kernel, """" & Get_Pref (Custom_Editor)
                    & """ is not a valid external editor",
                 Mode => Error);
         return;
      end if;

      if Path = No_File then
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
           (Path.Full_Name, Args (Args'First + 1 .. Args'Last));
      else
         Spawn_New_Process
           (Path.Full_Name, Args (Args'First + 1 .. Args'Last), Success);
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
                 (Path.Full_Name, Args (Args'First + 1 .. Args'Last));
               exit when Result = 0;
               delay 0.5;
            end loop;
         end if;
      end if;

      Free (Args.all);
      Unchecked_Free (Args);
   end Client_Command;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Edit_With_External_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Line   : Integer := 1;
      Column : Visible_Column_Type := 1;
   begin
      Trace (Me, "Edit file with external editor "
             & Display_Full_Name (File_Information (Context.Context)));

      if Has_Entity_Column_Information (Context.Context) then
         Line := Line_Information (Context.Context);
         Column := Entity_Column_Information (Context.Context);
      end if;

      Select_Client;
      Client_Command
        (Kernel => Get_Kernel (Context.Context),
         File   => File_Information (Context.Context),
         Line   => Line,
         Column => Column);
      return Commands.Success;

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

      --  Create the preferences

      Default_External_Editor := Supported_Client_Preferences.Create
        (Manager  => Manager,
         Path     => "Editor:External Editors",
         Name     => "External-Editor-Default-Editor",
         Label    => -"External editor",
         Doc      => -"The default external editor to use",
         Default  => Gnuclient);

      Custom_Editor := Create
        (Manager => Manager,
         Path    => -"Editor:External Editors",
         Name    => "External-Editor-Custom-Command",
         Label   => -"Custom editor command",
         Doc     => -"Command to use for launching a custom editor",
         Default => "emacs +%l %f");

      Register_Action
        (Kernel, "edit with external editor", new Edit_With_External_Command,
         Description =>
           -("Edit the file with an external editor, as configued in the"
           & " preferences"),
         Filter      => Lookup_Filter (Kernel, "File"));

      Register_Module
        (Module                  => Module_ID (External_Editor_Module_Id),
         Kernel                  => Kernel,
         Module_Name             => External_Editor_Module_Name,
         Priority                => Default_Priority + 1);
   end Register_Module;

end External_Editor_Module;
