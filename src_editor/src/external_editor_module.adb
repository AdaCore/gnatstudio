-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
--                            ACT-Europe                             --
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

with Ada.Exceptions;           use Ada.Exceptions;
with Basic_Types;              use Basic_Types;
with GNAT.Expect;              use GNAT.Expect;
with GNAT.OS_Lib;              use GNAT.OS_Lib;
with Glib;                     use Glib;
with Glib.Object;              use Glib.Object;
with Glib.Values;              use Glib.Values;
with Glide_Intl;               use Glide_Intl;
with Glide_Kernel.Modules;     use Glide_Kernel.Modules;
with Glide_Kernel.Preferences; use Glide_Kernel.Preferences;
with Glide_Kernel;             use Glide_Kernel;
with Glide_Kernel.Timeout;     use Glide_Kernel.Timeout;
with Gtk.Main;                 use Gtk.Main;
with Gtk.Menu;                 use Gtk.Menu;
with Gtk.Menu_Item;            use Gtk.Menu_Item;
with Traces;                   use Traces;
with Unchecked_Deallocation;

package body External_Editor_Module is

   Timeout : constant Guint32 := 500;
   --  Timeout in millisecond to check the external editor processes.
   --  This is intentionnaly very big currently, since all we want is to
   --  properly handle the death of an external process, to remove zombie
   --  processes, but we do not process the actual output.

   External_Editor_User_Data : constant String := "External_Editor";
   --  String used to identify the user data in the kernel

   Me : constant Debug_Handle := Create ("External_Editor_Module");

   type External_Client is record
      Command_Name      : GNAT.OS_Lib.String_Access;
      --  The command to load a file.
      --  The following substitutions are provided:
      --     %l = line to display
      --     %c = column to display
      --     %f = file to display
      --     %e = extended lisp command
      --     %% = percent sign ('%')

      Lisp_Command_Name : GNAT.OS_Lib.String_Access;
      --  The command to use to emit any lisp command, null if this is not
      --  possible. Same substitutions as above

      Server_Start_Command : GNAT.OS_Lib.String_Access;
      --  The lisp command to provide to Emacs to start the server.
      --  This code will not attempt to start a server if this is
      --  null. Otherwise, if Command_Name fails, then Emacs is started with
      --  the lisp command Server_Start_Command, and then Command_Name is
      --  attempted again.
      --  If this is null, then Command_Name is started in the background, so
      --  as not to block the current process.

      Extra_Test : GNAT.OS_Lib.String_Access;
      --  Name of an extra executable to look for on the executable path to
      --  test whether this client is available. This should be used in cases
      --  where Command_Name starts a terminal that contains the editor
   end record;

   type External_Clients is array (Supported_Clients) of External_Client;

   type External_Server is record
      Command_Name : GNAT.OS_Lib.String_Access;
      --  The command to start a server.
      --  The following substitutions are provided:
      --     %e = lisp command to use to start the server. This is provided
      --          automatically by the client
      --     %% = percent sign ('%')
   end record;

   type Supported_Servers is (Glide, Emacs);
   type External_Servers is array (Supported_Servers) of External_Server;

   Clients : constant External_Clients :=
     (None => (null, null, null, null),
      Gnuclient =>
        (Command_Name         => new String' ("gnuclient -q +%l %f"),
         Lisp_Command_Name    => new String' ("gnudoit -q %e"),
         Server_Start_Command => new String'
           ("(progn (load-library ""gnuserv"") (gnuserv-start))"),
         Extra_Test            => null),

      Emacsclient =>
        (Command_Name         => new String' ("emacsclient -n +%l:%c %f"),
         Lisp_Command_Name    => null,
         Server_Start_Command => new String' ("(server-start)"),
         Extra_Test           => null),

      Emacs =>
        (Command_Name         => new String' ("emacs +%l %f"),
         Lisp_Command_Name    => null,
         Server_Start_Command => null,
         Extra_Test           => null),

      Vim =>
        (Command_Name         => new String'
           ("xterm -geometry 80x50 -exec vim +%l %f"),
         Lisp_Command_Name    => null,
         Server_Start_Command => null,
         Extra_Test           => new String' ("vim")),

      Vi =>
        (Command_Name         => new String'
           ("xterm -geometry 80x50 -exec vi +%l %f"),
         Lisp_Command_Name    => null,
         Server_Start_Command => null,
         Extra_Test           => new String' ("vi")));

   Servers : constant External_Servers :=
     (Glide =>
        (Command_Name         => new String'
           ("glide -emacs --eval -emacs %e")),
      Emacs =>
        (Command_Name         => new String' ("emacs --eval %e")));

   type Process_Descriptor_Array is array (Positive range <>)
     of Process_Descriptor_Access;
   type Process_Descriptor_Array_Access is access
     Process_Descriptor_Array;

   procedure Unchecked_Free is new Unchecked_Deallocation
     (Process_Descriptor_Array, Process_Descriptor_Array_Access);

   type External_Client_Information is record
      Client : Supported_Clients := None;
      --  The index of the client we are currently using

      Processes : Process_Descriptor_Array_Access;
      --  The list of external processes that have been started.

      Timeout_Id : Gtk.Main.Timeout_Handler_Id := 0;
      --  The timeout loop that takes care of all the spawned external
      --  editors.
   end record;

   type External_Client_Information_Access is access
     External_Client_Information;

   package Client_User_Data is new Glib.Object.User_Data
     (External_Client_Information_Access);

   procedure Spawn_Server
     (Kernel         : access Kernel_Handle_Record'Class;
      Current_Client : in out External_Client_Information;
      Success        : out Boolean);
   --  Start Emacs and the server, so that a client can connect to it.
   --  False is returned if the server could not be started.

   procedure Client_Command
     (Kernel         : access Kernel_Handle_Record'Class;
      Current_Client : in out External_Client_Information;
      File           : String := "";
      Line           : Natural := 1;
      Column         : Natural := 1;
      Extended_Lisp  : String := "");
   --  Calls the client with the appropriate parameters

   procedure Select_Client
     (Kernel         : access Kernel_Handle_Record'Class;
      Current_Client : in out External_Client_Information);
   --  Select the appropriate external editor to use. They are tested in the
   --  order given in Clients, and are selected if both Command_Name and
   --  Lisp_Command_Name are found on the path.

   procedure Substitute
     (Args : Argument_List_Access; F, C, L, E  : String := "");
   --  Does all the substitutions in Args for %f, %c, %l, %e and %%.

   procedure External_Editor_Contextual
     (Object  : access GObject_Record'Class;
      Context : access Selection_Context'Class;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class);
   --  Add entries to the conextual menu if necessary

   procedure On_Edit_File
     (Widget : access GObject_Record'Class;
      Context : Selection_Context_Access);
   --  Edit the file described in the context

   function Mime_Action
     (Kernel    : access Kernel_Handle_Record'Class;
      Mime_Type : String;
      Data      : GValue_Array;
      Mode      : Mime_Mode := Read_Write) return Boolean;
   --  Handle an edition request

   procedure Spawn_New_Process
     (Kernel         : access Kernel_Handle_Record'Class;
      Current_Client : in out External_Client_Information;
      Command        : String;
      Args           : GNAT.OS_Lib.Argument_List;
      Result         : out Boolean);
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

   -------------------
   -- Select_Client --
   -------------------

   procedure Select_Client
     (Kernel         : access Kernel_Handle_Record'Class;
      Current_Client : in out External_Client_Information)
   is
      Path  : GNAT.OS_Lib.String_Access;
      Args  : Argument_List_Access;
      Match : Boolean;
      Default_Client : constant String :=
        Get_Pref (Kernel, Default_External_Editor);
   begin
      --  If the user has specified a default client, use that one.
      if Default_Client /= "" then
         begin
            Current_Client.Client :=
              Supported_Clients'Value (Default_Client);
            return;

         exception
            when others => null;
         end;
      end if;

      Current_Client.Client := None;

      for C in Clients'Range loop
         Match := False;

         if Clients (C).Command_Name /= null then
            Args := Argument_String_To_List (Clients (C).Command_Name.all);

            Path := Locate_Exec_On_Path (Args (Args'First).all);
            if Path /= null then
               Free (Path);
               Match := True;
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
            Current_Client.Client := C;
            exit;
         end if;
      end loop;

      if Current_Client.Client /= None then
         Trace (Me, "Current client is "
                & Clients (Current_Client.Client).Command_Name.all);
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
     (Args : Argument_List_Access; F, C, L, E  : String := "")
   is
      function Str_Substitute (Str : String) return String;
      --  Substitute the parameters in Str (%f, %l,...).
      --  The returned array must be freed by the caller

      --------------------
      -- Str_Substitute --
      --------------------

      function Str_Substitute (Str : String) return String is
         Index : Natural := Str'First;
      begin
         while Index < Str'Last loop
            if Str (Index) = '%' then
               case Str (Index + 1) is
                  when 'f' =>
                     return Str (Str'First .. Index - 1) & F
                       & Str_Substitute (Str (Index + 2 .. Str'Last));

                  when 'c' =>
                     return Str (Str'First .. Index - 1) & C
                       & Str_Substitute (Str (Index + 2 .. Str'Last));

                  when 'l' =>
                     return Str (Str'First .. Index - 1) & L
                       & Str_Substitute (Str (Index + 2 .. Str'Last));

                  when 'e' =>
                     return Str (Str'First .. Index - 1) & E
                       & Str_Substitute (Str (Index + 2 .. Str'Last));

                  when '%' =>
                     return Str (Str'First .. Index - 1) & '%'
                       & Str_Substitute (Str (Index + 2 .. Str'Last));

                  when others =>
                     null;
               end case;
            end if;

            Index := Index + 1;
         end loop;

         return Str;
      end Str_Substitute;

   begin
      for A in Args'Range loop
         declare
            S : constant String := Str_Substitute (Args (A).all);
         begin
            if S /= Args (A).all then
               Free (Args (A));
               Args (A) := new String' (S);
            end if;
         end;
      end loop;
   end Substitute;

   ----------------------
   -- External_Timeout --
   ----------------------

   function External_Timeout (D : Process_Data) return Boolean is
      Result : Expect_Match;
      Old : Process_Descriptor_Array_Access;
      Current_Client : External_Client_Information_Access :=
        Client_User_Data.Get (D.Kernel, External_Editor_User_Data);
      J : Integer := Current_Client.Processes'First;
   begin
      while Current_Client.Processes /= null
        and then J <= Current_Client.Processes'Last
      loop
         begin
            Expect
              (Descriptor => Current_Client.Processes (J).all,
               Result     => Result,
               Regexp     => ".+",
               Timeout    => 1);

            J := J + 1;

         exception
            when Process_Died =>
               Trace (Me, "External editor died");
               Close (Current_Client.Processes (J).all);

               if Current_Client.Processes'Length = 1 then
                  Unchecked_Free (Current_Client.Processes);
               else
                  Old := Current_Client.Processes;
                  Current_Client.Processes := new Process_Descriptor_Array
                    (1 .. Old'Length - 1);
                  Current_Client.Processes (1 .. J - 1) := Old (1 .. J - 1);
                  Current_Client.Processes (J .. Current_Client.Processes'Last)
                    := Old (J + 1 .. Old'Last);
                  Unchecked_Free (Old);
               end if;

            when others =>
               null;
         end;
      end loop;

      return Current_Client.Processes /= null;
   end External_Timeout;

   ----------------------
   -- External_Timeout --
   ----------------------

   procedure External_Timeout_Destroy (D : in out Process_Data) is
   begin
      Trace (Me, "Last external editor was killed");
      Client_User_Data.Get
        (D.Kernel, External_Editor_User_Data).Timeout_Id := 0;
   end External_Timeout_Destroy;

   -----------------------
   -- Spawn_New_Process --
   -----------------------

   procedure Spawn_New_Process
     (Kernel         : access Kernel_Handle_Record'Class;
      Current_Client : in out External_Client_Information;
      Command        : String;
      Args           : GNAT.OS_Lib.Argument_List;
      Result         : out Boolean)
   is
      Old : Process_Descriptor_Array_Access;
      Process : Process_Descriptor;
   begin
      Non_Blocking_Spawn
        (Descriptor  => Process,
         Command     => Command,
         Args        => Args,
         Buffer_Size => 0,
         Err_To_Out  => True);

      if Current_Client.Processes = null then
         Current_Client.Processes := new Process_Descriptor_Array (1 .. 1);
      else
         Old := Current_Client.Processes;
         Current_Client.Processes := new Process_Descriptor_Array
           (1 .. Old'Length + 1);
         Current_Client.Processes (1 .. Old'Length) := Old.all;
         Unchecked_Free (Old);
      end if;

      Current_Client.Processes (Current_Client.Processes'Last) :=
        new Process_Descriptor' (Process);

      if Current_Client.Timeout_Id = 0 then
         Current_Client.Timeout_Id := Process_Timeout.Add
           (Interval => Timeout,
            Func     => External_Timeout'Access,
            D        => Process_Data'
              (Kernel     => Kernel_Handle (Kernel),
               Descriptor => null,
               Name       => null),
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
     (Kernel         : access Kernel_Handle_Record'Class;
      Current_Client : in out External_Client_Information;
      Success        : out Boolean)
   is
      Args    : Argument_List_Access;
      Path    : GNAT.OS_Lib.String_Access;

   begin
      Success := False;
      if Clients (Current_Client.Client).Server_Start_Command = null then
         Trace (Me, "No server start command specified");
         return;
      end if;

      for S in Servers'Range loop
         Args := Argument_String_To_List (Servers (S).Command_Name.all);
         Path := Locate_Exec_On_Path (Args (Args'First).all);

         if Path /= null then
            Substitute
              (Args,
               E => Clients (Current_Client.Client)
                    .Server_Start_Command.all);

            Spawn_New_Process
              (Kernel, Current_Client, Path.all, Args.all, Success);
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
      Pd     : Process_Descriptor;
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
     (Kernel         : access Kernel_Handle_Record'Class;
      Current_Client : in out External_Client_Information;
      File           : String := "";
      Line           : Natural := 1;
      Column         : Natural := 1;
      Extended_Lisp  : String := "")
   is
      Max_Tries : constant := 40;
      Line_Str  : constant String := Natural'Image (Line);
      Col_Str   : constant String := Natural'Image (Column);
      Result    : Integer := 0;
      Success   : Boolean;
      Path      : GNAT.OS_Lib.String_Access;
      Args      : Argument_List_Access;

   begin
      if Extended_Lisp /= ""
        and then Clients (Current_Client.Client).Lisp_Command_Name
          /= null
      then
         Args := Argument_String_To_List
           (Clients (Current_Client.Client).Lisp_Command_Name.all);

      elsif Extended_Lisp = ""
        and then Clients (Current_Client.Client).Command_Name /= null
      then
         Args := Argument_String_To_List
           (Clients (Current_Client.Client).Command_Name.all);

      else
         Trace (Me, "No client available");
         return;
      end if;

      Substitute (Args,
                  F => File,
                  C => Col_Str (Col_Str'First + 1 .. Col_Str'Last),
                  L => Line_Str (Line_Str'First + 1 .. Line_Str'Last),
                  E => Extended_Lisp);

      Path := Locate_Exec_On_Path (Args (Args'First).all);

      if Path = null then
         Trace (Me, Args (Args'First).all & " not found on PATH");
         Free (Args.all);
         Unchecked_Free (Args);
         return;
      end if;

      if Clients (Current_Client.Client).Server_Start_Command /= null then
         Result := Blocking_Spawn
           (Path.all, Args (Args'First + 1 .. Args'Last));
      else
         Spawn_New_Process
           (Kernel, Current_Client, Path.all,
            Args (Args'First + 1 .. Args'Last), Success);
      end if;

      --  If we couldn't send the command, it probably means that Emacs wasn't
      --  started, at least not with the server.
      if Result /= 0
        and then Clients (Current_Client.Client).Server_Start_Command /= null
      then
         Spawn_Server (Kernel, Current_Client, Success);

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

   ------------------
   -- On_Edit_File --
   ------------------

   procedure On_Edit_File
     (Widget : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      pragma Unreferenced (Widget);

      File           : constant File_Selection_Context_Access :=
        File_Selection_Context_Access (Context);
      Line           : Integer := 1;
      Column         : Integer := 1;
      Current_Client : External_Client_Information_Access;

   begin
      Push_State (Get_Kernel (File), Busy);
      Trace (Me, "Edit file with external editor " & File_Information (File));

      if Context.all in Entity_Selection_Context'Class then
         Line := Line_Information (Entity_Selection_Context_Access (Context));
         Column := Column_Information
           (Entity_Selection_Context_Access (Context));
      end if;

      Current_Client := Client_User_Data.Get
        (Get_Kernel (Context), External_Editor_User_Data);

      Client_Command
        (Kernel => Get_Kernel (Context),
         Current_Client => Current_Client.all,
         File   => Directory_Information (File) & File_Information (File),
         Line   => Line,
         Column => Column);
      Pop_State (Get_Kernel (File));

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
         Pop_State (Get_Kernel (File));
   end On_Edit_File;

   --------------------------------
   -- External_Editor_Contextual --
   --------------------------------

   procedure External_Editor_Contextual
     (Object  : access GObject_Record'Class;
      Context : access Selection_Context'Class;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class)
   is
      pragma Unreferenced (Object);

      File  : File_Selection_Context_Access;
      Mitem : Gtk_Menu_Item;

   begin
      if Context.all in File_Selection_Context'Class then
         File := File_Selection_Context_Access (Context);

         if Has_Directory_Information (File)
           and then Has_File_Information (File)
         then
            Gtk_New (Mitem, -"Edit with external editor");
            Append (Menu, Mitem);
            Context_Callback.Connect
              (Mitem, "activate",
               Context_Callback.To_Marshaller (On_Edit_File'Access),
               Selection_Context_Access (Context));
         end if;
      end if;
   end External_Editor_Contextual;

   -----------------
   -- Mime_Action --
   -----------------

   function Mime_Action
     (Kernel    : access Kernel_Handle_Record'Class;
      Mime_Type : String;
      Data      : GValue_Array;
      Mode      : Mime_Mode := Read_Write) return Boolean
   is
      pragma Unreferenced (Mode);
      Current_Client : External_Client_Information_Access;
   begin
      if Mime_Type = Mime_Source_File then
         declare
            File   : constant String  := Get_String (Data (Data'First));
            Line   : constant Gint    := Get_Int (Data (Data'First + 1));
            Column : constant Gint    := Get_Int (Data (Data'First + 2));

         begin
            if Is_Regular_File (File) then
               Push_State (Kernel_Handle (Kernel), Processing);
               Current_Client := Client_User_Data.Get
                 (Kernel, External_Editor_User_Data);
               Client_Command
                 (Kernel => Kernel,
                  Current_Client => Current_Client.all,
                  File   => File,
                  Line   => Natural (Line),
                  Column => Natural (Column));
               Pop_State (Kernel_Handle (Kernel));

               return True;
            end if;
         end;
      end if;

      return False;
   end Mime_Action;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
   is
      Priority       : Module_Priority := Default_Priority;
      Current_Client : External_Client_Information_Access;

   begin
      --  Memory is never freed, but is only allocated once per kernel.
      Current_Client := new External_Client_Information;

      if Get_Pref (Kernel, Always_Use_External_Editor) then
         Priority := Priority + 1;
      end if;

      Select_Client (Kernel, Current_Client.all);

      if Current_Client.Client /= None then
         Register_Module
           (Module                  => External_Editor_Module_Id,
            Kernel                  => Kernel,
            Module_Name             => External_Editor_Module_Name,
            Priority                => Priority,
            Contextual_Menu_Handler => External_Editor_Contextual'Access,
            Mime_Handler            => Mime_Action'Access);
      end if;

      Client_User_Data.Set (Kernel, Current_Client, External_Editor_User_Data);
   end Register_Module;

end External_Editor_Module;
