-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
--                            ACT-Europe                             --
--                                                                   --
-- GLIDE is free software; you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Ada.Exceptions;           use Ada.Exceptions;
with Basic_Types;              use Basic_Types;
with GNAT.OS_Lib;              use GNAT.OS_Lib;
with Glib;                     use Glib;
with Glib.Object;              use Glib.Object;
with Glib.Values;              use Glib.Values;
with Glide_Intl;               use Glide_Intl;
with Glide_Kernel.Modules;     use Glide_Kernel.Modules;
with Glide_Kernel.Preferences; use Glide_Kernel.Preferences;
with Glide_Kernel;             use Glide_Kernel;
with Gtk.Menu;                 use Gtk.Menu;
with Gtk.Menu_Item;            use Gtk.Menu_Item;
with Traces;                   use Traces;

package body External_Editor_Module is

   Me : Debug_Handle := Create ("External_Editor_Module");

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

   Current_Client : Supported_Clients := None;
   --  The index of the client we are currently using


   function Spawn_Server return Boolean;
   --  Start Emacs and the server, so that a client can connect to it.
   --  False is returned if the server could not be started.

   procedure Client_Command
     (File          : String := "";
      Line          : Natural := 1;
      Column        : Natural := 1;
      Extended_Lisp : String := "");
   --  Calls the client with the appropriate parameters

   procedure Select_Client (Kernel : access Kernel_Handle_Record'Class);
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

   -------------------
   -- Select_Client --
   -------------------

   procedure Select_Client (Kernel : access Kernel_Handle_Record'Class) is
      Path  : GNAT.OS_Lib.String_Access;
      Args  : Argument_List_Access;
      Match : Boolean;
      Default_Client : constant String :=
        Get_Pref (Kernel, Default_External_Editor);
   begin
      --  If the user has specified a default client, use that one.
      if Default_Client /= "" then
         begin
            Current_Client := Supported_Clients'Value (Default_Client);
            return;

         exception
            when others => null;
         end;
      end if;

      Current_Client := None;

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
            Current_Client := C;
            exit;
         end if;
      end loop;

      if Current_Client /= None then
         Trace (Me, "Current client is "
                  & Clients (Current_Client).Command_Name.all);
      else
         Trace (Me, "No available client");
      end if;

      --  ??? Should we check that DISPLAY is defined if the command doesn't
      --  ??? need a terminal ? It is necessary when we spawn Emacs, but not if
      --  ??? we simply use emacsclient or gnuclient.
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

   ------------------
   -- Spawn_Server --
   ------------------

   function Spawn_Server return Boolean is
      Args    : Argument_List_Access;
      Process : Process_Id := Invalid_Pid;
      Path    : GNAT.OS_Lib.String_Access;

   begin
      if Clients (Current_Client).Server_Start_Command = null then
         Trace (Me, "No server start command specified");
         return False;
      end if;

      for S in Servers'Range loop
         Args := Argument_String_To_List (Servers (S).Command_Name.all);
         Path := Locate_Exec_On_Path (Args (Args'First).all);

         if Path /= null then
            Substitute
              (Args,
               E => Clients (Current_Client).Server_Start_Command.all);

            Process := Non_Blocking_Spawn (Path.all, Args.all);
            Free (Path);
            Free (Args.all);
            Unchecked_Free (Args);
            exit;
         end if;

         Free (Args.all);
         Unchecked_Free (Args);
      end loop;

      if Process = Invalid_Pid then
         Trace (Me, "Couldn't spawn emacs or the server");
         return False;
      end if;
      return True;
   end Spawn_Server;

   --------------------
   -- Client_Command --
   --------------------

   procedure Client_Command
     (File          : String := "";
      Line          : Natural := 1;
      Column        : Natural := 1;
      Extended_Lisp : String := "")
   is
      Max_Tries : constant := 40;
      Line_Str  : constant String := Natural'Image (Line);
      Col_Str   : constant String := Natural'Image (Column);
      Result    : Integer;
      Path      : GNAT.OS_Lib.String_Access;
      Args      : Argument_List_Access;
      Process   : Process_Id;
   begin
      if Extended_Lisp /= ""
        and then Clients (Current_Client).Lisp_Command_Name /= null
      then
         Args := Argument_String_To_List
           (Clients (Current_Client).Lisp_Command_Name.all);

      elsif Extended_Lisp = ""
        and then Clients (Current_Client).Command_Name /= null
      then
         Args := Argument_String_To_List
           (Clients (Current_Client).Command_Name.all);

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
         return;
      end if;

      if Clients (Current_Client).Server_Start_Command /= null then
         Result := Spawn (Path.all, Args (Args'First + 1 .. Args'Last));
      else
         Process := Non_Blocking_Spawn
           (Path.all, Args (Args'First + 1 .. Args'Last));
      end if;

      --  If we couldn't send the command, it probably means that Emacs wasn't
      --  started, at least not with the server.
      if Result /= 0
        and then Clients (Current_Client).Server_Start_Command /= null
        and then Spawn_Server
      then
         --  Give Emacs some time to start the server, and try a number of
         --  times.
         delay 1.0;

         for Try in 1 .. Max_Tries loop
            Result := Spawn (Path.all, Args (Args'First + 1 .. Args'Last));
            exit when Result = 0;
            delay 0.5;
         end loop;
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
      File : File_Name_Selection_Context_Access :=
        File_Name_Selection_Context_Access (Context);
      Line   : Integer := 1;
      Column : Integer := 1;
   begin
      Push_State (Get_Kernel (File), Processing);

      Trace (Me, "Edit file with external editor " & File_Information (File));

      if Context.all in Entity_Selection_Context'Class then
         Line := Line_Information (Entity_Selection_Context_Access (Context));
         Column := Column_Information
           (Entity_Selection_Context_Access (Context));
      end if;

      Client_Command
        (File   => Directory_Information (File) & File_Information (File),
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
      File  : File_Name_Selection_Context_Access;
      Mitem : Gtk_Menu_Item;
   begin
      if Current_Client /= None
        and then Context.all in File_Name_Selection_Context'Class
      then
         File := File_Name_Selection_Context_Access (Context);

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
   begin
      if Mime_Type = Mime_Source_File then
         declare
            File      : constant String  := Get_String (Data (Data'First));
            Line      : constant Gint    := Get_Int (Data (Data'First + 1));
            Column    : constant Gint    := Get_Int (Data (Data'First + 2));
         begin
            Push_State (Kernel_Handle (Kernel), Processing);
            Client_Command
              (File   => File,
               Line   => Natural (Line),
               Column => Natural (Column));
            Pop_State (Kernel_Handle (Kernel));

            return True;
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
      Priority_Modifier : Module_Priority := 0;
   begin
      if Get_Pref (Kernel, Always_Use_External_Editor) then
         Priority_Modifier := 1;
      end if;

      Select_Client (Kernel);

      if Current_Client /= None then
         External_Editor_Module_Id := Register_Module
           (Kernel                  => Kernel,
            Module_Name             => External_Editor_Module_Name,
            Priority                => Default_Priority + Priority_Modifier,
            Contextual_Menu_Handler => External_Editor_Contextual'Access,
            Mime_Handler            => Mime_Action'Access);
      end if;


      --  Close the standard error stream, so that messages printed when the
      --  client cannot connect to Emacs are not seen by the user.
      --  ??? This has global effect on the whole application.

      --  Close (Standerr);
   end Register_Module;

end External_Editor_Module;
