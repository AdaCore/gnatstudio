------------------------------------------------------------------------------
--                                  G P S                                   --
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

with Ada.Unchecked_Deallocation;
with Ada.Strings.Fixed;

pragma Warnings (Off, ".*is an internal GNAT unit");
with GNAT.Expect.TTY.Remote;
pragma Warnings (On, ".*is an internal GNAT unit");
with GNAT.OS_Lib;
with GNAT.Regpat;            use GNAT.Regpat;

with GPS.Intl;               use GPS.Intl;
with GPS.Kernel.Interactive; use GPS.Kernel.Interactive;
with GPS.Kernel.Hooks;       use GPS.Kernel.Hooks;

with Basic_Types;            use Basic_Types;
with Interactive_Consoles;   use Interactive_Consoles;
with Password_Manager;       use Password_Manager;
with GNATCOLL.Traces;        use GNATCOLL.Traces;
with XML_Utils;              use XML_Utils;

package body Remote.Db is

   Me : constant Trace_Handle := Create ("GPS.REMOTE.DB");

   Prompt_Regexp : constant Pattern_Matcher :=
                     Compile
                       ("^[^\n]*[#$%>\]}\\] *$",
                        Multiple_Lines or Single_Line);
   --  Default regexp for shell prompts

   Login_Regexp  : constant Pattern_Matcher :=
                     Compile
                       ("^[^\n]*([Ll]ogin|[Nn]ame|[Cc]onnexion)[^\n]*: *$",
                        Multiple_Lines or Single_Line);
   --  Default regexp for login prompts

   procedure Parse_Machine_Node
     (Db        : access Remote_Db_Type;
      Kernel    : access Kernel_Handle_Record'Class;
      Node      : XML_Utils.Node_Ptr;
      Is_System : Boolean);
   --  Parse a remote_machine_descriptor node

   procedure Parse_Remote_Path_Node
     (Db        : access Remote_Db_Type;
      Kernel    : access Kernel_Handle_Record'Class;
      Node      : XML_Utils.Node_Ptr;
      Is_System : Boolean);
   --  Parse a remote_path node

   procedure Parse_Shell_Node
     (Db        : access Remote_Db_Type;
      Kernel    : access Kernel_Handle_Record'Class;
      Node      : XML_Utils.Node_Ptr);
   --  Parse a remote_shell_config node

   procedure Parse_Access_Tool_Node
     (Db        : access Remote_Db_Type;
      Kernel    : access Kernel_Handle_Record'Class;
      Node      : XML_Utils.Node_Ptr);
   --  Parse a remote_connection_config node

   procedure Parse_Sync_Tool_Node
     (Db        : access Remote_Db_Type;
      Kernel    : access Kernel_Handle_Record'Class;
      Node      : XML_Utils.Node_Ptr);
   --  Parse a remote_sync_config node

   procedure Free (Shell      : in out Shell_Access);
   procedure Free (Tool       : in out Access_Tool_Access);
   procedure Free (Sync_Tool  : in out Sync_Tool_Access);
   procedure Free (Shells     : in out Shell_Db.Map);
   procedure Free (Tools      : in out Access_Tools_Db.Map);
   procedure Free (Sync_Tools : in out Sync_Tools_Db.Map);
   procedure Free (Machines   : in out Machine_Db.Map);
   procedure Free (Points     : in out Mount_Points_Db.Map);
   --  Free the memory used by the parameters

   ------------------------
   -- Parse_Machine_Node --
   ------------------------

   procedure Parse_Machine_Node
     (Db        : access Remote_Db_Type;
      Kernel    : access Kernel_Handle_Record'Class;
      Node      : XML_Utils.Node_Ptr;
      Is_System : Boolean)
   is
      Nickname           : constant String :=
                             Get_Attribute (Node, "nickname");
      Network_Name       : constant String :=
                             Get_Attribute (Node, "network_name");
      Remote_Access      : constant String :=
                             Get_Attribute (Node, "remote_access");
      Remote_Shell       : constant String :=
                             Get_Attribute (Node, "remote_shell");
      Remote_Sync        : constant String :=
                             Get_Attribute (Node, "remote_sync", "rsync");
      Debug_Console      : constant String :=
                             Get_Attribute (Node, "debug_console", "false");
      Field              : XML_Utils.String_Ptr;
      Max_Nb_Connections : Natural;
      User_Name          : GNAT.Strings.String_Access;
      Timeout            : Natural;
      Cr_Lf              : Cr_Lf_Handling;
      Extra_Init_Cmds    : GNAT.Strings.String_List_Access;
      Nb_Init_Cmds       : Natural;
      Child              : Node_Ptr;
      Cmd                : Node_Ptr;
      Desc               : Machine_Access;

   begin
      if Nickname = "" then
         Kernel.Insert
           (-("XML Error: remote_machine_descriptor tags missing" &
              " a nickname attribute"),
            Add_LF => True, Mode => Error);

         return;
      end if;

      if Network_Name = "" then
         Kernel.Insert
           (-("XML Error: remote_machine_descriptor tags missing" &
              " a network_name attribute"),
            Add_LF => True, Mode => Error);

         return;
      end if;

      if Remote_Access = "" then
         Kernel.Insert
           (-("XML Error: remote_machine_descriptor tags missing" &
              " a remote_access attribute"),
            Add_LF => True, Mode => Error);

         return;
      end if;

      if Remote_Shell = "" then
         Kernel.Insert
           (-("XML Error: remote_machine_descriptor tags missing" &
              " a remote_shell attribute"),
            Add_LF => True, Mode => Error);

         return;
      end if;

      Field := Get_Field (Node, "max_nb_connections");

      if Field /= null then
         begin
            Max_Nb_Connections := Natural'Value (Field.all);
         exception
            when Constraint_Error =>
               Max_Nb_Connections := 3;
         end;
      else
         Max_Nb_Connections := 3;
      end if;

      Field := Get_Field (Node, "user_name");

      if Field /= null then
         User_Name := new String'(Field.all);
      else
         User_Name := new String'("");
      end if;

      Field := Get_Field (Node, "cr_lf");

      if Field /= null then
         Cr_Lf := Cr_Lf_Handling'Value (Field.all);
      else
         Cr_Lf := Auto;
      end if;

      Field := Get_Field (Node, "timeout");

      if Field /= null then
         begin
            Timeout := Natural'Value (Field.all);
         exception
            when Constraint_Error =>
               Timeout := 5000;
         end;
      else
         Timeout := 5000;
      end if;

      Extra_Init_Cmds := null;
      Nb_Init_Cmds := 0;
      Child := null;

      if Node.Child /= null then
         Child := Find_Tag (Node.Child, "extra_init_commands");
      end if;

      if Child /= null then
         Cmd := Child.Child;

         while Cmd /= null loop
            Nb_Init_Cmds := Nb_Init_Cmds + 1;
            Cmd := Cmd.Next;
         end loop;
      end if;

      if Nb_Init_Cmds /= 0 then
         Extra_Init_Cmds := new String_List (1 .. Nb_Init_Cmds);
         Cmd := Child.Child;

         for J in Extra_Init_Cmds'Range loop
            Extra_Init_Cmds (J) := new String'(Cmd.Value.all);
            Cmd := Cmd.Next;
         end loop;
      end if;

      Desc := new Machine_Type'
        (Kernel              => Kernel,
         Nickname            => new String'(Nickname),
         Network_Name        => new String'(Network_Name),
         Access_Tool_Name    => new String'(Remote_Access),
         Access_Tool         => null,
         Shell_Name          => new String'(Remote_Shell),
         Shell               => null,
         Sync_Tool_Name      => new String'(Remote_Sync),
         Sync_Tool           => null,
         Max_Nb_Connections  => Max_Nb_Connections,
         User_Name           => User_Name,
         Timeout             => Timeout,
         Cr_Lf               => Cr_Lf,
         Extra_Init_Commands => Extra_Init_Cmds,
         Use_Dbg             => Debug_Console = "true",
         User_Data           => null,
         Ref_Counter         => 1);

      Add_Or_Replace (Db, Desc, Is_System => Is_System);
   end Parse_Machine_Node;

   ----------------------------
   -- Parse_Remote_Path_Node --
   ----------------------------

   procedure Parse_Remote_Path_Node
     (Db        : access Remote_Db_Type;
      Kernel    : access Kernel_Handle_Record'Class;
      Node      : XML_Utils.Node_Ptr;
      Is_System : Boolean)
   is
      --  ??? We should support the system defined paths.
      pragma Unreferenced (Is_System);

      function Get_Sync (S : String) return Synchronisation_Type;
      --  Get sync type from S

      --------------
      -- Get_Sync --
      --------------

      function Get_Sync (S : String) return Synchronisation_Type is
      begin
         return Synchronisation_Type'Value (S);
      exception
         when Constraint_Error =>
            return Synchronisation_Type'First;
      end Get_Sync;

      Nickname : constant String := Get_Attribute (Node, "server_name", "");
      M_Point  : Mount_Point;
      Child    : Node_Ptr := Node.Child;

   begin
      while Child /= null loop
         M_Point :=
           (Local_Root  => Get_File_Child (Child, "local_path"),
            Remote_Root => Get_File_Child (Child, "remote_path", Nickname),
            Sync        => Get_Sync (Get_Attribute (Child, "sync", "Never")));

         if M_Point.Local_Root = No_File
           or else M_Point.Remote_Root = No_File
         then
            Kernel.Insert
              (-("XML Error: local_path and/or remote_path invalid in remote"
                 & " mode configuration"),
               Mode => Error);
            return;
         end if;

         --  At this point, we use M_Point.Remote_Path.Get_Host as it might
         --  differ from Nickname (not used anymore, redundant).
         if Db.Mount_Points.Contains (M_Point.Remote_Root.Get_Host) then
            declare
               Old : constant Mount_Point_Array :=
                       Db.Mount_Points.Element (M_Point.Remote_Root.Get_Host);
               Arr : Mount_Point_Array (Old'First .. Old'Last + 1);
            begin
               Arr (Old'First .. Old'Last) := Old;
               Arr (Arr'Last) := M_Point;
               Db.Mount_Points.Replace (M_Point.Remote_Root.Get_Host, Arr);
            end;
         else
            Db.Mount_Points.Insert
              (M_Point.Remote_Root.Get_Host, (1 => M_Point));
         end if;

         Child := Child.Next;
      end loop;
   end Parse_Remote_Path_Node;

   ----------------------
   -- Parse_Shell_Node --
   ----------------------

   procedure Parse_Shell_Node
     (Db        : access Remote_Db_Type;
      Kernel    : access Kernel_Handle_Record'Class;
      Node      : XML_Utils.Node_Ptr)
   is
      Tmp             : XML_Utils.String_Ptr;
      Shell_Name      : constant String :=
                          Get_Attribute (Node, "name", "");
      Shell_Cmd       : String_Access;
      Generic_Prompt  : Pattern_Matcher_Access;
      GPS_Prompt      : Pattern_Matcher_Access;
      FS              : FS_Type;
      No_Echo_Cmd     : String_Access;
      Init_Cmds       : String_List_Access;
      Exit_Cmds       : String_List_Access;
      Cd_Cmd          : String_Access;
      Get_Status_Cmd  : String_Access;
      Get_Status_Ptrn : Pattern_Matcher_Access;
      Desc            : Shell_Access;
      Cursor          : Machine_Db.Cursor;
      Machine_Desc    : Machine_Access;

      function Get_Command_List (Node : Node_Ptr) return String_List_Access;
      --  Retrieve all children's values as String_List.

      ----------------------
      -- Get_Command_List --
      ----------------------

      function Get_Command_List (Node : Node_Ptr) return String_List_Access is
         N_Cmds : Natural;
         Child  : Node_Ptr;
         Ret    : String_List_Access;

      begin
         if Node = null then
            return new String_List'(1 .. 0 => <>);
         end if;

         Child := Node.Child;
         N_Cmds := 0;

         while Child /= null loop
            N_Cmds := N_Cmds + 1;
            Child  := Child.Next;
         end loop;

         Ret    := new String_List (1 .. N_Cmds);
         Child  := Node.Child;
         N_Cmds := 0;

         while Child /= null loop
            N_Cmds := N_Cmds + 1;
            Ret (N_Cmds) := new String'(Child.Value.all);
            Child  := Child.Next;
         end loop;

         return Ret;
      end Get_Command_List;

   begin
      if Shell_Name = "" then
         Kernel.Insert
           (-"XML Error: missing 'name' attribute in remote_shell_config",
            Mode => Error);
         return;

      elsif Db.Shells.Contains (Shell_Name) then
         Kernel.Insert
           (-"XML Error: remote_shell_config " & Shell_Name &
            (-" is defined twice"),
            Mode => Error);
         return;
      end if;

      Tmp := Get_Field (Node, "start_command");
      if Tmp = null then
         Kernel.Insert
           (-"XML Error for " & Shell_Name &
            (-": missing 'start_command' child in remote_shell_config"),
            Mode => Error);
         return;
      end if;
      Shell_Cmd := new String'(Tmp.all);

      Tmp := Get_Field (Node, "generic_prompt");
      if Tmp /= null then
         Generic_Prompt := new Pattern_Matcher'
           (Compile (Tmp.all, Multiple_Lines));
      else
         Generic_Prompt := new Pattern_Matcher'(Prompt_Regexp);
      end if;

      Tmp := Get_Field (Node, "gps_prompt");
      if Tmp = null then
         Kernel.Insert
           ("XML Error for shell " & Shell_Name &
            ": missing 'gps_prompt' child in remote_shell_config",
            Mode => Error);
         return;
      end if;
      GPS_Prompt := new Pattern_Matcher'(Compile (Tmp.all, Multiple_Lines));

      Tmp := Get_Field (Node, "filesystem");
      if Tmp = null then
         Kernel.Insert
           ("XML Error in shell " & Shell_Name &
            ": missing 'filesystem' child in remote_shell_config",
            Mode => Error);
         return;
      end if;

      if Tmp.all = "windows" then
         FS := FS_Windows;
      elsif Tmp.all = "unix" then
         FS := FS_Unix;
      elsif Tmp.all = "unix-case-insensitive" then
         FS := FS_Unix_Case_Insensitive;
      else
         Kernel.Insert
           ("XML Error for shell " & Shell_Name &
            ": 'filesystem' child has " & Tmp.all &
            " value. Only 'windows' or 'unix' values are supported",
            Mode => Error);
         return;
      end if;

      Tmp := Get_Field (Node, "no_echo_command");
      if Tmp = null then
         No_Echo_Cmd := new String'("");
      else
         No_Echo_Cmd := new String'(Tmp.all);
      end if;

      Init_Cmds := Get_Command_List (Find_Tag (Node.Child, "init_commands"));
      Exit_Cmds := Get_Command_List (Find_Tag (Node.Child, "exit_commands"));

      Tmp := Get_Field (Node, "cd_command");
      if Tmp = null then
         Kernel.Insert
           ("XML Error for shell " & Shell_Name &
            ": missing 'cd_command' child in remote_shell_config",
            Mode => Error);
         return;
      end if;
      Cd_Cmd := new String'(Tmp.all);

      Tmp := Get_Field (Node, "get_status_command");
      if Tmp = null then
         Kernel.Insert
           ("XML Error for shell " & Shell_Name &
            ": missing 'get_status_command' child in " &
            "remote_shell_config",
            Mode => Error);
         return;
      end if;
      Get_Status_Cmd := new String'(Tmp.all);

      Tmp := Get_Field (Node, "get_status_ptrn");
      if Tmp = null then
         Kernel.Insert
           ("XML Error for shell " & Shell_Name &
            ": missing 'get_status_ptrn' child in remote_shell_config",
            Mode => Error);
         return;
      end if;
      Get_Status_Ptrn := new Pattern_Matcher'
        (Compile (Tmp.all, Multiple_Lines));

      Desc := new Shell_Record'
        (Name             => new String'(Shell_Name),
         Filesystem       => FS,
         Start_Cmd        => Shell_Cmd,
         No_Echo_Cmd      => No_Echo_Cmd,
         Init_Cmds        => Init_Cmds,
         Exit_Cmds        => Exit_Cmds,
         Cd_Cmd           => Cd_Cmd,
         Get_Status_Cmd   => Get_Status_Cmd,
         Get_Status_Ptrn  => Get_Status_Ptrn,
         Generic_Prompt   => Generic_Prompt,
         Prompt           => GPS_Prompt);

      --  Let's assume the Db is now resolved with the added shell
      Db.Resolved := True;

      Db.Shells.Insert (Shell_Name, Desc);

      for J in 1 .. 2 loop
         if J = 1 then
            Cursor := Db.Machines.First;
         else
            Cursor := Db.Sys_Machines.First;
         end if;

         while Machine_Db.Has_Element (Cursor) loop
            Machine_Desc := Machine_Db.Element (Cursor);

            if Machine_Desc.Shell = null
              and then Machine_Desc.Shell_Name.all = Shell_Name
            then
               Machine_Desc.Shell := Desc;
            end if;

            if Machine_Desc.Access_Tool = null
              or else Machine_Desc.Shell = null
              or else Machine_Desc.Sync_Tool = null
            then
               Db.Resolved := False;
            end if;

            Machine_Db.Next (Cursor);
         end loop;
      end loop;
   end Parse_Shell_Node;

   ----------------------------
   -- Parse_Access_Tool_Node --
   ----------------------------

   procedure Parse_Access_Tool_Node
     (Db        : access Remote_Db_Type;
      Kernel    : access Kernel_Handle_Record'Class;
      Node      : XML_Utils.Node_Ptr)
   is
      Child                     : Node_Ptr;
      Name                      : constant String :=
                                    Get_Attribute (Node, "name");
      Start_Command             : String_Access;
      Start_Command_Common_Args : String_List_Access;
      Start_Command_User_Args   : String_List_Access;
      Interrupt                 : String_Access;
      Tmp                       : XML_Utils.String_Ptr;
      User_Prompt_Ptrn          : Pattern_Matcher_Access;
      Password_Prompt_Ptrn      : Pattern_Matcher_Access;
      Passphrase_Prompt_Ptrn    : Pattern_Matcher_Access;
      Extra_Ptrn_Length         : Natural;
      Extra_Ptrns               : Extra_Prompt_Array_Access;
      Auto_Answer               : Boolean;
      Use_Pipes                 : Boolean;
      Desc                      : Access_Tool_Access;
      Cursor                    : Machine_Db.Cursor;
      Machine_Desc              : Machine_Access;

   begin
      if Name = "" then
         Kernel.Insert
           (-("XML Error: remote_connection_config tag is missing a " &
              "name attribute."),
            Add_LF => True, Mode => Error);
         return;

      elsif Db.Access_Tools.Contains (Name) then
         Kernel.Insert
           (-("XML Error: remote_connection_config has a duplicate " &
              "configuration for " & Name),
            Add_LF => True, Mode => Error);
         return;
      end if;

      Child := Find_Tag (Node.Child, "start_command");

      if Child /= null then
         Use_Pipes :=
           Boolean'Value (Get_Attribute (Child, "use_pipes", "false"));
         Start_Command := GNAT.OS_Lib.Locate_Exec_On_Path (Child.Value.all);

         if Start_Command = null then
            Trace
              (Me, "Ignoring Access tool " & Name & " as it cannot be found");
            --  No such tool on the system, just ignore this config
            return;

         elsif Ada.Strings.Fixed.Index (Start_Command.all, "Windows") in
           Start_Command'Range
         then
            --  Microsoft tools located in the Windows folder are not supported
            Trace
              (Me, "Ignoring Access tool " & Name &
               " as this version is not supported by GPS");
            Free (Start_Command);

            return;
         end if;

      else
         Kernel.Insert
           (-("XML Error: remote_connection_config is missing a " &
              "start_command field for " & Name),
            Add_LF => True, Mode => Error);

         return;
      end if;

      Tmp := Get_Field (Node, "start_command_common_args");
      if Tmp = null then
         Start_Command_Common_Args := new String_List'(1 .. 0 => <>);
      else
         Start_Command_Common_Args :=
           GNAT.OS_Lib.Argument_String_To_List (Tmp.all);
      end if;

      Tmp := Get_Field (Node, "start_command_user_args");
      if Tmp = null then
         Start_Command_User_Args := new String_List'(1 .. 0 => <>);
      else
         Start_Command_User_Args :=
           GNAT.OS_Lib.Argument_String_To_List (Tmp.all);
      end if;

      Tmp := Get_Field (Node, "send_interrupt");
      if Tmp = null then
         Interrupt := new String'("");
      else
         Interrupt := new String'(Tmp.all);
      end if;

      Tmp := Get_Field (Node, "user_prompt_ptrn");
      if Tmp /= null then
         User_Prompt_Ptrn := new Pattern_Matcher'
           (Compile (Tmp.all, Single_Line or Multiple_Lines));
      else
         User_Prompt_Ptrn := new Pattern_Matcher'(Login_Regexp);
      end if;

      Tmp := Get_Field (Node, "password_prompt_ptrn");
      if Tmp /= null then
         Password_Prompt_Ptrn := new Pattern_Matcher'
           (Compile (Tmp.all, Single_Line or Multiple_Lines));
      else
         Password_Prompt_Ptrn := new Pattern_Matcher'
           (Get_Default_Password_Regexp);
      end if;

      Tmp := Get_Field (Node, "passphrase_prompt_ptrn");
      if Tmp /= null then
         Passphrase_Prompt_Ptrn := new Pattern_Matcher'
           (Compile (Tmp.all, Single_Line or Multiple_Lines));
      else
         Passphrase_Prompt_Ptrn := new Pattern_Matcher'
           (Get_Default_Passphrase_Regexp);
      end if;

      Child := Node.Child;
      Extra_Ptrn_Length := 0;

      while Child /= null loop
         if Child.Tag.all = "extra_ptrn" then
            Extra_Ptrn_Length := Extra_Ptrn_Length + 1;
         end if;

         Child := Child.Next;
      end loop;

      Extra_Ptrns := new Extra_Prompt_Array (1 .. Extra_Ptrn_Length);

      Child := Node.Child;
      Extra_Ptrn_Length := 0;

      while Child /= null loop
         if Child.Tag.all = "extra_ptrn" then
            Extra_Ptrn_Length := Extra_Ptrn_Length + 1;
            Auto_Answer := Boolean'Value
              (Get_Attribute (Child, "auto_answer", "true"));

            if Auto_Answer then
               Extra_Ptrns (Extra_Ptrn_Length) :=
                 (Auto_Answer => True,
                  Ptrn        => new Pattern_Matcher'(Compile
                    (Child.Value.all, Single_Line or Multiple_Lines)),
                  Answer      => new String'(Get_Attribute
                    (Child, "answer", "")));
            else
               Extra_Ptrns (Extra_Ptrn_Length) :=
                 (Auto_Answer => False,
                  Ptrn        => new Pattern_Matcher'(Compile
                    (Child.Value.all, Single_Line or Multiple_Lines)),
                  Question    => new String'(Get_Attribute
                    (Child, "question", "")));
            end if;
         end if;

         Child := Child.Next;
      end loop;

      Desc := new Access_Tool_Record'
        (Name                   => new String'(Name),
         Start_Cmd              => Start_Command,
         Start_Cmd_Common_Args  => Start_Command_Common_Args,
         Start_Cmd_User_Args    => Start_Command_User_Args,
         Send_Interrupt         => Interrupt,
         User_Prompt_Ptrn       => User_Prompt_Ptrn,
         Password_Prompt_Ptrn   => Password_Prompt_Ptrn,
         Passphrase_Prompt_Ptrn => Passphrase_Prompt_Ptrn,
         Extra_Prompts          => Extra_Ptrns,
         Use_Pipes              => Use_Pipes,
         Max_Password_Prompt    => 3);

      Db.Access_Tools.Insert (Name, Desc);

      --  Let's assume the Db is now resolved with the added access tool
      Db.Resolved := True;

      for J in 1 .. 2 loop
         if J = 1 then
            Cursor := Db.Machines.First;
         else
            Cursor := Db.Sys_Machines.First;
         end if;

         while Machine_Db.Has_Element (Cursor) loop
            Machine_Desc := Machine_Db.Element (Cursor);

            if Machine_Desc.Access_Tool = null
              and then Machine_Desc.Access_Tool_Name.all = Name
            then
               Machine_Desc.Access_Tool := Desc;
            end if;

            if Machine_Desc.Access_Tool = null
              or else Machine_Desc.Shell = null
              or else Machine_Desc.Sync_Tool = null
            then
               Db.Resolved := False;
            end if;

            Machine_Db.Next (Cursor);
         end loop;
      end loop;
   end Parse_Access_Tool_Node;

   --------------------------
   -- Parse_Sync_Tool_Node --
   --------------------------

   procedure Parse_Sync_Tool_Node
     (Db        : access Remote_Db_Type;
      Kernel    : access Kernel_Handle_Record'Class;
      Node      : XML_Utils.Node_Ptr)
   is
      Name         : constant String := Get_Attribute (Node, "name");
      Child        : Node_Ptr;
      Args         : String_List_Access;
      Desc         : Sync_Tool_Access;
      Cursor       : Machine_Db.Cursor;
      Machine_Desc : Machine_Access;
   begin
      if Name = "" then
         Kernel.Insert
           (-("XML Error: remote_connection_config tag is missing a " &
              "name attribute."),
            Add_LF => True, Mode => Error);
         return;

      elsif Db.Access_Tools.Contains (Name) then
         Kernel.Insert
           (-("XML Error: remote_connection_config has a duplicate " &
              "configuration for " & Name),
            Add_LF => True, Mode => Error);
         return;
      end if;

      Child := Find_Tag (Node.Child, "arguments");

      if Child /= null then
         Args := GNAT.OS_Lib.Argument_String_To_List (Child.Value.all);
      end if;

      Desc := new Sync_Tool_Record'
        (Name => new String'(Name),
         Args => Args);

      Db.Sync_Tools.Insert (Name, Desc);

      --  Let's assume the Db is now resolved with the added sync tool
      Db.Resolved := True;

      for J in 1 .. 2 loop
         if J = 1 then
            Cursor := Db.Machines.First;
         else
            Cursor := Db.Sys_Machines.First;
         end if;

         while Machine_Db.Has_Element (Cursor) loop
            Machine_Desc := Machine_Db.Element (Cursor);

            if Machine_Desc.Sync_Tool = null
              and then Machine_Desc.Sync_Tool_Name.all = Name
            then
               Machine_Desc.Sync_Tool := Desc;
            end if;

            if Machine_Desc.Access_Tool = null
              or else Machine_Desc.Shell = null
              or else Machine_Desc.Sync_Tool = null
            then
               Db.Resolved := False;
            end if;

            Machine_Db.Next (Cursor);
         end loop;
      end loop;
   end Parse_Sync_Tool_Node;

   -------------------------
   -- Initialize_Database --
   -------------------------

   function Initialize_Database return Remote_Db_Type_Access is
      Ret : constant Remote_Db_Type_Access := new Remote_Db_Type;
   begin
      Gexpect.Db.Define_Machine_Db (Ret);
      GNATCOLL.Remote.Db.Define_Remote_Configuration (Ret);

      return Ret;
   end Initialize_Database;

   ----------
   -- Free --
   ----------

   procedure Free (Shell : in out Shell_Access) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Shell_Record, Shell_Access);
   begin
      if Shell /= null then
         Free (Shell.Name);
         Free (Shell.Start_Cmd);
         Free (Shell.No_Echo_Cmd);
         Free (Shell.Init_Cmds);
         Free (Shell.Exit_Cmds);
         Free (Shell.Cd_Cmd);
         Free (Shell.Get_Status_Cmd);
         Unchecked_Free (Shell.Get_Status_Ptrn);
         Unchecked_Free (Shell.Generic_Prompt);
         Unchecked_Free (Shell.Prompt);
         Unchecked_Free (Shell);
      end if;
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Prompt : in out Extra_Prompt) is
   begin
      Unchecked_Free (Prompt.Ptrn);

      case Prompt.Auto_Answer is
         when True  => Free (Prompt.Answer);
         when False => Free (Prompt.Question);
      end case;
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Prompts : in out Extra_Prompt_Array_Access) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Extra_Prompt_Array, Extra_Prompt_Array_Access);
   begin
      if Prompts /= null then
         for P in Prompts'Range loop
            Free (Prompts (P));
         end loop;
         Unchecked_Free (Prompts);
      end if;
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Tool : in out Access_Tool_Access) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Access_Tool_Record, Access_Tool_Access);
   begin
      if Tool /= null then
         Free (Tool.Name);
         Free (Tool.Start_Cmd);
         Free (Tool.Start_Cmd_Common_Args);
         Free (Tool.Start_Cmd_User_Args);
         Free (Tool.Send_Interrupt);
         Unchecked_Free (Tool.User_Prompt_Ptrn);
         Unchecked_Free (Tool.Password_Prompt_Ptrn);
         Unchecked_Free (Tool.Passphrase_Prompt_Ptrn);
         Free (Tool.Extra_Prompts);
         Unchecked_Free (Tool);
      end if;
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Shells : in out Shell_Db.Map) is
   begin
      for Item of Shells loop
         Free (Item);
      end loop;
      Shell_Db.Clear (Shells);
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Tools : in out Access_Tools_Db.Map) is
   begin
      for Item of Tools loop
         Free (Item);
      end loop;
      Access_Tools_Db.Clear (Tools);
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Sync_Tool  : in out Sync_Tool_Access) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Sync_Tool_Record, Sync_Tool_Access);
   begin
      if Sync_Tool /= null then
         Free (Sync_Tool.Name);
         Free (Sync_Tool.Args);
         Unchecked_Free (Sync_Tool);
      end if;
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Sync_Tools : in out Sync_Tools_Db.Map) is
   begin
      for Item of Sync_Tools loop
         Free (Item);
      end loop;
      Sync_Tools_Db.Clear (Sync_Tools);
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Machines : in out Machine_Db.Map) is
      use Machine_Db;
   begin
      Clear (Machines);
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Points : in out Mount_Points_Db.Map) is
      use Mount_Points_Db;
   begin
      Clear (Points);
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (DB : in out Remote_Db_Type_Access) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Remote_Db_Type'Class, Remote_Db_Type_Access);
   begin
      if DB /= null then
         Free (DB.Shells);
         Free (DB.Access_Tools);
         Free (DB.Sync_Tools);
         Free (DB.Machines);
         Free (DB.Sys_Machines);
         Free (DB.Mount_Points);
         Unchecked_Free (DB);
      end if;
   end Free;

   -------------------
   -- Read_From_XML --
   -------------------

   procedure Read_From_XML
     (Db        : access Remote_Db_Type;
      Kernel    : access Kernel_Handle_Record'Class;
      Node      : XML_Utils.Node_Ptr;
      Is_System : Boolean)
   is
   begin
      if Node.Tag.all = "remote_machine_descriptor" then
         Trace (Me, "Read_From_XML: 'remote_machine_descriptor'");
         Parse_Machine_Node (Db, Kernel, Node, Is_System);
      elsif Node.Tag.all = "remote_path_config" then
         Trace (Me, "Read_Form_XML: 'remote_path_config'");
         Parse_Remote_Path_Node (Db, Kernel, Node, Is_System);
      elsif Node.Tag.all = "remote_shell_config" then
         Trace (Me, "Read_From_XML: 'remote_shell_config'");
         Parse_Shell_Node (Db, Kernel, Node);
      elsif Node.Tag.all = "remote_connection_config" then
         Trace (Me, "Read_From_XML: 'remote_connection_config'");
         Parse_Access_Tool_Node (Db, Kernel, Node);
      elsif Node.Tag.all = "remote_sync_config" then
         Trace (Me, "Read_From_XML: 'remote_sync_config'");
         Parse_Sync_Tool_Node (Db, Kernel, Node);
      end if;
   end Read_From_XML;

   -----------------
   -- Save_To_XML --
   -----------------

   procedure Save_To_XML
     (Db      : access Remote_Db_Type;
      Node    : XML_Utils.Node_Ptr)
   is
      Machine_Cursor : Machine_Db.Cursor;
      Mount_P_Cursor : Mount_Points_Db.Cursor;
      Main_Child     : XML_Utils.Node_Ptr;
      Child          : XML_Utils.Node_Ptr;
      Machine        : Machine_Access;

   begin
      Machine_Cursor := Db.Machines.First;

      while Machine_Db.Has_Element (Machine_Cursor) loop
         Machine := Machine_Db.Element (Machine_Cursor);

         Main_Child := new XML_Utils.Node;
         Main_Child.Tag := new String'("remote_machine_descriptor");
         Add_Child (Node, Main_Child, True);

         Set_Attribute
           (Main_Child, "nickname", Nickname (Machine.all));
         Set_Attribute
           (Main_Child, "network_name", Network_Name (Machine.all));
         Set_Attribute
           (Main_Child, "remote_access", Access_Tool (Machine.all));
         Set_Attribute
           (Main_Child, "remote_shell", Shell (Machine.all));
         Set_Attribute
           (Main_Child, "remote_sync", Sync_Tool (Machine.all));
         Set_Attribute
           (Main_Child, "debug_console", Use_Dbg (Machine.all)'Img);

         Child := new XML_Utils.Node;
         Child.Tag := new String'("max_nb_connections");
         Child.Value := new String'(Max_Nb_Connections (Machine.all)'Img);
         Add_Child (Main_Child, Child, True);

         Child := new XML_Utils.Node;
         Child.Tag := new String'("user_name");
         Child.Value := new String'(User_Name (Machine.all));
         Add_Child (Main_Child, Child, True);

         Child := new XML_Utils.Node;
         Child.Tag := new String'("cr_lf");
         Child.Value := new String'(Cr_Lf (Machine.all)'Img);
         Add_Child (Main_Child, Child, True);

         Child := new XML_Utils.Node;
         Child.Tag := new String'("timeout");
         Child.Value := new String'(Timeout (Machine.all)'Img);
         Add_Child (Main_Child, Child, True);

         if Machine.Extra_Init_Commands /= null then
            Child := new XML_Utils.Node;
            Child.Tag := new String'("extra_init_commands");
            Add_Child (Main_Child, Child, True);

            for J in Machine.Extra_Init_Commands'Range loop
               declare
                  Cmd : Node_Ptr;
               begin
                  Cmd := new XML_Utils.Node;
                  Cmd.Tag := new String'("command");
                  Cmd.Value :=
                    new String'(Machine.Extra_Init_Commands (J).all);
                  Add_Child (Child, Cmd, True);
               end;
            end loop;
         end if;

         Machine_Db.Next (Machine_Cursor);
      end loop;

      Mount_P_Cursor := Db.Mount_Points.First;

      Main_Child := new XML_Utils.Node;
      Main_Child.Tag := new String'("remote_path_config");
      Add_Child (Node, Main_Child, True);

      while Mount_Points_Db.Has_Element (Mount_P_Cursor) loop
         declare
            Mount_Pts : constant Mount_Point_Array :=
                          Mount_Points_Db.Element (Mount_P_Cursor);
         begin
            for J in Mount_Pts'Range loop
               Child := new XML_Utils.Node;
               Child.Tag := new String'("mirror_path");
               Add_Child (Main_Child, Child);

               Set_Attribute (Child, "sync", Mount_Pts (J).Sync'Img);
               Add_File_Child
                 (Child, "local_path", Mount_Pts (J).Local_Root);
               Add_File_Child
                 (Child, "remote_path", Mount_Pts (J).Remote_Root);
            end loop;
         end;

         Mount_Points_Db.Next (Mount_P_Cursor);
      end loop;
   end Save_To_XML;

   -------------------
   -- Is_Configured --
   -------------------

   overriding function Is_Configured
     (Db       : Remote_Db_Type;
      Nickname : String)
      return Boolean
   is
      Machine : Machine_Access;
   begin
      --  The DB is fully resolved: no need to check for shell or access tools
      --  initialization in Machine.

      if Db.Resolved then

         return Db.Machines.Contains (Nickname)
           or else Db.Sys_Machines.Contains (Nickname);

      elsif Db.Machines.Contains (Nickname)
        or else Db.Sys_Machines.Contains (Nickname)
      then
         Machine := Db.Get_Machine (Nickname);

         return Machine.Shell /= null
           and then Machine.Access_Tool /= null;
      else
         return False;
      end if;
   end Is_Configured;

   -----------------
   -- Get_Servers --
   -----------------

   overriding function Get_Servers (Db : Remote_Db_Type) return String_List
   is
      N_Machines : Natural := Natural (Db.Machines.Length);
      Cursor     : Machine_Db.Cursor;

   begin
      Trace (Me, "Remote.Db.Get_Servers:");
      Cursor := Db.Sys_Machines.First;
      while Machine_Db.Has_Element (Cursor) loop
         --  Take care of overridden system machines.
         if not Db.Machines.Contains
           (Machine_Db.Element (Cursor).Nickname.all)
         then
            N_Machines := N_Machines + 1;
         end if;
         Machine_Db.Next (Cursor);
      end loop;

      declare
         Ret    : String_List (1 .. N_Machines);
         Idx    : Natural := 1;
      begin
         Cursor := Db.Machines.First;

         while Machine_Db.Has_Element (Cursor) loop
            Ret (Idx) := Machine_Db.Element (Cursor).Nickname;
            Trace (Me, " - " & Ret (Idx).all);

            Idx := Idx + 1;
            Machine_Db.Next (Cursor);
         end loop;

         Cursor := Db.Sys_Machines.First;

         while Machine_Db.Has_Element (Cursor) loop
            --  Take care of overridden system machines.
            if not Db.Machines.Contains
              (Machine_Db.Element (Cursor).Nickname.all)
            then
               Ret (Idx) := Machine_Db.Element (Cursor).Nickname;
               Trace (Me, " - " & Ret (Idx).all);

               Idx := Idx + 1;
            end if;

            Machine_Db.Next (Cursor);
         end loop;

         return Ret;
      end;
   end Get_Servers;

   ----------------
   -- Get_Shells --
   ----------------

   function Get_Shells (Db : Remote_Db_Type) return String_List
   is
      Ret    : String_List (1 .. Natural (Db.Shells.Length));
      Cursor : Shell_Db.Cursor;
   begin
      Cursor := Db.Shells.First;
      for J in Ret'Range loop
         Ret (J) := new String'(Shell_Db.Element (Cursor).Name.all);
         Shell_Db.Next (Cursor);
      end loop;

      return Ret;
   end Get_Shells;

   ----------------------
   -- Get_Access_Tools --
   ----------------------

   function Get_Access_Tools (Db : Remote_Db_Type) return String_List
   is
      Ret    : String_List (1 .. Natural (Db.Access_Tools.Length));
      Cursor : Access_Tools_Db.Cursor;
   begin
      Cursor := Db.Access_Tools.First;
      for J in Ret'Range loop
         Ret (J) := new String'(Access_Tools_Db.Element (Cursor).Name.all);
         Access_Tools_Db.Next (Cursor);
      end loop;

      return Ret;
   end Get_Access_Tools;

   ---------------------
   -- Get_Sync_Tools --
   ---------------------

   function Get_Sync_Tools (Db : Remote_Db_Type) return String_List
   is
      Ret    : String_List (1 .. Natural (Db.Sync_Tools.Length));
      Cursor : Sync_Tools_Db.Cursor;
   begin
      Cursor := Db.Sync_Tools.First;
      for J in Ret'Range loop
         Ret (J) := new String'(Sync_Tools_Db.Element (Cursor).Name.all);
         Sync_Tools_Db.Next (Cursor);
      end loop;

      return Ret;
   end Get_Sync_Tools;

   ----------------
   -- Get_Server --
   ----------------

   overriding function Get_Server
     (Db       : Remote_Db_Type;
      Nickname : String)
      return Gexpect.Machine_Access
   is
   begin
      return Gexpect.Machine_Access
        (Db.Get_Machine (Nickname));
   end Get_Server;

   ----------------
   -- Get_Server --
   ----------------

   overriding function Get_Server
     (Config   : Remote_Db_Type;
      Nickname : String)
      return GNATCOLL.Remote.Server_Access
   is
   begin
      return GNATCOLL.Remote.Server_Access
        (Config.Get_Machine (Nickname));
   end Get_Server;

   -----------------
   -- Get_Machine --
   -----------------

   function Get_Machine
     (Db       : Remote_Db_Type;
      Nickname : String) return Machine_Access
   is
   begin
      if Db.Machines.Contains (Nickname) then
         return Db.Machines.Element (Nickname);
      else
         return Db.Sys_Machines.Element (Nickname);
      end if;
   end Get_Machine;

   ----------------------
   -- Get_Mount_Points --
   ----------------------

   function Get_Mount_Points
     (Config   : Remote_Db_Type;
      Nickname : String) return Mount_Point_Array is
   begin
      if Config.Mount_Points.Contains (Nickname) then
         return Config.Mount_Points.Element (Nickname);
      else
         return (1 .. 0 => <>);
      end if;
   end Get_Mount_Points;

   ----------------------
   -- Set_Mount_Points --
   ----------------------

   procedure Set_Mount_Points
     (Config       : access Remote_Db_Type;
      Nickname     : String;
      Mount_Points : Mount_Point_Array) is
   begin
      if Config.Mount_Points.Contains (Nickname) then
         Config.Mount_Points.Delete (Nickname);
      end if;

      if Mount_Points'Length > 0 then
         Config.Mount_Points.Insert (Nickname, Mount_Points);
      end if;
   end Set_Mount_Points;

   --------------------
   -- Is_Sys_Default --
   --------------------

   function Is_Sys_Default
     (Config   : Remote_Db_Type;
      Nickname : String) return Boolean
   is
   begin
      return not Config.Machines.Contains (Nickname);
   end Is_Sys_Default;

   ---------------------
   -- Has_Sys_Default --
   ---------------------

   function Has_Sys_Default
     (Config   : Remote_Db_Type;
      Nickname : String) return Boolean is
   begin
      return Config.Sys_Machines.Contains (Nickname);
   end Has_Sys_Default;

   ---------------------
   -- Get_Sys_Default --
   ---------------------

   function Get_Sys_Default
     (Config   : Remote_Db_Type;
      Nickname : String) return Machine_Access is
   begin
      return Config.Sys_Machines.Element (Nickname);
   end Get_Sys_Default;

   --------------------
   -- Add_Or_Replace --
   --------------------

   procedure Add_Or_Replace
     (Config    : access Remote_Db_Type;
      Machine   : Machine_Access;
      Is_System : Boolean := False)
   is
   begin
      if Machine.Shell = null
        and then Config.Shells.Contains (Machine.Shell_Name.all)
      then
         Machine.Shell := Config.Shells.Element (Machine.Shell_Name.all);
      else
         Config.Resolved := False;
      end if;

      if Machine.Access_Tool = null
        and then Config.Access_Tools.Contains (Machine.Access_Tool_Name.all)
      then
         Machine.Access_Tool :=
           Config.Access_Tools.Element (Machine.Access_Tool_Name.all);
      else
         Config.Resolved := False;
      end if;

      if Machine.Sync_Tool = null
        and then Config.Sync_Tools.Contains (Machine.Sync_Tool_Name.all)
      then
         Machine.Sync_Tool :=
           Config.Sync_Tools.Element (Machine.Sync_Tool_Name.all);
      else
         Config.Resolved := False;
      end if;

      if Is_System then
         if Config.Sys_Machines.Contains (Machine.Nickname.all) then
            Insert (Machine.Kernel,
                    -"Warning: " &
                    Machine.Nickname.all &
                    (-" is defined twice !"),
                    Mode => Error);
            return;
         end if;

      elsif Config.Machines.Contains (Machine.Nickname.all) then
         if Config.Machines.Element (Machine.Nickname.all) /= Machine then
            Config.Remove (Machine.Nickname.all);
         else
            Trace (Me, "Add_Or_Replace called on the very same machine.");
            --  Updating with the very same machine.
            --  Close all previous connections and exit.

            GNAT.Expect.TTY.Remote.Close_All (Machine.Nickname.all);

            return;
         end if;
      end if;

      if Is_System then
         Config.Sys_Machines.Insert (Machine.Nickname.all, Machine);
      else
         Config.Machines.Insert (Machine.Nickname.all, Machine);
      end if;

      Server_List_Hook.Run (Machine.Kernel);
   end Add_Or_Replace;

   ------------
   -- Remove --
   ------------

   procedure Remove
     (Config   : access Remote_Db_Type;
      Nickname : String)
   is
      Old : Machine_Access;
   begin
      if not Config.Machines.Contains (Nickname) then
         return;
      end if;

      Old := Config.Machines.Element (Nickname);
      GNAT.Expect.TTY.Remote.Close_All (Nickname);
      Config.Machines.Delete (Nickname);

      if Config.Mount_Points.Contains (Nickname) then
         Config.Mount_Points.Delete (Nickname);
      end if;

      Server_List_Hook.Run (Old.Kernel);
      Unref (Old);
   end Remove;

   ---------------------
   -- Nb_Mount_Points --
   ---------------------

   overriding function Nb_Mount_Points
     (Config   : Remote_Db_Type;
      Nickname : String) return Natural is
   begin
      return Get_Mount_Points (Config, Nickname)'Length;
   end Nb_Mount_Points;

   --------------------------------
   -- Get_Mount_Point_Local_Root --
   --------------------------------

   overriding function Get_Mount_Point_Local_Root
     (Config   : Remote_Db_Type;
      Nickname : String;
      Index    : Natural) return FS_String
   is
      Ret : Filesystem_String renames
        Get_Mount_Points (Config, Nickname) (Index).Local_Root.Full_Name;
   begin
      return FS_String (Ret);
   end Get_Mount_Point_Local_Root;

   -------------------------------
   -- Get_Mount_Point_Host_Root --
   -------------------------------

   overriding function Get_Mount_Point_Host_Root
     (Config   : Remote_Db_Type;
      Nickname : String;
      Index    : Natural) return FS_String
   is
      Ret : Filesystem_String renames
        Get_Mount_Points (Config, Nickname) (Index).Remote_Root.Full_Name;
   begin
      return FS_String (Ret);
   end Get_Mount_Point_Host_Root;

   ---------
   -- Ref --
   ---------

   overriding procedure Ref (Machine : in out Machine_Type) is
   begin
      Machine.Ref_Counter := Machine.Ref_Counter + 1;
   end Ref;

   -----------
   -- Unref --
   -----------

   overriding procedure Unref (Machine : access Machine_Type) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Machine_Type'Class, Machine_Access);
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Machine_User_Data_Type'Class, Machine_User_Data_Access);

      The_Machine : Machine_Access := Machine_Access (Machine);
   begin
      Machine.Ref_Counter := Machine.Ref_Counter - 1;

      if Machine.Ref_Counter = 0 then
         Free (Machine.Nickname);
         Free (Machine.Network_Name);
         Free (Machine.Access_Tool_Name);
         Free (Machine.Shell_Name);
         Free (Machine.Extra_Init_Commands);
         Free (Machine.User_Name);
         Free (Machine.Sync_Tool_Name);

         if The_Machine.User_Data /= null then
            Free (The_Machine.User_Data.all);
            Unchecked_Free (The_Machine.User_Data);
         end if;

         Unchecked_Free (The_Machine);
      end if;
   end Unref;

   -----------------
   -- New_Machine --
   -----------------

   function New_Machine
     (Kernel   : access Kernel_Handle_Record'Class;
      Nickname : String) return Machine_Type
   is
      Ret : Machine_Type;
   begin
      Ret.Kernel := Kernel;
      Ret.Nickname := new String'(Nickname);
      return Ret;
   end New_Machine;

   --------------
   -- Nickname --
   --------------

   overriding function Nickname
     (Machine : Machine_Type)
      return String
   is
   begin
      return Machine.Nickname.all;
   end Nickname;

   ------------------
   -- Network_Name --
   ------------------

   overriding function Network_Name
     (Machine : Machine_Type)
      return String
   is
   begin
      if Machine.Network_Name = null then
         return "";
      end if;
      return Machine.Network_Name.all;
   end Network_Name;

   ----------------------
   -- Set_Network_Name --
   ----------------------

   procedure Set_Network_Name
     (Machine       : in out Machine_Type;
      Network_Name  : String)
   is
   begin
      Free (Machine.Network_Name);
      Machine.Network_Name := new String'(Network_Name);
   end Set_Network_Name;

   -----------------
   -- Access_Tool --
   -----------------

   overriding function Access_Tool
     (Machine : Machine_Type)
      return String
   is
   begin
      if Machine.Access_Tool_Name = null then
         return "";
      end if;
      return Machine.Access_Tool_Name.all;
   end Access_Tool;

   ---------------------
   -- Set_Access_Tool --
   ---------------------

   procedure Set_Access_Tool
     (Machine      : in out Machine_Type;
      Access_Tool  : String)
   is
   begin
      Free (Machine.Access_Tool_Name);
      Machine.Access_Tool_Name := new String'(Access_Tool);
      Machine.Access_Tool      := null;
   end Set_Access_Tool;

   -----------
   -- Shell --
   -----------

   overriding function Shell
     (Machine : Machine_Type)
      return String
   is
   begin
      if Machine.Shell_Name = null then
         return "";
      end if;

      return Machine.Shell_Name.all;
   end Shell;

   ---------------
   -- Set_Shell --
   ---------------

   procedure Set_Shell
     (Machine : in out Machine_Type;
      Shell   : String)
   is
   begin
      Free (Machine.Shell_Name);
      Machine.Shell_Name := new String'(Shell);
      Machine.Shell      := null;
   end Set_Shell;

   ---------------
   -- Sync_Tool --
   ---------------

   overriding function Sync_Tool
     (Machine : Machine_Type) return String
   is
   begin
      if Machine.Sync_Tool_Name = null then
         return "";
      end if;

      return Machine.Sync_Tool_Name.all;
   end Sync_Tool;

   --------------------
   -- Sync_Tool_Args --
   --------------------

   overriding function Sync_Tool_Args
     (Machine : Machine_Type) return String_List is
   begin
      if Machine.Sync_Tool = null then
         raise Invalid_Remote_Configuration;
      end if;

      if Machine.Sync_Tool.Args = null then
         return (1 .. 0 => <>);
      end if;

      return Machine.Sync_Tool.Args.all;
   end Sync_Tool_Args;

   -------------------
   -- Set_Sync_Tool --
   -------------------

   procedure Set_Sync_Tool
     (Machine   : in out Machine_Type;
      Sync_Func : String)
   is
   begin
      Free (Machine.Sync_Tool_Name);
      Machine.Sync_Tool_Name := new String'(Sync_Func);
      Machine.Sync_Tool := null;
   end Set_Sync_Tool;

   -------------------------
   -- Extra_Init_Commands --
   -------------------------

   overriding function Extra_Init_Commands
     (Machine : Machine_Type)
      return String_List
   is
   begin
      if Machine.Extra_Init_Commands = null then
         return (1 .. 0 => <>);
      end if;

      return Machine.Extra_Init_Commands.all;
   end Extra_Init_Commands;

   -----------------------------
   -- Set_Extra_Init_Commands --
   -----------------------------

   procedure Set_Extra_Init_Commands
     (Machine : in out Machine_Type;
      Cmds    : String_List)
   is
   begin
      Free (Machine.Extra_Init_Commands);
      Machine.Extra_Init_Commands := new String_List'(Cmds);
   end Set_Extra_Init_Commands;

   ---------------
   -- User_Name --
   ---------------

   overriding function User_Name
     (Machine : Machine_Type)
      return String
   is
   begin
      if Machine.User_Name = null then
         return "";
      end if;

      return Machine.User_Name.all;
   end User_Name;

   -------------------
   -- Set_User_Name --
   -------------------

   overriding procedure Set_User_Name
     (Machine   : in out Machine_Type;
      User_Name : String)
   is
   begin
      Free (Machine.User_Name);
      Machine.User_Name := new String'(User_Name);
   end Set_User_Name;

   ------------------------
   -- Max_Nb_Connections --
   ------------------------

   overriding function Max_Nb_Connections
     (Machine : Machine_Type)
      return Natural
   is
   begin
      return Machine.Max_Nb_Connections;
   end Max_Nb_Connections;

   ----------------------------
   -- Set_Max_Nb_Connections --
   ----------------------------

   procedure Set_Max_Nb_Connections
     (Machine : in out Machine_Type;
      Nb      : Natural)
   is
   begin
      Machine.Max_Nb_Connections := Nb;
   end Set_Max_Nb_Connections;

   -------------
   -- Timeout --
   -------------

   overriding function Timeout
     (Machine : Machine_Type)
      return Natural
   is
   begin
      return Machine.Timeout;
   end Timeout;

   -----------------
   -- Set_Timeout --
   -----------------

   procedure Set_Timeout
     (Machine   : in out Machine_Type;
      MSeconds  : Natural)
   is
   begin
      Machine.Timeout := MSeconds;
   end Set_Timeout;

   -----------
   -- Cr_Lf --
   -----------

   overriding function Cr_Lf
     (Machine : Machine_Type)
      return Cr_Lf_Handling
   is
   begin
      return Machine.Cr_Lf;
   end Cr_Lf;

   ---------------
   -- Set_Cr_Lf --
   ---------------

   procedure Set_Cr_Lf
     (Machine : in out Machine_Type;
      Cr_Lf   : Cr_Lf_Handling)
   is
   begin
      Machine.Cr_Lf := Cr_Lf;
   end Set_Cr_Lf;

   -------------
   -- Use_Dbg --
   -------------

   overriding function Use_Dbg
     (Machine : Machine_Type) return Boolean is
   begin
      return Machine.Use_Dbg;
   end Use_Dbg;

   -----------------
   -- Set_Use_Dbg --
   -----------------

   procedure Set_Use_Dbg
     (Machine : in out Machine_Type;
      Dbg     : Boolean)
   is
   begin
      Machine.Use_Dbg := Dbg;
   end Set_Use_Dbg;

   ---------
   -- Dbg --
   ---------

   overriding procedure Dbg
     (Machine : access Machine_Type;
      Str     : String;
      Mode    : Mode_Type)
   is
      Console : Interactive_Console;
   begin
      Console := Create_Interactive_Console
        (Machine.Kernel, Machine.Nickname.all & " session");

      case Mode is
         when Input =>
            Insert (Console, Str, Add_LF => True, Highlight => True);
         when Output =>
            Insert (Console, Str, Add_LF => False, Highlight => False);
      end case;
   end Dbg;

   -------------------
   -- Shell_Command --
   -------------------

   overriding function Shell_Command
     (Machine : Machine_Type)
      return String
   is
   begin
      if Machine.Shell = null then
         raise Invalid_Remote_Configuration;
      end if;

      return Machine.Shell.Start_Cmd.all;
   end Shell_Command;

   --------------------------
   -- Shell_Generic_Prompt --
   --------------------------

   overriding function Shell_Generic_Prompt
     (Machine : Machine_Type)
      return Pattern_Matcher_Access
   is
   begin
      if Machine.Shell = null then
         raise Invalid_Remote_Configuration;
      end if;

      return Machine.Shell.Generic_Prompt;
   end Shell_Generic_Prompt;

   -----------------------------
   -- Shell_Configured_Prompt --
   -----------------------------

   overriding function Shell_Configured_Prompt
     (Machine : Machine_Type)
      return Pattern_Matcher_Access
   is
   begin
      if Machine.Shell = null then
         raise Invalid_Remote_Configuration;
      end if;

      return Machine.Shell.Prompt;
   end Shell_Configured_Prompt;

   --------------
   -- Shell_FS --
   --------------

   overriding function Shell_FS
     (Machine : Machine_Type)
      return FS_Type
   is
   begin
      if Machine.Shell = null then
         raise Invalid_Remote_Configuration;
      end if;

      return Machine.Shell.Filesystem;
   end Shell_FS;

   -----------------------
   -- Shell_No_Echo_Cmd --
   -----------------------

   overriding function Shell_No_Echo_Cmd
     (Machine : Machine_Type)
      return String
   is
   begin
      if Machine.Shell = null then
         raise Invalid_Remote_Configuration;
      end if;

      return Machine.Shell.No_Echo_Cmd.all;
   end Shell_No_Echo_Cmd;

   ---------------------
   -- Shell_Init_Cmds --
   ---------------------

   overriding function Shell_Init_Cmds
     (Machine : Machine_Type)
      return String_List
   is
   begin
      if Machine.Shell = null then
         raise Invalid_Remote_Configuration;
      end if;

      return Machine.Shell.Init_Cmds.all;
   end Shell_Init_Cmds;

   ---------------------
   -- Shell_Exit_Cmds --
   ---------------------

   overriding function Shell_Exit_Cmds
     (Machine : Machine_Type)
      return String_List
   is
   begin
      if Machine.Shell = null then
         raise Invalid_Remote_Configuration;
      end if;

      return Machine.Shell.Exit_Cmds.all;
   end Shell_Exit_Cmds;

   ------------------
   -- Shell_Cd_Cmd --
   ------------------

   overriding function Shell_Cd_Cmd
     (Machine : Machine_Type)
      return String
   is
   begin
      if Machine.Shell = null then
         raise Invalid_Remote_Configuration;
      end if;

      return Machine.Shell.Cd_Cmd.all;
   end Shell_Cd_Cmd;

   --------------------------
   -- Shell_Get_Status_Cmd --
   --------------------------

   overriding function Shell_Get_Status_Cmd
     (Machine : Machine_Type)
      return String
   is
   begin
      if Machine.Shell = null then
         raise Invalid_Remote_Configuration;
      end if;

      return Machine.Shell.Get_Status_Cmd.all;
   end Shell_Get_Status_Cmd;

   ------------------------------
   -- Shell_Get_Status_Pattern --
   ------------------------------

   overriding function Shell_Get_Status_Pattern
     (Machine : Machine_Type)
      return Pattern_Matcher_Access
   is
   begin
      if Machine.Shell = null then
         raise Invalid_Remote_Configuration;
      end if;

      return Machine.Shell.Get_Status_Ptrn;
   end Shell_Get_Status_Pattern;

   -------------------------
   -- Access_Tool_Command --
   -------------------------

   overriding function Access_Tool_Command
     (Machine : Machine_Type)
      return String
   is
   begin
      if Machine.Access_Tool = null then
         raise Invalid_Remote_Configuration;
      end if;

      return Machine.Access_Tool.Start_Cmd.all;
   end Access_Tool_Command;

   -----------------------------
   -- Access_Tool_Common_Args --
   -----------------------------

   overriding function Access_Tool_Common_Args
     (Machine : Machine_Type)
      return String_List
   is
   begin
      if Machine.Access_Tool = null then
         raise Invalid_Remote_Configuration;
      end if;

      return Machine.Access_Tool.Start_Cmd_Common_Args.all;
   end Access_Tool_Common_Args;

   ---------------------------
   -- Access_Tool_User_Args --
   ---------------------------

   overriding function Access_Tool_User_Args
     (Machine : Machine_Type)
      return String_List
   is
   begin
      if Machine.Access_Tool = null then
         raise Invalid_Remote_Configuration;
      end if;

      return Machine.Access_Tool.Start_Cmd_User_Args.all;
   end Access_Tool_User_Args;

   --------------------------------
   -- Access_Tool_Send_Interrupt --
   --------------------------------

   overriding function Access_Tool_Send_Interrupt
     (Machine : Machine_Type)
      return String
   is
   begin
      if Machine.Access_Tool = null then
         raise Invalid_Remote_Configuration;
      end if;

      return Machine.Access_Tool.Send_Interrupt.all;
   end Access_Tool_Send_Interrupt;

   ----------------------------------
   -- Access_Tool_User_Prompt_Ptrn --
   ----------------------------------

   overriding function Access_Tool_User_Prompt_Ptrn
     (Machine : Machine_Type)
      return Pattern_Matcher_Access
   is
   begin
      if Machine.Access_Tool = null then
         raise Invalid_Remote_Configuration;
      end if;

      return Machine.Access_Tool.User_Prompt_Ptrn;
   end Access_Tool_User_Prompt_Ptrn;

   --------------------------------------
   -- Access_Tool_Password_Prompt_Ptrn --
   --------------------------------------

   overriding function Access_Tool_Password_Prompt_Ptrn
     (Machine : Machine_Type)
      return Pattern_Matcher_Access
   is
   begin
      if Machine.Access_Tool = null then
         raise Invalid_Remote_Configuration;
      end if;

      return Machine.Access_Tool.Password_Prompt_Ptrn;
   end Access_Tool_Password_Prompt_Ptrn;

   ----------------------------------------
   -- Access_Tool_Passphrase_Prompt_Ptrn --
   ----------------------------------------

   overriding function Access_Tool_Passphrase_Prompt_Ptrn
     (Machine : Machine_Type)
      return Pattern_Matcher_Access
   is
   begin
      if Machine.Access_Tool = null then
         raise Invalid_Remote_Configuration;
      end if;

      return Machine.Access_Tool.Passphrase_Prompt_Ptrn;
   end Access_Tool_Passphrase_Prompt_Ptrn;

   -------------------------------
   -- Access_Tool_Extra_Prompts --
   -------------------------------

   overriding function Access_Tool_Extra_Prompts
     (Machine : Machine_Type)
      return Extra_Prompt_Array
   is
   begin
      if Machine.Access_Tool = null then
         raise Invalid_Remote_Configuration;
      end if;

      return Machine.Access_Tool.Extra_Prompts.all;
   end Access_Tool_Extra_Prompts;

   ---------------------------
   -- Access_Tool_Use_Pipes --
   ---------------------------

   overriding function Access_Tool_Use_Pipes
     (Machine : Machine_Type)
      return Boolean
   is
   begin
      if Machine.Access_Tool = null then
         raise Invalid_Remote_Configuration;
      end if;

      return Machine.Access_Tool.Use_Pipes;
   end Access_Tool_Use_Pipes;

   --------------
   -- Set_Data --
   --------------

   overriding procedure Set_Data
     (Machine : in out Machine_Type;
      Data    : Machine_User_Data_Access)
   is
   begin
      Machine.User_Data := Data;
   end Set_Data;

   --------------
   -- Get_Data --
   --------------

   overriding function Get_Data
     (Machine : Machine_Type)
      return Machine_User_Data_Access
   is
   begin
      return Machine.User_Data;
   end Get_Data;

   ----------------------
   -- Execute_Remotely --
   ----------------------

   overriding procedure Execute_Remotely
     (Server              : access Machine_Type;
      Args                : GNAT.Strings.String_List;
      Status              : out Boolean;
      Execution_Directory : FS_String := "") is
   begin
      GNAT.Expect.TTY.Remote.Sync_Execute
        (Host                => Server.Nickname.all,
         Args                => Args,
         Status              => Status,
         Execution_Directory => Filesystem_String (Execution_Directory));
   end Execute_Remotely;

   ----------------------
   -- Execute_Remotely --
   ----------------------

   overriding procedure Execute_Remotely
     (Server              : access Machine_Type;
      Args                : GNAT.Strings.String_List;
      Result              : out GNAT.Strings.String_Access;
      Status              : out Boolean;
      Execution_Directory : FS_String := "") is
   begin
      GNAT.Expect.TTY.Remote.Sync_Execute
        (Host                => Server.Nickname.all,
         Args                => Args,
         Out_Value           => Result,
         Status              => Status,
         Execution_Directory => Filesystem_String (Execution_Directory));
   end Execute_Remotely;

   --------------------
   -- Spawn_Remotely --
   --------------------

   overriding procedure Spawn_Remotely
     (Server              : access Machine_Type;
      Descriptor          : out GNAT.Expect.Process_Descriptor_Access;
      Args                : GNAT.Strings.String_List) is
   begin
      GNAT.Expect.TTY.Remote.Remote_Spawn
        (Descriptor      => Descriptor,
         Target_Nickname => Server.Nickname.all,
         Args            => Args);
   end Spawn_Remotely;

end Remote.Db;
