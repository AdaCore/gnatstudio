-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                   Copyright (C) 2003                              --
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

with Glib;               use Glib;
with Gtk.Enums;          use Gtk.Enums;
with Remote_Connections; use Remote_Connections;
with GNAT.OS_Lib;        use GNAT.OS_Lib;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.Expect;        use GNAT.Expect;
with GNAT.Regpat;        use GNAT.Regpat;
pragma Warnings (Off);
with GNAT.Expect.TTY;    use GNAT.Expect.TTY;
pragma Warnings (On);
with Gtkada.Dialogs;     use Gtkada.Dialogs;
with GUI_Utils;          use GUI_Utils;
with String_Utils;       use String_Utils;
with System;
with Traces;             use Traces;
with Ada.Calendar;       use Ada.Calendar;
with Ada.Characters.Handling;  use Ada.Characters.Handling;
with GNAT.Calendar;      use GNAT.Calendar;
with Ada.Exceptions;     use Ada.Exceptions;
with Src_Info;

package body SSH_Protocol is

   Me : constant Debug_Handle := Create ("Remote");
   Full_Me : constant Debug_Handle := Create ("Remote_Full");

   Prompt_String : constant String := "---GPS--#";
   --  The string to print to simulate a prompt. It should match Prompt_Regexp

   End_Of_File_Mark : constant String := "GPSEOF";
   --  Mark sent at the end of a file when it is written back to the remote
   --  host

   Max_Passwd_Attempts : constant := 3;
   --  Maximum number of attempts to enter the password

   type Commands_Record is record
      Description         : String_Access;
      --  A description of the protocol

      Name                : String_Access;
      --  Name of the protocol; This is also the suffix in URLs

      Shell_Cmd           : String_Access;
      --  Command to execute to initialize the connection

      Read_File_Cmd       : String_Access;
      --  Command to execute on the remote host to see the contents of a file

      Exit_Status_Cmd     : String_Access;
      --  Command to execute on the host to get the status of the previous
      --  command. This is used in conjunction with the test commands. If this
      --  notion doesn't make sense on the system, this command should be the
      --  empty string.

      Is_Regular_File_Cmd : String_Access;
      --  Command to execute to find whether a file is readable. This will be
      --  executed just before Exit_Status_Cmd, and the file is considered
      --  readable if the combination of the two outputs is exactly "0"

      Is_Writable_Cmd     : String_Access;
      --  Same as Is_Regular_File_Cmd, but check whether the file is writable

      Is_Directory_Cmd    : String_Access;
      --  Same as Is_Regular_File_Cmd, but check whether the file is in fact
      --  a directory

      Delete_File_Cmd     : String_Access;
      --  Command to execute to delete a file

      Timestamp_Cmd       : String_Access;
      --  Command to execute to get the timestamp. The excepted output of the
      --  command is of the form "ddd mm yyyy" or "ddd mm hh:mm"

      Write_File_Cmd      : String_Access;
      --  Command to execute to write the file on the remote host. The contents
      --  of the file is sent on standard input, and the string
      --  End_Of_File_Mark is sent on its own at the end of the file

      Has_Shell_Prompt    : Boolean;
      --  Whether we are expecting a prompt from the remote shell, or whether
      --  we should emulate it.

      Set_Readable_Cmd    : String_Access;
      Set_Writable_Cmd    : String_Access;
      Set_Unreadable_Cmd  : String_Access;
      Set_Unwritable_Cmd  : String_Access;
      --  Commands to use to change the readable/writable status of files
   end record;
   --  In these commands, the following substitutions are available:
   --    %F => full local file name on the host, utf8 encoded
   --    %f => base local file name
   --    %d => directory local file name
   --    %h => host name
   --    %u => user name

   type Generic_Connection_Record is new Remote_Connection_Record with record
      Is_Open  : Boolean := False;
      Fd       : TTY_Process_Descriptor;
      Commands : Commands_Record;
   end record;
   type Generic_Connection is access all Generic_Connection_Record'Class;

   function Get_Protocol
     (Protocol : access Generic_Connection_Record) return String;
   function Get_Description
     (Connection : access Generic_Connection_Record) return String;
   procedure Close
     (Connection : access Generic_Connection_Record;
      GPS_Termination : Boolean);
   function Is_Regular_File
     (Connection      : access Generic_Connection_Record;
      Local_Full_Name : Glib.UTF8_String) return Boolean;
   function Read_File
     (Connection      : access Generic_Connection_Record;
      Local_Full_Name : Glib.UTF8_String)
      return GNAT.OS_Lib.String_Access;
   procedure Delete
     (Connection      : access Generic_Connection_Record;
      Local_Full_Name : Glib.UTF8_String);
   function Is_Writable
     (Connection      : access Generic_Connection_Record;
      Local_Full_Name : Glib.UTF8_String) return Boolean;
   function Is_Directory
     (Connection      : access Generic_Connection_Record;
      Local_Full_Name : Glib.UTF8_String) return Boolean;
   function File_Time_Stamp
     (Connection      : access Generic_Connection_Record;
      Local_Full_Name : Glib.UTF8_String)
      return Ada.Calendar.Time;
   procedure Write
     (Connection      : access Generic_Connection_Record;
      Local_Full_Name : Glib.UTF8_String;
      Contents        : String);
   procedure Set_Writable
     (Connection      : access Generic_Connection_Record;
      Local_Full_Name : Glib.UTF8_String;
      Writable        : Boolean);
   procedure Set_Readable
     (Connection      : access Generic_Connection_Record;
      Local_Full_Name : Glib.UTF8_String;
      Readable        : Boolean);
   function Factory
     (Protocol   : access Generic_Connection_Record;
      User, Host : String;
      Passwd     : String := "";
      Reuse      : Boolean := False) return Remote_Connection;

   procedure Ensure_Connection
     (Connection : access Generic_Connection_Record'Class);
   --  Make sure the connection is already open

   function Send_Cmd_And_Get_Result
     (Connection : access Generic_Connection_Record'Class;
      Cmd        : String;
      Cmd2       : String := "") return String;
   --  Send a command, and get its output.
   --  If Cmd2 is not the empty string, it is executed also, and its output is
   --  appended to the one of Cmd

   function Send_Cmd_And_Get_Result
     (Connection : access Generic_Connection_Record'Class;
      Cmd     : String) return Boolean;
   --  Execute command, and return its exit status.

   function Substitute
     (Cmd, Local_Full_Name, Host, User : String) return String;
   --  Substitute the special variables in Cmd (%f, %F, ...)

   procedure Trace_Filter_Input
     (Descriptor : Process_Descriptor'Class;
      Str        : String;
      User_Data  : System.Address := System.Null_Address);
   procedure Trace_Filter_Output
     (Descriptor : Process_Descriptor'Class;
      Str        : String;
      User_Data  : System.Address := System.Null_Address);
   --  Filter all input/output of the shell

   ------------------------
   -- Trace_Filter_Input --
   ------------------------

   procedure Trace_Filter_Input
     (Descriptor : Process_Descriptor'Class;
      Str        : String;
      User_Data  : System.Address := System.Null_Address)
   is
      pragma Unreferenced (Descriptor, User_Data);
   begin
      Trace (Full_Me, '>' & Str);
   end Trace_Filter_Input;

   -------------------------
   -- Trace_Filter_Output --
   -------------------------

   procedure Trace_Filter_Output
     (Descriptor : Process_Descriptor'Class;
      Str        : String;
      User_Data  : System.Address := System.Null_Address)
   is
      pragma Unreferenced (Descriptor, User_Data);
   begin
      Trace (Full_Me, '<' & Str);
   end Trace_Filter_Output;

   -----------------------
   -- Register_Protocol --
   -----------------------

   procedure Register_Protocol is
      SSH, Telnet, Rsh : Generic_Connection := new Generic_Connection_Record;
   begin
      SSH.Commands :=
        (Name                => new String'("ssh"),
         Description         => new String'
           ("This protocol is used to create and keep open a permanent SSH"
            & " connection to the remote host. Files are transfered inline"
            & " and the remote host is assumed to be a Unix-like system, which"
            & " can execute a /bin/sh shell. ""ssh"" itself must be found in"
            & " your PATH."),
         Shell_Cmd           => new String'("ssh %h -l %u -e none -q /bin/sh"),
         Has_Shell_Prompt    => False,
         Read_File_Cmd       => new String'("cat %F"),
         Exit_Status_Cmd     => new String'("echo $?"),
         Is_Regular_File_Cmd => new String'("test -r %F"),
         Is_Writable_Cmd     => new String'("test -w %F"),
         Is_Directory_Cmd    => new String'("test -d %F"),
         Delete_File_Cmd     => new String'("rm -f %F"),
         Timestamp_Cmd       =>
            new String'("ls -l %F 2>/dev/null| awk '{print $6,$7,$8}'"),
         Write_File_Cmd      =>
            new String'("cat > %F <<'" & End_Of_File_Mark & "'"),
         Set_Readable_Cmd    => new String'("chmod u+r %F"),
         Set_Writable_Cmd    => new String'("chmod u+w %F"),
         Set_Unreadable_Cmd  => new String'("chmod u-r %F"),
         Set_Unwritable_Cmd  => new String'("chmod u-w %F"));
      Remote_Connections.Register_Protocol (SSH);

      --  Share the strings, it doesn't matter since we don't free them
      Telnet.Commands                  := SSH.Commands;
      Telnet.Commands.Name             := new String'("telnet");
      Telnet.Commands.Has_Shell_Prompt := True;
      Telnet.Commands.Description      := new String'
        ("This protocol is used to create and keep open a permanent telnet"
         & " connection to the remote host. Files are transfered inline"
         & " and the remote host is assumed to be a Unix-like system, which"
         & " can execute a /bin/sh shell. ""telnet"" itself must be found"
         & " in your PATH.");
      Telnet.Commands.Shell_Cmd   := new String'("telnet -E %h ");
      Remote_Connections.Register_Protocol (Telnet);

      Rsh.Commands                  := SSH.Commands;
      Rsh.Commands.Name             := new String'("rsh");
      Rsh.Commands.Has_Shell_Prompt := True;
      Rsh.Commands.Description      := new String'
        ("This protocol is used to create and keep open a permanent rsh"
         & " connection to the remote host. Files are transfered inline"
         & " and the remote host is assumed to be a Unix-like system, which"
         & " can execute a /bin/sh shell. ""rsh"" itself must be found"
         & " in your PATH.");
      Rsh.Commands.Shell_Cmd   := new String'("rsh -l %u %h ");
      Remote_Connections.Register_Protocol (Rsh);

   end Register_Protocol;

   -----------------------
   -- Ensure_Connection --
   -----------------------

   procedure Ensure_Connection
     (Connection : access Generic_Connection_Record'Class) is
   begin
      if not Connection.Is_Open then
         declare
            Login_Match        : constant := 1;
            Passwd_Match       : constant := 2;
            Wrong_Passwd_Match : constant := 3;
            Shell_Prompt_Match : constant := 4;
            Unknown_Host_Match : constant := 5;
            Regexps            : constant Compiled_Regexp_Array :=
              (Login_Match        => Login_Regexp,
               Passwd_Match       => Passwd_Regexp,
               Wrong_Passwd_Match => Wrong_Passwd_Regexp,
               Shell_Prompt_Match => Shell_Prompt_Regexp,
               Unknown_Host_Match => Unknown_Host_Regexp);
            Connect_Cmd : constant String := Substitute
              (Connection.Commands.Shell_Cmd.all, "",
               Get_Host (Connection), Get_User (Connection));

            Args : Argument_List_Access;
            Passwd_Attempts : Natural := 1;

            --  Note: We *must* use a TTY_Process_Description, or we won't
            --  detect when we get asked for the password
            Result   : Expect_Match;
            Response : Message_Dialog_Buttons;
            pragma Unreferenced (Response);
         begin
            Args := Argument_String_To_List (Connect_Cmd);
            Trace (Me, "Connecting with " & Connect_Cmd);

            --  Do not create a TTY on the remote site, so that commands we
            --  send are not echoed. Otherwise, getting a file results in the
            --  "cat" command sometimes appearing
            Non_Blocking_Spawn
              (Connection.Fd,
               Command     => Args (Args'First).all,
               Args        => Args (Args'First + 1 .. Args'Last),
               Buffer_Size => 0,
               Err_To_Out  => True);

            if Active (Full_Me) then
               Add_Filter (Connection.Fd, Trace_Filter_Output'Access, Output);
               Add_Filter (Connection.Fd, Trace_Filter_Input'Access, Input);
            end if;

            Free (Args);

            if not Connection.Commands.Has_Shell_Prompt then
               Send (Connection.Fd, "echo " & Prompt_String, Add_LF => True);
            end if;

            loop
               Expect
                 (Connection.Fd,
                  Result,
                  Regexps,
                  Timeout => 10_000);   --  10 seconds

               case Result is
                  when Expect_Timeout =>
                     Response := Message_Dialog
                       ("Timeout when connection to "
                        & Get_Host (Connection),
                        Dialog_Type   => Error,
                        Buttons       => Button_OK,
                        Title         => "Time out",
                        Justification => Justify_Left);
                     Close (Connection.Fd);
                     Connection.Is_Open := False;
                     return;

                  when Login_Match =>
                     Send
                       (Connection.Fd, Get_User (Connection), Add_LF => True);

                  when Passwd_Match =>
                     Trace (Me, "Asking for password");
                     if Get_Passwd (Connection) /= "" then
                        Send (Connection.Fd,
                              Get_Passwd (Connection), Add_LF => True);
                     else
                        declare
                           Passwd : constant String := Query_Password
                             (Get_User (Connection) & "'s password on "
                              & Get_Host (Connection));
                        begin
                           Set_Passwd (Connection, Passwd);
                           if Passwd = "" then
                              Interrupt (Connection.Fd);
                              Connection.Is_Open := False;
                              return;
                           end if;
                           Send
                             (Connection.Fd, Passwd, Add_LF => True);

                           if not Connection.Commands.Has_Shell_Prompt then
                              Send (Connection.Fd, "echo " & Prompt_String,
                                    Add_LF => True);
                           end if;
                        end;
                     end if;

                  when Wrong_Passwd_Match =>
                     Trace (Me, "Invalid password "
                              & Expect_Out (Connection.Fd));
                     Passwd_Attempts := Passwd_Attempts + 1;
                     Response := Message_Dialog
                       ("Incorrect password",
                        Dialog_Type   => Error,
                        Buttons       => Button_OK,
                        Title         => "Incorrect password",
                        Justification => Justify_Left);
                     Set_Passwd (Connection, "");

                     if Passwd_Attempts > Max_Passwd_Attempts then
                        Close (Connection.Fd);
                        Connection.Is_Open := False;
                        return;
                     end if;

                  when Shell_Prompt_Match =>
                     Connection.Is_Open := True;
                     Trace (Me, "Connected");

                     if Connection.Commands.Has_Shell_Prompt then
                        Send (Connection.Fd, "stty -echo", Add_LF => True);
                        Expect
                          (Connection.Fd, Result, Shell_Prompt_Regexp.all);
                        Flush (Connection.Fd);
                     end if;

                     return;

                  when Unknown_Host_Match =>
                     Trace (Me, "Invalid host " & Expect_Out (Connection.Fd));
                     Response := Message_Dialog
                       ("Unknown host: " & Get_Host (Connection),
                        Dialog_Type   => Error,
                        Buttons       => Button_OK,
                        Title         => "Unknown host",
                        Justification => Justify_Left);
                     Close (Connection.Fd);
                     Connection.Is_Open := False;
                     return;

                  when others =>
                     null;
               end case;
            end loop;

         exception
            when Process_Died =>
               Close (Connection.Fd);
               Connection.Is_Open := False;
         end;
      end if;
   end Ensure_Connection;

   -----------------------------
   -- Send_Cmd_And_Get_Result --
   -----------------------------

   function Send_Cmd_And_Get_Result
     (Connection : access Generic_Connection_Record'Class;
      Cmd        : String;
      Cmd2       : String := "") return String
   is
      Result  : Expect_Match;
      Matched : Match_Array (0 .. 0);
   begin
      Trace (Me, "Sending " & Cmd);
      Send (Connection.Fd, Cmd, Add_LF => True, Empty_Buffer => True);

      if Cmd2 /= "" then
         Send (Connection.Fd, Cmd2, Add_LF => True);
      end if;

      if not Connection.Commands.Has_Shell_Prompt then
         Send (Connection.Fd, "echo " & Prompt_String, Add_LF => True);
      end if;

      Expect (Connection.Fd, Result, Shell_Prompt_Regexp.all, Matched);

      declare
         Output : constant String := Expect_Out (Connection.Fd);
      begin
         --  We should also ignore the last newline character, part of the
         --  prompt
         return Output (Output'First .. Matched (0).First - 2);
      end;

   exception
      when Process_Died =>
         Close (Connection, GPS_Termination => False);
         Connection.Is_Open := False;
         return "";
   end Send_Cmd_And_Get_Result;

   -----------------------------
   -- Send_Cmd_And_Get_Result --
   -----------------------------

   function Send_Cmd_And_Get_Result
     (Connection : access Generic_Connection_Record'Class;
      Cmd     : String) return Boolean
   is
      Result : constant String := Send_Cmd_And_Get_Result
        (Connection, Cmd & ';' & Connection.Commands.Exit_Status_Cmd.all);
      Index : Natural := Result'First;
      Last  : Natural;
      Success : Boolean;
   begin
      Skip_Blanks (Result, Index);
      Last := Index + 1;
      Skip_Blanks (Result, Last);
      Success := Index <= Result'Last
        and then Result (Index) = '0'
        and then Last > Result'Last;
      Trace (Me, "Result is " & Success'Img);
      return Success;
   end Send_Cmd_And_Get_Result;

   ------------------
   -- Get_Protocol --
   ------------------

   function Get_Protocol
     (Protocol : access Generic_Connection_Record) return String is
   begin
      return Protocol.Commands.Name.all;
   end Get_Protocol;

   ---------------------
   -- Get_Description --
   ---------------------

   function Get_Description
     (Connection : access Generic_Connection_Record) return String is
   begin
      return Connection.Commands.Description.all;
   end Get_Description;

   -----------
   -- Close --
   -----------

   procedure Close
     (Connection : access Generic_Connection_Record;
      GPS_Termination : Boolean)
   is
   begin
      if GPS_Termination then
         Close (Connection.Fd);
         Close
           (Remote_Connection_Record (Connection.all)'Access, GPS_Termination);
      else
         --  Do nothing, since we want to keep the connection open as long as
         --  possible
         null;
      end if;
   end Close;

   ---------------------
   -- Is_Regular_File --
   ---------------------

   function Is_Regular_File
     (Connection      : access Generic_Connection_Record;
      Local_Full_Name : Glib.UTF8_String) return Boolean
   is
      C : constant String := Substitute
        (Connection.Commands.Is_Regular_File_Cmd.all, Local_Full_Name,
         Get_Host (Connection), Get_User (Connection));
   begin
      Ensure_Connection (Connection);
      return Send_Cmd_And_Get_Result (Connection, C);
   end Is_Regular_File;

   ----------------
   -- Substitute --
   ----------------

   function Substitute
     (Cmd, Local_Full_Name, Host, User : String) return String
   is
      Substrings : Substitution_Array :=
        ((new String'("F"), new String'(Local_Full_Name)),
         (new String'("f"), new String'(Base_Name (Local_Full_Name))),
         (new String'("d"), new String'(Dir_Name (Local_Full_Name))),
         (new String'("h"), new String'(Host)),
         (new String'("u"), new String'(User)));
      C : constant String := Substitute
        (Cmd,
         Substitution_Char => '%',
         Substrings        => Substrings);
   begin
      Free (Substrings);
      return C;
   end Substitute;

   ---------------
   -- Read_File --
   ---------------

   function Read_File
     (Connection      : access Generic_Connection_Record;
      Local_Full_Name : Glib.UTF8_String)
      return GNAT.OS_Lib.String_Access
   is
      Cmd : constant String := Substitute
        (Connection.Commands.Read_File_Cmd.all, Local_Full_Name,
         Get_Host (Connection), Get_User (Connection));
   begin
      Ensure_Connection (Connection);
      return new String'(Send_Cmd_And_Get_Result (Connection, Cmd));
   end Read_File;

   ------------
   -- Delete --
   ------------

   procedure Delete
     (Connection      : access Generic_Connection_Record;
      Local_Full_Name : Glib.UTF8_String)
   is
      Cmd : constant String := Substitute
        (Connection.Commands.Delete_File_Cmd.all, Local_Full_Name,
         Get_Host (Connection), Get_User (Connection));
      Success : Boolean;
      pragma Unreferenced (Success);
   begin
      Ensure_Connection (Connection);
      Success := Send_Cmd_And_Get_Result (Connection, Cmd);
   end Delete;

   -----------------
   -- Is_Writable --
   -----------------

   function Is_Writable
     (Connection      : access Generic_Connection_Record;
      Local_Full_Name : Glib.UTF8_String) return Boolean
   is
      C : constant String := Substitute
        (Connection.Commands.Is_Writable_Cmd.all, Local_Full_Name,
         Get_Host (Connection), Get_User (Connection));
   begin
      Ensure_Connection (Connection);
      return Send_Cmd_And_Get_Result (Connection, C);
   end Is_Writable;

   ------------------
   -- Is_Directory --
   ------------------

   function Is_Directory
     (Connection      : access Generic_Connection_Record;
      Local_Full_Name : Glib.UTF8_String) return Boolean
   is
      C : constant String := Substitute
        (Connection.Commands.Is_Directory_Cmd.all, Local_Full_Name,
         Get_Host (Connection), Get_User (Connection));
   begin
      Ensure_Connection (Connection);
      return Send_Cmd_And_Get_Result (Connection, C);
   end Is_Directory;

   ---------------------
   -- File_Time_Stamp --
   ---------------------

   function File_Time_Stamp
     (Connection      : access Generic_Connection_Record;
      Local_Full_Name : Glib.UTF8_String)
      return Ada.Calendar.Time
   is
      function Safe_Value (S : String; Default : Integer := 1) return Integer;
      --  Same as 'Value but doesn't crash on invalid input

      function Safe_Value
        (S : String; Default : Integer := 1) return Integer is
      begin
         return Integer'Value (S);
      exception
         when Constraint_Error =>
            return Default;
      end Safe_Value;

      C : constant String := Substitute
        (Connection.Commands.Timestamp_Cmd.all, Local_Full_Name,
         Get_Host (Connection), Get_User (Connection));
   begin
      Ensure_Connection (Connection);

      declare
         Result : constant String := Send_Cmd_And_Get_Result (Connection, C);
         Index  : Natural := Result'First;
         Last   : Natural := Index;
         Month  : Month_Number := 1;
         Day    : Day_Number;
         Year   : Year_Number := Ada.Calendar.Year (Clock);
         Hour   : Hour_Number := 1;
         Minute : Minute_Number := 1;
      begin
         --  First part is the month. This can be either a digit or a literal

         Skip_To_Blank (Result, Last);

         if Last <= Result'Last
           and then Last > 2 + Index
         then
            declare
               M : constant String := To_Lower (Result (Index .. Index + 2));
            begin
               if M = "jan" then
                  Month := 1;
               elsif M = "feb" then
                  Month := 2;
               elsif M = "mar" then
                  Month := 3;
               elsif M = "apr" then
                  Month := 4;
               elsif M = "may" then
                  Month := 5;
               elsif M = "jun" then
                  Month := 6;
               elsif M = "jul" then
                  Month := 7;
               elsif M = "aug" then
                  Month := 8;
               elsif M = "sep" then
                  Month := 9;
               elsif M = "oct" then
                  Month := 10;
               elsif M = "nov" then
                  Month := 11;
               else
                  Month := 12;
               end if;
            end;
         end if;

         Skip_Blanks (Result, Last);
         Index := Last;
         Skip_To_Blank (Result, Last);

         Day := Day_Number (Safe_Value (Result (Index .. Last - 1)));

         Skip_Blanks (Result, Last);
         Index := Last;
         Skip_To_Blank (Result, Last);

         if Last - Index = 4 then
            Year := Year_Number (Safe_Value (Result (Index .. Last - 1)));
         elsif Index + 2 <= Result'Last
           and then Result (Index + 2) = ':'
         then
            Hour := Hour_Number (Safe_Value (Result (Index .. Index + 1)));
            Minute :=
              Minute_Number (Safe_Value (Result (Index + 3 .. Index + 4)));
         end if;

         return GNAT.Calendar.Time_Of
           (Year, Month, Day, Hour, Minute, Second => 0);
      end;

      return Src_Info.No_Time;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception " & Exception_Information (E));
         return Src_Info.No_Time;
   end File_Time_Stamp;

   -----------
   -- Write --
   -----------

   procedure Write
     (Connection      : access Generic_Connection_Record;
      Local_Full_Name : Glib.UTF8_String;
      Contents        : String)
   is
      C : constant String := Substitute
        (Connection.Commands.Write_File_Cmd.all, Local_Full_Name,
         Get_Host (Connection), Get_User (Connection));
   begin
      Ensure_Connection (Connection);

      declare
         Result : constant String := Send_Cmd_And_Get_Result
           (Connection, C, Cmd2 => Contents & ASCII.LF & End_Of_File_Mark);
         pragma Unreferenced (Result);
      begin
         null;
      end;
   end Write;

   -------------
   -- Factory --
   -------------

   function Factory
     (Protocol   : access Generic_Connection_Record;
      User, Host : String;
      Passwd     : String := "";
      Reuse      : Boolean := False) return Remote_Connection
   is
      C : Generic_Connection;
   begin
      if Reuse then
         return Remote_Connection (Protocol);
      else
         C := new Generic_Connection_Record;
         C.Commands := Protocol.Commands;
         C.Is_Open := False;
         Initialize (C, User, Host, Passwd);
         return Remote_Connection (C);
      end if;
   end Factory;

   ------------------
   -- Set_Writable --
   ------------------

   procedure Set_Writable
     (Connection      : access Generic_Connection_Record;
      Local_Full_Name : Glib.UTF8_String;
      Writable        : Boolean) is
   begin
      Ensure_Connection (Connection);
      if Writable then
         declare
            C : constant String := Substitute
              (Connection.Commands.Set_Writable_Cmd.all, Local_Full_Name,
               Get_Host (Connection), Get_User (Connection));
            Result : constant String :=
              Send_Cmd_And_Get_Result (Connection, C);
            pragma Unreferenced (Result);
         begin
            null;
         end;
      else
         declare
            C : constant String := Substitute
              (Connection.Commands.Set_Unwritable_Cmd.all, Local_Full_Name,
               Get_Host (Connection), Get_User (Connection));
            Result : constant String :=
              Send_Cmd_And_Get_Result (Connection, C);
            pragma Unreferenced (Result);
         begin
            null;
         end;
      end if;
   end Set_Writable;

   ------------------
   -- Set_Readable --
   ------------------

   procedure Set_Readable
     (Connection      : access Generic_Connection_Record;
      Local_Full_Name : Glib.UTF8_String;
      Readable        : Boolean) is
   begin
      Ensure_Connection (Connection);
      if Readable then
         declare
            C : constant String := Substitute
              (Connection.Commands.Set_Readable_Cmd.all, Local_Full_Name,
               Get_Host (Connection), Get_User (Connection));
            Result : constant String :=
              Send_Cmd_And_Get_Result (Connection, C);
            pragma Unreferenced (Result);
         begin
            null;
         end;
      else
         declare
            C : constant String := Substitute
              (Connection.Commands.Set_Unreadable_Cmd.all, Local_Full_Name,
               Get_Host (Connection), Get_User (Connection));
            Result : constant String :=
              Send_Cmd_And_Get_Result (Connection, C);
            pragma Unreferenced (Result);
         begin
            null;
         end;
      end if;
   end Set_Readable;

end SSH_Protocol;
