-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2005                           --
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

with Ada.Calendar;       use Ada.Calendar;
with Ada.Characters.Handling;  use Ada.Characters.Handling;
with Ada.Exceptions;     use Ada.Exceptions;
with Ada.IO_Exceptions;  use Ada.IO_Exceptions;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.Regpat;               use GNAT.Regpat;
pragma Warnings (Off);
with GNAT.Expect.TTY;    use GNAT.Expect.TTY;
pragma Warnings (On);
with GNAT.Calendar;      use GNAT.Calendar;
with Gtk.Enums;          use Gtk.Enums;
with Gtkada.Dialogs;     use Gtkada.Dialogs;
with GUI_Utils;          use GUI_Utils;
with OS_Utils;           use OS_Utils;
with String_Utils;       use String_Utils;
with Traces;             use Traces;
with VFS;                use VFS;

package body Remote_Connections.Custom is

   Me : constant Debug_Handle := Create ("Remote_Connections.Custom");

   Custom_Root : Custom_Connection_Access := null;
   --  List of all custom connections

   Prompt_String : constant String := "---GPS--#";
   --  The string to print to simulate a prompt. It should match Prompt_Regexp

   Internal_Prompt_Regexp : constant Pattern_Matcher :=
     Compile ('^' & Prompt_String & '$', Multiple_Lines);

   End_Of_File_Mark : constant String := "GPSEOF";
   --  Mark sent at the end of a file when it is written back to the remote
   --  host

   Temporary_Dir : constant String := Get_Tmp_Dir;
   --  Where temporary files are stored. This could also be in ~/.gps or
   --  the root project's object directory.

   Ls_Regexp : constant Pattern_Matcher := Compile
     (".(........)\s+\d+\s+([\w\d]+)\s+([\w\d]+)\s+(\d+)\s+(\w\w\w) (\d\d)"
      & "\s+((\d\d):(\d\d))?\s*(\d\d\d\d)?");
   Ls_Month_Parens : constant := 5;
   Ls_Day_Parens   : constant := 6;
   Ls_Hour_Parens  : constant := 8;
   Ls_Min_Parens   : constant := 9;
   Ls_Year_Parens  : constant := 10;
   --  Regexp to match the output of ls, and extract the required fields

   Max_Passwd_Attempts : constant := 2;
   --  Maximum number of attempts to enter the password

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Get_String (S : in String_Ptr) return String;
   --  returns "" if S is null, S.all else

   procedure Ensure_Connection
     (Connection : access Custom_Connection'Class);
   --  Make sure the connection is already open

   function Send_Cmd_And_Get_Result
     (Connection : access Custom_Connection'Class;
      Cmd        : String;
      Cmd2       : String := "";
      Cmd3       : String := "") return String;
   --  Send a command, and get its output.
   --  If Cmd2 is not the empty string, it is executed also, and its output is
   --  appended to the one of Cmd

   function Send_Cmd_And_Get_Result
     (Connection : access Custom_Connection'Class;
      Cmd     : String;
      Cmd2    : String := "") return Boolean;
   --  Execute command, and return its exit status.

   function Substitute
     (Cmd, Local_Full_Name : String;
      Connection           : access Custom_Connection'Class;
      Temporary_File : String := "") return String;
   --  Substitute the special variables in Cmd (%f, %F, ...)
   --  %t is only defined if Temporary_File is not the empty string.
   --  User_Name_Is_Explicit should be true if the user name was specified by
   --  the user in the command.

   procedure Open_Connection
     (Cmd                  : String;
      Connection           : access Custom_Connection'Class;
      Is_Interactive_Shell : Boolean;
      Fd                   : out TTY_Process_Descriptor;
      Is_Open              : out Boolean);
   --  Open a new connection with Cmd.
   --  The password is assumed to be the same as in Connection. However, if it
   --  doesn't match, the user will be asked interactively.
   --  If Is_Interactive_Shell is true, the connection is assumed to be
   --  interactive, and the shell will be setup.
   --  Is_Open is set to False if the connection couldn't be initialized.

   ----------------
   -- Get_String --
   ----------------

   function Get_String (S : in String_Ptr) return String is
   begin
      if S = null then
         return "";
      else
         return S.all;
      end if;
   end Get_String;

   -----------------------
   -- Ensure_Connection --
   -----------------------

   procedure Ensure_Connection
     (Connection : access Custom_Connection'Class)
   is
      Min_Delay_Between_Attempts : constant Duration := 10.0;  --  seconds
   begin
      if not Connection.Is_Open then
         if Clock - Connection.Last_Connection_Attempt >=
           Min_Delay_Between_Attempts
         then
            Connection.Last_Connection_Attempt := Clock;

            declare
               Connect_Cmd : constant String := Substitute
                 (Connection.Commands.Shell_Cmd.all, "", Connection);
            begin
               Open_Connection
                 (Connect_Cmd, Connection, True,
                  Connection.Fd, Connection.Is_Open);
            end;
         end if;
      end if;
   end Ensure_Connection;

   -----------------------------
   -- Send_Cmd_And_Get_Result --
   -----------------------------

   function Send_Cmd_And_Get_Result
     (Connection : access Custom_Connection'Class;
      Cmd        : String;
      Cmd2       : String := "";
      Cmd3       : String := "") return String
   is
      Result  : Expect_Match;
      Matched : Match_Array (0 .. 0);
   begin
      Trace (Me, "Sending " & Cmd);
      Send (Connection.Fd, Cmd, Add_LF => True, Empty_Buffer => True);

      if Cmd2 /= "" then
         Send (Connection.Fd, Cmd2, Add_LF => True);
      end if;

      if Cmd3 /= "" then
         Send (Connection.Fd, Cmd3, Add_LF => True);
      end if;

      if not Connection.Commands.Has_Shell_Prompt then
         Send (Connection.Fd, "echo " & Prompt_String, Add_LF => True);
      end if;

      Expect (Connection.Fd, Result, Internal_Prompt_Regexp, Matched);

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
     (Connection : access Custom_Connection'Class;
      Cmd     : String;
      Cmd2    : String := "") return Boolean
   is
   begin
      if Connection.Commands.Exit_Status_Cmd = null then
         return True;
      else
         declare
            Result : constant String := Send_Cmd_And_Get_Result
              (Connection, Cmd, Cmd2,
               Connection.Commands.Exit_Status_Cmd.all);
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
         end;
      end if;
   end Send_Cmd_And_Get_Result;

   ----------------
   -- Substitute --
   ----------------

   function Substitute
     (Cmd, Local_Full_Name : String;
      Connection           : access Custom_Connection'Class;
      Temporary_File : String := "") return String
   is
      function Sub (Param : String; Quoted : Boolean) return String;
      --  Perform the substitution for Param

      function Sub (Param : String; Quoted : Boolean) return String is
         pragma Unreferenced (Quoted);
      begin
         if Param = "F" then
            return Local_Full_Name;
         elsif Param = "f" then
            return Base_Name (Local_Full_Name);
         elsif Param = "d" then
            return Dir_Name (Local_Full_Name);
         elsif Param = "h" then
            return Get_Host (Connection);
         elsif Param = "u" then
            return Get_User (Connection);
         elsif Param = "U" then
            if User_Is_Specified (Connection) then
               if Connection.Commands.User_Name_In_Shell_Cmd = null then
                  raise Invalid_Substitution;
               else
                  return Substitute
                    (Connection.Commands.User_Name_In_Shell_Cmd.all,
                     Local_Full_Name, Connection, Temporary_File);
               end if;
            else
               return "";
            end if;
         elsif Param = "t" then
            return Temporary_File;
         else
            raise Invalid_Substitution;
         end if;
      end Sub;

      C : constant String := Substitute
        (Cmd,
         Substitution_Char => '%',
         Callback          => Sub'Unrestricted_Access);
   begin
      return C;
   end Substitute;

   ---------------------
   -- Open_Connection --
   ---------------------

   procedure Open_Connection
     (Cmd                  : String;
      Connection           : access Custom_Connection'Class;
      Is_Interactive_Shell : Boolean;
      Fd                   : out TTY_Process_Descriptor;
      Is_Open              : out Boolean)
   is
      Login_Match        : constant := 1;
      Passwd_Match       : constant := 2;
      Wrong_Passwd_Match : constant := 3;
      Shell_Prompt_Match : constant := 4;
      Unknown_Host_Match : constant := 5;
      Scp_Match          : constant := 6;
      Regexps            : constant Compiled_Regexp_Array :=
        (Login_Match        => Login_Regexp,
         Passwd_Match       => Passwd_Regexp,
         Wrong_Passwd_Match => Wrong_Passwd_Regexp,
         Shell_Prompt_Match => Shell_Prompt_Regexp,
         Unknown_Host_Match => Unknown_Host_Regexp,
         Scp_Match          => Scp_Regexp);

      Args : Argument_List_Access;
      Passwd_Attempts : Natural := 1;

      --  Note: We *must* use a TTY_Process_Description, or we won't
      --  detect when we get asked for the password
      Result   : Expect_Match;
      Response : Message_Dialog_Buttons;
      pragma Unreferenced (Response);

   begin
      Args := Argument_String_To_List (Cmd);
      Trace (Me, "Connecting with """ & Cmd & """");

      --  Do not create a TTY on the remote site, so that commands we
      --  send are not echoed. Otherwise, getting a file results in the
      --  "cat" command sometimes appearing
      Non_Blocking_Spawn
        (Fd,
         Command     => Args (Args'First).all,
         Args        => Args (Args'First + 1 .. Args'Last),
         Buffer_Size => 0,
         Err_To_Out  => True);

      Free (Args);

      if Is_Interactive_Shell
        and then not Connection.Commands.Has_Shell_Prompt
      then
         Send (Fd, "echo " & Prompt_String, Add_LF => True);
      end if;

      loop
         Expect (Fd, Result, Regexps, Timeout => 10_000);

         case Result is
            when Expect_Timeout =>
               Response := Message_Dialog
                 ("Timeout when connection to "
                  & Get_Host (Connection),
                  Dialog_Type   => Error,
                  Buttons       => Button_OK,
                  Title         => "Time out",
                  Justification => Justify_Left);
               Close (Fd);
               Is_Open := False;
               return;

            when Login_Match =>
               Send (Fd, Get_User (Connection), Add_LF => True);

            when Passwd_Match =>
               Trace (Me, "Asking for password");
               if Get_Passwd (Connection) /= "" then
                  Send (Fd, Get_Passwd (Connection), Add_LF => True);
               else
                  declare
                     Passwd : constant String := Query_Password
                       (Get_User (Connection) & "'s password on "
                        & Get_Host (Connection));
                  begin
                     Set_Passwd (Connection, Passwd);
                     if Passwd = "" then
                        Trace (Me, "Interrupted password query");
                        Interrupt (Fd);
                        Is_Open := False;
                        return;
                     end if;

                     Send (Fd, Passwd, Add_LF => True);

                     if Is_Interactive_Shell
                       and then not Connection.Commands.Has_Shell_Prompt
                     then
                        Send (Fd, "echo " & Prompt_String,
                              Add_LF => True);
                     end if;
                  end;
               end if;

            when Wrong_Passwd_Match =>
               Trace (Me, "Invalid password " & Expect_Out (Fd));
               Passwd_Attempts := Passwd_Attempts + 1;
               Response := Message_Dialog
                 ("Incorrect password",
                  Dialog_Type   => Error,
                  Buttons       => Button_OK,
                  Title         => "Incorrect password",
                  Justification => Justify_Left);
               Set_Passwd (Connection, "");

               if Passwd_Attempts > Max_Passwd_Attempts then
                  Close (Fd);
                  Is_Open := False;
                  return;
               end if;

            when Shell_Prompt_Match =>
               Is_Open := True;
               Trace (Me, "Connected");

               if Is_Interactive_Shell
                 and then Connection.Commands.Has_Shell_Prompt
               then
                  Send (Fd, "PS1=" & Prompt_String, Add_LF => True);
                  Expect (Fd, Result, Internal_Prompt_Regexp);
                  Send (Fd, "stty -echo", Add_LF => True);

                  Expect (Fd, Result, Internal_Prompt_Regexp);
                  Flush (Fd);
               end if;

               return;

            when Unknown_Host_Match =>
               Is_Open := False;
               Trace (Me, "Invalid host " & Expect_Out (Fd));
               Response := Message_Dialog
                 ("Unknown host: " & Get_Host (Connection),
                  Dialog_Type   => Error,
                  Buttons       => Button_OK,
                  Title         => "Unknown host",
                  Justification => Justify_Left);
               Close (Fd);
               Connection.Is_Open := False;
               return;

            when Scp_Match =>
               Is_Open := True;
               Trace (Me, "Scp OK");
               Close (Fd);
               return;

            when others =>
               null;
         end case;
      end loop;

   exception
      when Process_Died =>
         Trace (Me, "Timeout");
         Set_Passwd (Connection, "");
         Close (Fd);
         Is_Open := False;
   end Open_Connection;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Connection : access Custom_Connection'Class;
      Top        : Glib.Xml_Int.Node_Ptr)
   is
      Node        : Node_Ptr;
      Tmp_Str     : String_Ptr;

      function Get_String (S : String_Ptr) return String_Ptr;
      --  Return a deep copy of S, or null if S is null.

      procedure Parse_Boolean
        (Node   : Node_Ptr;
         Name   : String;
         Result : in out Boolean);
      --  Parse a boolean from field Name in a given Node.
      --  If the field cannot be found, set Result is not changed.

      procedure Parse_String
        (Node   : Node_Ptr;
         Name   : String;
         Result : in out String_Ptr);
      --  Parse a string from field Name is a given Node.
      --  If the field cannot be found, the result is not changed

      ----------------
      -- Get_String --
      ----------------

      function Get_String (S : String_Ptr) return String_Ptr is
      begin
         if S = null then
            return null;
         else
            return new String'(S.all);
         end if;
      end Get_String;

      -------------------
      -- Parse_Boolean --
      -------------------

      procedure Parse_Boolean
        (Node   : Node_Ptr;
         Name   : String;
         Result : in out Boolean)
      is
         Field : String_Ptr;
      begin
         Field := Get_Field (Node, Name);

         if Field /= null then
            Result := Boolean'Value (Field.all);
         end if;

      exception
         when Constraint_Error =>
            Result := False;
      end Parse_Boolean;

      ------------------
      -- Parse_String --
      ------------------

      procedure Parse_String
        (Node   : Node_Ptr;
         Name   : String;
         Result : in out String_Ptr)
      is
         Field : String_Ptr;
      begin
         Field := Get_Field (Node, Name);

         if Field /= null then
            if Field.all /= "null" then
               Result := Get_String (Field);
            else
               Result := null;
            end if;
         end if;
      end Parse_String;

   begin
      --  Insert connection in custom connections list
      Connection.Next := Custom_Root;
      Custom_Root     := Custom_Connection_Access (Connection);

      Connection.Name :=
        Get_String (Get_Field (Top, "Name"));
      Connection.Description :=
        Get_String (Get_Field (Top, "Description"));

      --  If Parent is defined, first set Commands to parent's ones
      Tmp_Str := Get_Field (Top, "Parent");
      if Tmp_Str /= null then
         declare
            Tmp_Connect : Custom_Connection_Access := Custom_Root.Next;
            The_Name    : constant String := To_Lower (Tmp_Str.all);
         begin
            while Tmp_Connect /= null loop
               if To_Lower (Tmp_Str.all) = The_Name then
                  Connection.Commands := Tmp_Connect.Commands;
                  exit;
               end if;

               Tmp_Connect := Tmp_Connect.Next;
            end loop;
         end;
      end if;

      --  Get Commands node
      Node := Find_Tag (Top.Child, "Commands");

      if Node /= null then
         Parse_String
           (Node   => Node,
            Name   => "Shell_Cmd",
            Result => Connection.Commands.Shell_Cmd);
         Parse_String
           (Node   => Node,
            Name   => "User_Name_In_Shell_Cmd",
            Result => Connection.Commands.User_Name_In_Shell_Cmd);
         Parse_String
           (Node   => Node,
            Name   => "Read_File_Cmd",
            Result => Connection.Commands.Read_File_Cmd);
         Parse_String
           (Node   => Node,
            Name   => "Inline_Read_File_Cmd",
            Result => Connection.Commands.Inline_Read_File_Cmd);
         Parse_String
           (Node   => Node,
            Name   => "Exit_Status_Cmd",
            Result => Connection.Commands.Exit_Status_Cmd);
         Parse_String
           (Node   => Node,
            Name   => "Is_Regular_File_Cmd",
            Result => Connection.Commands.Is_Regular_File_Cmd);
         Parse_String
           (Node   => Node,
            Name   => "Is_Writable_Cmd",
            Result => Connection.Commands.Is_Writable_Cmd);
         Parse_String
           (Node   => Node,
            Name   => "Is_Directory_Cmd",
            Result => Connection.Commands.Is_Directory_Cmd);
         Parse_String
           (Node   => Node,
            Name   => "Delete_File_Cmd",
            Result => Connection.Commands.Delete_File_Cmd);
         Parse_String
           (Node   => Node,
            Name   => "Timestamp_Cmd",
            Result => Connection.Commands.Timestamp_Cmd);
         Parse_String
           (Node   => Node,
            Name   => "Write_File_Cmd",
            Result => Connection.Commands.Write_File_Cmd);
         Parse_String
           (Node   => Node,
            Name   => "Inline_Write_File_Cmd",
            Result => Connection.Commands.Inline_Write_File_Cmd);
         Parse_Boolean
           (Node   => Node,
            Name   => "Has_Shell_Prompt",
            Result => Connection.Commands.Has_Shell_Prompt);
         Parse_String
           (Node   => Node,
            Name   => "Set_Readable_Cmd",
            Result => Connection.Commands.Set_Readable_Cmd);
         Parse_String
           (Node   => Node,
            Name   => "Set_Writable_Cmd",
            Result => Connection.Commands.Set_Writable_Cmd);
         Parse_String
           (Node   => Node,
            Name   => "Set_Unreadable_Cmd",
            Result => Connection.Commands.Set_Unreadable_Cmd);
         Parse_String
           (Node   => Node,
            Name   => "Set_Unwritable_Cmd",
            Result => Connection.Commands.Set_Unwritable_Cmd);
      end if;

      Remote_Connections.Register_Protocol (Connection);
   end Initialize;

   ------------------
   -- Get_Protocol --
   ------------------

   function Get_Protocol
     (Connection : access Custom_Connection) return String is
   begin
      return Get_String (Connection.Name);
   end Get_Protocol;

   ---------------------
   -- Get_Description --
   ---------------------

   function Get_Description
     (Connection : access Custom_Connection) return String is
   begin
      return Get_String (Connection.Description);
   end Get_Description;

   -----------
   -- Close --
   -----------

   procedure Close
     (Connection      : access Custom_Connection;
      GPS_Termination : Boolean) is
   begin
      if Connection.Is_Open and GPS_Termination then
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
     (Connection      : access Custom_Connection;
      Local_Full_Name : Glib.UTF8_String) return Boolean is
   begin
      if Connection.Commands.Is_Regular_File_Cmd = null then
         --  Assume the file exists, we'll make sure when we try to fetch it
         return True;
      else
         Trace (Me, "Is_Regular_File " & Local_Full_Name);
         Ensure_Connection (Connection);

         --  If we can't open the connection, assume the file doesn't exist.
         --  Otherwise, GPS will always believe there is an auto-save file
         --  available for the remote files.
         --  On the other hand, this means that when attempting to open an
         --  invalid remote file, GPS will try to create a new file since it
         --  thinks it doesn't have a regular file.

         return Connection.Is_Open and then Send_Cmd_And_Get_Result
           (Connection,
            Substitute
              (Connection.Commands.Is_Regular_File_Cmd.all, Local_Full_Name,
               Connection));
      end if;
   end Is_Regular_File;

   ---------------
   -- Read_File --
   ---------------

   function Read_File
     (Connection      : access Custom_Connection;
      Local_Full_Name : Glib.UTF8_String)
      return GNAT.OS_Lib.String_Access
   is
   begin
      if Connection.Commands.Inline_Read_File_Cmd /= null then
         Trace (Me, "Read_File inline " & Local_Full_Name);
         Ensure_Connection (Connection);

         if not Connection.Is_Open then
            Trace (Me, "File could not be read");
            return null;
         else
            return new String'
              (Send_Cmd_And_Get_Result
                 (Connection, Substitute
                    (Connection.Commands.Inline_Read_File_Cmd.all,
                     Local_Full_Name, Connection)));
         end if;

      elsif Connection.Commands.Read_File_Cmd /= null then
         declare
            Base_Tmp    : String (1 .. Temp_File_Len);
            Index       : Natural;
            Current_Dir : constant String := Get_Current_Dir;
            Fd          : File_Descriptor;
            Success     : Boolean;
            Result      : String_Access;
            Pd          : TTY_Process_Descriptor;

         begin
            Change_Dir (Temporary_Dir);
            Create_Temp_File (Fd, Base_Tmp);
            Change_Dir (Current_Dir);

            for J in Base_Tmp'Range loop
               exit when Base_Tmp (J) = ASCII.NUL;
               Index := J;
            end loop;

            declare
               Str_Base_Tmp : constant String :=
                 Temporary_Dir & Base_Tmp (Base_Tmp'First .. Index);
            begin
               if Connection.Commands.Read_File_Cmd
                 (Connection.Commands.Read_File_Cmd'First) = '@'
               then
                  Open_Connection
                    (Substitute
                       (Connection.Commands.Read_File_Cmd.all
                          (Connection.Commands.Read_File_Cmd'First + 1
                           .. Connection.Commands.Read_File_Cmd'Last),
                        Local_Full_Name, Connection,
                        Str_Base_Tmp),
                     Connection,
                     False,
                     Pd,
                     Is_Open => Success);

                  if Success then
                     Close (Pd);
                  else
                     return null;
                  end if;

               else
                  Trace (Me, "Read_File " & Local_Full_Name);
                  Ensure_Connection (Connection);

                  if not Connection.Is_Open then
                     Trace (Me, "File could not be read");
                     return null;
                  else
                     declare
                        Result : constant String := Send_Cmd_And_Get_Result
                          (Connection, Substitute
                             (Connection.Commands.Read_File_Cmd.all,
                              Local_Full_Name, Connection,
                              Str_Base_Tmp));
                        pragma Unreferenced (Result);

                     begin
                        null;
                     end;

                  end if;
               end if;
            end;

            Result := Read_File (Temporary_Dir & Base_Tmp);
            Delete_File (Temporary_Dir & Base_Tmp, Success);
            return Result;
         end;

      else
         return null;
      end if;
   end Read_File;

   ------------
   -- Delete --
   ------------

   procedure Delete
     (Connection      : access Custom_Connection;
      Local_Full_Name : Glib.UTF8_String)
   is
      Success : Boolean;
      pragma Unreferenced (Success);
   begin
      if Connection.Commands.Delete_File_Cmd = null then
         --  Don't do anything
         null;
      else
         Trace (Me, "Delete " & Local_Full_Name);
         Ensure_Connection (Connection);
         Success := Connection.Is_Open and then Send_Cmd_And_Get_Result
           (Connection,
            Substitute
              (Connection.Commands.Delete_File_Cmd.all, Local_Full_Name,
               Connection));
      end if;
   end Delete;

   -----------------
   -- Is_Writable --
   -----------------

   function Is_Writable
     (Connection      : access Custom_Connection;
      Local_Full_Name : Glib.UTF8_String) return Boolean
   is
   begin
      if Connection.Commands.Is_Writable_Cmd = null then
         --  Assume it is writable if we do not have a command defined for
         --  this, we'll check when we try to write it.

         return Connection.Commands.Write_File_Cmd /= null
           or else Connection.Commands.Inline_Write_File_Cmd /= null;

      else
         Trace (Me, "Is_Writable " & Local_Full_Name);
         Ensure_Connection (Connection);
         return Connection.Is_Open and then Send_Cmd_And_Get_Result
           (Connection,
            Substitute
              (Connection.Commands.Is_Writable_Cmd.all, Local_Full_Name,
               Connection));
      end if;
   end Is_Writable;

   ------------------
   -- Is_Directory --
   ------------------

   function Is_Directory
     (Connection      : access Custom_Connection;
      Local_Full_Name : Glib.UTF8_String) return Boolean is
   begin
      if Connection.Commands.Is_Directory_Cmd = null then
         --  Assume it is a directory only if it ends with a directory
         --  separator
         return Local_Full_Name (Local_Full_Name'Last) = '/'
           or else Local_Full_Name (Local_Full_Name'Last) = '\';
      else
         Trace (Me, "Is_Directory " & Local_Full_Name);
         Ensure_Connection (Connection);
         return Connection.Is_Open and then Send_Cmd_And_Get_Result
           (Connection,
            Substitute
              (Connection.Commands.Is_Directory_Cmd.all, Local_Full_Name,
               Connection));
      end if;
   end Is_Directory;

   ---------------------
   -- File_Time_Stamp --
   ---------------------

   function File_Time_Stamp
     (Connection      : access Custom_Connection;
      Local_Full_Name : Glib.UTF8_String)
      return Ada.Calendar.Time is
      function Safe_Value (S : String; Default : Integer := 1) return Integer;
      --  Same as 'Value but doesn't crash on invalid input

      function Month_Name_To_Number (M : String) return Month_Number;
      --  Return the month number corresponding to M

      ----------------
      -- Safe_Value --
      ----------------

      function Safe_Value
        (S : String; Default : Integer := 1) return Integer is
      begin
         return Integer'Value (S);
      exception
         when Constraint_Error =>
            return Default;
      end Safe_Value;

      --------------------------
      -- Month_Name_To_Number --
      --------------------------

      function Month_Name_To_Number (M : String) return Month_Number is
      begin
         if M = "jan" then
            return 1;
         elsif M = "feb" then
            return 2;
         elsif M = "mar" then
            return 3;
         elsif M = "apr" then
            return 4;
         elsif M = "may" then
            return 5;
         elsif M = "jun" then
            return 6;
         elsif M = "jul" then
            return 7;
         elsif M = "aug" then
            return 8;
         elsif M = "sep" then
            return 9;
         elsif M = "oct" then
            return 10;
         elsif M = "nov" then
            return 11;
         else
            return 12;
         end if;
      end Month_Name_To_Number;

   begin
      if Connection.Commands.Timestamp_Cmd = null then
         return VFS.No_Time;

      else
         Trace (Me, "Time_Stamp " & Local_Full_Name);
         Ensure_Connection (Connection);
         if not Connection.Is_Open then
            return VFS.No_Time;
         end if;

         declare
            C       : constant String := Substitute
              (Connection.Commands.Timestamp_Cmd.all, Local_Full_Name,
               Connection);
            Result  : constant String :=
              Send_Cmd_And_Get_Result (Connection, C);
            Month   : Month_Number := 1;
            Day     : Day_Number;
            Year    : Year_Number := Ada.Calendar.Year (Clock);
            Hour    : Hour_Number := 1;
            Minute  : Minute_Number := 1;
            Matched : Match_Array (0 .. 20);

         begin
            if Result = "" then
               return VFS.No_Time;
            end if;

            Match (Ls_Regexp, Result, Matched);
            if Matched (0) /= No_Match then
               Month := Month_Name_To_Number
                 (To_Lower (Result
                              (Matched (Ls_Month_Parens).First
                                 .. Matched (Ls_Month_Parens).Last)));

               Day := Day_Number
                 (Safe_Value
                    (Result
                       (Matched (Ls_Day_Parens).First
                          .. Matched (Ls_Day_Parens).Last)));

               if Matched (Ls_Hour_Parens) /= No_Match then
                  Hour := Hour_Number
                    (Safe_Value
                       (Result
                          (Matched (Ls_Hour_Parens).First
                             .. Matched (Ls_Hour_Parens).Last)));
                  Minute := Minute_Number
                    (Safe_Value
                       (Result
                          (Matched (Ls_Min_Parens).First
                             .. Matched (Ls_Min_Parens).Last)));
               end if;

               if Matched (Ls_Year_Parens) /= No_Match then
                  Year := Year_Number
                    (Safe_Value
                       (Result
                          (Matched (Ls_Year_Parens).First
                             .. Matched (Ls_Year_Parens).Last)));
               end if;

               return GNAT.Calendar.Time_Of
                 (Year, Month, Day, Hour, Minute, Second => 0);
            end if;
         end;
      end if;

      return VFS.No_Time;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception " & Exception_Information (E));
         return VFS.No_Time;
   end File_Time_Stamp;

   -----------
   -- Write --
   -----------

   procedure Write
     (Connection      : access Custom_Connection;
      Local_Full_Name : Glib.UTF8_String;
      Temporary_File  : String)
   is
      Success : Boolean;
      Pd      : TTY_Process_Descriptor;
   begin
      if Connection.Commands.Inline_Write_File_Cmd /= null then
         Trace (Me, "Write inline " & Local_Full_Name);
         Ensure_Connection (Connection);

         if Connection.Is_Open then
            declare
               Tmp : String_Access := Read_File (Temporary_File);
               C : constant String := Substitute
                 (Connection.Commands.Inline_Write_File_Cmd.all,
                  Local_Full_Name, Connection);
               Result : constant Boolean := Send_Cmd_And_Get_Result
                 (Connection, C,
                  Cmd2 => Tmp.all & ASCII.LF & End_Of_File_Mark);

            begin
               Free (Tmp);

               if not Result then
                  raise Use_Error;
               end if;
            end;
         else
            raise Use_Error;
         end if;

      elsif Connection.Commands.Write_File_Cmd /= null then
         if Connection.Commands.Write_File_Cmd
           (Connection.Commands.Write_File_Cmd'First) = '@'
         then
            Open_Connection
              (Substitute
                 (Connection.Commands.Write_File_Cmd
                    (Connection.Commands.Write_File_Cmd'First + 1
                       .. Connection.Commands.Write_File_Cmd'Last),
                  Local_Full_Name,
                  Connection,
                  Temporary_File),
               Connection,
               False,
               Pd,
               Success);
            if Success then
               Close (Pd);
            else
               raise Use_Error;
            end if;

         else
            declare
               Result : constant String := Send_Cmd_And_Get_Result
                 (Connection,
                  Substitute
                    (Connection.Commands.Write_File_Cmd.all,
                     Local_Full_Name, Connection,
                     Temporary_File));
               pragma Unreferenced (Result);

            begin
               null;
            end;
         end if;
      end if;
   end Write;

   ------------------
   -- Set_Writable --
   ------------------

   procedure Set_Writable
     (Connection      : access Custom_Connection;
      Local_Full_Name : Glib.UTF8_String;
      Writable        : Boolean) is
   begin
      if Connection.Commands.Set_Writable_Cmd = null then
         --  Nothing to do
         null;

      else
         Trace (Me, "Set_Writable " & Local_Full_Name);
         Ensure_Connection (Connection);

         if Connection.Is_Open then
            if Writable then
               declare
                  C : constant String := Substitute
                    (Connection.Commands.Set_Writable_Cmd.all, Local_Full_Name,
                     Connection);
                  Result : constant String :=
                    Send_Cmd_And_Get_Result (Connection, C);
                  pragma Unreferenced (Result);

               begin
                  null;
               end;

            else
               declare
                  C : constant String := Substitute
                    (Connection.Commands.Set_Unwritable_Cmd.all,
                     Local_Full_Name, Connection);
                  Result : constant String :=
                    Send_Cmd_And_Get_Result (Connection, C);
                  pragma Unreferenced (Result);

               begin
                  null;
               end;
            end if;
         end if;
      end if;
   end Set_Writable;

   ------------------
   -- Set_Readable --
   ------------------

   procedure Set_Readable
     (Connection      : access Custom_Connection;
      Local_Full_Name : Glib.UTF8_String;
      Readable        : Boolean) is
   begin
      if Connection.Commands.Set_Readable_Cmd = null then
         --  Nothing to do
         null;

      else
         Trace (Me, "Set_Readable " & Local_Full_Name);
         Ensure_Connection (Connection);

         if Connection.Is_Open then
            if Readable then
               declare
                  C : constant String := Substitute
                    (Connection.Commands.Set_Readable_Cmd.all, Local_Full_Name,
                     Connection);
                  Result : constant String :=
                    Send_Cmd_And_Get_Result (Connection, C);
                  pragma Unreferenced (Result);
               begin
                  null;
               end;

            else
               declare
                  C : constant String := Substitute
                    (Connection.Commands.Set_Unreadable_Cmd.all,
                     Local_Full_Name, Connection);
                  Result : constant String :=
                    Send_Cmd_And_Get_Result (Connection, C);
                  pragma Unreferenced (Result);

               begin
                  null;
               end;
            end if;
         end if;
      end if;
   end Set_Readable;

   -------------
   -- Factory --
   -------------

   function Factory
     (Protocol   : access Custom_Connection;
      User, Host : String;
      Passwd     : String := "";
      Reuse      : Boolean := False) return Remote_Connection
   is
      C : Custom_Connection_Access;
   begin
      if Reuse then
         return Remote_Connection (Protocol);
      else
         C := new Custom_Connection'(Protocol.all);
         C.Is_Open := False;
         Initialize (C, User, Host, Passwd);
         return Remote_Connection (C);
      end if;
   end Factory;

end Remote_Connections.Custom;
