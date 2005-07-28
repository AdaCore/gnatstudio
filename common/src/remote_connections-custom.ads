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

--  This is a generic remote connection that is customized from a XML file.
--  See remote_connections for a complete spec

with Ada.Calendar;    use Ada.Calendar;
with GNAT.Expect;     use GNAT.Expect;
with GNAT.Expect.TTY; use GNAT.Expect.TTY;
with GNAT.OS_Lib;     use GNAT.OS_Lib;
with Glib;            use Glib;
with Glib.Xml_Int;    use Glib.Xml_Int;
with VFS;             use VFS;

package Remote_Connections.Custom is

   type Return_Enum is
     (OK,
      --  tells gps that the command is successfull
      NOK,
      --  tells gps that the command is unsuccessfull
      NOK_InvalidPassword,
      --  same as NOK + reports to the user that the password is invalid
      NOK_UnknownHost,
      --  same as NOK + reports to the user that the host is invalid
      NOK_Timeout,
      --  the command execution timed out
      No_Statement
      --  nothing to return
     );

   type Action_Enum is
     (Null_Action,
      --  null action: does nothing
      Spawn,
      --  local shell command
      Set_Session,
      --  set spawned command as session shell
      Return_Value,
      --  returns command result
      Input_Login,
      --  ask user for a login and send it to remote
      Input_Password,
      --  ask user for a password ans send it to remote
      Send,
      --  send param to remote
      Send_File,
      --  send file to remote
      Read_File,
      --  reads remote output as file
      Read_Timestamp,
      --  reads remote output as timestamp
      Read_Tmp_File,
      --  reads tmp file as file
      Create_Tmp_File
      --  creates tmp file for reading transfer
     );

   procedure Initialize_Regexps (Top : Glib.Xml_Int.Node_Ptr);
   --  Initialize Connection based on the content of an XML node.
   --  The connection is automatically registered

   type Custom_Connection is new Remote_Connection_Record with private;
   type Custom_Connection_Access is access all Custom_Connection'Class;

   procedure Initialize
     (Connection : access Custom_Connection'Class;
      Top        : Glib.Xml_Int.Node_Ptr);
   --  Initialize Connection based on the content of an XML node.
   --  The connection is automatically registered

   function Get_Protocol
     (Connection      : access Custom_Connection) return String;
   function Get_Description
     (Connection      : access Custom_Connection) return String;
   procedure Close
     (Connection      : access Custom_Connection;
      GPS_Termination : Boolean);
   function Is_Regular_File
     (Connection      : access Custom_Connection;
      Local_Full_Name : Glib.UTF8_String) return Boolean;
   function Read_File
     (Connection      : access Custom_Connection;
      Local_Full_Name : Glib.UTF8_String)
      return GNAT.OS_Lib.String_Access;
   procedure Delete
     (Connection      : access Custom_Connection;
      Local_Full_Name : Glib.UTF8_String);
   function Is_Writable
     (Connection      : access Custom_Connection;
      Local_Full_Name : Glib.UTF8_String) return Boolean;
   function Is_Directory
     (Connection      : access Custom_Connection;
      Local_Full_Name : Glib.UTF8_String) return Boolean;
   function File_Time_Stamp
     (Connection      : access Custom_Connection;
      Local_Full_Name : Glib.UTF8_String)
      return Ada.Calendar.Time;
   procedure Write
     (Connection      : access Custom_Connection;
      Local_Full_Name : Glib.UTF8_String;
      Temporary_File  : String);
   procedure Set_Writable
     (Connection      : access Custom_Connection;
      Local_Full_Name : Glib.UTF8_String;
      Writable        : Boolean);
   procedure Set_Readable
     (Connection      : access Custom_Connection;
      Local_Full_Name : Glib.UTF8_String;
      Readable        : Boolean);
   function Factory
     (Protocol   : access Custom_Connection;
      User, Host : String;
      Passwd     : String := "";
      Reuse      : Boolean := False) return Remote_Connection;

private

   type Answer_Regexp;
   type Answer_Regexp_Access is access all Answer_Regexp;

   type Answer_Regexp is record
      Id     : String_Ptr;
      Regexp : Pattern_Matcher_Access;
      Next   : Answer_Regexp_Access;
   end record;

   Null_Answer_Regexp : constant Answer_Regexp :=
     (Id     => null,
      Regexp => null,
      Next   => null);

   type Action_Record (kind : Action_Enum);
   type Action_Access is access all Action_Record;

   type Answer_Record;
   type Answer_Access is access all Answer_Record;

   type Action_Record (Kind : Action_Enum) is record
      Answers : Answer_Access;
      Next    : Action_Access;
      case Kind is
         when Spawn =>
            Cmd         : String_Ptr;

         when Return_Value =>
            Value       : Return_Enum;

         when Send =>
            Param       : String_Ptr;

         when others =>
            null;
      end case;
   end record;

   Null_Action_Record : constant Action_Record :=
     (kind    => Null_Action,
      Answers => null,
      Next    => null);

   type Answer_Record is record
      Regexp  : Pattern_Matcher_Access;
      Actions : Action_Access;
      Next    : Answer_Access;
   end record;

   Null_Answer_Record : constant Answer_Record :=
     (Regexp  => null,
      Actions => null,
      Next    => null);

   type Cmd_Enum is
     (Open_Session_Cmd,
      --  Command to execute to initialize the connection

      Read_File_Cmd,
      --  Command to execute on the remote host to see the contents of a file.

      Is_Regular_File_Cmd,
      --  Command to execute to find whether a file is readable. This will be
      --  executed just before Exit_Status_Cmd, and the file is considered
      --  readable if the combination of the two outputs is exactly "0"

      Is_Writable_Cmd,
      --  Same as Is_Regular_File_Cmd, but check whether the file is writable

      Is_Directory_Cmd,
      --  Same as Is_Regular_File_Cmd, but check whether the file is in fact
      --  a directory

      Delete_File_Cmd,
      --  Command to execute to delete a file

      Timestamp_Cmd,
      --  Command to execute to get the timestamp. The expected output of the
      --  command is that of the Unix "ls" utility.
      --  ??? Add support for other output formats, through regexps

      Write_File_Cmd,
      --  Command to execute to write the file on the remote host.

      Set_Readable_Cmd,
      Set_Writable_Cmd,
      Set_Unreadable_Cmd,
      Set_Unwritable_Cmd
      --  Commands to use to change the readable/writable status of files
     );

   type Command_Array is array (Cmd_Enum) of Action_Access;

   type Custom_Connection is new Remote_Connection_Record with record
      Description : String_Ptr;
      --  A description of the protocol

      Name : String_Ptr;
      --  Name of the protocol; This is also the suffix in URLs

      Max_Password_Attempts : Natural := 0;
      Password_Attempts     : Natural;
      --  Max and current number of password attempts

      Commands : Command_Array;
      --  All commands for the protocol

      --  Intermal values --

      Buffer               : GNAT.OS_Lib.String_Access;
      --  Buffer containing data readed
      Timestamp            : Ada.Calendar.Time;
      --  Last read timestamp

      Is_Open              : Boolean := False;
      Fd                   : TTY_Process_Descriptor;

      Last_Connection_Attempt : Time := VFS.No_Time;
      --  Time when we last attempted a connection, so that we avoid
      --  asking the password over and over again when the user cancelled a
      --  query.

      In_Get_Return_Status_Method : Boolean := False;

      Next : Custom_Connection_Access;
   end record;

end Remote_Connections.Custom;
