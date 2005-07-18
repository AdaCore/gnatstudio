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

   type Commands_Record is record
      Shell_Cmd           : String_Ptr;
      --  Command to execute to initialize the connection

      User_Name_In_Shell_Cmd : String_Ptr;
      --  The substitution for %U in Shell_Cmd (this is only called if the
      --  user name was specified explicitly by the user, and %U is replaced
      --  with "" otherwise. Note that this part is substituted in turn, so it
      --  can contain special characters like %u

      Read_File_Cmd        : String_Ptr;
      Inline_Read_File_Cmd : String_Ptr;
      --  Command to execute on the remote host to see the contents of a file.
      --  The inline version is used if it is defined, and uses the same
      --  connection as previously open. The normal version indicates that the
      --  file will be fetched directly, and copied to a local temporary file
      --  the name of which is supplied in the %t parameter.
      --  If Read_File_Cmd starts with the '@' character, a new connection is
      --  started to emit the command, rather than sending the command to the
      --  already open connection. The new connection is immediately closed
      --  after the command.

      Exit_Status_Cmd     : String_Ptr;
      --  Command to execute on the host to get the status of the previous
      --  command. This is used in conjunction with the test commands. If this
      --  notion doesn't make sense on the system, this command should be the
      --  empty string.

      Is_Regular_File_Cmd : String_Ptr;
      --  Command to execute to find whether a file is readable. This will be
      --  executed just before Exit_Status_Cmd, and the file is considered
      --  readable if the combination of the two outputs is exactly "0"

      Is_Writable_Cmd     : String_Ptr;
      --  Same as Is_Regular_File_Cmd, but check whether the file is writable

      Is_Directory_Cmd    : String_Ptr;
      --  Same as Is_Regular_File_Cmd, but check whether the file is in fact
      --  a directory

      Delete_File_Cmd     : String_Ptr;
      --  Command to execute to delete a file

      Timestamp_Cmd       : String_Ptr;
      --  Command to execute to get the timestamp. The expected output of the
      --  command is that of the Unix "ls" utility.
      --  ??? Add support for other output formats, through regexps

      Write_File_Cmd        : String_Ptr;
      Inline_Write_File_Cmd : String_Ptr;
      --  Command to execute to write the file on the remote host. The contents
      --  of the file is sent on standard input, and the string
      --  End_Of_File_Mark is sent on its own at the end of the file
      --  The inline version is used if it is defined, and uses the same
      --  connection as previously open.
      --  The same behavior is provided for Write_File_Cmd as for
      --  Read_File_Cmd.

      Has_Shell_Prompt    : Boolean := False;
      --  Whether we are expecting a prompt from the remote shell, or whether
      --  we should emulate it.

      Set_Readable_Cmd    : String_Ptr;
      Set_Writable_Cmd    : String_Ptr;
      Set_Unreadable_Cmd  : String_Ptr;
      Set_Unwritable_Cmd  : String_Ptr;
      --  Commands to use to change the readable/writable status of files
   end record;
   --  In these commands, the following substitutions are available:
   --    %F => full local file name on the host, utf8 encoded
   --    %f => base local file name
   --    %d => directory local file name
   --    %h => host name
   --    %u => user name
   --    %t => temporary local file to use (only for read and write)
   --    %U => value of User_Name_In_Shell_Cmd, only if the user name was
   --          explicitly specified by the user, the empty string otherwise
   --  Any of these command can be left to null, and a reasonnable default is
   --  provided.

   type Custom_Connection is new Remote_Connection_Record with record
      Description : String_Ptr;
      --  A description of the protocol

      Name : String_Ptr;
      --  Name of the protocol; This is also the suffix in URLs

      Is_Open  : Boolean := False;
      Fd       : TTY_Process_Descriptor;
      Commands : Commands_Record;

      Last_Connection_Attempt : Time := VFS.No_Time;
      --  Time when we last attempted a connection, so that we avoid
      --  asking the password over and over again when the user cancelled a
      --  query.

      Next : Custom_Connection_Access := null;
   end record;

end Remote_Connections.Custom;
