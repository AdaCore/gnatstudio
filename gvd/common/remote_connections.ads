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

--  This package provides a number of services to access remote hosts and
--  files.
--  A number of primitive operations are defined for the Connections. Some of
--  them cannot be fullfilled by all Connections, which should then return a
--  default value. For instance, HTTP doesn't provide an easy way to query the
--  timestamp of the file.
--  This package makes a distinction between protocols (e.g. ssh, http,...) and
--  a connection, which is a triple (protocol, user password).

with Glib;
with Ada.Calendar;
with GNAT.OS_Lib;
with GNAT.Expect;

package Remote_Connections is

   type Remote_Connection_Record is abstract tagged private;
   type Remote_Connection is access all Remote_Connection_Record'Class;
   --  The description of a Connection.
   --  This is specialized for a Protocol/User/Host triple, so if you are
   --  connecting to two different hosts through ssh for instance, you get
   --  asked the password twice, once for each host.

   function Get_Protocol
     (Connection : access Remote_Connection_Record) return String is abstract;
   --  Return the name of the protocol (this is the part that should be used
   --  in file names just before "://" (aka scheme in RFC 2396)

   function Get_Description
     (Connection : access Remote_Connection_Record) return String is abstract;
   --  Return a description of the protocol

   function Get_User
     (Connection : access Remote_Connection_Record) return String;
   --  Return the name of the user for which this Connection was open

   function Get_Host
     (Connection : access Remote_Connection_Record) return String;
   --  Return the remote host. This is not necessary a name, it could be an
   --  IP address as well.

   function Get_Passwd
     (Connection : access Remote_Connection_Record) return String;
   --  Return the password that the user used for this connection

   procedure Set_Passwd
     (Connection : access Remote_Connection_Record; Passwd : String);
   --  Change the passwd

   procedure Initialize
     (Connection : access Remote_Connection_Record;
      User, Host : String;
      Passwd     : String := "");
   --  Initialize a new connection. No connection to the remote host is
   --  required at this point, and it can be done lazily the first time a
   --  request is made.
   --  If Passwd is the empty string, it will be asked interactively to the
   --  user upon connection.

   procedure Close
     (Connection : access Remote_Connection_Record;
      GPS_Termination : Boolean);
   --  Close the connection. For permanent connections, this subprogram is
   --  allowed to do nothing. In some other cases, a connection can be shared
   --  for multiple files, and reference-counting can be used by incrementing
   --  a counter in Factory.
   --  If the connection is actually closed, the parent's inherited Close
   --  must be called to remove the connection from the list of active
   --  connections.
   --  GPS_Termination is set to True when GPS is exiting; at this point, the
   --  connection must be terminated to release the ressources to the system.

   function Is_Regular_File
     (Connection      : access Remote_Connection_Record;
      Local_Full_Name : Glib.UTF8_String) return Boolean is abstract;
   --  Return True if Local_Full_Name exists on the remote host.

   function Read_File
     (Connection      : access Remote_Connection_Record;
      Local_Full_Name : Glib.UTF8_String)
      return GNAT.OS_Lib.String_Access is abstract;
   --  Return the contents of an entire file.
   --  If the file cannot be found, return null.
   --  The caller is responsible for freeing the returned memory.
   --  No special encoding/decoding for charsets is done on the file.

   procedure Delete
     (Connection      : access Remote_Connection_Record;
      Local_Full_Name : Glib.UTF8_String) is abstract;
   --  Remove the file from the disk.
   --  If this operation is not available for this protocol, nothing is done.

   function Is_Writable
     (Connection      : access Remote_Connection_Record;
      Local_Full_Name : Glib.UTF8_String) return Boolean is abstract;
   --  Return True if File is writable.
   --  Some protocols are read-only (HTTP), and will always return False.

   function Is_Directory
     (Connection      : access Remote_Connection_Record;
      Local_Full_Name : Glib.UTF8_String) return Boolean is abstract;
   --  Return True if File is in fact a directory

   function File_Time_Stamp
     (Connection      : access Remote_Connection_Record;
      Local_Full_Name : Glib.UTF8_String)
      return Ada.Calendar.Time is abstract;
   --  Return the timestamp for this file.
   --  If the Connection doesn't support this operation, or the file
   --  doesn't exists, it should return a date of No_Time, so as to force, when
   --  possible, a read operation from the caller.

   procedure Write
     (Connection      : access Remote_Connection_Record;
      Local_Full_Name : Glib.UTF8_String;
      Temporary_File  : String) is abstract;
   --  Overwrite the contents of Local_Full_Name with Contents.

   procedure Set_Writable
     (Connection      : access Remote_Connection_Record;
      Local_Full_Name : Glib.UTF8_String;
      Writable        : Boolean) is abstract;
   --  If Writable is True, make the file writable, otherwise make the file
   --  unwritable.

   procedure Set_Readable
     (Connection      : access Remote_Connection_Record;
      Local_Full_Name : Glib.UTF8_String;
      Readable        : Boolean) is abstract;
   --  If Readable is True, make the file readable, otherwise make the file
   --  unreadable.

   --------------
   -- Registry --
   --------------
   --  The following subprograms provide a registry of currently known/open
   --  Connections.
   --  New protocols can be registered dynamically.
   --  When the user tries to access a remote host, we first check whether
   --  a similar connection was already open (same user name and host)*

   function Factory
     (Protocol   : access Remote_Connection_Record;
      User, Host : String;
      Passwd     : String := "";
      Reuse      : Boolean := False) return Remote_Connection is abstract;
   --  Create and return a new connection, using the same protocol as Protocol.
   --  If Passwd is the empty string, the password should be asked
   --  interactively upon connection.
   --  If Reuse is True, and the Protocol allows sharing of the connection,
   --  then Protocol itself can be returned.

   procedure Register_Protocol
     (Protocol : access Remote_Connection_Record'Class);
   --  Register a new connection factory. When a file starting with
   --  "protocol_name://" is seen, and if we do not have such a connection
   --  already, Factory is returned so that a new connection is initialized.

   function Get_Connection
     (Protocol, User, Host : String;
      Force_New : Boolean := False) return Remote_Connection;
   --  Get an existing (or create a new) connection.
   --  If such a connection already exists, and Force_New is false, we reuse
   --  that connection, so as to save time in the connection (and handshake for
   --  some protocols).
   --  If such a connection already exists, and Force_New is true, a new
   --  connection will be created. However, we will try and reuse the same
   --  password if possible, for user convenience.

   procedure Close_All_Connections;
   --  Force a closing of all the connections

   -------------
   -- Helpers --
   -------------
   --  The following subprograms are provided as convenience function that are
   --  shared by most Connections.
   --  See also the function GUI_Utils.Query_Password

   procedure Parse_URL
     (URL         : String;
      Protocol    : out GNAT.OS_Lib.String_Access;
      Remote_User : out GNAT.OS_Lib.String_Access;
      Remote_Host : out GNAT.OS_Lib.String_Access;
      Remote_Path : out GNAT.OS_Lib.String_Access);
   --  Parses a string starting with "protocol://user@host/dir/foo", and return
   --  its components. If these are not specified, the current user and/or
   --  current hostname are returned.
   --  All output parameters are set to null if URL is not a valid URL.

   function User_Login_Name return String;
   --  Return the login name of the user.
   --  This is based on the effective uid, not the real uid.
   --  If the environment variable LOGNAME, USERNAME or USER is set, these are
   --  used preferrably to the uid

   function Login_Regexp        return GNAT.Expect.Pattern_Matcher_Access;
   function Passwd_Regexp       return GNAT.Expect.Pattern_Matcher_Access;
   function Wrong_Passwd_Regexp return GNAT.Expect.Pattern_Matcher_Access;
   function Unknown_Host_Regexp return GNAT.Expect.Pattern_Matcher_Access;
   function Shell_Prompt_Regexp return GNAT.Expect.Pattern_Matcher_Access;
   --  Various precompiled regular expressions that can be used to detect
   --  standard messages (login request, password request, warning for
   --  incorrect password, and warning for incorrect host name).
   --  These are suitable for most Connections

private

   type Remote_Connection_Record is abstract tagged record
      Remote_User : GNAT.OS_Lib.String_Access;
      Remote_Host : GNAT.OS_Lib.String_Access;
      Passwd      : GNAT.OS_Lib.String_Access;
   end record;

end Remote_Connections;
