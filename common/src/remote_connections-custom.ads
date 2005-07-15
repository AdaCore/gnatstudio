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

-- This is a generic remote connection that is customiwed from a XML file.
-- see remote_connections for a complete spec

with Glib.Xml_Int;

package Remote_Connections.Custom is

   type Custom_Connection is new Remote_Connection_Record with private;
   type Custom_Connection_Access is access all Custom_Connection`Class;

   procedure Initialize
     (Connection : access Custom_Connection'Class;
      Top        : Glib.Xml_Int.Node_Ptr);
   -- Initialize Connection based on the content of an XML node.
   -- The connection is automatically registered

   function Get_Protocol
     (Protocol : access Custom_Connection) return String;
   function Get_Description
     (Connection : access Custom_Connection) return String;
   procedure Close
     (Connection : access Custom_Connection;
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

   procedure Trace_Filter_Input
     (Descriptor : Process_Descriptor'Class;
      Str        : String;
      User_Data  : System.Address := System.Null_Address);
   procedure Trace_Filter_Output
     (Descriptor : Process_Descriptor'Class;
      Str        : String;
      User_Data  : System.Address := System.Null_Address);
   --  Filter all input/output of the shell

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

private

   type Custom_Connection is new Remote_Connection_Record with record
      null;
   end record;


end Remote_Connections.Custom;
