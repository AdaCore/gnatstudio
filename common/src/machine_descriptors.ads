-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2006-2007                      --
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

--  This package contains remote machine descriptions.

with GNAT.OS_Lib;
with GNAT.Strings;    use GNAT.Strings;

with Connection_Debuggers; use Connection_Debuggers;

package Machine_Descriptors is

   type Machine_Descriptor_Record is tagged record
      Nickname            : String_Access;
      --  Identifier of the machine
      Network_Name        : String_Access;
      --  Used to access the server using the network
      Access_Name         : String_Access;
      --  Tool used to remotely access the server
      Shell_Name          : String_Access;
      --  Shell used on the remote server
      Extra_Init_Commands : GNAT.OS_Lib.Argument_List_Access := null;
      --  User specific init commands
      User_Name           : String_Access;
      --  User name used for connection
      Timeout             : Natural := 5000;
      --  Timeout value used when connecting to the machine (in ms)
      Max_Nb_Connections  : Natural := 3;
      --  Maximum number of simultaneous connections on the machine
      Ref                 : Natural := 0;
      --  Ref counter
      Dbg                 : Connection_Debugger := null;
      --  Connection debug console.
   end record;
   type Machine_Descriptor is access all Machine_Descriptor_Record'Class;

   type Machine_Descriptor_Item is tagged;
   type Machine_Descriptor_Access is access all Machine_Descriptor_Item'Class;

   type Machine_Descriptor_Item is abstract tagged record
      Desc : Machine_Descriptor;
      Next : Machine_Descriptor_Access;
   end record;

   procedure Close (Desc : access Machine_Descriptor_Item) is abstract;
   --  Close the connections associated with Desc.

   procedure Register_Machine_Descriptor
     (Machine    : Machine_Descriptor;
      Descriptor : Machine_Descriptor_Access);
   --  Adds a new machine descriptor.

   procedure Unref (Desc : in out Machine_Descriptor);
   --  Does Ref - 1. Free allocated memory for the descriptor if ref=0

   procedure Remove_Machine_Descriptor (Desc : in out Machine_Descriptor);
   --  Removes a machine descriptor.

   procedure Remove_All_Machine_Descriptors;
   --  Removes all machine descriptors.

   function Get_Machine_Descriptor
     (Nickname : String) return Machine_Descriptor;
   --  Get machine descriptor from nickname

   function Get_Machine_Descriptor_Access
     (Nickname : String) return Machine_Descriptor_Access;
   --  Get machine descriptor from nickname
   --  Raise Invalid_Nickname if the machine was not found.

   function Get_Machine_Descriptor (N : Natural) return Machine_Descriptor;
   --  Retrieve the descriptor of the Nth configured machine

   function Get_Nb_Machine_Descriptor return Natural;
   --  Get the total number of Machine Descriptor configured

   function Get_Nickname (N : Natural) return String;
   --  Retrieve the nickname of the Nth configured machine
   --  Raise Invalid_Nickname if N does not correspond to a server

   procedure Close_All;
   --  Close all connections associated with all machines.

end Machine_Descriptors;
