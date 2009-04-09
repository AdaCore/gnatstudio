-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                 Copyright (C) 2006-2009, AdaCore                  --
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

--  This package handles the remote servers assignment

with GNATCOLL.VFS;

package Remote is

   type Server_Type is
     (GPS_Server,
      Build_Server,
      Execution_Server,
      Debug_Server,
      Tools_Server);
   --  GPS_Server is always the local server
   --  Tools_Server is a special value pointing to Build_Server in the
   --   general case, but is the local server when the dual compilation
   --   mode is activated

   Local_Nickname         : String renames GNATCOLL.VFS.Local_Host;
   Display_Local_Nickname : constant String := "(local)";

   subtype Distant_Server_Type is Server_Type
     range Build_Server .. Debug_Server;

   procedure Assign
     (Server   : Distant_Server_Type;
      Nickname : String);

   function Is_Local (Server : Server_Type) return Boolean;
   --  Tell is the server is the localhost

   function Get_Nickname (Server : Server_Type) return String;
   --  Get the nickname of a server

   function Get_Printable_Nickname (Server : Server_Type) return String;
   --  Get the nickname of a server. If server is local, Local_Nickname is
   --  returned

   function Multi_Unit_Index_Char (Server : Server_Type) return Character;
   --  Return '~', except on VMS (todo) where it should return '$'

end Remote;
