-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2006                         --
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
--  This package handles lists of remote paths and allows translation of paths
--  between a remote host and the local machine.

with Ada.Containers.Doubly_Linked_Lists;

package Remote.Path.Translator is

   ----------------------
   -- Main mirror list --
   ----------------------

   package Mirror_List is new Ada.Containers.Doubly_Linked_Lists
     (Element_Type => Mirror_Path);

   type Mirror_List_Access is access all Mirror_List.List'Class;

   function Get_List (Server : Server_Type) return Mirror_List_Access;
   --  Retrieve the list of paths for a specified server kind

   function Get_List (Server : String) return Mirror_List_Access;
   --  Same as above, with a server nickname

   -----------------------
   -- Utility functions --
   -----------------------

   function To_Remote_Possible
     (Path : String;
      To   : String) return Boolean;
   --  Tell if path equivalence on To server exists

   function To_Local_Possible
     (Path : String;
      From : String) return Boolean;
   --  Tell if From'path equivalence on local server exists

   function To_Remote
     (Path       : String;
      To         : Server_Type;
      Unix_Style : Boolean := False) return String;
   --  Translate a local file/directory path to server 'To'
   --  if Unix_Style is set, the translated path will have a unix style.

   function To_Remote
     (Path       : String;
      To         : String;
      Unix_Style : Boolean := False) return String;
   --  Same as above, using To's nickname instead of Server_Type

   function To_Local
     (Path : String;
      From : Server_Type) return String;
   --  Translate a remote file/directory path from server 'From' to local path.

   function To_Local
     (Path : String;
      From : String) return String;
   --  Same as above, using From's nickname instead of Server_Type.

   function To_Unix_Path
     (Path       : String;
      Server     : Server_Type;
      Use_Cygwin : Boolean := False) return String;
   --  Transform a remote path into unix path style.
   --  Use_Cygwin forces cygwin style path if filesystem of server is
   --  windows fs

end Remote.Path.Translator;
