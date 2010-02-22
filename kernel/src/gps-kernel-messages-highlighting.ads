-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                    Copyright (C) 2010, AdaCore                    --
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

--  This package implements integration between message container and
--  source editor to handle message highlighting. It is good candidate
--  to be moved to source editor in the future.

private with Ada.Containers.Hashed_Sets;
private with GPS.Kernel.Styles;

package GPS.Kernel.Messages.Highlighting is

   procedure Register (Kernel : not null access Kernel_Handle_Record'Class);
   --  Creates and registers highlighting manager

   procedure Unregister (Kernel : not null access Kernel_Handle_Record'Class);
   --  Unregister and destroys highlighting manager

private

   function Hash
     (Item : GPS.Kernel.Styles.Style_Access) return Ada.Containers.Hash_Type;
   --  Returns hash value constructed from style's name

   use type GPS.Kernel.Styles.Style_Access;

   package Style_Sets is
     new Ada.Containers.Hashed_Sets
       (GPS.Kernel.Styles.Style_Access, Hash, GPS.Kernel.Styles."=");

   type Style_Set_Access is access all Style_Sets.Set;

   type Key is record
      Category : Ada.Strings.Unbounded.Unbounded_String;
      File     : GNATCOLL.VFS.Virtual_File;
   end record;

   function Hash (Item : Key) return Ada.Containers.Hash_Type;
   --  Returns has value constructed from the category's and file's name

   package Style_Maps is
     new Ada.Containers.Hashed_Maps (Key, Style_Set_Access, Hash, "=", "=");

   --  I826-008 workaround: we manage the set of all styles used per file per
   --  category to unhighlight all occurrences of what styles and avoid
   --  potencial glitches.

   type Highlighting_Manager
     (Kernel : not null access Kernel_Handle_Record'Class) is
     new Abstract_Listener with record
      Map : Style_Maps.Map;
   end record;

   type Highlighting_Manager_Access is access all Highlighting_Manager'Class;

   procedure File_Opened
     (Self : not null access Highlighting_Manager;
      File : GNATCOLL.VFS.Virtual_File);

   overriding procedure File_Removed
     (Self     : not null access Highlighting_Manager;
      Category : Ada.Strings.Unbounded.Unbounded_String;
      File     : GNATCOLL.VFS.Virtual_File);

   overriding procedure Message_Property_Changed
     (Self     : not null access Highlighting_Manager;
      Message  : not null access Abstract_Message'Class;
      Property : String);

   overriding procedure Message_Removed
     (Self    : not null access Highlighting_Manager;
      Message : not null access Abstract_Message'Class);

end GPS.Kernel.Messages.Highlighting;
