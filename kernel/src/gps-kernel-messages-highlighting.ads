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
--  to be moved to sourc editor in the future, but it uses direct access
--  to container's internals because equivalent operations is not
--  provided now.

package GPS.Kernel.Messages.Highlighting is

   procedure Register (Kernel : not null access Kernel_Handle_Record'Class);

private

   --  I826-008 workaround: we manage the set of all styles used per file per
   --  category to unhighlight all occurences of what styles and avoid
   --  potencial glitches.
   --  XXX It is not implemented now.

   type Highlighting_Manager
     (Kernel : not null access Kernel_Handle_Record'Class) is
     new Abstract_Listener with null record;

   type Highlighting_Manager_Access is access all Highlighting_Manager'Class;

   procedure File_Opened
     (Self : not null access Highlighting_Manager;
      File : GNATCOLL.VFS.Virtual_File);

   overriding procedure Message_Property_Changed
     (Self     : not null access Highlighting_Manager;
      Message  : not null access Abstract_Message'Class;
      Property : String);

   overriding procedure Message_Removed
     (Self    : not null access Highlighting_Manager;
      Message : not null access Abstract_Message'Class);

end GPS.Kernel.Messages.Highlighting;
