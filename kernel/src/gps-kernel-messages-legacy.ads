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
--  This package contains stuff to support legacy code. It must be
--  reviewed and removed at some point after switching of all modules
--  to use messages container.

with GPS.Kernel.Standard_Hooks;

package GPS.Kernel.Messages.Legacy is

   procedure Add_Action_Item
     (Kernel   : not null access Kernel_Handle_Record'Class;
      Category : String;
      File     : GNATCOLL.VFS.Virtual_File;
      Line     : Natural;
      Column   : Natural;
      Message  : String;
      Action   : GPS.Kernel.Standard_Hooks.Action_Item);
   --  Add an action item to be associated to a specified location.
   --  If Action is null, the action item will be removed from that location.

   function Category_Count
     (Kernel   : not null access Kernel_Handle_Record'Class;
      Category : String) return Natural;
   --  Returns the number of entries for a given category

   function Get_Message_At
     (Self     : not null access constant Messages_Container'Class;
      Category : String;
      File     : GNATCOLL.VFS.Virtual_File;
      Line     : Natural;
      Column   : Basic_Types.Visible_Column_Type)
      return Message_Access;
   --  Returns last inserted mesage at the specified location if any;
   --  otherwise returns null.

end GPS.Kernel.Messages.Legacy;
