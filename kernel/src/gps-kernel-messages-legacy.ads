------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2010-2019, AdaCore                     --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------
--  This package contains stuff to support legacy code. It must be
--  reviewed and removed at some point after switching of all modules
--  to use messages container.

package GPS.Kernel.Messages.Legacy is

   procedure Set_Action_Item
     (Kernel   : not null access Kernel_Handle_Record'Class;
      Category : String;
      File     : GNATCOLL.VFS.Virtual_File;
      Line     : Natural;
      Column   : Natural;
      Message  : String;
      Action   : Action_Item);
   --  Associates an action item to the primary message at the specified
   --  location. If Action is null, the action item will be removed from
   --  that location.

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
