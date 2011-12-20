------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2010-2012, AdaCore                     --
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

--  This package implements integration between message container and
--  locations view to be sure every time when new message is added the
--  locations view is opened and highlighted.

private with Glib.Main;

package GPS.Kernel.Messages.View is

   procedure Register (Kernel : not null access Kernel_Handle_Record'Class);
   --  Creates and registers locations view manager

   procedure Unregister (Kernel : not null access Kernel_Handle_Record'Class);
   --  Unregister and deallocates locations view manager

   procedure Do_Not_Goto_First_Location
     (Kernel : not null access Kernel_Handle_Record'Class);
   --  Temporary disable "goto first location" feature. It will be reenabled
   --  automatically once event processing loop reach idle state.

   procedure Expand_File
     (Kernel   : not null access Kernel_Handle_Record'Class;
      Category : String;
      File     : GNATCOLL.VFS.Virtual_File);
   --  Send request to expand specified category and file. First file's message
   --  will be selected.

   procedure Expand_Category
     (Kernel   : not null access Kernel_Handle_Record'Class;
      Category : String);
   --  Send request to expand specified category. First category's and first
   --  file's message will be selected.

private

   type View_Manager
     (Kernel : not null access Kernel_Handle_Record'Class) is
     new Abstract_Listener with
      record
         Goto_First_Location : Boolean := True;
         Idle_Handler        : Glib.Main.G_Source_Id := Glib.Main.No_Source_Id;
      end record;

   type View_Manager_Access is access all View_Manager'Class;

   overriding procedure Message_Added
     (Self    : not null access View_Manager;
      Message : not null access Abstract_Message'Class);

   overriding procedure Category_Added
     (Self     : not null access View_Manager;
      Category : Ada.Strings.Unbounded.Unbounded_String);

end GPS.Kernel.Messages.View;
