------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2002-2026, AdaCore                     --
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

--  The Virtual File System module

with GPS.Kernel;   use GPS.Kernel;
with GNATCOLL.VFS;

package VFS_Module is

   procedure Rename_File
     (Kernel                  : access GPS.Kernel.Kernel_Handle_Record'Class;
      File                    : GNATCOLL.VFS.Virtual_File;
      New_File                : GNATCOLL.VFS.Virtual_File;
      Success                 : out Boolean;
      Prj_Changed             : out Boolean;
      Display_Confirm_Dialogs : Boolean := True);
   --  Rename File to New_File, setting Success to True when succeeding.
   --  This procedure will display error and warning dialogs when needed
   --  (e.g: if the file to rename is a directory mentioned in the project).
   --  Prj_Changed is set to True when the project has been modified after the
   --  renaming.
   --  If Display_Confirm_Dialogs is True, confimation dialogs will be
   --  displayed when the renamed file (or directory) belongs to the project.

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
   --  Register the VFS module in the list

end VFS_Module;
