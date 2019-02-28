------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2016-2019, AdaCore                     --
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

--  The History view

with Commands;                use Commands;
with GNATCOLL.VFS;            use GNATCOLL.VFS;
with GPS.Kernel;              use GPS.Kernel;

package VCS2.History is

   procedure Register_Module
     (Kernel : not null access Kernel_Handle_Record'Class);
   --  Create actions for this module

   function Create_Show_History_Command
     (Kernel    : not null access Kernel_Handle_Record'Class;
      File      : Virtual_File;
      Commit_ID : String) return Commands.Command_Access;
   --  Create a new command that will open the History view, highlight the
   --  given commit, and display its details.

end VCS2.History;
