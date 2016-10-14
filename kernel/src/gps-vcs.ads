------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2016, AdaCore                          --
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

--  Support for GPS.VCS_Engines.
--  This contains types needed for the hooks, so that changes to VCS_Engines
--  do not force a whole recompilation of the project.

with Ada.Strings.Unbounded;  use Ada.Strings.Unbounded;

package GPS.VCS is

   -------------------
   -- File statuses --
   -------------------

   type VCS_File_Status is mod 2 ** 16;
   --  The status of the file. This is a bitmask, since multiple statuses can
   --  be possible at the same time. For instance:
   --    * Status_Modified or Status_Staged_Modified
   --      the file was staged for commit, then further modified. Or only
   --      part of the file was added to the index.
   --    * Status_Modified or Status_Staged_Deleted
   --      the file was staged for commit, and removed from work tree. The
   --      next commit should include whatever was staged when using git, but
   --      not necessarily when using other engines.

   Status_Unmodified      : constant VCS_File_Status := 2 ** 0;

   Status_Modified        : constant VCS_File_Status := 2 ** 1;
   --  Modified in non-staging area

   Status_Staged_Modified : constant VCS_File_Status := 2 ** 2;
   --  Modified, and added to the staging area

   Status_Staged_Added    : constant VCS_File_Status := 2 ** 3;
   --  New file, added to the staging area

   Status_Deleted         : constant VCS_File_Status := 2 ** 4;
   --  Deleted, but not yet marked as such in the staging area

   Status_Staged_Deleted  : constant VCS_File_Status := 2 ** 5;
   --  Deleted, and marked as such in the staging area

   Status_Staged_Renamed  : constant VCS_File_Status := 2 ** 6;
   --  Renamed in the staging area

   Status_Staged_Copied   : constant VCS_File_Status := 2 ** 7;
   --  Copied from another file, and marked in the staging area

   Status_Untracked       : constant VCS_File_Status := 2 ** 8;
   --  Not under version control

   Status_Ignored         : constant VCS_File_Status := 2 ** 9;
   --  Explicitly ignored by the VCS engine (.gitignore, .svnignore,...)

   Status_Conflict        : constant VCS_File_Status := 2 ** 10;
   --  Merge conflict. The exact conflict (modified by both, modified by one
   --  but deleted by the other,...) is not known.

   Status_Local_Locked    : constant VCS_File_Status := 2 ** 11;
   --  Some systems need to lock the file to make it writable.

   Status_Locked_By_Other : constant VCS_File_Status := 2 ** 12;
   --  the file is locked in the repository, and will not be updated. If both
   --  Local_Locked and Locked_By_Other are set, then the lock has been stolen
   --  or broken, and is invalid

   type VCS_File_Properties is record
      Status       : VCS_File_Status;
      Version      : Unbounded_String;
      Repo_Version : Unbounded_String;
   end record;
   --  Version and Repo_Version are only set for file-based VCS systems.

end GPS.VCS;
