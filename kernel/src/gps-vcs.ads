------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2016-2018, AdaCore                     --
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
with GNATCOLL.Projects;      use GNATCOLL.Projects;
with GNATCOLL.Scripts;       use GNATCOLL.Scripts;
with GNATCOLL.VFS;           use GNATCOLL.VFS;
with GPS.Scripts;            use GPS.Scripts;

package GPS.VCS is

   type Abstract_VCS_Engine is abstract tagged private;
   type Abstract_VCS_Engine_Access is access all Abstract_VCS_Engine'Class;
   --  For now kept is so that changes to the latter do not force a full
   --  recompilation because of the hooks

   type Abstract_VCS_Repository is interface;
   type Abstract_VCS_Repository_Access is
      access all Abstract_VCS_Repository'Class;

   function Get_VCS
     (Self     : not null access Abstract_VCS_Repository;
      Project  : Project_Type)
      return not null Abstract_VCS_Engine_Access
     is abstract;
   --  Return the VCS to use for a given project.
   --  A given engine might be shared by multiple projects
   --  Engine will be freed automatically when no other project references it

   function Guess_VCS_For_Directory
     (Self      : not null access Abstract_VCS_Repository;
      Directory : Virtual_File) return not null Abstract_VCS_Engine_Access
     is abstract;
   --  For now, we assume there is a single VCS for a given directory (one
   --  possibly use case for multiple VCS is to have a local vcs and a
   --  remote one, but this is handled by local_history.py instead).
   --
   --  We cannot assume that any of the files in the directory is also a
   --  project source, so we can't use Get_VCS above.
   --  Instead, we check whether Directory or any of its parents has a result
   --  for Get_VCS. This kinda assume that a directory either contains
   --  project sources, or is beneath the directory that contains the VCS
   --  repo (root/.git for instance).

   procedure Invalidate_All_Caches
     (Self    : not null access Abstract_VCS_Repository) is abstract;
   --  Invalid all caches for all VCS, so that the next Ensure_* calls
   --  will reload from the disk

   -------------
   -- Scripts --
   -------------

   VCS_Class_Name        : constant String := "VCS2";

   function Create_VCS_Instance
     (Script : access Scripting_Language_Record'Class;
      VCS    : not null access Abstract_VCS_Engine'Class)
      return Class_Instance;
   function Get_VCS
     (Inst   : Class_Instance)
      return not null access Abstract_VCS_Engine'Class;
   function Has_VCS (Inst   : Class_Instance) return Boolean;
   procedure Set_VCS_Instance
     (VCS    : not null access Abstract_VCS_Engine'Class;
      Inst   : Class_Instance);
   --  Convert between Ada and python types

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
   --  The file is locked in the repository, and will not be updated. If both
   --  Local_Locked and Locked_By_Other are set, then the lock has been stolen
   --  or broken, and is invalid

   Status_Needs_Update    : constant VCS_File_Status := 2 ** 13;
   --  A more recent version of the file exists in the repository.
   --  This only applies to file-based repositories

   Mask_Staged            : constant VCS_File_Status :=
     Status_Staged_Modified
     or Status_Staged_Added
     or Status_Staged_Deleted
     or Status_Staged_Renamed
     or Status_Staged_Copied;
   --  All statuses that indicate the file is staged for the next commit

   Mask_Modified_Unstaged : constant VCS_File_Status :=
     Status_Modified
     or Status_Deleted
     or Status_Conflict
     or Status_Needs_Update;
   --  All statuses that indicate the file has some unstaged changes

   Mask_Untracked         : constant VCS_File_Status :=
     Status_Untracked;
   --  All status that indicate the file is untracked (and thus needs to be
   --  explicitly added or ignored).

   type VCS_File_Properties is record
      Status       : VCS_File_Status;
      Version      : Unbounded_String;
      Repo_Version : Unbounded_String;
   end record;
   --  Version and Repo_Version are only set for file-based VCS systems.

   -----------------
   -- VCS Engines --
   -----------------

   procedure Free (Self : in out Abstract_VCS_Engine);
   --  Free the memory used by Self

   function Get_Tooltip_For_File
     (VCS        : not null access Abstract_VCS_Engine;
      Dummy_File : GNATCOLL.VFS.Virtual_File)
      return String is ("");
   --  Return a description of the file's properties, suitable for display
   --  in tooltips.

   function Get_VCS_File_Status
     (VCS  : not null access Abstract_VCS_Engine;
      File : GNATCOLL.VFS.Virtual_File)
      return VCS_File_Status is (Status_Untracked);
   --  A convenient getter

   function File_Properties_From_Cache
     (Self       : not null access Abstract_VCS_Engine;
      Dummy_File : Virtual_File)
      return VCS_File_Properties
   is ((Status_Untracked, Null_Unbounded_String, Null_Unbounded_String));
   --  Return the current known status of the file.
   --  By default, files are assumed to be "unmodified". Calling one of the
   --  Async_Fetch_Status_* procedures above will ensure that the proper status
   --  is eventually set in the cache, and returned by this function.
   --  The typical workflow to show file status is therefore:
   --
   --        ... Connect to VCS_File_Status_Update_Hook
   --        St := Eng.File_Status_From_Cache (File);
   --        ... display the status as currently known
   --        Eng.Async_Fetch_Status_For_File (File);
   --        --  monitor the hook to update the displayed status

   procedure Set_Files_Status_In_Cache
     (Self         : not null access Abstract_VCS_Engine;
      Files        : GNATCOLL.VFS.File_Array;
      Props        : VCS_File_Properties) is null;
   --  Update the files status in the cache, and emit the
   --  VCS_File_Status_Changed hook if needed. This should only be called
   --  when you write your own VCS engine. Other code should use one of the
   --  Async_Fetch_Status_* subprograms instead.

   type Status_Display is record
      Label     : Unbounded_String;
      Icon_Name : Unbounded_String;
   end record;
   --  Display properties for a given status

   function Name
     (Self : not null access Abstract_VCS_Engine) return String is ("");
   --  The name of the engine

   function Get_Display
     (Self         : not null access Abstract_VCS_Engine;
      Dummy_Status : VCS_File_Status) return Status_Display
     is ((Null_Unbounded_String, Null_Unbounded_String));
   --  How to display the status

   procedure Ensure_Status_For_Project
     (Self        : not null access Abstract_VCS_Engine;
      Project     : Project_Type) is null;
   procedure Ensure_Status_For_Files
     (Self        : not null access Abstract_VCS_Engine;
      Files       : File_Array) is null;
   --  Ensure that all files in the project have a known VCS status in the
   --  cache. This is done asynchronously, and results in possibly calls
   --  to the VCS_File_Status_Update_Hook.

   procedure Make_File_Writable
     (Self       : not null access Abstract_VCS_Engine;
      File       : GNATCOLL.VFS.Virtual_File;
      Writable   : Boolean);
   --  Make a file writable on the disk, possibly only using
   --  GNATCOLL.VFS.Set_Writable, but possibly stealing a lock or any other
   --  operation.

private

   type Engine_Proxy is new Script_Proxy with null record;
   overriding function Class_Name
     (Self : Engine_Proxy) return String is (VCS_Class_Name);

   package Engine_Proxies is new Script_Proxies
     (Element_Type => Abstract_VCS_Engine_Access,
      Proxy        => Engine_Proxy);

   type Abstract_VCS_Engine is abstract tagged record
      Instances   : Engine_Proxy;
   end record;
end GPS.VCS;
