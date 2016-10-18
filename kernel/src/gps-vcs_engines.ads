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

--  An abstract definition of what VCS engines are.

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Hashed_Maps;
with Ada.Strings.Hash;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with GNATCOLL.Projects;          use GNATCOLL.Projects;
with GNATCOLL.VFS;               use GNATCOLL.VFS;
with GPS.Kernel;                 use GPS.Kernel;
with GPS.VCS;                    use GPS.VCS;

package GPS.VCS_Engines is

   type VCS_Engine is abstract tagged private;
   type VCS_Engine_Access is access all VCS_Engine'Class;

   procedure Finalize (Kernel : not null access Kernel_Handle_Record'Class);
   --  Free all memory by this module

   -------------
   -- Factory --
   -------------

   type VCS_Engine_Factory is abstract tagged private;
   type VCS_Engine_Factory_Access is access all VCS_Engine_Factory'Class;

   function Name
     (Self : not null access VCS_Engine_Factory'Class) return String;
   --  Return the name of the VCS system

   function Create_Engine
     (Self : not null access VCS_Engine_Factory;
      Repo : String)
     return not null VCS_Engine_Access
     is abstract;
   --  Create a new VCS engine for the given repo.
   --  The meaning of Repo depends on the type of VCS, and is what is returned
   --  by Find_Repo.

   function Find_Repo
     (Self  : not null access VCS_Engine_Factory;
      File  : Virtual_File)
      return String
      is abstract;
   --  Given a file, try to find its repository, either on the disk or via
   --  environment variables.
   --  This function should return the empty string when no repository could
   --  be found.

   procedure Register_Factory
     (Kernel  : not null access Kernel_Handle_Record'Class;
      Name    : String;
      Factory : not null access VCS_Engine_Factory'Class);
   --  Register a new factory.
   --  Name is the value that should be used for the IDE'VCS_Kind project
   --  property.

   package Name_To_Factory is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,
      Element_Type    => VCS_Engine_Factory_Access,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => "=");

   function All_VCS_Factories
     (Kernel   : not null access Kernel_Handle_Record'Class)
      return access Name_To_Factory.Map;
   --  Return the list of all registered factories.
   --  Indexes are always lower-cased.
   --  The returned value must not be freed.

   function Get_VCS_Factory
     (Kernel   : not null access Kernel_Handle_Record'Class;
      Name     : String)
      return access VCS_Engine_Factory'Class;
   --  Return an engine for the given system (or null)

   -------------
   -- Engines --
   -------------

   procedure Set_VCS
     (Kernel   : not null access Kernel_Handle_Record'Class;
      Location : Virtual_File;
      Engine   : not null VCS_Engine_Access);
   function Get_VCS
     (Kernel   : not null access Kernel_Handle_Record'Class;
      Location : Virtual_File)
      return not null VCS_Engine_Access;
   function Get_VCS
     (Kernel   : not null access Kernel_Handle_Record'Class;
      Project  : Project_Type)
      return not null VCS_Engine_Access
     is (Get_VCS (Kernel, Project.Project_Path));
   --  Sets or return the VCS to use for a given project.
   --  This can be used two ways:
   --  - Location can be a project file, and this will be used by Get_VCS
   --    to retrieve the VCS for that project.
   --  - Or the location could be that of a repository (a .git, .svn, .CVS,...)
   --    to be shared amongst multiple projects. If you use this, the engine
   --    will not be freed until GPS exists.
   --
   --  The location could in fact be a URL if this is how a working directory
   --  finds its repository (for instance the value of an environment variable)
   --
   --  A given engine might be shared by multiple projects
   --  Engine will be freed automatically when no other project references it

   function No_VCS_Engine
     (Kernel   : not null access Kernel_Handle_Record'Class)
      return not null access VCS_Engine'Class;
   --  An engine to be used when no other VCS could be found

   -------------------
   -- File statuses --
   -------------------

   procedure Async_Fetch_Status_For_File
     (Self    : not null access VCS_Engine;
      File    : Virtual_File) is null;
   procedure Async_Fetch_Status_For_Project
     (Self    : not null access VCS_Engine;
      Project : Project_Type) is null;
   procedure Async_Fetch_Status_For_All_Files
     (Self    : not null access VCS_Engine) is null;
   --  Force the computation of the current statuses of files.
   --  This does not check the cache status.
   --  Fetching is asynchronous, since it can take a long time depending on
   --  the system used. Whenever information is retrieved for one file, the
   --  hook VCS_File_Status_Changed will be run for all files which status has
   --  changed.
   --  Should not be called directly, consider Ensure_Status_* instead

   function Ensure_Status_For_File
     (Self    : not null access VCS_Engine'Class;
      File    : Virtual_File) return Boolean;
   function Ensure_Status_For_Project
     (Self    : not null access VCS_Engine'Class;
      Project : Project_Type) return Boolean;
   function Ensure_Status_For_All_Source_Files
     (Self    : not null access VCS_Engine'Class) return Boolean;
   --  If any of the files in the set does not have a valid cache entry, then
   --  the corresponding Async_Fetch_Status_* operation will be called.
   --  Otherwise, these procedures assume the cache is up-to-date and do not
   --  recompute anything.
   --  These functions return True if some status is already available in the
   --  cache (it might not be up-to-date with regards to the disk), and False
   --  if a background command was spawned to compute the initial status.
   --
   --  Ensure_Status_For_All_Source_Files is for all source files of projects
   --  that use Self as their VCS engine. This function does not force the
   --  computation for files outside of the project, even if they are under
   --  version control, although in general it is expected that Self will
   --  compute their status anyway.

   function File_Properties_From_Cache
     (Self    : not null access VCS_Engine'Class;
      File    : Virtual_File)
     return VCS_File_Properties;
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

   procedure Invalidate_File_Status_Cache
     (Self    : not null access VCS_Engine'Class);
   --  Mark all entries in the cache as not being up-to-date.
   --  The next call to one of the Async_Fetch_Status_* procedures will
   --  therefore trigger queries to the actual VCS engine to refresh the cache.

   Default_Properties : constant VCS_File_Properties :=
     (Status_Unmodified, Null_Unbounded_String, Null_Unbounded_String);

   procedure Set_File_Status_In_Cache
     (Self         : not null access VCS_Engine'Class;
      File         : Virtual_File;
      Props        : VCS_File_Properties := Default_Properties);
   --  Update the file status in the cache, and emit the
   --  VCS_File_Status_Changed hook if needed. This should only be called
   --  when you write your own VCS engine. Other code should use one of the
   --  Async_Fetch_Status_* subprograms instead.

   ----------------------
   -- Labels and icons --
   ----------------------
   --  This section provides subprograms that let VCS engines configure how
   --  things are displayed to the user. As much as possible, the vocabulary
   --  of the VCS should be used, even though default versions are provided

   type Status_Display is record
      Label     : Unbounded_String;
      Icon_Name : Unbounded_String;
   end record;
   --  Display properties for a given status

   function Get_Display
     (Self   : not null access VCS_Engine'Class;
      Status : VCS_File_Status) return Status_Display;
   --  How to display the status

   procedure Override_Display
     (Self    : not null access VCS_Engine'Class;
      Status  : VCS_File_Status;
      Display : Status_Display);
   --  Override the label and icon to use for a status.
   --  Status should be one of the values possibly returned by Async_Fetch_*
   --  (the possible combinations depend on Self).

   ----------
   -- Misc --
   ----------

   function Name (Self : not null access VCS_Engine) return String is abstract;
   --  The name of the engine

   function Kernel
     (Self : not null access VCS_Engine'Class)
      return not null Kernel_Handle;
   --  Return the kernel.

   procedure Free (Self : in out VCS_Engine) is null;
   --  Free memory associated with Self, including all cached data.

   type Dummy_VCS_Engine is new VCS_Engine with private;
   --  An engine that does nothing, used when the project is not setup for
   --  VCS operations

private
   type VCS_Engine_Factory is abstract tagged record
      Name : Unbounded_String;
   end record;

   type VCS_File_Cache_Entry is record
      Need_Update  : Boolean;
      Props        : VCS_File_Properties;
   end record;

   package VCS_File_Cache is new Ada.Containers.Hashed_Maps
     (Key_Type        => Virtual_File,
      Element_Type    => VCS_File_Cache_Entry,
      Hash            => GNATCOLL.VFS.Full_Name_Hash,
      Equivalent_Keys => "=");
   use VCS_File_Cache;

   function Identity (Self : VCS_File_Status) return Ada.Containers.Hash_Type
     is (Ada.Containers.Hash_Type (Self));
   package VCS_Status_Displays is new Ada.Containers.Hashed_Maps
     (Key_Type        => VCS_File_Status,
      Element_Type    => Status_Display,
      Hash            => Identity,
      Equivalent_Keys => "=");
   use VCS_Status_Displays;

   type VCS_Engine is abstract tagged record
      Kernel   : Kernel_Handle;
      Cache    : VCS_File_Cache.Map;
      Displays : VCS_Status_Displays.Map;
   end record;

   type Dummy_VCS_Engine is new VCS_Engine with null record;

   overriding function Name
     (Self : not null access Dummy_VCS_Engine) return String is ("unknown");

   function Kernel
     (Self : not null access VCS_Engine'Class)
      return not null Kernel_Handle is (Self.Kernel);

end GPS.VCS_Engines;
