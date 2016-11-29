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

with Ada.Containers.Hashed_Maps;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with GNATCOLL.Projects;          use GNATCOLL.Projects;
with GNATCOLL.VFS;               use GNATCOLL.VFS;
with GNAT.Strings;               use GNAT.Strings;
with GPS.Kernel;                 use GPS.Kernel;
with GPS.VCS;                    use GPS.VCS;

package VCS2.Engines is

   type VCS_Engine is abstract new Abstract_VCS_Engine with private;
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
     (Self        : not null access VCS_Engine_Factory;
      Working_Dir : Virtual_File)
     return not null VCS_Engine_Access
     is abstract;
   --  Create a new VCS engine for the given repo.
   --  The meaning of Repo depends on the type of VCS, and is what is returned
   --  by Find_Repo.

   function Find_Working_Directory
     (Self  : not null access VCS_Engine_Factory;
      File  : Virtual_File)
      return Virtual_File
      is abstract;
   --  Given a file, try to find its working directory, either on the disk or
   --  via environment variables.
   --  This function should return No_File when no repository could be found.

   procedure Register_Factory
     (Kernel  : not null access Kernel_Handle_Record'Class;
      Name    : String;
      Factory : not null access VCS_Engine_Factory'Class);
   --  Register a new factory.
   --  Name is the value that should be used for the IDE'VCS_Kind project
   --  property.

   -------------
   -- Engines --
   -------------

   type VCS_Repository is new Abstract_VCS_Repository with record
      Kernel    : not null access Kernel_Handle_Record'Class;
   end record;

   procedure Compute_VCS_Engines
      (Kernel : not null access Kernel_Handle_Record'Class);
   --  Create (or reuse) the VCS engines necessary for the project.
   --  All other engines are freed.

   overriding function Get_VCS
     (Self      : not null access VCS_Repository;
      Project   : Project_Type)
      return not null Abstract_VCS_Engine_Access;
   overriding function Guess_VCS_For_Directory
     (Self      : not null access VCS_Repository;
      Directory : Virtual_File) return not null Abstract_VCS_Engine_Access;

   procedure For_Each_VCS
     (Kernel    : not null access Kernel_Handle_Record'Class;
      Callback  : not null access procedure
        (VCS : not null access VCS_Engine'Class));
   --  Executes Callback for each VCS engine in use for the project

   function VCS_Count
     (Kernel   : not null access Kernel_Handle_Record'Class)
      return Natural;
   --  Return the number of VCS systems in use for the loaded project tree

   -------------------
   -- File statuses --
   -------------------

   procedure Async_Fetch_Status_For_Files
     (Self      : not null access VCS_Engine;
      Files     : File_Array) is null;
   procedure Async_Fetch_Status_For_Project
     (Self      : not null access VCS_Engine;
      Project   : Project_Type) is null;
   procedure Async_Fetch_Status_For_All_Files
     (Self      : not null access VCS_Engine) is null;
   --  Force the computation of the current statuses of files.
   --  This does not check the cache status.
   --  Fetching is asynchronous, since it can take a long time depending on
   --  the system used. Whenever information is retrieved for one file, the
   --  hook VCS_File_Status_Changed will be run for all files which status has
   --  changed.
   --  Should not be called directly, consider Ensure_Status_* instead

   type Task_Visitor is abstract tagged private;
   type Task_Visitor_Access is access all Task_Visitor'Class;
   procedure Free (Self : in out Task_Visitor) is null;
   procedure On_Terminate
     (Self  : not null access Task_Visitor;
      VCS   : access VCS_Engine'Class) is null;
   --  An object called at various point during algorithms.
   --  This base type only provides a standard callback for when a background
   --  task terminates. Further derivation provide algorithm-specific
   --  additional callbacks.

   procedure Ensure_Status_For_Files
     (Self        : not null access VCS_Engine;
      Files       : File_Array;
      Visitor     : access Task_Visitor'Class);
   procedure Ensure_Status_For_Project
     (Self        : not null access VCS_Engine;
      Project     : Project_Type;
      Visitor     : access Task_Visitor'Class);
   procedure Ensure_Status_For_All_Source_Files
     (Self        : not null access VCS_Engine;
      Visitor     : access Task_Visitor'Class := null);
   --  If any of the files in the set does not have a valid cache entry, then
   --  the corresponding Async_Fetch_Status_* operation will be called.
   --  Otherwise, these procedures assume the cache is up-to-date and do not
   --  recompute anything.
   --  This is fully asynchronous, nothing might have been done or started
   --  when these procedures return.
   --
   --  Ensure_Status_For_All_Source_Files is for all source files of projects
   --  that use Self as their VCS engine. This function does not force the
   --  computation for files outside of the project, even if they are under
   --  version control, although in general it is expected that Self will
   --  compute their status anyway.
   --
   --  On_Complete is automatically freed after having executed.

   procedure Ensure_Status_For_All_Files_In_All_Engines
     (Kernel   : not null access Kernel_Handle_Record'Class;
      Visitor  : access Task_Visitor'Class := null);
   --  For all VCS engines of the project, ensure that the status for all files
   --  is known.
   --  The callback is executed for each VCS that terminates its processing,
   --  and then once with no VCS when all of them have been processed

   overriding procedure Ensure_Status_For_Files
     (Self        : not null access VCS_Engine;
      Files       : File_Array);
   overriding procedure Ensure_Status_For_Project
     (Self        : not null access VCS_Engine;
      Project     : Project_Type);
   overriding function File_Properties_From_Cache
     (Self    : not null access VCS_Engine;
      File    : Virtual_File)
     return VCS_File_Properties;
   overriding procedure Set_File_Status_In_Cache
     (Self         : not null access VCS_Engine;
      File         : Virtual_File;
      Props        : VCS_File_Properties);
   overriding function Get_Tooltip_For_File
     (VCS     : not null access VCS_Engine;
      File    : GNATCOLL.VFS.Virtual_File)
     return String;

   function Default_File_Status
     (Self    : not null access VCS_Engine)
     return VCS_File_Status is (Status_Untracked);
   --  The default status to use for files not in the cache yet.  This can make
   --  a large difference on startup: if set to untracked, the GPS project
   --  (git) needs 1.3s to set the initial cache. If set to Unmodified, it
   --  takes 0.002s.

   procedure Invalidate_File_Status_Cache
     (Self    : not null access VCS_Engine'Class;
      File    : Virtual_File := No_File);
   --  Mark th entry for file (or all entries if No_File) in the cache as
   --  not being up-to-date.
   --  The next call to one of the Async_Fetch_Status_* procedures will
   --  therefore trigger queries to the actual VCS engine to refresh the cache.

   procedure Invalidate_All_Caches
     (Kernel  : not null access Kernel_Handle_Record'Class);
   --  Invalid all caches for all VCS, so that the next Ensure_* calls
   --  will reload from the disk

   ----------------------
   -- Labels and icons --
   ----------------------
   --  This section provides subprograms that let VCS engines configure how
   --  things are displayed to the user. As much as possible, the vocabulary
   --  of the VCS should be used, even though default versions are provided

   overriding function Get_Display
     (Self   : not null access VCS_Engine;
      Status : VCS_File_Status) return Status_Display;

   procedure Override_Display
     (Self    : not null access VCS_Engine'Class;
      Status  : VCS_File_Status;
      Display : Status_Display);
   --  Override the label and icon to use for a status.
   --  Status should be one of the values possibly returned by Async_Fetch_*
   --  (the possible combinations depend on Self).

   function Label_Version
     (Self : not null access VCS_Engine) return String
     is ("Revision");
   function Label_Repo_Version
     (Self : not null access VCS_Engine) return String
     is ("Repository Revision");
   --  Labels to use when displaying versions in the GUI

   -----------
   -- Files --
   -----------

   procedure For_Each_File_In_Cache
     (Self     : not null access VCS_Engine'Class;
      Callback : not null access procedure
        (File  : GNATCOLL.VFS.Virtual_File;
         Props : VCS_File_Properties));
   --  For all files in the cache, execute the callbacks.
   --  The contents of the cache might only be initialized after a call to
   --  Ensure_* has finished executing in the background. So in general you
   --  should always connect to the VCS_File_Status_Changed hook to monitor
   --  changes to this cache while Ensure_* is running.
   --  However, files that have the Self.Default_Status status will eventually
   --  be inserted in the cache, but not result in a call to the hook, so if
   --  they are not already in the cache when For_Each_File_In_Cache they might
   --  never be seen by the caller. To handle this, the recommend approach is:
   --
   --      Ensure_Status_For_All_Files_In_All_Engines
   --        (Kernel, new On_Complete);
   --      procedure Execute (Self : not null access On_Complete;
   --                         VCS  : not null access VCS_Engine'Class) is
   --      begin
   --          For_Each_File_In_Cache (VCS, ...);
   --      end;

   ----------------
   -- Operations --
   ----------------

   procedure Stage_Or_Unstage_Files
     (Self    : not null access VCS_Engine;
      Files   : GNATCOLL.VFS.File_Array;
      Stage   : Boolean) is abstract;
   --  Stage or unstage the files so that they are part of the next commit.
   --  This is supported natively by some VCS (git), but is emulated for others
   --  like subversion.
   --  This function then updates the status for the Files, and emits the
   --  appropriate hooks to report the change. This is done possibly
   --  asynchronously.

   procedure Commit_Staged_Files
     (Self    : not null access VCS_Engine;
      Message : String) is abstract;
   --  Commit all staged files with the corresponding message, then refresh
   --  the status of files (this is done asynchronously).
   --  Must emit the VCS_Commit_Done_Hook.

   type History_Visitor is new Task_Visitor with private;
   type History_Visitor_Access is access all History_Visitor'Class;
   procedure On_History_Line
     (Self    : not null access History_Visitor;
      ID      : String;
      Author  : String;
      Date    : String;
      Subject : String;
      Parents : in out GNAT.Strings.String_List_Access;
      Names   : in out GNAT.Strings.String_List_Access) is null;
   --  Called for every line in the history.
   --  Subject should be the first line of the commit message.
   --  Parents is a list of ID, the ancestor commits.
   --  Names is a list of labels associated with this commit (typically branch
   --  names or tags for instance).
   --  Parents and Names must be freed by the caller, although this procedure
   --  might reset them to null if it needs to store keep them.

   procedure Async_Fetch_History
     (Self        : not null access VCS_Engine;
      Visitor     : not null access History_Visitor'Class) is abstract;
   procedure Queue_Fetch_History
     (Self        : not null access VCS_Engine'Class;
      Visitor     : not null access History_Visitor'Class);
   --  Fetch history for the whole repository.
   --  Visitor is freed automatically when no longer needed.
   --  Only call Queue_Fetch_History from your code, Async_Fetch_History is
   --  the actual implementation but doesn't ensure that a single command runs
   --  at a given time.

   ----------
   -- Misc --
   ----------

   procedure Set_Working_Directory
     (Self        : not null access VCS_Engine'Class;
      Working_Dir : Virtual_File);
   function Working_Directory
     (Self : not null access VCS_Engine'Class) return Virtual_File;
   --  Return the root directory of the working directory.

   function Kernel
     (Self : not null access VCS_Engine'Class)
      return not null Kernel_Handle;
   --  Return the kernel.

   procedure Set_Run_In_Background
     (Self       : not null access VCS_Engine'Class;
      Background : Boolean);
   --  This should be called whenever some background processing is done for
   --  Self. This is used internally to ensure that a single vcs command is
   --  run at a given time, to avoid possible conflicts for VCS systems that
   --  do not allow this, but also because GPS cannot know what information
   --  will be retrieved.
   --  For instance, with git, if we call Ensure_Status_For_Files for
   --  file1.adb and then for file2.adb, the first call will in fact also
   --  get the status for file2.adb. So the second command is useless.
   --
   --  This function can be called multiple times with a True parameter, and
   --  will then need to be called an equal number of times with False.

   procedure Set_Active_VCS
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      VCS    : not null access VCS_Engine'Class);
   function Active_VCS
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
      return VCS_Engine_Access;
   --  Return the active VCS (or null)
   --  This is the VCS to which operations like "commit", "log",... apply

private
   type VCS_Engine_Factory is abstract tagged record
      Name : Unbounded_String;
   end record;

   type VCS_File_Cache_Entry is record
      Need_Update  : Boolean;
      Props        : VCS_File_Properties;
   end record;

   type Task_Visitor is abstract tagged record
      Refcount : Natural := 1;
   end record;

   type History_Visitor is new Task_Visitor with null record;

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

   type VCS_Command is abstract tagged record
      Visitor     : Task_Visitor_Access;
   end record;
   type VCS_Command_Access is access all VCS_Command'Class;
   --  A command that is queued for execution in a specific VCS.
   --  Does not use the Commands.Command from GPS since we do not compute
   --  progress or name (these commands are always python based).

   procedure Free (Self : in out VCS_Command) is null;
   --  Free memory used by Self

   procedure Execute
      (Self   : not null access VCS_Command;
       VCS    : not null access VCS_Engine'Class) is abstract;
   --  Execute the command

   type Queue_Item is record
      Command     : VCS_Command_Access;
   end record;
   package Command_Queues is new Ada.Containers.Vectors
      (Positive, Queue_Item);

   type VCS_Engine is abstract new Abstract_VCS_Engine with record
      Kernel      : Kernel_Handle;
      Cache       : VCS_File_Cache.Map;
      Displays    : VCS_Status_Displays.Map;
      Working_Dir : Virtual_File;

      Run_In_Background : Integer := 0;
      Queue             : Command_Queues.Vector;
      --  Queue of commands (see Set_Run_In_Background)
      --  When a background command is executing, the first item in the queue
      --  is that command.

      In_Use   : Boolean := True;
      --  True if any file depends on this engine. In practice, engines no in
      --  use are freed, so this is used as a temporary flag while computing
      --  which engines to keep.
   end record;

   function Kernel
     (Self : not null access VCS_Engine'Class)
      return not null Kernel_Handle is (Self.Kernel);
   function Working_Directory
     (Self : not null access VCS_Engine'Class)
      return Virtual_File is (Self.Working_Dir);

end VCS2.Engines;
