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

--  An abstract definition of what VCS engines are.

with Ada.Containers.Hashed_Maps;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Glib.Main;                  use Glib.Main;
with GNATCOLL.Projects;          use GNATCOLL.Projects;
with GNATCOLL.VFS;               use GNATCOLL.VFS;
with GNAT.Strings;               use GNAT.Strings;
with GPS.Kernel;                 use GPS.Kernel;
with GPS.VCS;                    use GPS.VCS;
with GPS_Unbounded_String_Vectors;
with Gtk.Widget;                 use Gtk.Widget;

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

   procedure For_Each_Registered_Factory
     (Kernel   : not null access Kernel_Handle_Record'Class;
      Callback : not null access procedure (Name : String));
   --  Executes Callback for each registered VCS system

   -------------
   -- Engines --
   -------------

   overriding procedure Free (Self : in out VCS_Engine);

   type VCS_Repository is new Abstract_VCS_Repository with record
      Kernel    : not null access Kernel_Handle_Record'Class;
   end record;

   procedure Reset_VCS_Engines
      (Kernel : not null access Kernel_Handle_Record'Class);
   --  Reset all stored VCS engines.
   --  This should be called every time the project changes

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
   overriding procedure Invalidate_All_Caches
     (Self    : not null access VCS_Repository);

   procedure For_Each_VCS
     (Kernel    : not null access Kernel_Handle_Record'Class;
      Callback  : not null access procedure
        (VCS : not null access VCS_Engine'Class));
   --  Executes Callback for each VCS engine in use for the project

   function VCS_Count
     (Kernel   : not null access Kernel_Handle_Record'Class)
      return Natural;
   --  Return the number of VCS systems in use for the loaded project tree

   -------------
   -- Visitor --
   -------------
   --  Most operations in this package are asynchronous, i.e. they will report
   --  their result at some later point in the future.
   --  In their python implementation, we use the GPS workflows framework,
   --  combined with python's yield statement and coroutines. We do not have
   --  the same facility in Ada, so we use a visitor. This is an object with
   --  various primitive operations that are called when operations complete.

   type Task_Visitor is abstract tagged limited private;
   type Task_Visitor_Access is access all Task_Visitor'Class;
   --  An object called at various point during algorithms.
   --  This base type only provides a standard callback for when a background
   --  task terminates. Further derivation provide algorithm-specific
   --  additional callbacks.

   procedure Free (Self : in out Task_Visitor) is null;
   --  Called when the visitor is no longer needed.

   procedure On_Start
     (Self  : not null access Task_Visitor) is null;
   --  Called when the command to which the visitor belongs starts running,
   --  since commands are queue (we only want to execute one at a time per
   --  engine).

   procedure On_Terminate
     (Self  : not null access Task_Visitor;
      VCS   : access VCS_Engine'Class) is null;
   --  This is called whether the action succeeded or not.

   procedure On_Success
     (Self   : not null access Task_Visitor;
      Kernel : not null access Kernel_Handle_Record'Class) is null;
   --  Called when the plugin reports that the action has succeeded.

   type Commit_Flags is mod Integer'Last;
   Commit_Unpushed    : constant Commit_Flags := 2 ** 1;
   Commit_Uncommitted : constant Commit_Flags := 2 ** 2;

   type Name_Kind is (Name_Head, Name_Local, Name_Remote, Name_Tag);
   type Name_Description is record
      Name    : Ada.Strings.Unbounded.Unbounded_String;
      Kind    : Name_Kind;
   end record;
   type Commit_Names is array (Natural range <>) of Name_Description;
   type Commit_Names_Access is access all Commit_Names;
   --  Description for the names associated with a commit (a tag
   --  name, a branch name,...)

   procedure Free (Self : in out Commit_Names_Access);

   procedure On_History_Line
     (Self    : not null access Task_Visitor;
      ID      : String;
      Author  : String;
      Date    : String;
      Subject : String;
      Parents : in out GPS_Unbounded_String_Vectors.Vector;
      Names   : in out Commit_Names_Access;
      Flags   : Commit_Flags := 0) is null;
   --  Called for every line in the history ('git log', 'cvs log',...)
   --  Subject should be the first line of the commit message.
   --  Parents is a list of ID, the ancestor commits.
   --  Names is a list of labels associated with this commit (typically branch
   --  names or tags for instance).
   --  Parents and Names must be freed by the caller, although this procedure
   --  might reset them to null if it needs to keep them.

   procedure On_Commit_Details
     (Self    : not null access Task_Visitor;
      ID      : String;
      Header  : String;
      Message : String) is null;
   --   Called when details for a specific commit are available.

   procedure On_Annotation
     (Self       : not null access Task_Visitor;
      File       : Virtual_File;
      First_Line : Positive;
      Ids        : String_List;
      Text       : String_List) is null;
   --  Called when annotations have been computed for a file.
   --  Annotations are made available for lines
   --    First_Line .. First_Line + Text'Length - 1
   --
   --  Ids are the unique commit ids for each line, text is the information to
   --  display on the side.
   --  An entry can be null if it is the same as for the previous line

   procedure On_Diff_Computed
     (Self    : not null access Task_Visitor;
      Diff    : String) is null;
   --  Called when a diff or patch has been computed, and returns the contents
   --  of that patch.

   procedure On_File_Computed
     (Self     : not null access Task_Visitor;
      Contents : String) is null;
   --  Called when the contents of a file (as of a specific version) has been
   --  computed

   type Branch_Info is record
      Name       : GNAT.Strings.String_Access;
      Is_Current : Boolean;
      Emblem     : GNAT.Strings.String_Access;
      Id         : GNAT.Strings.String_Access;
   end record;
   type Branches_Array is array (Natural range <>) of Branch_Info;

   procedure On_Branches
     (Self       : not null access Task_Visitor;
      Category   : String;
      Iconname   : String;
      Can_Rename : Boolean;
      Branches   : Branches_Array) is null;
   --  Called when information about available branches has been computed.
   --  All branches are displayed in the same category ("BRANCHES", "REMOTES",
   --  "TAGS",...)

   procedure On_Tooltip
     (Self     : not null access Task_Visitor;
      Text     : String) is null;
   --  Called when extra information is available for a tooltip.

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
     (Self      : not null access VCS_Engine;
      From_User : Boolean) is null;
   --  Force the computation of the current statuses of files.
   --  This does not check the cache status.
   --  Fetching is asynchronous, since it can take a long time depending on
   --  the system used. Whenever information is retrieved for one file, the
   --  hook VCS_File_Status_Changed will be run for all files which status has
   --  changed.
   --  Should not be called directly, consider Ensure_Status_* instead.
   --  From_User is set to True when this procedure is called as a result of
   --  the user pressing 'reload' in the Commits view.

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
      Visitor     : access Task_Visitor'Class := null;
      From_User   : Boolean);
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
   --
   --  See Async_Fetch_Status_For_All_Files for the documentation for
   --  From_User.

   procedure Ensure_Status_For_All_Files_In_All_Engines
     (Kernel    : not null access Kernel_Handle_Record'Class;
      Visitor   : access Task_Visitor'Class := null;
      From_User : Boolean);
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
   overriding procedure Set_Files_Status_In_Cache
     (Self    : not null access VCS_Engine;
      Files   : GNATCOLL.VFS.File_Array;
      Props   : VCS_File_Properties);
   overriding function Get_Tooltip_For_File
     (VCS     : not null access VCS_Engine;
      File    : GNATCOLL.VFS.Virtual_File)
      return String;
   overriding function Get_VCS_File_Status
     (VCS  : not null access VCS_Engine;
      File : GNATCOLL.VFS.Virtual_File)
      return VCS_File_Status;

   function Default_File_Status
     (Self    : not null access VCS_Engine)
     return VCS_File_Status is (Status_No_VCS);
   --  The default status to use for files not in the cache yet.  This can make
   --  a large difference on startup: if set to untracked, the GPS project
   --  (git) needs 1.3s to set the initial cache. If set to Unmodified, it
   --  takes 0.002s.

   procedure Invalidate_File_Status_Cache
     (Self    : not null access VCS_Engine'Class;
      File    : Virtual_File := No_File);
   --  Mark the entry for file (or all entries if No_File) in the cache as
   --  not being up-to-date.
   --  The next call to one of the Async_Fetch_Status_* procedures will
   --  therefore trigger queries to the actual VCS engine to refresh the cache.

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
         Props : VCS_File_Properties);
      Only_If_Up_To_Date : Boolean := False);
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
   --
   --  If Only_If_Up_To_Date is true, only files for which the current status
   --  is known are actually returned.(see Invalidate_File_Status_Cache).

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

   type History_Filter is record
      Up_To_Lines         : Natural := Natural'Last;
      For_File            : GNATCOLL.VFS.Virtual_File := GNATCOLL.VFS.No_File;
      Filter              : Unbounded_String := Null_Unbounded_String;
      Select_Id           : Unbounded_String := Null_Unbounded_String;
      Current_Branch_Only : Boolean := False;
      Branch_Commits_Only : Boolean := False;
   end record;
   No_Filter : constant History_Filter := (others => <>);
   --  * Up_To_Lines is the number of lines that will be displayed. There is
   --    no need to return more than that.
   --  * For_File is set if the history for a specific file is requested. The
   --    default is to request the history for all files.
   --  * Filter is an extra filter typed by the user. It is interpreted by the
   --    specific VCS: matches commit message, code, ... This is a regular
   --    expression.
   --  * Current_Branch_Only should be true if only the history of the current
   --    branch is needed. Some systems cannot query multiple branches.
   --  * Branch_Commits_Only should be true if only commits related to
   --    branching points, merge points or end of branches should be returned.

   procedure Async_Fetch_History
     (Self        : not null access VCS_Engine;
      Visitor     : not null access Task_Visitor'Class;
      Filter      : History_Filter := No_Filter) is null;
   procedure Queue_Fetch_History
     (Self        : not null access VCS_Engine'Class;
      Visitor     : not null access Task_Visitor'Class;
      Filter      : History_Filter := No_Filter);
   --  Fetch history for the whole repository.
   --  Visitor is freed automatically when no longer needed.
   --  Visitor.On_History_Line is called whenever part of the history becomes
   --  available.
   --  Only call Queue_Fetch_History from your code, Async_Fetch_History is
   --  the actual implementation but doesn't ensure that a single command runs
   --  at a given time.

   procedure Async_Commit_Staged_Files
     (Self    : not null access VCS_Engine;
      Visitor : not null access Task_Visitor'Class;
      Message : String) is null;
   procedure Queue_Commit_Staged_Files
     (Self    : not null access VCS_Engine'Class;
      Visitor : not null access Task_Visitor'Class;
      Message : String);
   --  Commit all staged files with the corresponding message.

   procedure Async_Fetch_Commit_Details
     (Self        : not null access VCS_Engine;
      Ids         : not null GNAT.Strings.String_List_Access;
      Visitor     : not null access Task_Visitor'Class) is null;
   procedure Queue_Fetch_Commit_Details
     (Self        : not null access VCS_Engine'Class;
      Ids         : not null GNAT.Strings.String_List_Access;
      Visitor     : not null access Task_Visitor'Class);
   --  Asynchronously fetch detail information on the commits specified in Ids.
   --  Ids is freed automatically when no longer needed.
   --  Details are reported via Visitor.On_Commit_Details.
   --  Visitor is freed automatically when no longer needed

   procedure Async_Diff
     (Self        : not null access VCS_Engine;
      Visitor     : not null access Task_Visitor'Class;
      Ref         : String;
      File        : Virtual_File := No_File) is null;
   procedure Queue_Diff
     (Self        : not null access VCS_Engine'Class;
      Visitor     : not null access Task_Visitor'Class;
      Ref         : String;
      File        : Virtual_File := No_File);
   --  Asynchronously fetch a diff to compare either the specific file or the
   --  whole repository to a specific version.
   --  Ref will in general be a commit ID (as returned by Async_Fetch_History,
   --  or some special branch names depending on the VCS.
   --  One special value "HEAD" means the last commit on the current branch.
   --
   --  Reports the result via Visitor.On_Diff_Computed

   procedure Async_View_File
     (Self        : not null access VCS_Engine;
      Visitor     : not null access Task_Visitor'Class;
      Ref         : String;
      File        : Virtual_File) is null;
   procedure Queue_View_File
     (Self        : not null access VCS_Engine'Class;
      Visitor     : not null access Task_Visitor'Class;
      Ref         : String;
      File        : Virtual_File);
   --  Asynchronously fetch the contents of a file as of a specific version.
   --  Ref will in general be a commit ID (as returned by Async_Fetch_History,
   --  or some special branch names depending on the VCS.
   --  One special value "HEAD" means the last commit on the current branch.
   --
   --  Reports the result via Visitor.On_File_Computed

   procedure Async_Annotations
     (Self        : not null access VCS_Engine;
      Visitor     : not null access Task_Visitor'Class;
      File        : Virtual_File) is null;
   procedure Queue_Annotations
     (Self        : not null access VCS_Engine'Class;
      Visitor     : not null access Task_Visitor'Class;
      File        : Virtual_File);
   --  Compute line annotations for a specific file.
   --  These annotations show the last modification date, author, commit,...
   --  for each line in a file.
   --
   --  Reports the result via Visitor.On_Annotation

   procedure Async_Branches
     (Self        : not null access VCS_Engine;
      Visitor     : not null access Task_Visitor'Class) is null;
   procedure Queue_Branches
     (Self        : not null access VCS_Engine'Class;
      Visitor     : not null access Task_Visitor'Class);
   --  Compute available branches, tags and others for Self.
   --  Reports the result via one or more calls to Visitor.On_Branches

   type Branch_Action is (Action_Double_Click,
                          Action_Tooltip,
                          Action_Add,
                          Action_Remove,
                          Action_Rename);
   procedure Async_Action_On_Branch
     (Self         : not null access VCS_Engine;
      Visitor      : not null access Task_Visitor'Class;
      Action       : Branch_Action;
      Category, Id : String;
      Text         : String := "") is null;
   procedure Queue_Action_On_Branch
     (Self         : not null access VCS_Engine'Class;
      Visitor      : not null access Task_Visitor'Class;
      Action       : Branch_Action;
      Category, Id : String;
      Text         : String := "");
   --  Some action related to the Branches view.
   --  Depending on the action, the result should be reported differently to
   --  the caller:
   --     Action_Double_Click => simply perform the action
   --     Action_Tooltip      => use Visitor.On_Tooltip
   --     Action_Add          => simply perform the action
   --     Action_Remove       => simply perform the action
   --
   --  Category is the upper-cased name of the top-level node.
   --  The Id is as returned by Async_Branches.
   --  Text is only used when renaming, and can be ignored otherwise.

   procedure Async_Discard_Local_Changes
     (Self        : not null access VCS_Engine;
      Files       : GNATCOLL.VFS.File_Array) is null;
   procedure Queue_Discard_Local_Changes
     (Self        : not null access VCS_Engine'Class;
      Visitor     : access Task_Visitor'Class;
      Files       : GNATCOLL.VFS.File_Array_Access);
   --  Discard all changes done in any file in Files.
   --  Files is freed automatically.
   --  Calls Visitor.On_Terminate when done.

   procedure Async_Checkout
     (Self    : not null access VCS_Engine;
      Visitor : not null access Task_Visitor'Class;
      Commit  : String) is null;

   procedure Queue_Checkout
     (Self    : not null access VCS_Engine'Class;
      Visitor : not null access Task_Visitor'Class;
      Commit  : String);
   --  Checkout to the commit

   procedure Async_Checkout_File
     (Self    : not null access VCS_Engine;
      Visitor : not null access Task_Visitor'Class;
      Commit  : String;
      File    : Virtual_File) is null;

   procedure Queue_Checkout_File
     (Self    : not null access VCS_Engine'Class;
      Visitor : not null access Task_Visitor'Class;
      Commit  : String;
      File    : Virtual_File);
   --  Checkout specific file to the commit

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

   overriding function Get_Active_VCS
     (Self : not null access VCS_Repository)
      return Abstract_VCS_Engine_Access;
   overriding function Get_VCS_Selector
     (Self : not null access VCS_Repository)
      return Gtk_Widget;
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

   type Task_Visitor is abstract tagged limited record
      Refcount : Natural := 1;
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

   package Command_Queues is new Ada.Containers.Vectors
     (Positive, VCS_Command_Access);

   type VCS_Engine is abstract new Abstract_VCS_Engine with record
      Kernel      : Kernel_Handle;
      Cache       : VCS_File_Cache.Map;
      Displays    : VCS_Status_Displays.Map;
      Working_Dir : Virtual_File;

      Run_In_Background : Integer := 0;
      Queue_Current     : VCS_Command_Access;
      Queue             : Command_Queues.Vector;
      Queue_Id          : G_Source_Id := No_Source_Id;
      --  Queue of commands (see Set_Run_In_Background).
      --  Commands are started after a short idle, so that the current command
      --  can be properly unregistered from GPS task manager.
      --  We do not use GPS's standard command queues, because we only want to
      --  run one command at a time per VCS engine)

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
