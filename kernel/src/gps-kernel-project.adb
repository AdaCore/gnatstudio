------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2001-2025, AdaCore                     --
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

with Ada.Characters.Handling;          use Ada.Characters.Handling;
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;
with Ada.Strings.Fixed;
with Ada.Unchecked_Deallocation;
with GNAT.OS_Lib;                      use GNAT.OS_Lib;
pragma Warnings (Off, ".*is an internal GNAT unit");
with GNAT.Expect.TTY.Remote;           use GNAT.Expect.TTY.Remote;
pragma Warnings (On, ".*is an internal GNAT unit");

with VSS.Strings.Conversions;

with GNATCOLL.JSON;
with GNATCOLL.Projects;                use GNATCOLL.Projects;
with GNATCOLL.Traces;                  use GNATCOLL.Traces;
with GNATCOLL.Utils;                   use GNATCOLL.Utils;
with GNATCOLL.VFS;                     use GNATCOLL.VFS;

with Projects;                         use Projects;
with Remote;                           use Remote;

with Gtk.Enums;                        use Gtk.Enums;
with Gtkada.File_Selector;             use Gtkada.File_Selector;

with GPS.Intl;                         use GPS.Intl;
with GPS.Kernel.Custom;
with GPS.Kernel.Hooks;                 use GPS.Kernel.Hooks;
with GPS.Kernel.Messages;              use GPS.Kernel.Messages;
with GPS.Kernel.Messages.Tools_Output; use GPS.Kernel.Messages.Tools_Output;
with GPS.Kernel.Modules.UI;            use GPS.Kernel.Modules.UI;
with GPS.Kernel.Preferences;           use GPS.Kernel.Preferences;
with GPS.Kernel.Properties;            use GPS.Kernel.Properties;
with GPS.Kernel.Remote;                use GPS.Kernel.Remote;
with GPS.Kernel.MDI;                   use GPS.Kernel.MDI;
with GPS.Properties;                   use GPS.Properties;
with Xref;

package body GPS.Kernel.Project is

   Me : constant Trace_Handle := Create ("GPS.KERNEL.PROJECT");

   Report_Missing_Dirs : constant Trace_Handle :=
     Create ("GPS.INTERNAL.PROJECTS_MISSING_DIRS_WARNING",
             Default => Off);

   Location_Category : constant VSS.Strings.Virtual_String := "Project";
   --  Category uses in the Location window for errors related to loading the
   --  project file
   Location_Message_Flags : constant Message_Flags :=
     (Editor_Side => True,
      Editor_Line => False,
      Locations   => True);

   type GPS_Project_Tree is new Project_Tree with record
      Handle    : Kernel_Handle;
      Propagate : Boolean := True;
      Has_Undefined_Eternals : Boolean := False;
   end record;
   type GPS_Project_Tree_Access is access all GPS_Project_Tree'Class;
   overriding function Data_Factory
     (Self : GPS_Project_Tree) return Project_Data_Access;
   overriding procedure Recompute_View
     (Self   : in out GPS_Project_Tree;
      Errors : Error_Report := null);
   --  See inherited documentation

   type GPS_Project_Environment is new Project_Environment with record
      Kernel : access Kernel_Handle_Record'Class;
   end record;
   overriding procedure Spawn_Gnatls
     (Self         : GPS_Project_Environment;
      Fd           : out GNAT.Expect.Process_Descriptor_Access;
      Gnatls_Args  : GNAT.OS_Lib.Argument_List_Access;
      Errors       : Error_Report);
   overriding function Gnatls_Host
     (Self : GPS_Project_Environment) return String;
   overriding procedure Set_GNAT_Version
     (Self         : in out GPS_Project_Environment;
      Version      : String);

   package String_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String,   --  "section#key"
      Element_Type    => String,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => "=",
      "="             => "=");
   type Scenario_Vars_Property is new Property_Record with record
      Map : String_Maps.Map;
   end record;
   type Scenario_Vars_Property_Access is
     access all Scenario_Vars_Property'Class;
   overriding procedure Save
     (Self  : access Scenario_Vars_Property;
      Value : in out GNATCOLL.JSON.JSON_Value);
   overriding procedure Load
     (Self  : in out Scenario_Vars_Property;
      Value : GNATCOLL.JSON.JSON_Value);
   --  A property used to store the current scenario for the next
   --  GNAT Studio session.

   procedure Restore_Scenario_Vars
     (Kernel  : access Kernel_Handle_Record'Class;
      Project : Virtual_File);
   procedure Save_Scenario_Vars
     (Self    : not null access GPS_Project_Tree'Class);
   --  Restore the scenario variables set in previous sessions.
   --  They get their value from the following sources:
   --    1 - command line -Xvar=value switches
   --    2 - environment variables
   --    3 - saved value from previous GNAT Studio sessions
   --    4 - default from project
   --    5 - first valid value for the variable.
   --
   --  So here we set their value only if it doesn't exist yet in the
   --  environment (where it would be from 1 or 2).

   ----------
   -- Save --
   ----------

   overriding procedure Save
     (Self  : access Scenario_Vars_Property;
      Value : in out GNATCOLL.JSON.JSON_Value)
   is
      use String_Maps;
      use GNATCOLL.JSON;

      Values : JSON_Array;
      C      : String_Maps.Cursor := Self.Map.First;
   begin
      while Has_Element (C) loop
         declare
            Value : constant JSON_Value := Create_Object;
         begin
            Value.Set_Field ("name", Key (C));
            Value.Set_Field ("value", Element (C));
            Append (Values, Value);
         end;
         Next (C);
      end loop;
      Value.Set_Field ("value", Values);
   end Save;

   ----------
   -- Load --
   ----------

   overriding procedure Load
     (Self  : in out Scenario_Vars_Property;
      Value : GNATCOLL.JSON.JSON_Value)
   is
      use GNATCOLL.JSON;

      Values : constant JSON_Array := Value.Get ("value");
   begin
      for Index in 1 .. Length (Values) loop
         declare
            V : constant JSON_Value := Get (Values, Index);
         begin
            Self.Map.Include (V.Get ("name"), V.Get ("value"));
         end;
      end loop;
   end Load;

   ---------------------------
   -- Restore_Scenario_Vars --
   ---------------------------

   procedure Restore_Scenario_Vars
     (Kernel  : access Kernel_Handle_Record'Class;
      Project : Virtual_File)
   is
      use String_Maps;
      C     : String_Maps.Cursor;
      Found : Boolean;
      Vars  : Scenario_Vars_Property;
   begin
      Get_Property (Vars, Project, "scenario", Found);
      if Found then
         C := Vars.Map.First;
         while Has_Element (C) loop
            declare
               Name    : constant String := Key (C);
               Value   : constant String := Element (C);
            begin
               if Kernel.Registry.Environment.Value (Name) = "" then
                  Trace (Me, "Restoring environment var: "
                         & Name & "=" & Value);
                  Kernel.Registry.Environment.Change_Environment
                    (Name  => Name,
                     Value => Value);
               end if;
               Next (C);
            end;
         end loop;
      end if;
   end Restore_Scenario_Vars;

   ------------------------
   -- Save_Scenario_Vars --
   ------------------------

   procedure Save_Scenario_Vars
     (Self : not null access GPS_Project_Tree'Class)
   is
      Vars         : Scenario_Vars_Property_Access;
      Typed_Vars   : constant Scenario_Variable_Array :=
                       Self.Scenario_Variables;
      Untyped_Vars : constant Untyped_Variable_Array :=
                       Self.Untyped_Variables;
   begin
      --  Save existing scenario in the properties, so that we can restore it
      --  when the project is reloaded

      if Self.Status = From_File then
         Vars := new Scenario_Vars_Property;

         --  Save the typed scenario variables
         for Var of Typed_Vars loop
            --  Do not save if the value corresponds to the default value.
            if External_Default (Var) /= Value (Var) then
               Vars.Map.Include
                 (External_Name (Var),
                  Value (Var));
            end if;
         end loop;

         --  Save the untyped scenario variables
         for Var of Untyped_Vars loop
            if External_Default (Var) /= Value (Var) then
               Vars.Map.Include
                 (External_Name (Var),
                  Value (Var));
            end if;
         end loop;

         Set_Property
           (Kernel     => Self.Handle,
            File       => Self.Root_Project.Project_Path,
            Name       => "scenario",
            Property   => Property_Access (Vars),
            Persistent => True);
      end if;
   end Save_Scenario_Vars;

   --------------------------------
   -- Save_Scenario_Vars_On_Exit --
   --------------------------------

   procedure Save_Scenario_Vars_On_Exit
     (Handle : not null access Kernel_Handle_Record'Class)
   is
   begin
      Save_Scenario_Vars
        (GPS_Project_Tree_Access (Get_Registry (Handle).Tree));
   end Save_Scenario_Vars_On_Exit;

   ----------------------
   -- Set_GNAT_Version --
   ----------------------

   overriding procedure Set_GNAT_Version
     (Self    : in out GPS_Project_Environment;
      Version : String) is
   begin
      Self.Kernel.GNAT_Version_Cache :=
        VSS.Strings.Conversions.To_Virtual_String (Version);
   end Set_GNAT_Version;

   ------------------
   -- Spawn_Gnatls --
   ------------------

   overriding procedure Spawn_Gnatls
     (Self         : GPS_Project_Environment;
      Fd           : out GNAT.Expect.Process_Descriptor_Access;
      Gnatls_Args  : GNAT.OS_Lib.Argument_List_Access;
      Errors       : Error_Report)
   is
   begin
      if not Is_Local (Build_Server) then
         Remote_Spawn
           (Fd, Get_Nickname (Build_Server), Gnatls_Args.all,
            Err_To_Out => True);
      else
         --  Inherited version spawns gnatls locally
         Spawn_Gnatls (Project_Environment (Self), Fd, Gnatls_Args, Errors);
      end if;
   end Spawn_Gnatls;

   -----------------
   -- Gnatls_Host --
   -----------------

   overriding function Gnatls_Host
     (Self : GPS_Project_Environment) return String
   is
      pragma Unreferenced (Self);
   begin
      return Get_Nickname (Build_Server);
   end Gnatls_Host;

   ---------------------
   -- Create_Registry --
   ---------------------

   procedure Create_Registry
     (Handle : access Kernel_Handle_Record'Class;
      Result : out Projects.Project_Registry_Access)
   is
      Tree : constant Project_Tree_Access := new GPS_Project_Tree;
      Env  : constant Project_Environment_Access :=
         new GPS_Project_Environment'
            (Project_Environment with Kernel => Handle);
   begin
      Env.Set_Save_Config_File (Saved_Config_File);
      GPS_Project_Tree (Tree.all).Handle := Kernel_Handle (Handle);
      Result := Projects.Create (Tree => Tree, Env => Env);
   end Create_Registry;

   ------------------
   -- Data_Factory --
   ------------------

   overriding function Data_Factory
     (Self : GPS_Project_Tree) return Project_Data_Access
   is
      pragma Unreferenced (Self);
   begin
      return new GPS_Project_Data;
   end Data_Factory;

   --------------------
   -- Recompute_View --
   --------------------

   overriding procedure Recompute_View
     (Self   : in out GPS_Project_Tree;
      Errors : Error_Report := null)
   is
   begin
      Cleanup_Subdirs (Self);

      Get_Registry (Self.Handle).Reset_Cache;
      Recompute_View (Project_Tree (Self), Errors);

      --  If we are in the process of creating the kernel, no need to do
      --  anything else here
      --  ??? It would be nice to rely on a better indicator than this
      if Self.Handle.Get_Main_Window = null then
         return;
      end if;

      --  The current context might reference the old project tree:
      --  refresh it immediately.
      Self.Handle.Refresh_Context;

      if Self.Propagate then
         Project_View_Changed_Hook.Run (Self.Handle);
      end if;
   end Recompute_View;

   --------------------
   -- Recompute_View --
   --------------------

   function Recompute_View
     (Handle : access Kernel_Handle_Record'Class)
      return Boolean
   is
      Has_Error : Boolean := False;

      procedure Report_Error (S : String);

      ------------------
      -- Report_Error --
      ------------------

      procedure Report_Error (S : String) is
      begin
         Has_Error := True;
         Handle.Insert (S, Mode => Error, Add_LF => False);
         Parse_File_Locations (Handle, S, Location_Category);
      end Report_Error;

   begin
      Get_Registry (Handle).Reset_Cache;
      Get_Registry (Handle).Tree.Recompute_View
        (Errors => Report_Error'Unrestricted_Access);

      --  Refresh the menus and icons in the toolbars
      Update_Menus_And_Buttons (Handle);
      return not Has_Error;
   end Recompute_View;

   --------------------
   -- Recompute_View --
   --------------------

   procedure Recompute_View (Handle : access Kernel_Handle_Record'Class)
   is
      Dummy : Boolean;
   begin
      Dummy := Recompute_View (Handle);
   end Recompute_View;

   --------------------------
   -- Load_Default_Project --
   --------------------------

   procedure Load_Default_Project
     (Kernel               : access Kernel_Handle_Record'Class;
      Directory            : GNATCOLL.VFS.Virtual_File;
      Load_Default_Desktop : Boolean := True;
      Clear                : Boolean := True)
   is
      Block_Me : constant Block_Trace_Handle := Create (Me) with Unreferenced;

      function Find_Custom_File (Name : Filesystem_String) return Virtual_File;
      --  Parse GNATSTUDIO_CUSTOM_PATH for a file named Name overriding the
      --  installation one.
      --  If no such file exist then return the one from the installation.

      ----------------------
      -- Find_Custom_File --
      ----------------------

      function Find_Custom_File (Name : Filesystem_String) return Virtual_File
      is
         Env_Path  : constant File_Array := GPS.Kernel.Custom.Get_Custom_Path;
         Share_Dir : constant Virtual_File :=
           Create_From_Dir (Get_System_Dir (Kernel), "share/gnatstudio/");
      begin
         --  Search for an user file overriding the default one in
         --  GNATSTUDIO_CUSTOM_PATH
         for J in Env_Path'Range loop
            if Env_Path (J).Is_Directory then
               declare
                  File : constant Virtual_File :=
                    Create_From_Dir (Env_Path (J), Name);
               begin
                  if File.Is_Regular_File then
                     return File;
                  end if;
               end;
            elsif Env_Path (J).Is_Regular_File
              and then Env_Path (J).Base_Name = Name
            then
               return Env_Path (J);
            end if;
         end loop;

         --  Use the file from the installation
         return Create_From_Dir (Share_Dir, Name);
      end Find_Custom_File;

      Project    : Virtual_File := Create_From_Dir (Directory, "default.gpr");
      Default    : constant Virtual_File := Find_Custom_File ("default.gpr");
      Readonly   : constant Virtual_File := Find_Custom_File ("readonly.gpr");
      Found      : Boolean;
      Is_Default : Boolean := False;

      Ignore : Boolean;
      pragma Unreferenced (Ignore);

   begin
      --  Save all open children, and close everything. A new desktop will be
      --  open in the end anyway

      if not Save_MDI_Children (Kernel, Force => False) then
         return;
      end if;

      if Is_Regular_File (Project) then
         Found := True;

      elsif Is_Writable (Directory) and then Is_Regular_File (Default) then
         Copy (Default, Project.Full_Name, Found);
         Is_Default := True;

      elsif Is_Regular_File (Readonly) then
         Project := Readonly;
         Found := True;

      else
         Found := False;
      end if;

      if Found then
         Load_Project
           (Kernel, Project,
            Clear      => Clear,
            Is_Default => Is_Default);
      else
         Load_Empty_Project (Kernel);
      end if;

      --  Reload the default desktop

      if Load_Default_Desktop then
         Close_All_Children (Kernel);
         Ignore := Load_Desktop (Kernel);
      end if;
   end Load_Default_Project;

   --------------------------
   -- Load_Default_Project --
   --------------------------

   function Load_Default_Project
     (Kernel : not null access Kernel_Handle_Record'Class;
      Parent : not null access Gtk_Window_Record'Class) return Boolean
   is
      pragma Unreferenced (Parent);
   begin
      Load_Default_Project
        (Kernel,
         Directory => Get_Current_Dir,
         Clear     => False);

      return True;
   end Load_Default_Project;

   ------------------------
   -- Load_Empty_Project --
   ------------------------

   procedure Load_Empty_Project
     (Kernel : access Kernel_Handle_Record'Class)
   is
      Ignore : Boolean;
      pragma Unreferenced (Ignore);
   begin
      Trace (Me, "Load_Empty_Project");

      Close_All_Children (Kernel);

      Kernel.Registry.Tree.Load_Empty_Project
        (Kernel.Registry.Environment, Recompute_View => False);
      Project_Changed_Hook.Run (Kernel);
      Recompute_View (Kernel);   --  also resets the xref database.

      Ignore := Load_Desktop (Kernel);
   end Load_Empty_Project;

   ----------------------
   -- Load_Alire_Crate --
   ----------------------

   procedure Load_Alire_Crate
     (Kernel         : access Kernel_Handle_Record'Class;
      Alire_Manifest : GNATCOLL.VFS.Virtual_File)
   is
      Ignore : Boolean;
   begin
      --  Run the 'project_changing' hook with the alire.toml manifest and
      --  load an empty project meanwhile the  alire.py Python plugin is then
      --  responsible to parse it and load the .gpr file after setting all the
      --  needed dependencies and environment.
      Trace (Me, "Running the 'project_changing' hook for Alire");
      Project_Changing_Hook.Run (Kernel, Alire_Manifest);
      Trace (Me, "Finished running the 'project_changing' hook for Alire");
      Load_Empty_Project (Kernel);
      Ignore := Load_Desktop (Kernel);
   end Load_Alire_Crate;

   ------------------------------
   -- Reload_Project_If_Needed --
   ------------------------------

   procedure Reload_Project_If_Needed
     (Kernel : access Kernel_Handle_Record'Class;
      Recompute_View : Boolean := False)
   is
      procedure Report_Error (S : String);
      --  Output error messages from the project parser to the console

      ------------------
      -- Report_Error --
      ------------------

      procedure Report_Error (S : String) is
      begin
         --  Temporarily unset auto-jump, to handle the following case:
         --  the project file contains errors and couldn't be loaded, but it
         --  was also saved in the desktop. If that is the case, the project
         --  would be open several times otherwise
         Kernel.Insert (S, Mode => Error, Add_LF => False);
         Parse_File_Locations
           (Kernel, S, Location_Category, Allow_Auto_Jump_To_First => False);
      end Report_Error;

      Reloaded : Boolean := False;
   begin
      declare
         Project : constant Project_Type := Kernel.Registry.Tree.Root_Project;
      begin
         if Project.Name = "empty"
           and then Kernel.Last_Invalid_Project /= No_File
         then
            Load_Project (Kernel, Kernel.Last_Invalid_Project);
         else
            Remove_Category
              (Get_Messages_Container (Kernel),
               Category => Location_Category,
               Flags    => Empty_Message_Flags);

            Kernel.Registry.Tree.Reload_If_Needed
              (Reloaded, Errors => Report_Error'Unrestricted_Access,
               Recompute_View => Recompute_View);
         end if;
      exception
         when Invalid_Project =>
            null;
      end;

      if Reloaded then
         Project_Changed_Hook.Run (Kernel);

         if Recompute_View then
            Project_View_Changed_Hook.Run (Kernel);
         end if;
      end if;
   end Reload_Project_If_Needed;

   ------------------
   -- Load_Project --
   ------------------

   procedure Load_Project
     (Kernel       : access Kernel_Handle_Record'Class;
      Project      : GNATCOLL.VFS.Virtual_File;
      No_Save      : Boolean := False;
      Clear        : Boolean := True;
      Is_Default   : Boolean := False;
      Keep_Desktop : Boolean := False)
   is
      Block_Me  : constant Block_Trace_Handle :=
         Create (Me, (if Active (Me) then Project.Display_Full_Name else ""))
         with Unreferenced;

      Has_Error               : Boolean := False;

      Has_Undefined_Externals : Boolean := False;

      procedure Report_Error (S : String);
      --  Output error messages from the project parser to the console

      ------------------
      -- Report_Error --
      ------------------

      procedure Report_Error (S : String) is
      begin
         --  Temporarily unset auto-jump, to handle the following case:
         --  the project file contains errors and couldn't be loaded, but it
         --  was also saved in the desktop. If that is the case, the project
         --  would be open several times otherwise
         Has_Error := True;
         Kernel.Insert (S, Mode => Error, Add_LF => False);
         Parse_File_Locations
           (Kernel, S, Location_Category, Allow_Auto_Jump_To_First => False);

         --  If we have scenario variables relying on undefined externals,
         --  let's warn the user that GNAT Studio will chose the first values
         --  available for these scenario variables.

         declare
            Undefined_External_Msg : constant String :=
                                "undefined external";
         begin
            Has_Undefined_Externals :=
              Ada.Strings.Fixed.Index
                (S, Pattern => Undefined_External_Msg) /= 0;
         end;
      end Report_Error;

      Ignore : Boolean;
      pragma Unreferenced (Ignore);
      New_Project_Loaded  : Boolean;
      Same_Project        : Boolean;
      Local_Project       : GNATCOLL.VFS.Virtual_File;
      Previous_Project    : Virtual_File;

   begin
      Trace (Me, "Clearing messages");

      Remove_Category
        (Get_Messages_Container (Kernel),
         Category => Location_Category,
         Flags    => Empty_Message_Flags);

      --  Are we reloading the same project ?
      if Get_Registry (Kernel).Tree.Status = From_File
        and then Project_Path (Get_Project (Kernel)) = Project
      then
         Same_Project := True;
      else
         Same_Project := False;
      end if;

      if Get_Project (Kernel) /= No_Project
        and then Get_Registry (Kernel).Tree.Status = From_File
      then
         Previous_Project := Project_Path (Get_Project (Kernel));
      else
         Previous_Project := GNATCOLL.VFS.No_File;
      end if;

      --  Unless we are reloading the same project

      if not No_Save
        and then not Save_MDI_Children (Kernel, Force => False)
      then
         return;
      end if;

      if not Same_Project then
         --  Never save automatically the desktop for the default project

         if Kernel.Registry.Tree.Status = From_File
           and then Save_Desktop_On_Exit.Get_Pref
         then
            Save_Desktop (Kernel);
         end if;

         --  Close all children before loading the new projet, in case a new
         --  editor needs to be loaded to display error messages

         if not Keep_Desktop then
            Close_All_Children (Kernel);
         end if;

         --  Clear the console so that obsolete messages are not displayed
         if Clear then
            Kernel.Clear_Messages;
         end if;

         Save_Scenario_Vars (GPS_Project_Tree_Access (Kernel.Registry.Tree));
      end if;

      if Is_Regular_File (Project) then
         Trace (Me, "Running the 'project_changing' hook");
         Project_Changing_Hook.Run (Kernel, Project);
         Trace (Me, "Finished running the 'project_changing' hook");

         if not Is_Local (Project) then
            Trace (Me, "Converting to Local Project");
            --  Loading a remote project. Convert its path to the local path
            --  Note that Running the Project_Changing_Hook has already
            --  set the build_server to Project's Host

            Local_Project := To_Local (Project);

            if not Is_Regular_File (Local_Project) then
               Kernel.Insert
                 ((-"Cannot find remote project file ")
                  & Display_Full_Name (Project) & (-" at local place ")
                  & Display_Full_Name (Local_Project) &
                  (-". Please check your remote configuration."),
                  Mode => Error, Add_LF => False);

               --  Need to run Project_Changing hook to reset build_server
               Project_Changing_Hook.Run (Kernel, Previous_Project);

               Ignore := Load_Desktop (Kernel);
               return;
            end if;

         else
            Local_Project := Project;

            --  If we are running locally force a call to gnatls to get the
            --  value of ADA_PROJECT_PATH and the default search path.
            --  (This is a hack to handle modification of ADA_PROJECT_PATH
            --  while GS is running see FB07-010)
            Kernel.Registry.Environment.Invalidate_Gnatls_Cache;
         end if;

         Change_Dir (Dir (Local_Project));

         Get_Messages_Container (Kernel).Remove_Category
           (Location_Category, Location_Message_Flags);

         --  Doing backup saves while loading a project has no effect, so for
         --  performance's sake disables them.
         Set_Is_Loading (True);

         if not Kernel.Get_Ignore_Saved_Scenario_Values then
            Restore_Scenario_Vars (Kernel, Local_Project);
         end if;

         begin
            GPS_Project_Tree_Access (Kernel.Registry.Tree).Propagate := False;
            Kernel.Registry.Tree.Load_Empty_Project
              (Env            => Kernel.Registry.Environment,
               Recompute_View => True);  -- Needed to reset the subprojects
            GPS_Project_Tree_Access (Kernel.Registry.Tree).Propagate := True;
            New_Project_Loaded := True;
            Kernel.Registry.Tree.Load
              (Root_Project_Path => Local_Project,
               Env               => Kernel.Registry.Environment,
               Errors            => Report_Error'Unrestricted_Access,
               Recompute_View    => False,
               Report_Missing_Dirs => Active (Report_Missing_Dirs));
            Kernel.Last_Invalid_Project := No_File;
         exception
            when Invalid_Project =>
               New_Project_Loaded := False;
               Kernel.Last_Invalid_Project := Local_Project;
         end;

         if not New_Project_Loaded then
            --  Check if a remote configuration was applied and failure occured
            if not Is_Local (Build_Server) then
               Report_Error
                 (-"Error while loading project '" &
                  Display_Full_Name (Local_Project, True) &
                  (-"'. Trying with the build server set to (local)...") &
                  ASCII.LF);

               --  Reset the build server
               Trace (Me, "Reset the build server");
               Assign (Kernel_Handle (Kernel),
                       Build_Server,
                       "",
                       Local_Project,
                       Reload_Prj => False);
               Trace (Me, "Load the project locally");

               begin
                  Kernel.Registry.Tree.Load
                    (Root_Project_Path => Local_Project,
                     Errors            => Report_Error'Unrestricted_Access,
                     Env               => Kernel.Registry.Environment,
                     Recompute_View    => False,
                     Report_Missing_Dirs => Active (Report_Missing_Dirs));
                  New_Project_Loaded := True;
                  Kernel.Last_Invalid_Project := No_File;

               exception
                  when Invalid_Project =>
                     Kernel.Last_Invalid_Project := Local_Project;
               end;
            end if;

            Ignore := Load_Desktop (Kernel);

         elsif Is_Default then
            --  Successful load of default project
            Get_Registry (Kernel).Tree.Set_Status (Default);
         end if;

         --  Clear the registry caches
         Get_Registry (Kernel).Reset_Cache;

         Trace (Me, "Running the 'project_changed' hook");
         Project_Changed_Hook.Run (Kernel);
         Trace (Me, "Finished running the 'project_changed' hook");

         --  Make sure the subdirs is correctly set for objects
         Kernel.Set_Build_Mode (Kernel.Get_Build_Mode);

         --  Recompute the project view before loading the desktop, since the
         --  latter operation with also load files which might need to do xref
         --  queries.
         --  If the project results in errors, they will be properly displayed
         --  in the locations view when it is opened, since they are stored in
         --  a GUI independent model.

         Xref.Project_Changed (Kernel.Databases);
         --  Don't replace by "or else": we must always call Recompute_View
         Has_Error := Has_Error or not Recompute_View (Kernel);

         --  Reload the desktop, in case there is a project-specific setup
         --  already. We need to do this before running the hooks, in case some
         --  python script needs to open or refresh windows as a result.

         if not Same_Project and not Keep_Desktop then
            Ignore := Load_Desktop
              (Kernel, For_Project => Local_Project);
         end if;
         --  Reallow the backup save
         Set_Is_Loading (False);

      elsif not Same_Project then
         Kernel.Insert (-"Cannot find project file "
                        & Display_Full_Name (Project) & ASCII.LF,
                        Mode => Error, Add_LF => False);
         Ignore := Load_Desktop (Kernel);

         Xref.Project_Changed (Kernel.Databases);
      end if;

      if Is_Regular_File (Project) and then Has_Error then
         Open_File_Action_Hook.Run
           (Kernel  => Kernel,
            File    => Local_Project,
            Project => No_Project);
      end if;

      --  Display a warning message in the Messages view if we've found
      --  undefined externals while loading the project.

      if Has_Undefined_Externals then
         Kernel.Insert
           (Text =>
              "Some scenario variables relying on undefined "
            & "externals have been found while loading the "
            & "project: GNAT Studio will use the first available "
            & "values for these scenario variables as a fallback "
            & "(go to the Scenario view to see which values were "
            & "picked).",
            Mode => Error);
         Kernel.Insert
           (Text =>
              "The Project tree could be partially loaded in this "
              & "case please try a reload.",
            Mode => Error);
      end if;
   end Load_Project;

   -----------------
   -- Get_Project --
   -----------------

   function Get_Project (Handle : access Kernel_Handle_Record'Class)
      return Project_Type is
   begin
      return Handle.Registry.Tree.Root_Project;
   end Get_Project;

   ---------------------------
   -- Get_Root_Project_View --
   ---------------------------

   function Get_Root_Project_View
     (Self : not null access Kernel_Handle_Record'Class)
      return Projects.Views.Project_View_Reference is
   begin
      return
        Projects.Views.Create_Project_View_Reference
          (Self, Self.Registry.Tree.Root_Project);
   end Get_Root_Project_View;

   --------------------
   -- Lookup_Project --
   --------------------

   function Lookup_Project
     (Self : not null access Kernel_Handle_Record'Class;
      File : GNATCOLL.VFS.Virtual_File)
      return GNATCOLL.Projects.Project_Type is
   begin
      return Self.Registry.Tree.Project_From_Path (File);
   end Lookup_Project;

   --------------------------
   -- Get_Project_For_File --
   --------------------------

   function Get_Project_For_File
     (Tree : access Project_Tree'Class;
      File : GNATCOLL.VFS.Virtual_File) return Project_Type
   is
      Iter : Project_Iterator;
      Prj  : Project_Type;

   begin
      if Is_Directory (File) then
         if Tree.Directory_Belongs_To_Project
           (File.Full_Name, Direct_Only => False)
         then
            return Tree.Root_Project;   --   ??? Not accurate
         else
            return No_Project;
         end if;

      else
         --  Do we have a source file ?
         declare
            F_Info : constant File_Info'Class :=
                       File_Info'Class (Tree.Info_Set (File).First_Element);
         begin
            Prj := F_Info.Project;
         end;
         if Prj /= No_Project then
            return Prj;
         end if;
      end if;

      Iter := Start (Tree.Root_Project, Include_Extended => False);
      loop
         Prj := Current (Iter);
         exit when Prj = No_Project;

         Next (Iter);

         --  First check if the file is a project file
         if File = Project_Path (Prj) then
            return Prj;
         end if;
      end loop;

      return No_Project;
   end Get_Project_For_File;

   ------------------------
   -- Scenario_Variables --
   ------------------------

   function Scenario_Variables (Kernel : access Kernel_Handle_Record'Class)
      return Scenario_Variable_Array is
   begin
      return Kernel.Registry.Tree.Scenario_Variables;
   end Scenario_Variables;

   -----------------------
   -- Untyped_Variables --
   -----------------------

   function Untyped_Variables
     (Kernel : not null access Kernel_Handle_Record'Class)
      return Untyped_Variable_Array is
   begin
      return Kernel.Registry.Tree.Untyped_Variables;
   end Untyped_Variables;

   ------------------
   -- Save_Project --
   ------------------

   function Save_Project
     (Kernel    : access Kernel_Handle_Record'Class;
      Project   : Project_Type;
      Recursive : Boolean := False) return Boolean
   is
      Iter                : Project_Iterator := Project.Start (Recursive);
      Current_Is_Modified : Boolean := False;
      Modified            : Boolean := False;
      Result              : Boolean := True;
   begin
      while Current (Iter) /= No_Project loop
         Current_Is_Modified := Current (Iter).Modified;
         Modified := Modified or else Current_Is_Modified;

         --  If the current project has been modified, save it
         if Current_Is_Modified then
            Result := Save_Single_Project (Kernel, Current (Iter)) and Result;
         end if;

         Next (Iter);
      end loop;

      --  Force a change in the icons in the explorer.
      --  ??? Probably not very efficient, however.

      if Modified then
         Project_View_Changed_Hook.Run (Kernel);
      end if;

      return Result;
   end Save_Project;

   -------------------------
   -- Save_Single_Project --
   -------------------------

   function Save_Single_Project
     (Kernel  : access Kernel_Handle_Record'Class;
      Project : Project_Type) return Boolean
   is
      Result : Boolean := True;

      procedure Report_Error (Msg : String);
      --  Report errors to the user

      ------------------
      -- Report_Error --
      ------------------

      procedure Report_Error (Msg : String) is
      begin
         Kernel.Insert (Msg, Mode => GPS.Kernel.Error);
         Parse_File_Locations (Kernel, Msg, "Project save");
         Result := False;
      end Report_Error;

   begin
      if Project.Save (Errors => Report_Error'Unrestricted_Access) then
         Project_Saved_Hook.Run (Kernel, Project => Project);
      end if;

      return Result;
   end Save_Single_Project;

   ------------------
   -- Get_Registry --
   ------------------

   function Get_Registry
     (Handle : access Kernel_Handle_Record'Class)
      return Projects.Project_Registry_Access is
   begin
      return Handle.Registry;
   end Get_Registry;

   ------------------
   -- Get_Switches --
   ------------------

   function Get_Switches
     (Project           : Project_Type;
      Tool              : Tool_Properties_Record;
      File              : GNATCOLL.VFS.Virtual_File := GNATCOLL.VFS.No_File;
      Use_Initial_Value : Boolean := False) return GNAT.OS_Lib.Argument_List
   is
      Value      : String_List_Access;
      Is_Default : Boolean;
   begin
      Project.Switches
        (To_Lower (To_String (Tool.Project_Package)),
         File,
         To_Lower (To_String (Tool.Project_Index)), Value, Is_Default);

      --  If no value was found, we might have to return the initial value
      if Value = null
        and then Use_Initial_Value
        and then Tool.Initial_Cmd_Line /= Null_Unbounded_String
      then
         Value := Argument_String_To_List (To_String (Tool.Initial_Cmd_Line));
      end if;

      if Value = null then
         return (1 .. 0 => null);
      end if;

      declare
         procedure Unchecked_Free is new Ada.Unchecked_Deallocation
           (Argument_List, Argument_List_Access);
         Cmd : constant Argument_List := Value.all;
      begin
         Unchecked_Free (Value);
         return Cmd;
      end;
   end Get_Switches;

   --------------------
   -- Add_Source_Dir --
   --------------------

   procedure Add_Source_Dir
     (Project            : GNATCOLL.Projects.Project_Type;
      Dir                : Virtual_File;
      Success            : out Boolean;
      Use_Relative_Paths : Boolean := False)
   is
      Dirs    : GNAT.Strings.String_List (1 .. 1);
      Sources : constant File_Array :=
        Project.Source_Dirs (Recursive => False);
      Found   : Boolean := False;
   begin
      Success := True;

      Ensure_Directory (Dir);
      if Use_Relative_Paths then
         Dirs (1) :=
           new String'(+Dir.Relative_Path (Project.Project_Path.Dir));
      else
         Dirs (1) := new String'(+Dir.Full_Name);
      end if;

      if not Is_Editable (Project) then
         Success := False;
         return;
      else
         for S in Sources'Range loop
            if Sources (S) = Dir then
               Found := True;
               exit;
            end if;
         end loop;

         if not Found then
            Project.Set_Attribute
              (Attribute => Source_Dirs_Attribute,
               Values    => Dirs,
               Index     => "",
               Prepend   => True);
         end if;

         Free (Dirs);
      end if;
   end Add_Source_Dir;

   ------------------------
   -- Is_Alire_Available --
   ------------------------

   function Is_Alire_Available
     (Kernel : not null access Kernel_Handle_Record'Class) return Boolean
   is
      pragma Unreferenced (Kernel);
   begin
      return Locate_On_Path ("alr") /= No_File;
   end Is_Alire_Available;

   ---------------------------------
   -- Display_Open_Project_Dialog --
   ---------------------------------

   function Display_Open_Project_Dialog
     (Kernel : not null access Kernel_Handle_Record'Class;
      Parent : not null access Gtk_Window_Record'Class) return Boolean
   is
      Filename : constant Virtual_File :=
        Select_File
          (-"Open Project",
           File_Pattern      => "*.gpr",
           Pattern_Name      => -"Project files",
           Parent            => Gtk_Window (Parent),
           Use_Native_Dialog => Use_Native_Dialogs.Get_Pref,
           Kind              => Open_File,
           History           => Get_History (Kernel));
   begin
      if Filename /= GNATCOLL.VFS.No_File then
         Load_Project (Kernel, Filename);
         return True;
      else
         return False;
      end if;
   end Display_Open_Project_Dialog;

   -------------------------------------
   -- Display_Open_Alire_Crate_Dialog --
   -------------------------------------

   function Display_Open_Alire_Crate_Dialog
     (Kernel : not null access Kernel_Handle_Record'Class;
      Parent : not null access Gtk_Window_Record'Class) return Boolean
   is
      Filename : constant Virtual_File :=
        Select_File
          (-"Open Project",
           File_Pattern      => "alire.toml",
           Pattern_Name      => -"Alire Crate files",
           Parent            => Gtk_Window (Parent),
           Use_Native_Dialog => Use_Native_Dialogs.Get_Pref,
           Kind              => Open_File,
           History           => Get_History (Kernel));
   begin
      if Filename /= GNATCOLL.VFS.No_File then
         Load_Alire_Crate (Kernel, Filename);
         return True;
      else
         return False;
      end if;
   end Display_Open_Alire_Crate_Dialog;

end GPS.Kernel.Project;
