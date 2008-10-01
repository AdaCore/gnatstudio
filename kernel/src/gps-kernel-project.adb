-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2008, AdaCore              --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Ada.Characters.Handling;   use Ada.Characters.Handling;
with GNAT.Strings;
with GNATCOLL.Utils;            use GNATCOLL.Utils;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
pragma Warnings (Off);
with GNAT.Expect.TTY.Remote;    use GNAT.Expect.TTY.Remote;
pragma Warnings (On);
with Ada.Unchecked_Deallocation;

with Projects;                  use Projects;
with Projects.Editor;           use Projects.Editor;
with Projects.Registry;         use Projects.Registry;
with Projects.Registry.Queries;
with Basic_Types;
with Entities;
with Glib.Xml_Int;              use Glib.Xml_Int;
with Prj;
with Remote.Path.Translator;    use Remote, Remote.Path.Translator;
with Traces;                    use Traces;
with GNATCOLL.VFS;              use GNATCOLL.VFS;

with Filesystems;               use Filesystems;
with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel.Console;        use GPS.Kernel.Console;
with GPS.Location_View;         use GPS.Location_View;
with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;
with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;
with GPS.Kernel.Properties;     use GPS.Kernel.Properties;
with GPS.Kernel.Remote;         use GPS.Kernel.Remote;
with GPS.Kernel.Standard_Hooks; use GPS.Kernel.Standard_Hooks;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;

package body GPS.Kernel.Project is

   Me : constant Debug_Handle := Create ("GPS.Kernel.Project");

   Location_Category : constant String := "Project";
   --  Category uses in the Location window for errors related to loading the
   --  project file

   procedure Compute_Predefined_Paths
     (Handle : access Kernel_Handle_Record'Class;
      Use_Cache : Boolean := True);
   --  Compute the predefined source and object paths, given the current
   --  project view associated with Handle.

   procedure On_Build_Server_Connection
     (Handle : access Kernel_Handle_Record'Class);
   --  Recompute the predefined source and object paths upon build server
   --  connection, when this information was previously retrieved from cache.

   type Registry_Error_Handler_Record is new Error_Handler_Record with record
      Handle : Kernel_Handle;
      Mode   : Message_Type := Error;
   end record;

   overriding procedure Report
     (Handler : access Registry_Error_Handler_Record; Msg : String);
   --  Used to report an error to the user.

   ------------
   -- Report --
   ------------

   overriding procedure Report
     (Handler : access Registry_Error_Handler_Record; Msg : String) is
   begin
      Insert (Handler.Handle, Msg, Mode => Handler.Mode);
   end Report;

   -------------------------------
   -- Predefined_Paths_Property --
   -------------------------------

   type Property_Index_Type is record
      Nickname : GNAT.Strings.String_Access;
      Gnatls   : GNAT.Strings.String_Access;
   end record;

   No_Index : constant Property_Index_Type :=
                (Nickname => null,
                 Gnatls   => null);

   function To_String (Idx : Property_Index_Type) return String;
   --  Translate the property_index_type into a unique string

   function To_String (Idx : Property_Index_Type) return String is
   begin
      return Idx.Nickname.all & "||" & Idx.Gnatls.all;
   end To_String;

   type Predefined_Paths_Property is new GPS.Kernel.Properties.Property_Record
   with record
      Source_Path  : Glib.String_Ptr;
      Object_Path  : Glib.String_Ptr;
      Project_Path : Glib.String_Ptr;
   end record;

   overriding procedure Save
     (Property : access Predefined_Paths_Property;
      Node     : in out Glib.Xml_Int.Node_Ptr);
   --  See inherited procedure

   overriding procedure Load
     (Property : in out Predefined_Paths_Property;
      From     : Glib.Xml_Int.Node_Ptr);
   --  See inherited procedure

   ----------
   -- Save --
   ----------

   overriding procedure Save
     (Property : access Predefined_Paths_Property;
      Node     : in out Glib.Xml_Int.Node_Ptr)
   is
      Child : Glib.Xml_Int.Node_Ptr;
   begin
      Child := new Glib.Xml_Int.Node;
      Child.Tag := new String'("source_path");
      Child.Value := new String'(Property.Source_Path.all);
      Add_Child (Node, Child);
      Child := new Glib.Xml_Int.Node;
      Child.Tag := new String'("object_path");
      Child.Value := new String'(Property.Object_Path.all);
      Add_Child (Node, Child);
      Child := new Glib.Xml_Int.Node;
      Child.Tag := new String'("project_path");
      Child.Value := new String'(Property.Project_Path.all);
      Add_Child (Node, Child);

   exception
      when E : others => Trace (Exception_Handle, E);
   end Save;

   ----------
   -- Load --
   ----------

   overriding procedure Load
     (Property : in out Predefined_Paths_Property;
      From     : Glib.Xml_Int.Node_Ptr)
   is
      Child : Glib.Xml_Int.Node_Ptr;
   begin
      Child := Find_Tag (From.Child, "source_path");
      Property.Source_Path := new String'(Child.Value.all);
      Child := Find_Tag (From.Child, "object_path");
      Property.Object_Path := new String'(Child.Value.all);
      Child := Find_Tag (From.Child, "project_path");
      Property.Project_Path := new String'(Child.Value.all);
   end Load;

   ---------------------------------------
   -- Invalidate_Predefined_Paths_Cache --
   ---------------------------------------

   procedure Invalidate_Predefined_Paths_Cache
     (Handle : access Kernel_Handle_Record'Class;
      Host   : String)
   is
      Property_Index   : Property_Index_Type := No_Index;
   begin
      Property_Index.Nickname := new String'(Host);
      Property_Index.Gnatls   := Handle.Gnatls_Cache;
      Remove_Property
        (Handle, "predefined_path", To_String (Property_Index), "gnatls");
      Free (Property_Index.Nickname);
   end Invalidate_Predefined_Paths_Cache;

   ------------------------------
   -- Compute_Predefined_Paths --
   ------------------------------

   procedure Compute_Predefined_Paths
     (Handle    : access Kernel_Handle_Record'Class;
      Use_Cache : Boolean := True)
   is
      Gnatls         : constant String := Get_Attribute_Value
        (Get_Project (Handle), Gnatlist_Attribute, Default => "gnatls");
      Gnatls_Args    : Argument_List_Access :=
                         Argument_String_To_List (Gnatls & " -v");
      Langs          : Argument_List := Get_Languages (Get_Project (Handle));
      Error_Handler  : aliased Registry_Error_Handler_Record;
      Property       : Predefined_Paths_Property;
      Prop_Access    : Property_Access;
      Property_Index : Property_Index_Type := No_Index;
      Success        : Boolean;

   begin
      if Basic_Types.Contains (Langs, "ada", Case_Sensitive => False) then
         --  If the gnatls commands hasn't changed, no need to recompute the
         --  predefined paths.

         if Use_Cache
           and then not Is_Local (Build_Server)
           and then Handle.Gnatls_Cache /= null
           and then Handle.Gnatls_Cache.all = Gnatls
           and then Handle.Gnatls_Server.all = Get_Nickname (Build_Server)
         then
            Free (Langs);
            Free (Gnatls_Args);
            return;
         end if;

         Free (Handle.Gnatls_Cache);
         Free (Handle.Gnatls_Server);
         Handle.Gnatls_Cache := new String'(Gnatls);
         Handle.Gnatls_Server := new String'(Get_Nickname (Build_Server));

         if not Is_Local (Build_Server)
           and then Use_Cache
           and then not Is_Ready_Session (Get_Nickname (Build_Server))
         then
            Property_Index.Nickname := Handle.Gnatls_Server;
            Property_Index.Gnatls   := Handle.Gnatls_Cache;
            Get_Property
              (Property, "predefined_path", To_String (Property_Index),
               Name => "gnatls", Found => Success);

            if Success then
               if Active (Me) then
                  Trace (Me, "set source path from cache to " &
                         Property.Source_Path.all);
                  Trace (Me, "set object path from cache to " &
                         Property.Object_Path.all);
                  Trace (Me, "set project path from cache to " &
                         Property.Project_Path.all);
               end if;

               Set_Predefined_Source_Path
                 (Handle.Registry.all, Property.Source_Path.all);
               Set_Predefined_Object_Path
                 (Handle.Registry.all, Property.Object_Path.all);
               Set_Predefined_Project_Path
                 (Handle.Registry.all, Property.Project_Path.all);
               Add_Hook
                 (Handle, Build_Server_Connected_Hook,
                  Wrapper (On_Build_Server_Connection'Access),
                  "compute_predefined_path");

               return;

            end if;
         end if;

         Error_Handler.Handle := Kernel_Handle (Handle);
         Error_Handler.Mode   := Info;

         Projects.Registry.Queries.Compute_Predefined_Paths
           (Handle.Registry,
            Handle.GNAT_Version,
            Gnatls_Args,
            Error_Handler'Unchecked_Access);

         if Property_Index /= No_Index then
            Property.Source_Path :=
              new String'(Get_Predefined_Source_Path (Handle.Registry.all));
            Property.Object_Path :=
              new String'(Get_Predefined_Object_Path (Handle.Registry.all));
            Property.Project_Path :=
              new String'(Get_Predefined_Project_Path (Handle.Registry.all));
            Prop_Access := new Predefined_Paths_Property'(Property);
            Set_Property
              (Handle,
               "predefined_path", To_String (Property_Index),
               Name       => "gnatls",
               Property   => Prop_Access,
               Persistent => True);
         end if;

      end if;

      Free (Gnatls_Args);
      Free (Langs);
   end Compute_Predefined_Paths;

   --------------------------------
   -- On_Build_Server_Connection --
   --------------------------------

   procedure On_Build_Server_Connection
     (Handle : access Kernel_Handle_Record'Class) is
   begin
      Trace (Me, -"Build server connected: recompute predefined paths");
      Compute_Predefined_Paths (Handle, False);
      Remove_Hook
        (Handle, Build_Server_Connected_Hook,
         Wrapper (On_Build_Server_Connection'Access));
   end On_Build_Server_Connection;

   --------------------------
   -- Load_Default_Project --
   --------------------------

   procedure Load_Default_Project
     (Kernel               : access Kernel_Handle_Record'Class;
      Directory            : GNATCOLL.VFS.Virtual_File;
      Load_Default_Desktop : Boolean := True;
      Clear                : Boolean := True)
   is
      Project             : Virtual_File :=
                              Create_From_Dir (Directory, "default.gpr");
      Share_Dir           : constant String :=
                              Get_System_Dir (Kernel) & "share/gps/";
      Default             : constant String := Share_Dir & "default.gpr";
      Readonly            : constant String := Share_Dir & "readonly.gpr";
      Found               : Boolean;
      Is_Default          : Boolean := False;

      Had_Project_Desktop : Boolean;
      pragma Unreferenced (Had_Project_Desktop);

   begin
      --  Save all open children, and close everything. A new desktop will be
      --  open in the end anyway

      if not Save_MDI_Children (Kernel, Force => False) then
         return;
      end if;

      Push_State (Kernel_Handle (Kernel), Busy);

      if Is_Regular_File (Project) then
         Found := True;
      elsif Is_Writable (Directory) and then Is_Regular_File (Default) then
         Copy_File (Default, Full_Name (Project).all, Found);
         Is_Default := True;
      elsif Is_Regular_File (Readonly) then
         Project := GNATCOLL.VFS.Create (Readonly);
         Found := True;
      else
         Found := False;
      end if;

      if Found then
         Load_Project
           (Kernel, Project,
            Clear                    => Clear,
            Is_Default               => Is_Default,
            Empty_Project_On_Failure => True);
      else
         Load_Empty_Project (Kernel);
      end if;

      --  Reload the default desktop

      if Load_Default_Desktop then
         Close_All_Children (Kernel);
         Had_Project_Desktop := Load_Desktop (Kernel);
      end if;

      Pop_State (Kernel_Handle (Kernel));
   end Load_Default_Project;

   ------------------------
   -- Load_Empty_Project --
   ------------------------

   procedure Load_Empty_Project
     (Kernel : access Kernel_Handle_Record'Class)
   is
      Had_Project_Desktop : Boolean;
      pragma Unreferenced (Had_Project_Desktop);
   begin
      Entities.Reset (Get_Database (Kernel));
      Load_Empty_Project (Kernel.Registry.all);
      Run_Hook (Kernel, Project_Changed_Hook);
      Recompute_View (Kernel);
      Close_All_Children (Kernel);
      Had_Project_Desktop := Load_Desktop (Kernel);
   end Load_Empty_Project;

   ------------------------------
   -- Reload_Project_If_Needed --
   ------------------------------

   procedure Reload_Project_If_Needed
     (Kernel : access Kernel_Handle_Record'Class)
   is
      procedure Report_Error (S : String);
      --  Output error messages from the project parser to the console.

      procedure Report_Error (S : String) is
         Old_Pref : constant Boolean := Auto_Jump_To_First.Get_Pref;
      begin
         --  Temporarily unset this, to handle the following case:
         --  the project file contains errors and couldn't be loaded, but it
         --  was also saved in the desktop. If that is the case, the project
         --  would be open several times otherwise
         Set_Pref (Auto_Jump_To_First, Kernel, False);
         Console.Insert (Kernel, S, Mode => Console.Error, Add_LF => False);
         Parse_File_Locations (Kernel, S, Location_Category);
         Set_Pref (Auto_Jump_To_First, Kernel, Old_Pref);
      end Report_Error;

      Reloaded : Boolean;
   begin
      Push_State (Kernel_Handle (Kernel), Busy);
      Reload_If_Needed
        (Kernel.Registry.all,
         Report_Error'Unrestricted_Access,
         Reloaded);
      if Reloaded then
         Run_Hook (Kernel, Project_Changed_Hook);
      end if;
      Pop_State (Kernel_Handle (Kernel));
   end Reload_Project_If_Needed;

   ------------------
   -- Load_Project --
   ------------------

   procedure Load_Project
     (Kernel                   : access Kernel_Handle_Record'class;
      Project                  : GNATCOLL.VFS.Virtual_File;
      No_Save                  : Boolean := False;
      Clear                    : Boolean := True;
      Is_Default               : Boolean := False;
      Empty_Project_On_Failure : Boolean := False)
   is
      procedure Report_Error (S : String);
      --  Output error messages from the project parser to the console.

      ------------------
      -- Report_Error --
      ------------------

      procedure Report_Error (S : String) is
         Old_Pref : constant Boolean := Auto_Jump_To_First.Get_Pref;
      begin
         --  Temporarily unset this, to handle the following case:
         --  the project file contains errors and couldn't be loaded, but it
         --  was also saved in the desktop. If that is the case, the project
         --  would be open several times otherwise

         Set_Pref (Auto_Jump_To_First, Kernel, False);
         Console.Insert (Kernel, S, Mode => Console.Error, Add_LF => False);
         Parse_File_Locations (Kernel, S, Location_Category);
         Set_Pref (Auto_Jump_To_First, Kernel, Old_Pref);
      end Report_Error;

      Had_Project_Desktop : Boolean;
      New_Project_Loaded  : Boolean;
      pragma Unreferenced (Had_Project_Desktop);
      Data                : aliased File_Hooks_Args;

      Same_Project     : Boolean;
      Local_Project    : GNATCOLL.VFS.Virtual_File;
      Load_Status      : Boolean;
      Previous_Project : Virtual_File;

   begin
      --  Are we reloading the same project ?
      if Status (Get_Project (Kernel)) = From_File
        and then Project_Path (Get_Project (Kernel)) = Project
      then
         Same_Project := True;
      else
         Same_Project := False;
      end if;

      if Get_Project (Kernel) /= No_Project
        and then Projects.Status (Get_Project (Kernel)) = From_File
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

         if Status (Get_Root_Project (Kernel.Registry.all)) = From_File
           and then Save_Desktop_On_Exit.Get_Pref
         then
            Save_Desktop (Kernel);
         end if;

         --  Close all children before loading the new projet, in case a new
         --  editor needs to be loaded to display error messages

         Close_All_Children (Kernel);

         --  Clear the console so that obsolete messages are not displayed
         if Clear then
            Console.Clear (Kernel);
         end if;
      end if;

      if Is_Regular_File (Project) then
         Push_State (Kernel_Handle (Kernel), Busy);

         Data.File := Project;
         Run_Hook (Kernel, Project_Changing_Hook, Data'Unchecked_Access);

         if not Is_Local (Project) then
            Trace (Me, "Converting to Local Project");
            --  Loading a remote project. Convert its path to the local path
            --  Note that Running the Project_Changing_Hook has already
            --  set the build_server to Project's Host

            Local_Project :=
              Create (To_Local (Full_Name (Project).all, Build_Server));

            if not Is_Regular_File (Local_Project) then
               Console.Insert
                 (Kernel, (-"Cannot find remote project file ")
                  & Full_Name (Project).all & (-" at local place ")
                  & Full_Name (Local_Project).all &
                  (-". Please check your remote configuration."),
                  Mode => Console.Error, Add_LF => False);

               --  Need to run Project_Changing hook to reset build_server
               Data.File := Previous_Project;
               Run_Hook (Kernel, Project_Changing_Hook, Data'Unchecked_Access);

               Had_Project_Desktop := Load_Desktop (Kernel);
               Pop_State (Kernel_Handle (Kernel));
               return;
            end if;

         else
            Local_Project := Project;
         end if;

         Change_Dir (Dir (Local_Project));

         --  When loading a new project, we need to reset the cache containing
         --  LI information, otherwise this cache might contain dangling
         --  references to projects that have been freed. Recompute_View does
         --  something similar but tries to limit the files that are reset, so
         --  the calls below will just speed up the processing in
         --  Recompute_View when a new project is loaded.

         --  Note that gnatls_server is set when computing predefined paths
         if Kernel.Gnatls_Server = null
           or else Kernel.Gnatls_Server.all /= Get_Nickname (Build_Server)
           or else not Same_Project
         then
            Entities.Reset (Get_Database (Kernel));
         end if;

         --  Always call Compute_Predefined_Paths who detects if recomputation
         --  is really needed. This is also used to get the value of
         --  ADA_PROJECT_PATH. and the default search path.
         --  If we are running locally, do not use the cache, and recompute the
         --  output of gnatls, so that users can possibly change the
         --  environment variables like ADA_PROJECT_PATH before reloading the
         --  project (FB07-010)

         Trace (Me, "Recompute predefined paths -- Local builder server ? "
                & Boolean'Image (Is_Local (Build_Server)));
         Compute_Predefined_Paths
           (Kernel, Use_Cache => not Is_Local (Build_Server));

         Remove_Location_Category (Kernel, Location_Category);
         Load (Registry           => Kernel.Registry.all,
               Root_Project_Path  => Local_Project,
               Errors             => Report_Error'Unrestricted_Access,
               New_Project_Loaded => New_Project_Loaded,
               Status             => Load_Status);

         if not Load_Status
           and then not Is_Default
         then
            --  Check if a remote configuration was applied and failure occured
            if not Is_Local (Build_Server) then
               Report_Error
                 (-"Error while loading project '" &
                  Full_Name (Local_Project, True).all &
                  (-"'. Trying with the build server set to (local)...") &
                  ASCII.LF);

               --  Reset the predefined paths cached, in case they are faulty
               Trace (Me, "Invalidate predefined paths cache");
               Invalidate_Predefined_Paths_Cache
                 (Kernel, Get_Nickname (Build_Server));

               --  Reset the build server
               Trace (Me, "Reset the build server");
               Assign (Kernel_Handle (Kernel),
                       Build_Server,
                       "",
                       Local_Project,
                       Reload_Prj => False);
               Trace (Me, "Recompute predefined paths");
               Compute_Predefined_Paths (Kernel);
               Trace (Me, "Load the project locally");
               Load (Registry           => Kernel.Registry.all,
                     Root_Project_Path  => Local_Project,
                     Errors             => Report_Error'Unrestricted_Access,
                     New_Project_Loaded => New_Project_Loaded,
                     Status             => Load_Status);

            elsif Previous_Project /= GNATCOLL.VFS.No_File then
               Report_Error (-"Couldn't parse the project "
                             & Full_Name (Local_Project).all
                             & ASCII.LF & (-"Reverting to previous project ")
                             & Full_Name (Previous_Project).all & ASCII.LF);
               Data.File := Previous_Project;
               Run_Hook (Kernel, Project_Changing_Hook, Data'Unchecked_Access);
               Compute_Predefined_Paths (Kernel);
               Load (Registry           => Kernel.Registry.all,
                     Root_Project_Path  => Previous_Project,
                     Errors             => Report_Error'Unrestricted_Access,
                     New_Project_Loaded => New_Project_Loaded,
                     Status             => Load_Status);

            else
               if Empty_Project_On_Failure then
                  Report_Error
                    (-"Error while loading project '" &
                     Full_Name (Local_Project, True).all &
                     (-"'. Loading the empty project.") & ASCII.LF);
                  Load_Empty_Project (Kernel);

               else
                  Report_Error
                    (-"Error while loading project '" &
                     Full_Name (Local_Project, True).all &
                     (-"'. Loading the default project.") & ASCII.LF);
                  Load_Default_Project
                    (Kernel, Dir (Local_Project), Clear => False);
               end if;

               Pop_State (Kernel_Handle (Kernel));
               return;
            end if;

         elsif Is_Default then
            --  Successful load of default project
            Set_Status (Get_Project (Kernel), Projects.Default);
         end if;

         if not New_Project_Loaded then
            Had_Project_Desktop := Load_Desktop (Kernel);
            Pop_State (Kernel_Handle (Kernel));
            return;
         end if;

         Run_Hook (Kernel, Project_Changed_Hook);
         Recompute_View (Kernel);

         --  Reload the desktop, in case there is a project-specific setup
         --  already
         if not Same_Project then
            Had_Project_Desktop := Load_Desktop (Kernel);
         end if;

         Pop_State (Kernel_Handle (Kernel));

      elsif not Same_Project then
         Console.Insert (Kernel, (-"Cannot find project file ")
                         & Full_Name (Project).all,
                         Mode => Console.Error, Add_LF => False);
         Had_Project_Desktop := Load_Desktop (Kernel);
      end if;
   end Load_Project;

   -----------------
   -- Get_Project --
   -----------------

   function Get_Project (Handle : access Kernel_Handle_Record'Class)
      return Project_Type is
   begin
      return Get_Root_Project (Handle.Registry.all);
   end Get_Project;

   --------------------
   -- Recompute_View --
   --------------------

   procedure Recompute_View (Handle : access Kernel_Handle_Record'Class) is
      procedure Report_Error (S : String);
      --  Handler called when the project parser finds an error.

      procedure Reset_File_If_External (S : in out Entities.Source_File);
      --  Reset the xref info for a source file that no longer belongs to the
      --  project

      ------------------
      -- Report_Error --
      ------------------

      procedure Report_Error (S : String) is
      begin
         --  No need to send to the result view, since these error messages do
         --  not contain line numbers.
         Console.Insert (Handle, S, Mode => Console.Error);
      end Report_Error;

      ----------------------------
      -- Reset_File_If_External --
      ----------------------------

      procedure Reset_File_If_External (S : in out Entities.Source_File) is
      begin
         if Get_Project_From_File
           (Handle.Registry.all,
            Source_Filename   => Entities.Get_Filename (S),
            Root_If_Not_Found => False) = No_Project
         then
            Entities.Reset (S);
         end if;
      end Reset_File_If_External;

   begin
      Push_State (Kernel_Handle (Handle), Busy);
      Compute_Predefined_Paths (Handle);
      Recompute_View (Handle.Registry.all, Report_Error'Unrestricted_Access);

      --  The list of source or ALI files might have changed, so we need to
      --  reset the cache containing LI information, otherwise this cache might
      --  contain dangling references to projects that have been freed. We used
      --  to do this only when loading a new project, but in fact that is not
      --  sufficient: when we look up xref info for a source file, if we
      --  haven't reset the cache we might get a reply pointing to a source
      --  file in a directory that is no longer part of the project in the new
      --  scenario.
      --
      --  In fact, we only reset the info for those source files that are no
      --  longer part of the project. This might take longer than dropping the
      --  whole database since in the former case we need to properly handle
      --  refcounting whereas Reset takes a shortcut. It is still probably
      --  cleaner to only reset what's needed.

      Entities.Foreach_Source_File
        (Get_Database (Handle), Reset_File_If_External'Access);

      --  Entities.Reset (Get_Database (Handle));

      Run_Hook (Handle, Project_View_Changed_Hook);
      Pop_State (Kernel_Handle (Handle));
   end Recompute_View;

   ---------------------------------
   -- Scenario_Variables_Cmd_Line --
   ---------------------------------

   function Scenario_Variables_Cmd_Line
     (Handle : access Kernel_Handle_Record'Class;
      Prefix : String) return String
   is
      Scenario_Vars : constant Scenario_Variable_Array :=
        Scenario_Variables (Handle);

      function Concat
        (Current : String; Index : Natural; Set_Var : String) return String;
      --  Concat the command line line for the Index-nth variable and the
      --  following ones to Current, and return the result.

      ------------
      -- Concat --
      ------------

      function Concat
        (Current : String; Index : Natural; Set_Var : String) return String is
      begin
         if Index > Scenario_Vars'Last then
            return Current;
         end if;

         return Concat
           (Current
            & Set_Var & External_Reference_Of (Scenario_Vars (Index))
            & "=" & Value_Of (Scenario_Vars (Index)) & " ",
            Index + 1,
            Set_Var);
      end Concat;

   begin
      --  A recursive function is probably not the most efficient way, but this
      --  prevents limits on the command line lengths. This also avoids the use
      --  of unbounded strings.
      return Concat ("", Scenario_Vars'First, Prefix);
   end Scenario_Variables_Cmd_Line;

   ------------------------
   -- Scenario_Variables --
   ------------------------

   function Scenario_Variables (Kernel : access Kernel_Handle_Record'Class)
      return Scenario_Variable_Array is
   begin
      return Scenario_Variables (Kernel.Registry.all);
   end Scenario_Variables;

   ------------------
   -- Save_Project --
   ------------------

   function Save_Project
     (Kernel    : access Kernel_Handle_Record'Class;
      Project   : Project_Type;
      Recursive : Boolean := False) return Boolean
   is
      Iter     : Imported_Project_Iterator := Start (Project, Recursive);
      Modified : Boolean := False;
      Result   : Boolean := True;
   begin
      while Current (Iter) /= No_Project loop
         Modified := Modified or else Project_Modified (Current (Iter));
         Result := Save_Single_Project (Kernel, Current (Iter)) and Result;

         Next (Iter);
      end loop;

      --  Force a change in the icons in the explorer.
      --  ??? Probably not very efficient, however.

      if Modified then
         Run_Hook (Kernel, Project_View_Changed_Hook);
      end if;

      return Result;
   end Save_Project;

   -------------------------
   -- Save_Single_Project --
   -------------------------

   function Save_Single_Project
     (Kernel  : access Kernel_Handle_Record'Class;
      Project : Projects.Project_Type) return Boolean
   is
      Result : Boolean := True;

      procedure Report_Error (Msg : String);
      --  Report errors to the user

      ------------------
      -- Report_Error --
      ------------------

      procedure Report_Error (Msg : String) is
      begin
         Insert (Kernel, Msg, Mode => GPS.Kernel.Console.Error);
         Parse_File_Locations (Kernel, Msg, "Project save");
         Result := False;
      end Report_Error;

      Data : aliased Project_Hooks_Args :=
        (Hooks_Data with Project => Project);
   begin
      if Save_Project
        (Project, Report_Error => Report_Error'Unrestricted_Access)
      then
         Run_Hook (Kernel, Project_Saved_Hook, Data'Access);
      end if;

      return Result;
   end Save_Single_Project;

   ------------------
   -- Get_Registry --
   ------------------

   function Get_Registry
     (Handle : access Kernel_Handle_Record'Class)
      return Projects.Registry.Project_Registry_Access is
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
      use type Prj.Variable_Value;
      Value : Prj.Variable_Value;
      Is_Default : Boolean;
   begin
      Get_Switches
        (Project,
         To_Lower (Tool.Project_Package.all),
         File,
         To_Lower (Tool.Project_Index.all), Value, Is_Default);

      --  If no value was found, we might have to return the initial value
      if Value = Prj.Nil_Variable_Value and then Use_Initial_Value then
         if Tool.Initial_Cmd_Line /= null then
            declare
               procedure Unchecked_Free is new Ada.Unchecked_Deallocation
                 (Argument_List, Argument_List_Access);
               Cmd : Argument_List_Access :=
                 Argument_String_To_List (Tool.Initial_Cmd_Line.all);
               Cmd2 : constant Argument_List := Cmd.all;
            begin
               Unchecked_Free (Cmd);
               return Cmd2;
            end;
         else
            return (1 .. 0 => null);
         end if;
      else
         return To_Argument_List (Get_Tree (Project), Value);
      end if;
   end Get_Switches;

end GPS.Kernel.Project;
