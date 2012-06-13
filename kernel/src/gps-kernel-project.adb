------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2012, AdaCore                     --
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
with GNAT.Strings;
with GNATCOLL.Projects;                use GNATCOLL.Projects;
with GNATCOLL.Utils;                   use GNATCOLL.Utils;
with GNAT.OS_Lib;                      use GNAT.OS_Lib;
pragma Warnings (Off);
with GNAT.Expect.TTY.Remote;           use GNAT.Expect.TTY.Remote;
pragma Warnings (On);
with Ada.Unchecked_Deallocation;

with Projects;                         use Projects;
with Basic_Types;
with Entities;
with XML_Utils;                        use XML_Utils;
with Remote;                           use Remote;
with Traces;                           use Traces;
with GNATCOLL.VFS;                     use GNATCOLL.VFS;
with Prj;
with Types;                            use Types;

with Gtk.Icon_Factory;  use Gtk.Icon_Factory;

with GPS.Intl;                         use GPS.Intl;
with GPS.Properties;                   use GPS.Properties;
with GPS.Kernel.Console;               use GPS.Kernel.Console;
with GPS.Kernel.Hooks;                 use GPS.Kernel.Hooks;
with GPS.Kernel.Messages;              use GPS.Kernel.Messages;
with GPS.Kernel.Messages.Tools_Output; use GPS.Kernel.Messages.Tools_Output;
with GPS.Kernel.Preferences;           use GPS.Kernel.Preferences;
with GPS.Kernel.Properties;            use GPS.Kernel.Properties;
with GPS.Kernel.Remote;                use GPS.Kernel.Remote;
with GPS.Kernel.Standard_Hooks;        use GPS.Kernel.Standard_Hooks;
with GPS.Kernel.MDI;                   use GPS.Kernel.MDI;
with GPS.Kernel.Xref;

package body GPS.Kernel.Project is

   Me : constant Debug_Handle := Create ("GPS.Kernel.Project");

   Location_Category : constant String := "Project";
   --  Category uses in the Location window for errors related to loading the
   --  project file
   Location_Message_Flags : constant Message_Flags :=
     (Editor_Side => True,
      Locations   => True);

   procedure Compute_Predefined_Paths
     (Handle : access Kernel_Handle_Record'Class;
      Use_Cache : Boolean := True);
   --  Compute the predefined source and object paths, given the current
   --  project view associated with Handle.

   procedure On_Build_Server_Connection
     (Handle : access Kernel_Handle_Record'Class);
   --  Recompute the predefined source and object paths upon build server
   --  connection, when this information was previously retrieved from cache.

   type GPS_Project_Tree is new Project_Tree with record
      Handle : Kernel_Handle;
   end record;

   overriding function Data_Factory
     (Self : GPS_Project_Tree) return Project_Data_Access;
   overriding procedure Recompute_View
     (Self   : in out GPS_Project_Tree;
      Errors : Error_Report := null);
   --  See inherited documentation

   procedure Do_Subdirs_Cleanup (Tree : Project_Tree'Class);
   --  Cleanup empty subdirs created when opening a project with prj.subdirs
   --  set.

   ------------------------
   -- Do_Subdirs_Cleanup --
   ------------------------

   procedure Do_Subdirs_Cleanup (Tree : Project_Tree'Class) is
   begin
      --  Nothing to do if Prj.Subdirs is not set
      if Prj.Subdirs = null then
         return;
      end if;

      declare
         Objs    : constant File_Array :=
           Root_Project (Tree).Object_Path (Recursive => True);
         Success : Boolean;
      begin
         for J in Objs'Range loop
            declare
               Dir : Virtual_File renames Objs (J);
            begin
               if Dir.Is_Directory then
                  --  Remove emtpy directories (this call won't remove the dir
                  --  if files or subdirectories are in it.
                  Dir.Remove_Dir (Success => Success);
               end if;
            end;
         end loop;
      end;
   end Do_Subdirs_Cleanup;

   ---------------------
   -- Create_Registry --
   ---------------------

   procedure Create_Registry (Handle : access Kernel_Handle_Record'Class) is
      Tree : constant Project_Tree_Access := new GPS_Project_Tree;
   begin
      GPS_Project_Tree (Tree.all).Handle := Kernel_Handle (Handle);
      Handle.Registry := Projects.Create (Tree => Tree);
      Tree.Load_Empty_Project (Env => Handle.Registry.Environment);
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
      procedure Reset_File_If_External (S : in out Entities.Source_File);
      --  Reset the xref info for a source file that no longer belongs to the
      --  project.

      ----------------------------
      -- Reset_File_If_External --
      ----------------------------

      procedure Reset_File_If_External (S : in out Entities.Source_File) is
         Info : constant File_Info := Self.Info (Entities.Get_Filename (S));
      begin
         if Info.Project = No_Project then
            Entities.Reset (S);
         end if;
      end Reset_File_If_External;

   begin
      Push_State (Self.Handle, Busy);

      Do_Subdirs_Cleanup (Self);
      Recompute_View (Project_Tree (Self), Errors);

      Compute_Predefined_Paths (Self.Handle);

      --  If we are in the process of creating the kernel, no need to do
      --  anything else here
      --  ??? It would be nice to rely on a better indicator than this
      if Get_Icon_Factory (Self.Handle) = null then
         Pop_State (Self.Handle);
         return;
      end if;

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
        (Get_Database (Self.Handle), Reset_File_If_External'Access);

      Run_Hook (Self.Handle, Project_View_Changed_Hook);

      --  The view has changed: recompute the cross-reference database

      if Active (Entities.SQLITE) then
         if Self.Handle.Xref_Db = null then
            Self.Handle.Xref_Db := new
              GPS.Kernel.Xref.GPS_Xref_Database;
         else
            Self.Handle.Xref_Db.Free;
         end if;

         GPS.Kernel.Xref.Setup
           (GPS.Kernel.Xref.GPS_Xref_Database_Access
              (Self.Handle.Xref_Db), Self.Handle);
      end if;

      Pop_State (Self.Handle);
   end Recompute_View;

   --------------------
   -- Recompute_View --
   --------------------

   procedure Recompute_View (Handle : access Kernel_Handle_Record'Class) is
      procedure Report_Error (S : String);
      procedure Report_Error (S : String) is
      begin
         Console.Insert (Handle, S, Mode => Console.Error, Add_LF => False);
         Parse_File_Locations (Handle, S, Location_Category);
      end Report_Error;

   begin
      Get_Registry (Handle).Tree.Recompute_View
        (Errors => Report_Error'Unrestricted_Access);
   end Recompute_View;

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

   type Predefined_Paths_Property is new GPS.Properties.Property_Record
   with record
      Source_Path  : File_Array_Access;
      Object_Path  : File_Array_Access;
      Project_Path : File_Array_Access;
   end record;

   overriding procedure Save
     (Property : access Predefined_Paths_Property;
      Node     : in out XML_Utils.Node_Ptr);
   --  See inherited procedure

   overriding procedure Load
     (Property : in out Predefined_Paths_Property;
      From     : XML_Utils.Node_Ptr);
   --  See inherited procedure

   function Tag_Name (Idx : Natural) return XML_Utils.UTF8_String;
   --  Return the tag name for path index idx

   --------------
   -- Tag_Name --
   --------------

   function Tag_Name (Idx : Natural) return XML_Utils.UTF8_String is
      Str : constant String := Idx'Img;
   begin
      return "path" & Str (Str'First + 1 .. Str'Last);
   end Tag_Name;

   ----------
   -- Save --
   ----------

   overriding procedure Save
     (Property : access Predefined_Paths_Property;
      Node     : in out XML_Utils.Node_Ptr)
   is
      Child : XML_Utils.Node_Ptr;
   begin
      Child := new XML_Utils.Node;
      Child.Tag := new String'("source_path");
      XML_Utils.Set_Attribute
        (Child, "nb_paths", Property.Source_Path'Length'Img);

      for J in Property.Source_Path'Range loop
         XML_Utils.Add_File_Child
           (Child, Tag_Name (J), Property.Source_Path (J));
      end loop;

      Add_Child (Node, Child);

      Child := new XML_Utils.Node;
      Child.Tag := new String'("object_path");
      XML_Utils.Set_Attribute
        (Child, "nb_paths", Property.Object_Path'Length'Img);

      for J in Property.Object_Path'Range loop
         XML_Utils.Add_File_Child
           (Child, Tag_Name (J), Property.Object_Path (J));
      end loop;

      Add_Child (Node, Child);

      Child := new XML_Utils.Node;
      Child.Tag := new String'("project_path");
      XML_Utils.Set_Attribute
        (Child, "nb_paths", Property.Project_Path'Length'Img);

      for J in Property.Project_Path'Range loop
         XML_Utils.Add_File_Child
           (Child, Tag_Name (J), Property.Project_Path (J));
      end loop;

      Add_Child (Node, Child);

   exception
      when E : others => Trace (Exception_Handle, E);
   end Save;

   ----------
   -- Load --
   ----------

   overriding procedure Load
     (Property : in out Predefined_Paths_Property;
      From     : XML_Utils.Node_Ptr)
   is
      Child : XML_Utils.Node_Ptr;
   begin
      Child := Find_Tag (From.Child, "source_path");
      Property.Source_Path := new File_Array
        (1 .. Natural'Value (Get_Attribute (Child, "nb_paths", "0")));

      for J in Property.Source_Path'Range loop
         Property.Source_Path (J) :=
           XML_Utils.Get_File_Child (Child, Tag_Name (J));
      end loop;

      Child := Find_Tag (From.Child, "object_path");
      Property.Source_Path := new File_Array
        (1 .. Natural'Value (Get_Attribute (Child, "nb_paths", "0")));

      for J in Property.Object_Path'Range loop
         Property.Object_Path (J) :=
           XML_Utils.Get_File_Child (Child, Tag_Name (J));
      end loop;

      Child := Find_Tag (From.Child, "project_path");
      Property.Project_Path := new File_Array
        (1 .. Natural'Value (Get_Attribute (Child, "nb_paths", "0")));

      for J in Property.Project_Path'Range loop
         Property.Project_Path (J) :=
           XML_Utils.Get_File_Child (Child, Tag_Name (J));
      end loop;
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
      procedure Report_Error (Msg : String);
      --  Report an error that occurred while parsing gnatls output

      procedure Report_Error (Msg : String) is
      begin
         Insert (Handle, Msg, Mode => Info);
      end Report_Error;

      Gnatls         : constant String :=
                         Get_Project (Handle).Attribute_Value
                           (Gnatlist_Attribute, Default => "gnatls");
      Gnatls_Args    : Argument_List_Access :=
                         Argument_String_To_List (Gnatls & " -v");
      Langs          : Argument_List := Get_Project (Handle).Languages;
      Property       : Predefined_Paths_Property;
      Prop_Access    : Property_Access;
      Property_Index : Property_Index_Type := No_Index;
      Success        : Boolean;

   begin
      --  If we never computed the predefined paths before, we always do it at
      --  least once, since this is needed to find the predefined projects.
      --  Otherwise we only do it if Ada is a supported language.

      if Handle.Gnatls_Cache = null
        or else Basic_Types.Contains (Langs, "ada", Case_Sensitive => False)
      then
         --  If the gnatls commands hasn't changed, no need to recompute the
         --  predefined paths.

         if Use_Cache
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
                         (+To_Path (Property.Source_Path.all)));
                  Trace (Me, "set object path from cache to " &
                         (+To_Path (Property.Object_Path.all)));
                  Trace (Me, "set project path from cache to " &
                         (+To_Path (Property.Project_Path.all)));
               end if;

               Handle.Registry.Environment.Set_Predefined_Source_Path
                 (Property.Source_Path.all);
               Handle.Registry.Environment.Set_Predefined_Object_Path
                 (Property.Object_Path.all);
               Handle.Registry.Environment.Set_Predefined_Project_Path
                 (Property.Project_Path.all);
               Add_Hook
                 (Handle, Build_Server_Connected_Hook,
                  Wrapper (On_Build_Server_Connection'Access),
                  "compute_predefined_path");

               return;

            end if;
         end if;

         Free (Handle.GNAT_Version);
         Projects.Compute_Predefined_Paths
           (Handle.Registry,
            Handle.GNAT_Version,
            Gnatls_Args,
            Report_Error'Unrestricted_Access);

         if Property_Index /= No_Index then
            Property.Source_Path := new File_Array'
              (Handle.Registry.Environment.Predefined_Source_Path);
            Property.Object_Path := new File_Array'
              (Handle.Registry.Environment.Predefined_Object_Path);
            Property.Project_Path := new File_Array'
              (Handle.Registry.Environment.Predefined_Project_Path);
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
      Share_Dir           : constant Virtual_File :=
                              Create_From_Dir
                                (Get_System_Dir (Kernel), "share/gps/");
      Default             : constant Virtual_File :=
                              Create_From_Dir (Share_Dir, "default.gpr");
      Readonly            : constant Virtual_File :=
                              Create_From_Dir (Share_Dir, "readonly.gpr");
      Found               : Boolean;
      Is_Default          : Boolean := False;

      Ignore : Boolean;
      pragma Unreferenced (Ignore);

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

      Pop_State (Kernel_Handle (Kernel));
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
      Close_All_Children (Kernel);
      Ignore := Load_Desktop (Kernel);

      Entities.Reset (Get_Database (Kernel));

      Kernel.Registry.Tree.Load_Empty_Project
        (Kernel.Registry.Environment, Recompute_View => False);

      Run_Hook (Kernel, Project_Changed_Hook);
      Recompute_View (Kernel);
   end Load_Empty_Project;

   ------------------------------
   -- Reload_Project_If_Needed --
   ------------------------------

   procedure Reload_Project_If_Needed
     (Kernel : access Kernel_Handle_Record'Class)
   is
      procedure Report_Error (S : String);
      --  Output error messages from the project parser to the console

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

      Reloaded : Boolean := False;
   begin
      Push_State (Kernel_Handle (Kernel), Busy);

      begin
         Kernel.Registry.Tree.Reload_If_Needed
           (Reloaded, Report_Error'Unrestricted_Access);
      exception
         when Invalid_Project =>
            Pop_State (Kernel_Handle (Kernel));
      end;

      if Reloaded then
         Run_Hook (Kernel, Project_Changed_Hook);
      end if;

      Pop_State (Kernel_Handle (Kernel));
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

      procedure Report_Error (S : String);
      --  Output error messages from the project parser to the console

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

      Ignore : Boolean;
      pragma Unreferenced (Ignore);
      New_Project_Loaded  : Boolean;
      Data                : aliased File_Hooks_Args;

      Same_Project     : Boolean;
      Local_Project    : GNATCOLL.VFS.Virtual_File;
      Previous_Project : Virtual_File;

   begin
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

            Local_Project := To_Local (Project);

            if not Is_Regular_File (Local_Project) then
               Console.Insert
                 (Kernel, (-"Cannot find remote project file ")
                  & Display_Full_Name (Project) & (-" at local place ")
                  & Display_Full_Name (Local_Project) &
                  (-". Please check your remote configuration."),
                  Mode => Console.Error, Add_LF => False);

               --  Need to run Project_Changing hook to reset build_server
               Data.File := Previous_Project;
               Run_Hook (Kernel, Project_Changing_Hook, Data'Unchecked_Access);

               Ignore := Load_Desktop (Kernel);
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

         Entities.Reset (Get_Database (Kernel));

         --  Reload the desktop, in case there is a project-specific setup
         --  already. We need to do this before doing the actual loading (in
         --  case errors result in the opening of the Locations window), and
         --  before running the hooks, in case some python script needs to open
         --  or refresh windows as a result.
         --  If we fail to load the project, we will reload another project
         --  anyway (corresponding to the default or empty project).

         if not Same_Project and not Keep_Desktop then
            Ignore := Load_Desktop
              (Kernel, For_Project => Local_Project);
         end if;

         --  Always call Compute_Predefined_Paths who detects if recomputation
         --  is really needed. This is also used to get the value of
         --  ADA_PROJECT_PATH. and the default search path.
         --  If we are running locally, do not use the cache, and recompute the
         --  output of gnatls, so that users can possibly change the
         --  environment variables like ADA_PROJECT_PATH before reloading the
         --  project (FB07-010)
         --  This call to Compute_Predefined_Paths is needed before even
         --  loading the project, in case it depends on some predefined
         --  projects. The loading of the project will call it a second time
         --  once we know the "gnat" attribute.

         Trace (Me, "Recompute predefined paths -- Local builder server ? "
                & Boolean'Image (Is_Local (Build_Server)));
         Compute_Predefined_Paths
           (Kernel, Use_Cache => not Is_Local (Build_Server));

         Get_Messages_Container (Kernel).Remove_Category
           (Location_Category, Location_Message_Flags);

         begin
            New_Project_Loaded := True;
            Kernel.Registry.Tree.Load
              (Root_Project_Path => Local_Project,
               Env               => Kernel.Registry.Environment,
               Errors            => Report_Error'Unrestricted_Access,
               Recompute_View    => False);

         exception
            when Invalid_Project =>
               New_Project_Loaded := False;
         end;

         if not New_Project_Loaded then
            --  Check if a remote configuration was applied and failure occured
            if not Is_Local (Build_Server) then
               Report_Error
                 (-"Error while loading project '" &
                  Display_Full_Name (Local_Project, True) &
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

               begin
                  Kernel.Registry.Tree.Load
                    (Root_Project_Path => Local_Project,
                     Errors            => Report_Error'Unrestricted_Access,
                     Env               => Kernel.Registry.Environment);
                  New_Project_Loaded := True;

               exception
                  when Invalid_Project => null;
               end;
            end if;

         elsif Is_Default then
            --  Successful load of default project
            Get_Registry (Kernel).Tree.Set_Status (Default);
         end if;

         if not New_Project_Loaded then
            Ignore := Load_Desktop (Kernel);
         end if;

         Run_Hook (Kernel, Project_Changed_Hook);

         Recompute_View (Kernel);

         Pop_State (Kernel_Handle (Kernel));

      elsif not Same_Project then
         Console.Insert (Kernel, (-"Cannot find project file ")
                         & Display_Full_Name (Project),
                         Mode => Console.Error, Add_LF => False);
         Ignore := Load_Desktop (Kernel);
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
            & Set_Var & External_Name (Scenario_Vars (Index))
            & "=" & Value (Scenario_Vars (Index))
            & " ",
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
      return Kernel.Registry.Tree.Scenario_Variables;
   end Scenario_Variables;

   ------------------
   -- Save_Project --
   ------------------

   function Save_Project
     (Kernel    : access Kernel_Handle_Record'Class;
      Project   : Project_Type;
      Recursive : Boolean := False) return Boolean
   is
      Iter     : Project_Iterator := Project.Start (Recursive);
      Modified : Boolean := False;
      Result   : Boolean := True;
   begin
      while Current (Iter) /= No_Project loop
         Modified := Modified or else Current (Iter).Modified;
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
         Insert (Kernel, Msg, Mode => GPS.Kernel.Console.Error);
         Parse_File_Locations (Kernel, Msg, "Project save");
         Result := False;
      end Report_Error;

      Data : aliased Project_Hooks_Args :=
               (Hooks_Data with Project => Project);
   begin
      if Project.Save (Errors => Report_Error'Unrestricted_Access) then
         Run_Hook (Kernel, Project_Saved_Hook, Data'Access);
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
        (To_Lower (Tool.Project_Package.all),
         File,
         To_Lower (Tool.Project_Index.all), Value, Is_Default);

      --  If no value was found, we might have to return the initial value
      if Value = null
        and then Use_Initial_Value
        and then Tool.Initial_Cmd_Line /= null
      then
         Value := Argument_String_To_List (Tool.Initial_Cmd_Line.all);
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

end GPS.Kernel.Project;
