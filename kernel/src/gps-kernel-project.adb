-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2006                       --
--                              AdaCore                              --
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

with GNAT.OS_Lib;               use GNAT.OS_Lib;
with GNAT.Expect;               use GNAT.Expect;
pragma Warnings (Off);
with GNAT.Expect.TTY.Remote;    use GNAT.Expect.TTY.Remote;
pragma Warnings (On);
with Ada.Unchecked_Deallocation;

with Projects;                  use Projects;
with Projects.Editor;           use Projects.Editor;
with Projects.Registry;         use Projects.Registry;
with Basic_Types;
with Entities;
with Prj;
with Remote_Servers;            use Remote_Servers;
with Traces;                    use Traces;
with VFS;                       use VFS;

with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel.Console;        use GPS.Kernel.Console;
with GPS.Location_View;         use GPS.Location_View;
with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;
with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;
with GPS.Kernel.Remote;         use GPS.Kernel.Remote;
with GPS.Kernel.Standard_Hooks; use GPS.Kernel.Standard_Hooks;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;

package body GPS.Kernel.Project is

   Me : constant Debug_Handle := Create ("GPS.Kernel.Project");

   Location_Category : constant String := "Project";
   --  Category uses in the Location window for errors related to loading the
   --  project file

   procedure Compute_Predefined_Paths
     (Handle : access Kernel_Handle_Record'Class);
   --  Compute the predefined source and object paths, given the current
   --  project view associated with Handle.

   type Registry_Error_Handler_Record is new Error_Handler_Record with record
      Handle : Kernel_Handle;
   end record;

   procedure Report
     (Handler : access Registry_Error_Handler_Record; Msg : String);
   --  Used to report an error to the user.

   ------------
   -- Report --
   ------------

   procedure Report
     (Handler : access Registry_Error_Handler_Record; Msg : String) is
   begin
      Insert (Handler.Handle, Msg, Mode => Error);
   end Report;

   ------------------------------
   -- Compute_Predefined_Paths --
   ------------------------------

   procedure Compute_Predefined_Paths
     (Handle : access Kernel_Handle_Record'Class)
   is
      Gnatls        : constant String := Get_Attribute_Value
        (Get_Project (Handle), Gnatlist_Attribute, Default => "gnatls");
      Gnatls_Args   : Argument_List_Access :=
                        Argument_String_To_List (Gnatls & " -v");
      Langs         : Argument_List := Get_Languages (Get_Project (Handle));
      Error_Handler : aliased Registry_Error_Handler_Record;

   begin
      if Basic_Types.Contains (Langs, "ada", Case_Sensitive => False) then
         --  If the gnatls commands hasn't changed, no need to recompute the
         --  predefined paths.

         if Handle.Gnatls_Cache /= null
           and then Handle.Gnatls_Cache.all = Gnatls
           and then Handle.Gnatls_Server.all = Get_Nickname (Build_Server)
         then
            return;
         end if;

         Free (Handle.Gnatls_Cache);
         Handle.Gnatls_Cache := new String'(Gnatls);
         Free (Handle.Gnatls_Server);
         Handle.Gnatls_Server := new String'(Get_Nickname (Build_Server));

         Error_Handler.Handle := Kernel_Handle (Handle);
         --  ??? cache this information if Build_Server is not the local
         --  machine, unless a recompute project is performed
         Projects.Registry.Compute_Predefined_Paths
           (Handle.Registry.all,
            Handle.GNAT_Version,
            Gnatls_Args,
            Error_Handler'Unchecked_Access);
         Free (Gnatls_Args);
      end if;

      Basic_Types.Free (Langs);
   end Compute_Predefined_Paths;

   --------------------------
   -- Load_Default_Project --
   --------------------------

   procedure Load_Default_Project
     (Kernel               : access Kernel_Handle_Record'Class;
      Directory            : VFS.Virtual_File;
      Load_Default_Desktop : Boolean := True)
   is
      Project             : Virtual_File :=
                              Create_From_Dir (Directory, "default.gpr");
      Share_Dir           : constant String :=
                              Get_System_Dir (Kernel) & "share/gps/";
      Default             : constant String := Share_Dir & "default.gpr";
      Readonly            : constant String := Share_Dir & "readonly.gpr";
      Found               : Boolean;
      Is_Readonly         : Boolean := False;
      Had_Project_Desktop : Boolean;
      pragma Unreferenced (Had_Project_Desktop);

   begin
      --  Save all open children, and close everything. A new desktop will be
      --  open in the end anway

      if not Save_MDI_Children (Kernel, Force => False) then
         return;
      end if;

      Push_State (Kernel_Handle (Kernel), Busy);

      if Is_Regular_File (Project) then
         Found := True;
      elsif Is_Writable (Directory) and then Is_Regular_File (Default) then
         Copy_File (Default, Full_Name (Project).all, Found);
      elsif Is_Regular_File (Readonly) then
         Project := VFS.Create (Readonly);
         Found := True;
         Is_Readonly := True;
      else
         Found := False;
      end if;

      if Found then
         Load_Project (Kernel, Project);

         if not Is_Readonly then
            Set_Status (Get_Project (Kernel), Projects.Default);
         end if;
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
         Old_Pref : constant Boolean := Get_Pref (Auto_Jump_To_First);
      begin
         --  Temporarily unset this, to handle the following case:
         --  the project file contains errors and couldn't be loaded, but it
         --  was also saved in the desktop. If that is the case, the project
         --  would be open several times otherwise
         Set_Pref (Kernel, Auto_Jump_To_First, False);
         Console.Insert (Kernel, S, Mode => Console.Error, Add_LF => False);
         Parse_File_Locations (Kernel, S, Location_Category);
         Set_Pref (Kernel, Auto_Jump_To_First, Old_Pref);
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
     (Kernel  : access Kernel_Handle_Record'class;
      Project : VFS.Virtual_File;
      No_Save : Boolean := False)
   is
      procedure Report_Error (S : String);
      --  Output error messages from the project parser to the console.

      ------------------
      -- Report_Error --
      ------------------

      procedure Report_Error (S : String) is
         Old_Pref : constant Boolean := Get_Pref (Auto_Jump_To_First);
      begin
         --  Temporarily unset this, to handle the following case:
         --  the project file contains errors and couldn't be loaded, but it
         --  was also saved in the desktop. If that is the case, the project
         --  would be open several times otherwise

         Set_Pref (Kernel, Auto_Jump_To_First, False);
         Console.Insert (Kernel, S, Mode => Console.Error, Add_LF => False);
         Parse_File_Locations (Kernel, S, Location_Category);
         Set_Pref (Kernel, Auto_Jump_To_First, Old_Pref);
      end Report_Error;

      Had_Project_Desktop : Boolean;
      New_Project_Loaded  : Boolean;
      pragma Unreferenced (Had_Project_Desktop);
      Data                : aliased File_Hooks_Args;

      Root_Project : constant Virtual_File :=
                       Project_Path (Get_Root_Project (Kernel.Registry.all));
      Same_Project : constant Boolean := Project = Root_Project;
      Local_Project : VFS.Virtual_File;

   begin
      --  Unless we are reloading the same project

      if not Same_Project then
         if not No_Save
           and then not Save_MDI_Children (Kernel, Force => False)
         then
            return;
         end if;

         --  Never save automatically the desktop for the default project

         if Status (Get_Root_Project (Kernel.Registry.all)) = From_File
           and then Get_Pref (Save_Desktop_On_Exit)
         then
            Save_Desktop (Kernel);
         end if;

         --  Close all children before loading the new projet, in case a new
         --  editor needs to be loaded to display error messages

         Close_All_Children (Kernel);
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
                  Mode => Console.Error, Add_Lf => False);

               --  Need to run Project_Changing hook to reset build_server
               Data.File := Root_Project;
               Run_Hook (Kernel, Project_Changing_Hook, Data'Unchecked_Access);

               Pop_State (Kernel_Handle (Kernel));
               return;
            end if;

         else
            Local_Project := Project;
         end if;

         Change_Dir (Dir (Local_Project));

         --  When loading a new project, we need to reset the cache containing
         --  LI information, otherwise this cache might contain dangling
         --  references to projects that have been freed.

         if not Same_Project then
            Entities.Reset (Get_Database (Kernel));
         end if;

         if Get_Predefined_Source_Path (Kernel.Registry.all) = ""
           or else not Is_Local (Build_Server)
         then
            Trace (Me, "Recompute predefined paths");
            Compute_Predefined_Paths (Kernel);
         end if;

         Remove_Location_Category (Kernel, Location_Category);
         Load (Registry           => Kernel.Registry.all,
               Root_Project_Path  => Local_Project,
               Errors             => Report_Error'Unrestricted_Access,
               New_Project_Loaded => New_Project_Loaded);

         if not New_Project_Loaded then
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
                         Mode => Console.Error, Add_Lf => False);
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

      ------------------
      -- Report_Error --
      ------------------

      procedure Report_Error (S : String) is
      begin
         --  No need to send to the result view, since these error messages do
         --  not contain line numbers.
         Console.Insert (Handle, S, Mode => Console.Error);
      end Report_Error;

   begin
      Push_State (Kernel_Handle (Handle), Busy);
      Recompute_View (Handle.Registry.all, Report_Error'Unrestricted_Access);
      Compute_Predefined_Paths (Handle);
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
      if Save_Project (Project, Report_Error'Unrestricted_Access) then
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
     (Handle            : access Kernel_Handle_Record'Class;
      Project           : Project_Type;
      In_Pkg            : String;
      File              : VFS.Virtual_File := VFS.No_File;
      Index             : String;
      Use_Initial_Value : Boolean := False) return GNAT.OS_Lib.Argument_List
   is
      use type Prj.Variable_Value;
      Value : Prj.Variable_Value;
      Is_Default : Boolean;
   begin
      Get_Switches
        (Project, In_Pkg, File, Get_String (Index), Value, Is_Default);

      --  If no value was found, we might have to return the initial value
      if Value = Prj.Nil_Variable_Value and then Use_Initial_Value then
         declare
            Tool_Name : constant String := Get_Tool_Name
              (Handle,
               Pkg_Name  => In_Pkg,
               Attribute => "default_switches",
               Index     => Index);
            Prop : Tool_Properties_Record;
         begin
            if Tool_Name /= "" then
               Prop := Get_Tool_Properties (Handle, Tool_Name);

               if Prop.Initial_Cmd_Line /= null then
                  declare
                     procedure Unchecked_Free is new Ada.Unchecked_Deallocation
                       (Argument_List, Argument_List_Access);
                     Cmd : Argument_List_Access :=
                       Argument_String_To_List (Prop.Initial_Cmd_Line.all);
                     Cmd2 : constant Argument_List := Cmd.all;
                  begin
                     Unchecked_Free (Cmd);
                     return Cmd2;
                  end;
               else
                  return (1 .. 0 => null);
               end if;
            else
               return (1 .. 0 => null);
            end if;
         end;
      else
         return To_Argument_List (Get_Tree (Project), Value);
      end if;
   end Get_Switches;

end GPS.Kernel.Project;
