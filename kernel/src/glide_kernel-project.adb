-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2004                       --
--                            ACT-Europe                             --
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
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.Case_Util;            use GNAT.Case_Util;

with Projects;           use Projects;
with Projects.Editor;    use Projects.Editor;
with Projects.Registry;  use Projects.Registry;
with Basic_Types;
with Prj;
with Types;                    use Types;
with Entities;

with Glide_Intl;               use Glide_Intl;
with Glide_Kernel.Console;     use Glide_Kernel.Console;
with Glide_Kernel.Timeout;     use Glide_Kernel.Timeout;
with Glide_Result_View;        use Glide_Result_View;
with Glide_Kernel.Hooks;       use Glide_Kernel.Hooks;

package body Glide_Kernel.Project is

   procedure Compute_Predefined_Paths
     (Handle : access Kernel_Handle_Record'Class);
   --  Compute the predefined source and object paths, given the current
   --  project view associated with Handle.

   ------------------------------
   -- Compute_Predefined_Paths --
   ------------------------------

   procedure Compute_Predefined_Paths
     (Handle : access Kernel_Handle_Record'Class)
   is
      Gnatls       : constant String := Get_Attribute_Value
        (Get_Project (Handle), Gnatlist_Attribute, Default => "gnatls");
      Gnatls_Args  : Argument_List_Access :=
                       Argument_String_To_List (Gnatls & " -v");
      Path         : String_Access;
      GNAT_Version : aliased String_Access;

   begin
      --  If the gnatls commands hasn't changed, no need to recompute the
      --  predefined paths.

      if Handle.Gnatls_Cache /= null
        and then Handle.Gnatls_Cache.all = Gnatls
      then
         return;
      end if;

      Free (Handle.Gnatls_Cache);
      Handle.Gnatls_Cache := new String'(Gnatls);

      --  ??? Should remove, when possible, the previous predefined project

      Path := Locate_Exec_On_Path (Gnatls_Args (1).all);

      if Path /= null then
         Compute_Predefined_Paths
           (Handle.Registry.all,
            Gnatls_Path  => Path.all,
            Gnatls_Args  => Gnatls_Args,
            GNAT_Version => GNAT_Version'Unchecked_Access);
         Handle.GNAT_Version := GNAT_Version;

         Free (Path);
         Free (Gnatls_Args);

      else
         Set_Predefined_Source_Path (Handle.Registry.all, "");
         Set_Predefined_Object_Path (Handle.Registry.all, "");
         Free (Gnatls_Args);
      end if;
   end Compute_Predefined_Paths;

   --------------------------
   -- Load_Default_Project --
   --------------------------

   procedure Load_Default_Project
     (Kernel               : access Kernel_Handle_Record'Class;
      Directory            : String;
      Load_Default_Desktop : Boolean := True)
   is
      Had_Project_Desktop : Boolean;
      pragma Unreferenced (Had_Project_Desktop);

      Iter : Tools_Htable.String_Hash_Table.Iterator;
      Prop : Tool_Properties_Record;
   begin
      --  Save all open children, and close everything. A new desktop will be
      --  open in the end anway

      if not Save_MDI_Children (Kernel, Force => False) then
         return;
      end if;

      Push_State (Kernel_Handle (Kernel), Busy);

      Load_Default_Project
        (Kernel.Registry.all,
         Normalize_Pathname (Directory, Resolve_Links => False));

      --  For all registered tool that contain default switches, set these up

      Tools_Htable.String_Hash_Table.Get_First (Kernel.Tools, Iter);
      loop
         Prop := Tools_Htable.String_Hash_Table.Get_Element (Iter);
         exit when Prop = No_Tool;

         if Prop.Initial_Cmd_Line /= null then
            declare
               Switches : Argument_List_Access :=
                 Argument_String_To_List (Prop.Initial_Cmd_Line.all);
               No_Scenario : constant Scenario_Variable_Array (1 .. 0) :=
                 (others => No_Variable);
            begin
               Update_Attribute_Value_In_Scenario
                 (Get_Project (Kernel),
                  Scenario_Variables => No_Scenario,
                  Attribute          => Build
                    (Prop.Project_Package.all, Prop.Project_Attribute.all),
                  Values             => Switches.all,
                  Attribute_Index    => Prop.Project_Index.all);
               Free (Switches);
            end;
         end if;

         Tools_Htable.String_Hash_Table.Get_Next (Kernel.Tools, Iter);
      end loop;

      Set_Project_Modified (Get_Project (Kernel), False);

      --  Compute the project

      Run_Hook (Kernel, Project_Changed_Hook);
      Recompute_View (Kernel);

      --  Reload the default desktop

      if Load_Default_Desktop then
         Close_All_Children (Kernel);
         Had_Project_Desktop := Load_Desktop (Kernel);
      end if;

      Pop_State (Kernel_Handle (Kernel));
   end Load_Default_Project;

   ------------------
   -- Load_Project --
   ------------------

   procedure Load_Project
     (Kernel : access Kernel_Handle_Record'class; Project : String)
   is
      procedure Report_Error (S : String);
      --  Output error messages from the project parser to the glide console.

      procedure Report_Error (S : String) is
      begin
         Console.Insert (Kernel, S, Mode => Console.Error, Add_LF => False);
         Parse_File_Locations (Kernel, S, "Project");
      end Report_Error;

      Had_Project_Desktop : Boolean;
      New_Project_Loaded : Boolean;
      pragma Unreferenced (Had_Project_Desktop);
      Same_Project : constant Boolean :=
        Project = Project_Path (Get_Root_Project (Kernel.Registry.all));

   begin
      --  Unless we are reloading the same project

      if not Same_Project then
         if not Save_MDI_Children (Kernel, Force => False) then
            return;
         end if;

         --  Close all children before loading the new projet, in case a new
         --  editor needs to be loaded to display error messages
         Close_All_Children (Kernel);
      end if;

      if Is_Regular_File (Project) then
         Push_State (Kernel_Handle (Kernel), Busy);
         Change_Dir (Dir_Name (Project));

         --  When loading a new project, we need to reset the cache containing
         --  LI information, otherwise this cache might contain dangling
         --  references to projects that have been freed.
         Entities.Reset (Get_Database (Kernel));
         Load (Registry           => Kernel.Registry.all,
               Root_Project_Path  => Project,
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
         Console.Insert (Kernel, (-"Cannot find project file ") & Project,
                         Mode => Console.Error, Add_Lf => False);
         Load_Default_Project (Kernel, Directory => Get_Current_Dir);
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

   procedure Save_Project
     (Kernel    : access Kernel_Handle_Record'Class;
      Project   : Project_Type;
      Recursive : Boolean := False)
   is
      Iter : Imported_Project_Iterator := Start (Project, Recursive);
      Modified : Boolean := False;
      Langs        : Argument_List := Get_Languages
        (Project, Recursive => True);
   begin
      while Current (Iter) /= No_Project loop
         Modified := Modified or else Project_Modified (Current (Iter));
         Save_Single_Project (Kernel, Current (Iter), Langs);

         Next (Iter);
      end loop;

      Basic_Types.Free (Langs);

      --  Force a change in the icons in the explorer.
      --  ??? Probably not very efficient, however.

      if Modified then
         Run_Hook (Kernel, Project_View_Changed_Hook);
      end if;
   end Save_Project;

   -------------------------
   -- Save_Single_Project --
   -------------------------

   procedure Save_Single_Project
     (Kernel    : access Kernel_Handle_Record'Class;
      Project   : Projects.Project_Type;
      Langs     : GNAT.OS_Lib.Argument_List)
   is
      procedure Report_Error (Msg : String);
      --  Report errors to the user

      ------------------
      -- Report_Error --
      ------------------

      procedure Report_Error (Msg : String) is
      begin
         Insert (Kernel, Msg, Mode => Glide_Kernel.Console.Error);
         Parse_File_Locations (Kernel, Msg, "Project save");
      end Report_Error;

      Args    : Argument_List (1 .. 2);
      Success : Boolean;
   begin
      --  A multi-language project ? If yes, we need to generate the Makefile

      if Langs'Length = 1 then
         To_Lower (Langs (Langs'First).all);
      end if;

      if Langs'Length > 1
        or else (Langs'Length = 1 and then Langs (Langs'First).all /= "ada")
      then
         if not Is_Regular_File (Project_Path (Project))
           or else Project_Modified (Project)
         then
            Save_Project (Project, Report_Error'Unrestricted_Access);
            Args (1) := new String'("-R");

            declare
               Name : constant String := Project_Path (Project);
            begin
               if not Is_Regular_File (Name)
                 or else Is_Writable_File (Name)
               then
                  --  call gpr2make -R Name

                  Free (Args (2));
                  Args (2) := new String'(Name);
                  Launch_Process
                    (Kernel_Handle (Kernel),
                     Command   => "gpr2make",
                     Arguments => Args,
                     Console   => Get_Console (Kernel),
                     Success   => Success);
               end if;
            end;

            Basic_Types.Free (Args);
         end if;

      else
         Save_Project (Project, Report_Error'Unrestricted_Access);
      end if;
   end Save_Single_Project;

   ------------------
   -- Get_Registry --
   ------------------

   function Get_Registry
     (Handle : access Kernel_Handle_Record'Class)
      return Projects.Registry.Project_Registry'Class is
   begin
      return Handle.Registry.all;
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
      L     : Prj.String_List_Id;
      Name  : Name_Id;
   begin
      Get_Switches
        (Project, In_Pkg, File, Get_String (Index), Value, Is_Default);

      --  If no value was found, we might have to return the initial value
      if Value = Prj.Nil_Variable_Value and then Use_Initial_Value then
         L := Prj.Nil_String;

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
                     Cmd : Argument_List_Access :=
                       Argument_String_To_List (Prop.Initial_Cmd_Line.all);
                  begin
                     for V in Cmd'Range loop
                        Name := Get_String (Cmd (V).all);
                        Prj.String_Elements.Increment_Last;
                        Prj.String_Elements.Table (Prj.String_Elements.Last) :=
                          Prj.String_Element'
                            (Value         => Name,
                             Display_Value => Name,
                             Location      => No_Location,
                             Flag          => False,
                             Next          => L);
                        L := Prj.String_Elements.Last;
                     end loop;

                     Value := Prj.Variable_Value'
                       (Kind     => Prj.List,
                        Location => No_Location,
                        Project  => Prj.No_Project,
                        Default  => True,
                        Values   => L);
                     Free (Cmd);
                  end;
               end if;
            end if;
         end;
      end if;

      return To_Argument_List (Value);
   end Get_Switches;

end Glide_Kernel.Project;
