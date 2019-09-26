------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2012-2019, AdaCore                     --
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

with Ada.Strings.Fixed;
with Ada.Unchecked_Deallocation;
with GNAT.Directory_Operations;
with GNAT.Strings;

with GNATCOLL.Scripts.Projects;
with GNATCOLL.Templates;
with GNATCOLL.Traces;            use GNATCOLL.Traces;
with GNATCOLL.Utils;             use GNATCOLL.Utils;

with GPS.Intl;                   use GPS.Intl;
with Config;                     use Config;
with Custom_Tools_Output;
with Shared_Macros;              use Shared_Macros;
with String_Utils;               use String_Utils;

package body Build_Command_Utils is

   Me : constant Trace_Handle := Create ("GPS.BUILD.BUILD_COMMAND_MANAGER");

   Max_Number_Of_Mains : constant := 128;
   --  The maximum number of Mains that we accept to display in the Menus
   --  and toolbar.

   function Is_Server_In_Mode
     (Registry   : Build_Config_Registry_Access;
      Mode : String) return Boolean;

   function Get_Mode_Server
     (Registry   : Build_Config_Registry_Access;
      Mode : String) return Remote.Server_Type;

   function Scenario_Variables_Cmd_Line
     (Adapter : Abstract_Build_Command_Adapter'Class;
      Prefix : String) return String;

   type Build_Command_Adapter is new Abstract_Build_Command_Adapter with record
      Last_Main_For_Background : Virtual_File := No_File;
      Project_File             : Virtual_File := No_File;
      Built_File               : Virtual_File := No_File;
      Status                   : Unbounded_String := To_Unbounded_String ("");
   end record;
   type Build_Command_Adapter_Access is access all Build_Command_Adapter;

   overriding function Get_Last_Main_For_Background_Target
     (Adapter : Build_Command_Adapter;
      Target : Target_Access) return Virtual_File;
   --  Return the Main to use for building Target as a background build.
   --  This is either the last main that was used, if it exists, or the first
   --  main defined for this target, if it exists.
   --  The full path to the target is returned.
   --  If the target is not found, "" is returned.

   overriding function Get_Background_Project_Full_Name
     (Adapter : Build_Command_Adapter) return Filesystem_String;

   overriding function Substitute
     (Adapter : Build_Command_Adapter;
      Param     : String;
      Quoted    : Boolean;
      Done      : access Boolean;
      Server    : Server_Type := GPS_Server;
      For_Shell : Boolean := False) return String;
   --  Wrapper around GPS.Kernel.Macros.Substitute

   overriding procedure Console_Insert
     (Adapter : in out Build_Command_Adapter;
      Text   : String;
      Add_LF : Boolean := True;
      Mode   : Message_Type := Info);

   overriding procedure Remove_Error_Builder_Message_From_File
     (Adapter : Build_Command_Adapter;
      File     : Virtual_File);
   --  Removes all messages for specified file in the error category.
   --  Do nothing when there is no such category or file.

   overriding function Get_Background_Environment_File
     (Adapter : Build_Command_Adapter) return Virtual_File;

   overriding function Get_Scenario_Variables
     (Adapter : Build_Command_Adapter) return Scenario_Variable_Array;

   overriding function Get_Untyped_Variables
     (Adapter : Build_Command_Adapter) return Untyped_Variable_Array;

   Invalid_Argument : exception;
   --  Raised by Expand_Arg below

   function Expand_Arg
     (Adapter      : Abstract_Build_Command_Adapter_Access;
      Target       : Target_Access;
      Arg          : String;
      Server       : Server_Type;
      Force_File   : Virtual_File;
      Main         : Virtual_File;
      Main_Project : Project_Type;
      Subdir       : Filesystem_String;
      Background   : Boolean;
      Simulate     : Boolean) return Expansion_Result;
   --  Expand macros contained in Arg.
   --  Caller must free the result.
   --  Will raise Invalid_Argument if an invalid/non existent argument is
   --  found.
   --  If Simulate is true, Invalid_Argument will never be raised, and no
   --  expansion will be done.

   procedure Free_Adapter is new Ada.Unchecked_Deallocation
     (Build_Command_Adapter, Build_Command_Adapter_Access);

   ---------------
   -- Get_Mains --
   ---------------

   function Get_Mains
     (Registry : Project_Registry_Access) return Project_And_Main_Array
   is
      Result       : Project_And_Main_Array (1 .. Max_Number_Of_Mains);
      Index        : Natural := Result'Last;
      The_Project  : Project_Type;
      M            : String_List_Access;
      File         : Virtual_File;

      --  The main units, when defined in an extended project, are
      --  always added for the extending project (since we always want
      --  to compile in the context of that project). So we always ignore
      --  extended projects in the loop.

      Iterator     : Project_Iterator :=
         Registry.Tree.Root_Project.Start (Include_Extended => False);

   begin
      --  The project Iterator starts with the leaf projects and ends with
      --  the root project. Reverse the order to be more user-friendly: in
      --  the majority of cases, users will want to see the mains defined
      --  in the root project first. To do this, we fill the result
      --  starting from the end

      while Current (Iterator) /= No_Project loop
         The_Project := Current (Iterator);

         if The_Project.Extending_Project = No_Project then

            --  Retrieve the list of mains either from the project itself or
            --  from the extended one, if any.
            M := The_Project.Attribute_Value
              (Attribute    => Main_Attribute,
               Use_Extended => True);

            if M /= null then
               declare
                  This_Project : Project_And_Main_Array (1 .. M'Length);
                  This_Index   : Natural := 1;
               begin
                  for Basename of M.all loop
                     if Basename.all /= "" then

                        --  Resolve to full path

                        if GNAT.Directory_Operations.File_Extension
                          (Basename.all) = ""
                        then
                           --  The project files used to support the form
                           --     for Main use ("basename");
                           --  If this is the case here, add ".adb" to get the
                           --  real name of  the source unit.
                           File := Registry.Tree.Create
                             (Filesystem_String (Basename.all & ".adb"),
                              Use_Object_Path => False);
                        else
                           File := Registry.Tree.Create
                             (Name            => Filesystem_String
                                (Basename.all),
                              Use_Object_Path => False);
                        end if;

                        if File = GNATCOLL.VFS.No_File then
                           File := Create_From_Base (+Basename.all);
                        end if;

                        This_Project (This_Index) :=
                          (Project_Path => The_Project.Project_Path,
                           Main => File);
                        This_Index := This_Index + 1;
                     end if;

                  end loop;

                  --  Do this gymnastics so that we list projects in reverse
                  --  from the iterator, but the target order is preserved
                  --  within one project.
                  Result (Index - This_Index + 2 .. Index) := This_Project
                    (1 .. This_Index - 1);
                  Index := Index - This_Index + 1;
               end;

               Free (M);
            end if;
         end if;

         Next (Iterator);
      end loop;

      return Result (Index + 1 .. Result'Last);
   end Get_Mains;

   --------------------------
   -- Get_Mains_Files_Only --
   --------------------------

   function Get_Mains_Files_Only (Registry : Project_Registry_Access)
      return GNATCOLL.VFS.File_Array
   is
      Mains        : constant Project_And_Main_Array := Get_Mains (Registry);
      Result       : GNATCOLL.VFS.File_Array (Mains'First .. Mains'Last);
   begin
      for J in Mains'First .. Mains'Last loop
         Result (J) := Mains (J).Main;
      end loop;
      return Result;
   end Get_Mains_Files_Only;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Adapter                    : in out Abstract_Build_Command_Adapter'Class;
      Kernel                     : not null access Core_Kernel_Record'Class;
      Context_Project                 : Project_Type;
      Context_Toolchains_Manager      : Toolchain_Manager;
      Context_File_Information        : Virtual_File;
      Kernel_Macros_Special_Character : Character;
      Trusted_Mode_Preference         : Boolean;
      Execute_Command_Preference      : String;
      Multi_Language_Builder          : Multi_Language_Builder_Policy) is
   begin
      Adapter.Kernel := Kernel;
      Adapter.Context_Project := Context_Project;
      Adapter.Context_Toolchains_Manager := Context_Toolchains_Manager;
      Adapter.Context_File_Information := Context_File_Information;
      Adapter.Kernel_Macros_Special_Character :=
         Kernel_Macros_Special_Character;
      Adapter.Trusted_Mode_Preference := Trusted_Mode_Preference;
      Adapter.Execute_Command_Preference :=
         To_Unbounded_String (Execute_Command_Preference);
      Adapter.Multi_Language_Builder := Multi_Language_Builder;
   end Initialize;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self     : access Builder_Context_Record'Class;
      Kernel   : GPS.Core_Kernels.Core_Kernel;
      Registry : Build_Config_Registry_Access) is
   begin
      Self.Kernel := Kernel;
      Self.Registry := Registry;
      Register_Module (Kernel, Self);
      Custom_Tools_Output.Register_Commands (Kernel);
   end Initialize;

   --------------------------------
   -- Interrupt_Background_Build --
   --------------------------------

   procedure Interrupt_Background_Build
     (Self    : access Builder_Context_Record;
      Command : out Command_Access) is
   begin
      Command := Self.Background_Build_Command;
      Self.Background_Build_Command := null;
   end Interrupt_Background_Build;

   -------------------------
   -- Get_Kernel_Registry --
   -------------------------

   function Get_Kernel_Registry
     (Adapter : Abstract_Build_Command_Adapter)
        return Project_Registry_Access is
   begin
      return Adapter.Kernel.Registry;
   end Get_Kernel_Registry;

   -------------------------
   -- Get_Context_Project --
   -------------------------

   function Get_Context_Project
     (Adapter : Abstract_Build_Command_Adapter) return Project_Type is
   begin
      return Adapter.Context_Project;
   end Get_Context_Project;

   ------------------------------------
   -- Get_Context_Toolchains_Manager --
   ------------------------------------

   function Get_Context_Toolchains_Manager
     (Adapter : Abstract_Build_Command_Adapter)
      return Toolchain_Manager is
   begin
      return Adapter.Context_Toolchains_Manager;
   end Get_Context_Toolchains_Manager;

   ----------------------------------
   -- Get_Context_File_Information --
   ----------------------------------

   function Get_Context_File_Information
     (Adapter : Abstract_Build_Command_Adapter) return Virtual_File is
   begin
      return Adapter.Context_File_Information;
   end Get_Context_File_Information;

   -----------------------------------------
   -- Get_Kernel_Macros_Special_Character --
   -----------------------------------------

   function Get_Kernel_Macros_Special_Character
     (Adapter : Abstract_Build_Command_Adapter)
      return Character is
   begin
      return Adapter.Kernel_Macros_Special_Character;
   end Get_Kernel_Macros_Special_Character;

   ---------------------------------
   -- Get_Trusted_Mode_Preference --
   ---------------------------------

   function Get_Trusted_Mode_Preference
     (Adapter :  Abstract_Build_Command_Adapter) return Boolean is
   begin
      return Adapter.Trusted_Mode_Preference;
   end Get_Trusted_Mode_Preference;

   ------------------------------------
   -- Get_Execute_Command_Preference --
   ------------------------------------

   function Get_Execute_Command_Preference
     (Adapter :  Abstract_Build_Command_Adapter) return String is
   begin
      return To_String (Adapter.Execute_Command_Preference);
   end Get_Execute_Command_Preference;

   --------------------------------
   -- Get_Multi_Language_Builder --
   --------------------------------

   function Get_Multi_Language_Builder
     (Adapter :  Abstract_Build_Command_Adapter)
      return Multi_Language_Builder_Policy is
   begin
      return Adapter.Multi_Language_Builder;
   end Get_Multi_Language_Builder;

   ---------------------------------
   -- Scenario_Variables_Cmd_Line --
   ---------------------------------

   function Scenario_Variables_Cmd_Line
     (Adapter : Abstract_Build_Command_Adapter'Class;
      Prefix  : String) return String
   is
      Scenario_Vars : constant Scenario_Variable_Array :=
        Get_Scenario_Variables (Adapter);
      Untyped_Vars  : constant Untyped_Variable_Array  :=
        Get_Untyped_Variables (Adapter);
      Res : Unbounded_String;

   begin
      for Var of Scenario_Vars loop
         Append (Res, Prefix & External_Name (Var) & "=" & Value (Var) & " ");
      end loop;

      for Var of Untyped_Vars loop
         Append (Res, Prefix & External_Name (Var) & "=" & Value (Var) & " ");
      end loop;
      return To_String (Res);
   end Scenario_Variables_Cmd_Line;

   ----------------
   -- Expand_Arg --
   ----------------

   function Expand_Arg
     (Adapter      : Abstract_Build_Command_Adapter_Access;
      Target       : Target_Access;
      Arg          : String;
      Server       : Server_Type;
      Force_File   : Virtual_File;
      Main         : Virtual_File;
      Main_Project : Project_Type;
      Subdir       : Filesystem_String;
      Background   : Boolean;
      Simulate     : Boolean) return Expansion_Result
   is
      Result : Expansion_Result;

      function Substitution
        (Param  : String; Quoted : Boolean) return String;
      --  Wrapper around GPS.Kernel.Macros.Substitute

      function Get_Attr_Value (Arg : String; Skip : Natural) return String;
      --  return the name of the attribute contained in Arg

      function Get_Index (A, B : Natural) return Natural;
      --  Return A if A /= 0, B otherwise

      function Multi_Language_Build return Boolean;
      --  Return True if the build is multi-language, False if build is Ada
      --  only.

      function Create_Command
        (Command : String; Tc : Toolchains.Toolchain) return Arg_List;
      --  Create an Arg_List containing the given Command, and possibly
      --  appended with --target=xxx if Tc is a cross toolchain.

      Protect_Python : Boolean := Uses_Python (Target);
      --  Whether to protect special characters when expanding a macro

      function Get_Python_Full_Name (File : Virtual_File) return String
      is
        (if Protect_Python then
            Protect (+File.Full_Name)
         else
            +File.Full_Name);
      --  If the Target uses python, Return a suitable python string from the
      --  file's full name, escaping the backslashes when needed.
      --  Otherwise, return the file's full name without any replacing.

      Environment : Project_Environment_Access renames
        Adapter.Kernel.Registry.Environment;

      ---------------
      -- Get_Index --
      ---------------

      function Get_Index (A, B : Natural) return Natural is
      begin
         if A = 0 then
            return B;
         else
            return A;
         end if;
      end Get_Index;

      ------------------
      -- Substitution --
      ------------------

      function Substitution
        (Param : String; Quoted : Boolean) return String
      is
         Done : aliased Boolean := False;
      begin
         if Param = "subdir" then
            return +Subdir;

         elsif Param = "subdirsarg" then
            if Subdir = "" then
               return "";
            else
               return "--subdirs=" & (+Subdir);
            end if;

         elsif Param = "eL" then
            if Get_Trusted_Mode_Preference (Adapter.all) then
               return "";
            else
               return "-eL";
            end if;

         elsif Param = "fp" then
            if not Simulate
              and then Force_File /= No_File
            then
               return +Base_Name (Force_File);
            end if;

            declare
               File : constant Virtual_File :=
                 Get_Context_File_Information (Adapter.all);
               Set  : File_Info_Set;
            begin
               if File = No_File then
                  if Simulate then
                     return "<current-file>";
                  else
                     Console_Insert
                       (Adapter.all, -"No file selected", Mode => Error);
                     raise Invalid_Argument;
                  end if;
               end if;

               Set := Get_Kernel_Registry (Adapter.all).Tree.Info_Set (File);

               if File_Info'Class (Set.First_Element).Project = No_Project then
                  if Simulate then
                     return "<current-file>";
                  else
                     Console_Insert
                       (Adapter.all,
                        -"Could not determine the project for file: "
                        & Display_Full_Name (File),
                        Mode => Error);

                     --  Do not normalize through VFS so as to preserve the
                     --  state of the file (since otherwise we would cache
                     --  the normalized value)
                     if File.Display_Full_Name /=
                       Normalize_Pathname
                         (Get_Python_Full_Name (File), Resolve_Links => True)
                         and then Get_Trusted_Mode_Preference (Adapter.all)
                     then
                        Console_Insert
                          (Adapter.all, -("You should check the project's"
                           & " scenario variable's values"
                           & " or disable the preference Fast Project"
                           & " Loading (for full support of symbolic links)"));
                     end if;

                     raise Invalid_Argument;
                  end if;

               else
                  if Background then
                     return +Base_Name
                       (Get_Background_Environment_File (Adapter.all));
                  else
                     --  We are launching a compile command involving File:
                     --  remove reference to File from the Locations View.
                     --  See F830-003.
                     if not Simulate then
                        Remove_Error_Builder_Message_From_File
                          (Adapter.all, File);
                     end if;

                     return +Base_Name (File);
                  end if;
               end if;
            end;

         elsif Param = "E" then
            if Main /= No_File then
               declare
                  Executable : Virtual_File;
               begin
                  if Get_Properties (Target).Target_Type /= "executable" then
                     Executable := Create_From_Dir
                       (Adapter.Context_Project.Executables_Directory,
                        Adapter.Context_Project.Executable_Name
                          (+Get_Python_Full_Name (Main)));
                  else
                     Executable := Main;
                  end if;
                  return Get_Python_Full_Name
                    (To_Remote (Executable, Get_Nickname (Server)));
               end;

            else
               Console_Insert
                 (Adapter.all,
                  -"Could not determine the executable name for main.",
                  Mode => Error);
               raise Invalid_Argument;
            end if;

         elsif Param = "TP" then
            return Get_Python_Full_Name (Main_Project.Project_Path);

         elsif Param = "T" then
            if Main /= No_File then
               return +Main.Base_Name;
            else
               if Background then
                  declare
                     M : constant Virtual_File :=
                       Get_Last_Main_For_Background_Target
                         (Adapter.all, Target);
                  begin
                     if M = No_File then
                        Console_Insert
                          (Adapter.all,
                           (-"Could not launch background build: no main(s)"
                            & " found for target ") & Get_Name (Target),
                           Mode => Error);
                        raise Invalid_Argument;
                     else
                        return +M.Base_Name;
                     end if;
                  end;
               else
                  Console_Insert
                    (Adapter.all, -"Could not determine the target to build.",
                     Mode => Error);
                  raise Invalid_Argument;
               end if;
            end if;

         elsif Param = "TT" then
            if Main /= No_File then
               return Get_Python_Full_Name
                 (Main.To_Remote (Get_Nickname (Server)));
            else
               if Background then
                  declare
                     M : constant Virtual_File :=
                       Get_Last_Main_For_Background_Target
                         (Adapter.all, Target);
                  begin
                     if M = No_File then
                        Console_Insert
                          (Adapter.all,
                           (-"Could not launch background build: no main(s)"
                            & " found for target ") & Get_Name (Target),
                           Mode => Error);
                        raise Invalid_Argument;
                     else
                        return Get_Python_Full_Name
                          (M.To_Remote (Get_Nickname (Server)));
                     end if;
                  end;
               else
                  Console_Insert
                    (Adapter.all, -"Could not determine the target to build.",
                     Mode => Error);
                  raise Invalid_Argument;
               end if;
            end if;

         elsif Param = "config" then
            if Environment.Get_Automatic_Config_File
              or else Environment.Get_Config_File = No_File
            then
               return "";
            else
               return "--config="
                  & Get_Python_Full_Name (Environment.Get_Config_File);
            end if;

         elsif Param = "autoconf" then
            if not Environment.Get_Automatic_Config_File
              or else Environment.Get_Config_File = No_File
            then
               return "";
            else
               return "--autoconf="
                 & Get_Python_Full_Name (Environment.Get_Config_File);
            end if;

         elsif Background
           and then not Simulate
           and then (Param = "pp" or else Param = "PP")
         then
            return +Get_Background_Project_Full_Name (Adapter.all);

         else
            declare
               Result : constant String := Adapter.Substitute
                 (Param, Quoted, Done'Access, Server => Server);
            begin
               if Done then
                  return Result;
               elsif Simulate then
                  return '%' & Param;
               else
                  raise Invalid_Argument;
               end if;
            end;
         end if;
      end Substitution;

      --------------------
      -- Get_Attr_Value --
      --------------------

      function Get_Attr_Value (Arg : String; Skip : Natural) return String is
         J    : constant Natural := Get_Index
           (Ada.Strings.Fixed.Index (Arg, "'"), Arg'First + Skip);
         K    : constant Natural := Get_Index
           (Ada.Strings.Fixed.Index (Arg (J .. Arg'Last), ","), Arg'Last);
         Pkg  : constant String := Arg (Arg'First + Skip + 1 .. J - 1);
         Attr : constant String := Arg (J + 1 .. K - 1);
      begin
         return Get_Context_Project (Adapter.all).Attribute_Value
           (Build (Pkg, Attr), Default => Arg (K + 1 .. Arg'Last - 1));
      end Get_Attr_Value;

      --------------------------
      -- Multi_Language_Build --
      --------------------------

      function Multi_Language_Build return Boolean
      is
         Policy : constant Multi_Language_Builder_Policy :=
           Get_Multi_Language_Builder (Adapter.all);
      begin
         case Policy is
            when Gprbuild =>
               return True;
            when Gnatmake =>
               return False;
         end case;
      end Multi_Language_Build;

      --------------------
      -- Create_Command --
      --------------------

      function Create_Command
        (Command : String; Tc : Toolchains.Toolchain) return Arg_List
      is
         Result : Arg_List;
      begin
         Result := Create (Command);

         if not Is_Native (Tc) then
            Append_Argument (Result, "--target=" & Get_Name (Tc), One_Arg);
         end if;

         return Result;
      end Create_Command;

   begin
      -------------------------
      --  IMPORTANT:
      --
      --  Any change in this list must be documented in
      --  switches_chooser.ads
      -------------------------

      --  Special case for all macros that potentially return multiple
      --  command line arguments.
      --  We are implementing a special case here since GPS.Kernel.Macros
      --  does not support returning an Argument_List.
      --  See H926-007.

      if Arg = "%X" then
         Result.Args := Parse_String
           (Scenario_Variables_Cmd_Line (Adapter.all, "-X"), Separate_Args);

      elsif Arg = "%vars" then
         Result.Args := Parse_String
           (Scenario_Variables_Cmd_Line (Adapter.all, ""), Separate_Args);

      elsif Arg = "%vars(-D)" then
         Result.Args := Parse_String
           (Scenario_Variables_Cmd_Line (Adapter.all, "-D"), Separate_Args);

      elsif Starts_With (Arg, "%attr(") and then Arg (Arg'Last) = ')' then
         Result.Args := Parse_String (Get_Attr_Value (Arg, 5), Separate_Args);

      elsif Starts_With (Arg, "%dirattr(") and then Arg (Arg'Last) = ')' then
         Result.Args := Parse_String (Get_Attr_Value (Arg, 8), Separate_Args);
         Set_Nth_Arg
           (Result.Args, 0,
            GNAT.Directory_Operations.Dir_Name (Nth_Arg (Result.Args, 0)));

      elsif Starts_With (Arg, "%baseattr(") and then Arg (Arg'Last) = ')' then
         Result.Args := Parse_String (Get_Attr_Value (Arg, 9), Separate_Args);
         Set_Nth_Arg
           (Result.Args, 0,
            GNAT.Directory_Operations.Base_Name (Nth_Arg (Result.Args, 0)));

      elsif Starts_With (Arg, "%switches(") and then Arg (Arg'Last) = ')' then
         declare
            List : GNAT.Strings.String_List_Access :=
                    Get_Context_Project (Adapter.all).Attribute_Value
                      (Build ("IDE", "Default_Switches"),
                       Index => Arg (Arg'First + 10 .. Arg'Last - 1));
         begin
            if List /= null and then List'Length /= 0 then
               Result.Args := Create (List (List'First).all);
               for J in List'First + 1 .. List'Last loop
                  Append_Argument (Result.Args, List (J).all, One_Arg);
               end loop;
            end if;

            Free (List);
         end;

      elsif Arg = "%builder"
        or else Arg = "%gprbuild"
        or else Arg = "%gprclean"
      then
         declare
            Prj   : constant Project_Type := Get_Context_Project (Adapter.all);
            Tc    : constant Toolchains.Toolchain :=
                      Get_Toolchain
                        (Get_Context_Toolchains_Manager (Adapter.all), Prj);
            Res   : Expansion_Result;
            Clean : constant Boolean := Arg = "%gprclean";

         begin
            if Arg = "%gprbuild" then
               Res.Args := Create_Command ("gprbuild", Tc);
            elsif Multi_Language_Build then
               if Clean then
                  Res.Args := Create_Command ("gprclean", Tc);
               else
                  Res.Args := Create_Command ("gprbuild", Tc);
               end if;
            else
               if Clean then
                  Res.Args := Create
                    (Prj.Attribute_Value
                       (GNAT_Attribute, Default => "gnat"));
                  Append_Argument (Res.Args, "clean", One_Arg);
               else
                  --  Compiler ("Ada") is the gnatmake command
                  Res.Args := Create (Get_Exe (Get_Compiler (Tc, "Ada")));
               end if;
            end if;

            return Res;
         end;

      elsif Arg = "%gnathub" then
         declare
            File : constant Virtual_File :=
              (Adapter.Kernel.Get_System_Dir
               / "libexec"
               / "gnatstudio"
               / "gnathub"
               / "bin"
               / ("gnathub" & (if Host = Windows then ".exe" else "")));
         begin
            if File.Is_Regular_File then
               Result.Args :=
                 Create (File.Display_Full_Name (Normalize => True));
            else
               Result.Args := Create ("gnathub");
            end if;
         end;

      elsif Arg = "%external" then
         Result.Args := Parse_String
           (Get_Execute_Command_Preference (Adapter.all), Separate_Args);

      elsif Arg = "[exec_dir]" then
         declare
            Prj : constant Project_Type :=
              Get_Context_Project (Adapter.all);
         begin
            Result.Dir := Executables_Directory (Prj);
         end;

      elsif Starts_With (Arg, "%python(")
        and then Arg (Arg'Last) = ')'
        and then Arg'Length >= 9
        and then Adapter.Kernel.Scripts /= null
      then
         declare
            Cmd : constant String := Arg (Arg'First + 8 .. Arg'Last - 1);
            Old_Protect_Python : constant Boolean := Protect_Python;
            Data               : Callback_Data'Class := Create
              (Adapter.Kernel.Scripts.Lookup_Scripting_Language ("python"), 0);

         begin
            --  Expand some macros recursively
            Protect_Python := True;
            Execute_Expression
              (Data,
               GNATCOLL.Templates.Substitute
                 (Cmd, Delimiter =>
                      Get_Kernel_Macros_Special_Character (Adapter.all),
                  Callback       => Substitution'Unrestricted_Access),
               Hide_Output => True);
            Protect_Python := Old_Protect_Python;

            --  Process the result of the python command
            Result.Args := Empty_Command_Line;

            begin
               --  Did we receive a single string ?
               declare
                  S : constant String := Data.Return_Value;
               begin
                  if S /= "" then
                     Append_Argument (Result.Args, S, One_Arg);
                  end if;
               end;

            exception
               when Invalid_Parameter =>
                  begin
                     --  Did we receive a list of strings ?
                     declare
                        L : constant List_Instance'Class :=
                          Data.Return_Value;
                     begin
                        for S in 1 .. L.Number_Of_Arguments loop
                           if String'(L.Nth_Arg (S)) /= "" then
                              Append_Argument
                                (Result.Args, L.Nth_Arg (S), One_Arg);
                           end if;
                        end loop;
                     end;

                  exception
                     when Invalid_Parameter =>

                        --  Did we receive a boolean
                        declare
                           B : constant Boolean := Data.Return_Value;
                        begin
                           if not B then
                              --  Cancel the command
                              raise Invalid_Argument;
                           end if;
                        end;
                  end;
            end;

            Free (Data);
         exception
            when E : others =>
               Trace (Me, E,
                      "Error while executing python command from"
                      & " command line argument: " & Arg & ASCII.LF);
         end;

      else
         --  Handle all macros that expand as a single string
         Result.Args :=
           Create (GNATCOLL.Templates.Substitute
                   (Str       => Arg,
                    Delimiter => Get_Kernel_Macros_Special_Character
                       (Adapter.all),
                    Callback  => Substitution'Unrestricted_Access));
      end if;

      return Result;
   end Expand_Arg;

   -------------------------
   -- Expand_Command_Line --
   -------------------------

   function Expand_Command_Line
     (Adapter    : Abstract_Build_Command_Adapter_Access;
      Cmd_Line   : Command_Line;
      Target     : Target_Access;
      Server     : Server_Type;
      Force_File : Virtual_File;
      Main       : Virtual_File;
      Main_Project : Project_Type;
      Subdir     : Filesystem_String;
      Background : Boolean;
      Simulate   : Boolean) return Expansion_Result
   is
      CL      : GNAT.Strings.String_List_Access :=
        Cmd_Line.To_String_List (Expanded => False);
      Result  : Expansion_Result;
      Final   : Expansion_Result;
      Failed  : Boolean := False;

   begin
      for J in CL'Range loop
         declare
            Arg : constant String := CL (J).all;
         begin
            Result := Expand_Arg
              (Adapter, Target, CL (J).all, Server,
               Force_File, Main, Main_Project, Subdir, Background, Simulate);
         exception
            when Invalid_Argument =>
               Console_Insert
                 (Adapter.all,
                  (-"Could not expand argument in command line: ") & Arg,
                  Mode => Error);
               Failed := True;
         end;

         if Result.Dir /= No_File then
            Final.Dir := Result.Dir;
         end if;

         for J in 0 .. Args_Length (Result.Args) loop
            if String'(Nth_Arg (Result.Args, J)) /= "" then
               Append_Argument (Final.Args, Nth_Arg (Result.Args, J), One_Arg);
            end if;
         end loop;
      end loop;

      if Failed then
         Console_Insert
           (Adapter.all, (-"Build command not launched."),
            Mode => Error);
         return (Empty_Command_Line, No_File, To_Unbounded_String (""));
      end if;

      Free (CL);
      return Final;
   end Expand_Command_Line;

   -----------------------------------------
   -- Get_Last_Main_For_Background_Target --
   -----------------------------------------

   overriding
   function Get_Last_Main_For_Background_Target
     (Adapter : Build_Command_Adapter;
      Target : Target_Access) return Virtual_File is
      pragma Unreferenced (Adapter);
      pragma Unreferenced (Target);
   begin
      return No_File;
   end Get_Last_Main_For_Background_Target;

   --------------------------------------
   -- Get_Background_Project_Full_Name --
   --------------------------------------

   overriding function Get_Background_Project_Full_Name
     (Adapter : Build_Command_Adapter) return Filesystem_String is
   begin
      return Adapter.Project_File.Full_Name.all;
   end Get_Background_Project_Full_Name;

   ----------------------------
   -- Get_Scenario_Variables --
   ----------------------------

   overriding function Get_Scenario_Variables
     (Adapter : Build_Command_Adapter) return Scenario_Variable_Array is
   begin
      return Adapter.Kernel.Registry.Tree.Scenario_Variables;
   end Get_Scenario_Variables;

   ---------------------------
   -- Get_Untyped_Variables --
   ---------------------------

   overriding function Get_Untyped_Variables
     (Adapter : Build_Command_Adapter) return Untyped_Variable_Array is
   begin
      return Adapter.Kernel.Registry.Tree.Untyped_Variables;
   end Get_Untyped_Variables;

   ----------------
   -- Substitute --
   ----------------

   overriding function Substitute
     (Adapter   : Build_Command_Adapter;
      Param     : String;
      Quoted    : Boolean;
      Done      : access Boolean;
      Server    : Server_Type := GPS_Server;
      For_Shell : Boolean := False) return String
   is
   begin
      return Shared_Macros_Substitute
        (Project_From_Kernel => Get_Context_Project (Adapter),
         Project_From_Param  => Get_Context_Project (Adapter),
         File_Information    => Get_Context_File_Information (Adapter),
         Param               => Param,
         Quoted              => Quoted,
         Done                => Done,
         Server              => Server,
         For_Shell           => For_Shell);
   end Substitute;

   --------------------
   -- Console_Insert --
   --------------------

   overriding procedure Console_Insert
     (Adapter : in out Build_Command_Adapter;
      Text   : String;
      Add_LF : Boolean := True;
      Mode   : Message_Type := Info) is
   begin
      Adapter.Kernel.Messages_Window.Insert
        (Text,
         Add_LF => Add_LF,
         Mode   => Mode);
      Trace (Me, Mode'Img & " : " & Text);
      if Mode = Error then
         Adapter.Status := Adapter.Status & Text;
         if Add_LF then
            Adapter.Status := Adapter.Status & ASCII.LF;
         end if;
      end if;
   end Console_Insert;

   --------------
   -- Registry --
   --------------

   function Registry
     (Self : access Builder_Context_Record)
      return Build_Config_Registry_Access is
   begin
      return Self.Registry;
   end Registry;

   --------------------------------------------
   -- Remove_Error_Builder_Message_From_File --
   --------------------------------------------

   overriding
   procedure Remove_Error_Builder_Message_From_File
     (Adapter : Build_Command_Adapter;
      File     : Virtual_File) is
   begin
      null;
   end Remove_Error_Builder_Message_From_File;

   -------------------------------------
   -- Get_Background_Environment_File --
   -------------------------------------

   overriding
   function Get_Background_Environment_File
     (Adapter : Build_Command_Adapter) return Virtual_File is
      pragma Unreferenced (Adapter);
   begin
      return No_File;
   end Get_Background_Environment_File;

   -----------------------
   -- Is_Server_In_Mode --
   -----------------------

   function Is_Server_In_Mode
     (Registry   : Build_Config_Registry_Access;
      Mode : String) return Boolean  is
      U : constant Unbounded_String := To_Unbounded_String (Mode);
   begin
      return Element_Mode (Registry, U).Is_Server;
   end Is_Server_In_Mode;

   ---------------------
   -- Get_Mode_Server --
   ---------------------

   function Get_Mode_Server
     (Registry   : Build_Config_Registry_Access;
      Mode : String) return Remote.Server_Type is
      U : constant Unbounded_String := To_Unbounded_String (Mode);
   begin
      return Element_Mode (Registry, U).Server;
   end Get_Mode_Server;

   ----------------
   -- Get_Server --
   ----------------

   function Get_Server
     (Registry   : Build_Config_Registry_Access;
      Mode       : String;
      Target     : Target_Access) return Server_Type is
      Server : Server_Type;
   begin
      if Is_Server_In_Mode (Registry, Mode) then
         Server := Get_Mode_Server (Registry, Mode);
      else
         Server := Get_Server (Target);
      end if;
      return Server;
   end Get_Server;

   ---------------------
   -- Get_Mode_Subdir --
   ---------------------

   function Get_Mode_Subdir
     (Registry : Build_Config_Registry_Access;
      Mode     : String) return Filesystem_String is
   begin
      return +To_String
        (Element_Mode
           (Registry, To_Unbounded_String (Mode)).Subdir);
   end Get_Mode_Subdir;

   -------------------------
   -- Expand_Command_Line --
   -------------------------

   function Expand_Command_Line
     (Builder    : Builder_Context;
      CL         : Command_Line;
      Target     : Target_Access;
      Server     : Server_Type;
      Force_File : Virtual_File;
      Main       : Virtual_File;
      Main_Project : Project_Type;
      Subdir     : Filesystem_String;
      Background : Boolean;
      Simulate   : Boolean) return Expansion_Result
   is
      Adapter   : Build_Command_Adapter_Access := new Build_Command_Adapter;
      Res       : Expansion_Result;
   begin
      Initialize
        (Adapter.all,
         Kernel                          => Builder.Kernel,
         Context_Project                 =>
           (if Main_Project = No_Project
            then Builder.Kernel.Registry.Tree.Root_Project
            else Main_Project),
         Context_Toolchains_Manager   => Builder.Kernel.Get_Toolchains_Manager,
         Context_File_Information        => No_File,
         Kernel_Macros_Special_Character => '%',
         Trusted_Mode_Preference         => True,
         Execute_Command_Preference      => "",
         Multi_Language_Builder          => Gprbuild);

      Res := Expand_Command_Line
        (Abstract_Build_Command_Adapter_Access (Adapter), CL, Target, Server,
         Force_File, Main, Main_Project, Subdir, Background, Simulate);
      Free_Adapter (Adapter);
      return Res;
   end Expand_Command_Line;

   -----------------
   -- Args_Length --
   -----------------

   function Args_Length (Result : Expansion_Result) return Integer is
   begin
      return Args_Length (Result.Args);
   end Args_Length;

   ------------
   -- Kernel --
   ------------

   function Kernel
     (Self : access Builder_Context_Record)
      return GPS.Core_Kernels.Core_Kernel is
   begin
      return Self.Kernel;
   end Kernel;

   -------------
   -- Nth_Arg --
   -------------

   function Nth_Arg (Result : Expansion_Result; N : Natural) return String is
   begin
      return Nth_Arg (Result.Args, N);
   end Nth_Arg;

   ---------
   -- Dir --
   ---------

   function Dir (Result : Expansion_Result) return Virtual_File is
   begin
      return Result.Dir;
   end Dir;

   ---------------
   -- Arguments --
   ---------------

   function Arguments (Result : Expansion_Result) return String is
   begin
      return To_Display_String (Result.Args);
   end Arguments;

   ------------
   -- Status --
   ------------

   function Status (Result : Expansion_Result) return String is
   begin
      return To_String (Result.Status);
   end Status;

   -------------------
   -- Set_Last_Main --
   -------------------

   procedure Set_Last_Main
     (Self   : access Builder_Context_Record;
      Target : String;
      Main   : Virtual_File)
   is
      Key : constant Unbounded_String := To_Unbounded_String (Target);
   begin
      Self.Last_Mains.Include (Key, Main);
   end Set_Last_Main;

   -------------------
   -- Get_Last_Main --
   -------------------

   function Get_Last_Main
     (Self   : access Builder_Context_Record;
      Target : String) return Virtual_File
   is
      Key : constant Unbounded_String := To_Unbounded_String (Target);
      Cur : Files.Cursor;
      use Files;
   begin
      Cur := Self.Last_Mains.Find (Key);
      if Cur /= Files.No_Element then
         return Element (Cur);
      else
         return No_File;
      end if;
   end Get_Last_Main;

   -----------------------
   -- Get_List_Of_Modes --
   -----------------------

   function Get_List_Of_Modes
     (Current  : String;
      Registry : Build_Config_Registry_Access;
      Model    : String) return GNAT.OS_Lib.Argument_List
   is
      Result : Argument_List (1 .. Number_Of_Modes (Registry));
      Index  : Natural;
      --  The first available element in Result;

      use Mode_Map;
      C : Mode_Map.Cursor;
      Mode : Mode_Record;
   begin
      if Result'Length = 0 then
         return (1 => new String'(""));
      end if;

      --  The first mode is the one selected in the combo

      Result (1) := new String'(Current);
      Index := 2;

      --  Find all the shadow modes

      C := First_Mode (Registry);

      while Has_Element (C) loop
         Mode := Element (C);

         if Mode.Shadow
           and then Mode.Active
         then
            declare
               use Model_List;
               C2 : Model_List.Cursor;
            begin
               C2 := Mode.Models.First;

               while Has_Element (C2) loop
                  if Element (C2).Model = Model then
                     Result (Index) := new String'(To_String (Mode.Name));
                     Index := Index + 1;
                  end if;

                  Next (C2);
               end loop;
            end;
         end if;

         Next (C);
      end loop;

      return Result (1 .. Index - 1);
   end Get_List_Of_Modes;

   ---------------------------------
   -- Current_Background_Build_Id --
   ---------------------------------

   function Current_Background_Build_Id
     (Self : access Builder_Context_Record) return String is
   begin
      return Integer'Image (Self.Background_Build_ID);
   end Current_Background_Build_Id;

   ----------------------------------
   -- Previous_Background_Build_Id --
   ----------------------------------

   function Previous_Background_Build_Id
     (Self : access Builder_Context_Record) return String is
   begin
      return Integer'Image (Self.Background_Build_ID - 1);
   end Previous_Background_Build_Id;

   -------------------------------
   -- Background_Build_Finished --
   -------------------------------

   procedure Background_Build_Finished
     (Self : access Builder_Context_Record) is
   begin
      if Self.Background_Build_ID = Integer'Last then
         --  Very very unlikely, but just in case.
         Self.Background_Build_ID := 1;
      else
         Self.Background_Build_ID := Self.Background_Build_ID + 1;
      end if;

      Self.Background_Build_Command := null;
   end Background_Build_Finished;

   ------------------------------
   -- Background_Build_Started --
   ------------------------------

   procedure Background_Build_Started
     (Self    : access Builder_Context_Record;
      Command : Command_Access) is
   begin
      Self.Background_Build_Command := Command;
   end Background_Build_Started;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy (Self : in out Builder_Context_Record) is
   begin
      for T in Target_Output_Type loop
         Self.Outputs (T).Clear;
      end loop;
   end Destroy;

   ----------------------------
   -- Append_To_Build_Output --
   ----------------------------

   procedure Append_To_Build_Output
     (Self       : access Builder_Context_Record;
      Line       : String;
      Target     : String;
      Shadow     : Boolean;
      Background : Boolean)
   is
      Inserted : Boolean := True;
      C : Target_Outputs.Cursor;
      T : Target_Output_Type;
      use Target_Outputs;
   begin
      if Shadow then
         T := Shadow_Output;

      elsif Background then
         T := Background_Output;

      else
         T := Normal_Output;
      end if;

      C := Self.Outputs (T).Find
        (To_Unbounded_String (Target));

      if C = Target_Outputs.No_Element then
         Self.Outputs (T).Insert
           (Key       => To_Unbounded_String (Target),
            New_Item  => To_Unbounded_String (Line & ASCII.LF),
            Position  => C,
            Inserted  => Inserted);
      else
         declare
            procedure Local_Append (Key : Unbounded_String;
                                    E   : in out Unbounded_String);
            --  Auxiliary subprogram to append to an unbounded string
            --  in place in the container.

            ------------------
            -- Local_Append --
            ------------------

            procedure Local_Append (Key : Unbounded_String;
                                    E   : in out Unbounded_String)
            is
               pragma Unreferenced (Key);
            begin
               Append (E, Line & ASCII.LF);
            end Local_Append;
         begin
            Self.Outputs (T).Update_Element
              (C, Local_Append'Access);
         end;
      end if;
   end Append_To_Build_Output;

   ----------------------
   -- Get_Build_Output --
   ----------------------

   function Get_Build_Output
     (Self       : access Builder_Context_Record;
      Target     : String;
      Shadow     : Boolean;
      Background : Boolean) return Unbounded_String
   is
      Output : Target_Output_Type;
   begin
      if Shadow then
         Output := Shadow_Output;
      elsif Background then
         Output := Background_Output;
      else
         Output := Normal_Output;
      end if;

      if Target = "" then
         declare
            C : Target_Outputs.Cursor;
            R : Unbounded_String;
         begin
            C := Self.Outputs (Output).First;

            while Target_Outputs.Has_Element (C) loop
               R := R & Target_Outputs.Element (C);
               Target_Outputs.Next (C);
            end loop;

            return R;
         end;
      else
         declare
            K : constant Unbounded_String := To_Unbounded_String (Target);
         begin
            if Self.Outputs (Output).Contains (K) then
               return Self.Outputs (Output).Element (K);
            else
               return Null_Unbounded_String;
            end if;
         end;
      end if;
   end Get_Build_Output;

   ------------------------
   -- Clear_Build_Output --
   ------------------------

   procedure Clear_Build_Output
     (Self       : access Builder_Context_Record;
      Shadow     : Boolean;
      Background : Boolean) is
   begin
      if Shadow then
         Self.Outputs (Shadow_Output).Clear;
      elsif Background then
         Self.Outputs (Background_Output).Clear;
      else
         Self.Outputs (Normal_Output).Clear;
      end if;
   end Clear_Build_Output;

   ----------------------------
   -- Clear_All_Build_Output --
   ----------------------------

   function Clear_All_Build_Output
     (Self       : access Builder_Context_Record;
      Shadow     : Boolean;
      Background : Boolean) return Target_Outputs.Cursor
   is
      C : Target_Outputs.Cursor;
   begin
      if Shadow then
         C := Self.Outputs (Shadow_Output).First;
      elsif Background then
         C := Self.Outputs (Background_Output).First;
      else
         C := Self.Outputs (Normal_Output).First;
      end if;

      return C;
   end Clear_All_Build_Output;

   ---------------------
   -- Get_Last_Build --
   ---------------------

   function Get_Last_Build
     (Self : access Builder_Context_Record) return Build_Information is
   begin
      return Self.Build;
   end Get_Last_Build;

   ---------------------
   -- Set_Last_Build --
   ---------------------

   procedure Set_Last_Build
     (Self   : access Builder_Context_Record;
      Build  : Build_Information) is
   begin
      Self.Build := Build;
   end Set_Last_Build;

   -------------------------
   -- Expand_Command_Line --
   -------------------------

   function Expand_Command_Line
     (Build_Registry   : Build_Config_Registry_Access;
      Kernel           : not null access Core_Kernel_Record'Class;
      Proj_Type        : Project_Type;
      Toolchains       : Toolchain_Manager;
      Command_Line     : String;
      Target_Name      : String;
      Mode_Name        : String;
      Project_File     : Virtual_File;
      Force_File       : Virtual_File;
      Main_File        : Virtual_File;
      Simulate         : Boolean;
      Trusted_Mode     : Boolean;
      Multi_Language_Builder : Multi_Language_Builder_Policy;
      Execute_Command        : String)
     return Expansion_Result
   is
      Adapter   : Build_Command_Adapter_Access := new Build_Command_Adapter;
      T         : constant Target_Access :=
         Get_Target_From_Name (Build_Registry, Target_Name);
      CL_Args   : Argument_List_Access :=
         Argument_String_To_List (Command_Line);
      Mode_Args : constant Command_Lines.Command_Line :=
        T.Apply_Mode_Args (Mode_Name, CL_Args.all);
      Res       : Expansion_Result;
   begin
      Initialize (Adapter.all, Kernel, Proj_Type, Toolchains,
                  Force_File, '%', Trusted_Mode, Execute_Command,
                  Multi_Language_Builder);
      Adapter.Project_File := Project_File;
      Res := Expand_Command_Line
         (Abstract_Build_Command_Adapter_Access (Adapter), Mode_Args, T,
          Get_Server (Build_Registry, Mode_Name, T), Force_File,
          Main         => Main_File,
          Main_Project => Proj_Type,
          Subdir       => Get_Mode_Subdir (Build_Registry, Mode_Name),
          Background   => False,
          Simulate     => Simulate);
      Res.Status := Adapter.Status;
      Free (CL_Args);
      Free_Adapter (Adapter);
      return Res;
   end Expand_Command_Line;

   -----------------
   -- Get_Project --
   -----------------

   function Get_Project (P : Project_And_Main) return Project_Type is
   begin
      return GNATCOLL.Scripts.Projects.Project_Tree.Project_From_Path
        (P.Project_Path);
   end Get_Project;

end Build_Command_Utils;
