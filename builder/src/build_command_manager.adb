-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                  Copyright (C) 2008-2010, AdaCore                 --
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
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Ada.Unchecked_Deallocation;
with Ada.Strings.Fixed;

with GNAT.Directory_Operations;

with GNATCOLL.Templates;          use GNATCOLL.Templates;
with GNATCOLL.Utils;              use GNATCOLL.Utils;

with Builder_Facility_Module;     use Builder_Facility_Module;
with Build_Configurations.Gtkada; use Build_Configurations.Gtkada;
with Commands.Builder;            use Commands.Builder;
with GPS.Kernel;                  use GPS.Kernel;
with GPS.Kernel.Console;          use GPS.Kernel.Console;
with GPS.Kernel.Contexts;         use GPS.Kernel.Contexts;
with GPS.Kernel.Macros;           use GPS.Kernel.Macros;
with GPS.Kernel.Messages;         use GPS.Kernel.Messages;
with GPS.Kernel.MDI;              use GPS.Kernel.MDI;
with GPS.Kernel.Preferences;      use GPS.Kernel.Preferences;
with GPS.Kernel.Project;          use GPS.Kernel.Project;
with GPS.Kernel.Hooks;            use GPS.Kernel.Hooks;
with GPS.Kernel.Standard_Hooks;   use GPS.Kernel.Standard_Hooks;
with GPS.Intl;                    use GPS.Intl;
with Projects;                    use Projects;
with Remote;                      use Remote;
with Extending_Environments;      use Extending_Environments;
with Toolchains;                  use Toolchains;
with Traces;                      use Traces;
with GNATCOLL.Any_Types;          use GNATCOLL.Any_Types;
with GNATCOLL.Arg_Lists;          use GNATCOLL.Arg_Lists;
with GNATCOLL.Projects;           use GNATCOLL.Projects;

package body Build_Command_Manager is

   Me : constant Debug_Handle := Create ("Build_Command_Manager");

   function Get_Last_Main_For_Background_Target
     (Kernel : GPS.Kernel.Kernel_Handle;
      Target : Target_Access) return String;
   --  Return the Main to use for building Target as a background build.
   --  This is either the last main that was used, if it exists, or the first
   --  main defined for this target, if it exists.
   --  The full path to the target is returned.
   --  If the target is not found, "" is returned.

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Argument_List, Argument_List_Access);

   Invalid_Argument : exception;
   --  Raised by Expand_Arg below

   type Expansion_Result is record
      Args : Arg_List;
      --  The list of arguments

      Dir  : Virtual_File := No_File;
      --  The directory in which to launch the compilation
   end record;

   function Expand_Command_Line
     (Kernel     : GPS.Kernel.Kernel_Handle;
      CL         : Argument_List;
      Target     : Target_Access;
      Server     : Server_Type;
      Force_File : Virtual_File;
      Main       : String;
      Subdir     : Filesystem_String;
      Background : Boolean;
      Simulate   : Boolean;
      Background_Env : Extending_Environment) return Expansion_Result;
   --  Expand all macros contained in CL using the GPS macro language.
   --  User must free the result.
   --  CL must contain at least one element.
   --  If Simulate is true, never fail on unknown parameters.

   function Expand_Arg
     (Kernel     : GPS.Kernel.Kernel_Handle;
      Context    : GPS.Kernel.Selection_Context;
      Target     : Target_Access;
      Arg        : String;
      Server     : Server_Type;
      Force_File : Virtual_File;
      Main       : String;
      Subdir     : Filesystem_String;
      Background : Boolean;
      Simulate   : Boolean;
      Background_Env : Extending_Environment) return Expansion_Result;
   --  Expand macros contained in Arg.
   --  Caller must free the result.
   --  Will raise Invalid_Argument if an invalid/non existent argument is
   --  found.
   --  If Simulate is true, Invalid_Argument will never be raised, and no
   --  expansion will be done.

   procedure Free (Ar : in out Argument_List);
   --  Free memory associated to Ar

   ----------
   -- Free --
   ----------

   procedure Free (Ar : in out Argument_List) is
   begin
      for A in Ar'Range loop
         Free (Ar (A));
      end loop;
   end Free;

   -----------------------------------------
   -- Get_Last_Main_For_Background_Target --
   -----------------------------------------

   function Get_Last_Main_For_Background_Target
     (Kernel : GPS.Kernel.Kernel_Handle;
      Target : Target_Access) return String
   is
      Last : constant String := Get_Last_Main (Get_Name (Target));
   begin
      if Last = "" then
         --  There is no last-launched main: compute the list of
         --  mains for this target

         declare
            Targets : constant Unbounded_String :=
              Get_Properties (Target).Target_Type;
            Data   : aliased String_Hooks_Args :=
              (Hooks_Data with
               Length => Length (Targets),
               Value  => To_String (Targets));
            Mains  : constant Any_Type :=
              Run_Hook_Until_Not_Empty
                (Kernel,
                 Compute_Build_Targets_Hook,
                 Data'Unchecked_Access);

         begin
            if Mains.Length = 0 then
               return "";
            end if;

            declare
               The_Main : constant String :=
                 Mains.List (1).Tuple (2).Str;
            begin
               Set_Last_Main (Get_Name (Target), The_Main);

               return The_Main;
            end;
         end;
      else
         return Last;
      end if;
   end Get_Last_Main_For_Background_Target;

   ----------------
   -- Expand_Arg --
   ----------------

   function Expand_Arg
     (Kernel     : GPS.Kernel.Kernel_Handle;
      Context    : GPS.Kernel.Selection_Context;
      Target     : Target_Access;
      Arg        : String;
      Server     : Server_Type;
      Force_File : Virtual_File;
      Main       : String;
      Subdir     : Filesystem_String;
      Background : Boolean;
      Simulate   : Boolean;
      Background_Env : Extending_Environment) return Expansion_Result
   is
      Result : Expansion_Result;

      function Substitution
        (Param  : String; Quoted : Boolean) return String;
      --  Wrapper around GPS.Kernel.Macros.Substitute

      function Get_Attr_Value (Arg : String; Skip : Natural) return String;
      --  return the name of the attribute contained in Arg

      function Get_Index (A, B : Natural) return Natural;
      --  Return A if A /= 0, B otherwise

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

         elsif Background
           and then not Simulate
           and then (Param = "pp" or else Param = "PP")
         then
            return +Get_Project (Background_Env).Full_Name.all;

         else
            declare
               Result : constant String := GPS.Kernel.Macros.Substitute
                 (Param, Context, Quoted, Done'Access, Server => Server);
            begin
               if Result = "" then
                  if Simulate then
                     return '%' & Param;
                  else
                     raise Invalid_Argument;
                  end if;

               else
                  return Result;
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
         return Get_Project (Get_Kernel (Context)).Attribute_Value
           (Build (Pkg, Attr), Default => Arg (K + 1 .. Arg'Last - 1));
      end Get_Attr_Value;

   begin
      --  ??? Special case for "%X"
      --  We are implementing a special case here since GPS.Kernel.Macros
      --  does not support returning an Argument_List.
      --  See H926-007.

      if Arg = "%X" then
         Result.Args := Parse_String
           (Scenario_Variables_Cmd_Line (Kernel, "-X"), Separate_Args);

      --  ??? Ditto for %vars
      elsif Arg = "%vars" then
         Result.Args := Parse_String
           (Scenario_Variables_Cmd_Line (Kernel, ""), Separate_Args);

      --  ??? Would be nice to support a generic %vars(xxx)
      elsif Arg = "%vars(-D)" then
         Result.Args := Parse_String
           (Scenario_Variables_Cmd_Line (Kernel, "-D"), Separate_Args);

      --  ??? Ditto for %eL
      elsif Arg = "%eL" then
         if GPS.Kernel.Preferences.Trusted_Mode.Get_Pref then
            return Result;
         else
            Result.Args := Create ("-eL");
         end if;

      --  ??? Ditto for %attr
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

      --  ??? Ditto for %switches
      elsif Starts_With (Arg, "%switches(") and then Arg (Arg'Last) = ')' then
         Result.Args := Create
            (Get_Project (Get_Kernel (Context)).Attribute_Value
              (Build ("IDE", "Default_Switches"), Default => "",
               Index => Arg (Arg'First + 10 .. Arg'Last - 1)));

      --  ??? Ditto for %builder, %gprbuild and %gprclean
      elsif Arg = "%builder"
        or else Arg = "%gprbuild"
        or else Arg = "%gprclean"
      then
         declare
            Builder  : constant Boolean := Arg /= "%gprclean";
            Prj      : constant Project_Type :=
                         Get_Project (Get_Kernel (Context));
            Tc       : constant Toolchains.Toolchain :=
                         Get_Toolchain
                           (Get_Kernel (Context).Get_Toolchains_Manager, Prj);
            Langs    : Argument_List := Prj.Languages (Recursive => True);

            Multi_Language_Build : Boolean := True;
            Res      : Expansion_Result;
         begin
            if Arg /= "%gprbuild"
              and then ((Langs'Length = 1
                         and then Langs (Langs'First).all = "ada")
                        or else Multi_Language_Builder.Get_Pref
                                 = GPS.Kernel.Preferences.Gnatmake)
            then
               --  Determine if the project has only Ada set, if so, set
               --  Multi_Language_Build to False.

               Multi_Language_Build := False;
            end if;

            Free (Langs);

            if Multi_Language_Build
              and then Multi_Language_Builder.Get_Pref = Gprbuild
            then
               if not Is_Native (Tc) then
                  if Builder then
                     Res.Args := Create ("gprbuild");
                     Append_Argument
                       (Res.Args,
                        "--target=" & Get_Name (Tc),
                        One_Arg);
                     return Res;
                  else
                     Res.Args := Create ("gprclean");
                     Append_Argument
                       (Res.Args,
                        "--target=" & Get_Name (Tc),
                        One_Arg);
                     return Res;
                  end if;

               elsif Builder then
                  Res.Args := Create ("gprbuild");
                  return Res;
               else
                  Res.Args := Create ("gprclean");
                  return Res;
               end if;

            elsif Builder then
               if Multi_Language_Build then
                  Res.Args := Create ("gprmake");
                  return Res;
               else
                  Res.Args := Create (Get_Exe (Get_Compiler (Tc, "Ada")));
                  return Res;
               end if;
            else
               Res.Args := Create
                 (Prj.Attribute_Value
                    (GNAT_Attribute, Default => "gnat"));
               Append_Argument (Res.Args, "clean", One_Arg);
               return Res;
            end if;
         end;

      elsif Arg = "%external" then
         Result.Args := Parse_String
           (GPS.Kernel.Preferences.Execute_Command.Get_Pref, Separate_Args);

      elsif Arg = "[exec_dir]" then
         declare
            Prj : constant Project_Type :=
              Get_Project (Get_Kernel (Context));
         begin
            Result.Dir := Executables_Directory (Prj);
         end;

      elsif Arg = "%fp" then
         if not Simulate
           and then Force_File /= No_File
         then
            --  We are launching a compile command involving Force_File:
            --  remove reference to File from the Locations View.
            --  See F830-003.
            Get_Messages_Container (Kernel).Remove_File
              (Error_Category, Force_File, Builder_Message_Flags);
            Result.Args := Create (+Base_Name (Force_File));
            return Result;
         end if;

         declare
            File : constant Virtual_File := File_Information (Context);
         begin
            if File = No_File then
               if Simulate then
                  Result.Args := Create ("<current-file>");
                  return Result;

               else
                  Console.Insert
                    (Kernel, -"No file selected", Mode => Console.Error);
                  raise Invalid_Argument;
               end if;

            elsif Get_Registry
              (Kernel).Tree.Info (File).Project = No_Project
            then
               if Simulate then
                  Result.Args := Create ("<current-file>");
                  return Result;

               else
                  Console.Insert
                    (Kernel, -"Could not determine the project for file: "
                     & Display_Full_Name (File),
                     Mode => Console.Error);

                  --  Do not normalize through VFS so as to preserve the state
                  --  of the file (since otherwise we would cache the
                  --  normalized value)
                  if File.Display_Full_Name /=
                    Normalize_Pathname
                      (File.Display_Full_Name, Resolve_Links => True)
                    and then GPS.Kernel.Preferences.Trusted_Mode.Get_Pref
                  then
                     Console.Insert
                       (Kernel, -("You should"
                        & " disable the preference Fast Project Loading for"
                        & " full support of symbolic links"));
                  end if;

                  raise Invalid_Argument;
               end if;

            else
               if Background then
                  Result.Args := Create
                    (+Base_Name (Get_File (Background_Env)));
               else
                  --  We are launching a compile command involving File:
                  --  remove reference to File from the Locations View.
                  --  See F830-003.
                  if not Simulate then
                     Get_Messages_Container (Kernel).Remove_File
                       (Error_Category, File, Builder_Message_Flags);
                  end if;

                  Result.Args := Create (+Base_Name (File));
               end if;
            end if;
         end;

      elsif Starts_With (Arg, "%TT") then
         if Main /= "" then
            Result.Args := Create (Main & Arg (Arg'First + 3 .. Arg'Last));
         else
            if Background then
               declare
                  M : constant String :=
                    Get_Last_Main_For_Background_Target (Kernel, Target);
               begin
                  if M = "" then
                     Console.Insert
                       (Kernel,
                        (-"Could not launch background build: no main(s)"
                         & " found for target ") & Get_Name (Target),
                        Mode => Console.Error);
                     raise Invalid_Argument;
                  else
                     Result.Args := Create
                       (M & Arg (Arg'First + 3 .. Arg'Last));
                  end if;
               end;
            else
               Console.Insert
                 (Kernel, -"Could not determine the target to build.",
                  Mode => Console.Error);
               raise Invalid_Argument;
            end if;
         end if;

      elsif Starts_With (Arg, "%T") then
         if Main /= "" then
            Result.Args := Create
              (GNAT.Directory_Operations.Base_Name (Main)
               & Arg (Arg'First + 2 .. Arg'Last));
         else
            if Background then
               declare
                  M : constant String :=
                    Get_Last_Main_For_Background_Target (Kernel, Target);
               begin
                  if M = "" then
                     Console.Insert
                       (Kernel,
                        (-"Could not launch background build: no main(s)"
                         & " found for target ") & Get_Name (Target),
                        Mode => Console.Error);
                     raise Invalid_Argument;
                  else
                     Result.Args := Create
                       (GNAT.Directory_Operations.Base_Name (M) &
                        Arg (Arg'First + 3 .. Arg'Last));
                  end if;
               end;
            else
               Console.Insert
                 (Kernel, -"Could not determine the target to build.",
                  Mode => Console.Error);
               raise Invalid_Argument;
            end if;
         end if;

      elsif Starts_With (Arg, "%E") then
         if Main /= "" then
            Result.Args := Create (Main);
         else
            Console.Insert
              (Kernel, -"Could not determine the executable name for main.",
               Mode => Console.Error);
            raise Invalid_Argument;
         end if;

      else
         Result.Args :=
           Create (GNATCOLL.Templates.Substitute
                   (Str       => Arg,
                    Delimiter => GPS.Kernel.Macros.Special_Character,
                    Callback  => Substitution'Unrestricted_Access));
      end if;

      return Result;
   end Expand_Arg;

   -------------------------
   -- Expand_Command_Line --
   -------------------------

   function Expand_Command_Line
     (Kernel     : GPS.Kernel.Kernel_Handle;
      CL         : Argument_List;
      Target     : Target_Access;
      Server     : Server_Type;
      Force_File : Virtual_File;
      Main       : String;
      Subdir     : Filesystem_String;
      Background : Boolean;
      Simulate   : Boolean;
      Background_Env : Extending_Environment) return Expansion_Result
   is
      Result  : Expansion_Result;
      Final   : Expansion_Result;
      Context : constant Selection_Context := Get_Current_Context (Kernel);

   begin
      for J in CL'Range loop
         if CL (J) = null then
            --  This should not happen
            Insert (Kernel, (-"Invalid command line"), Mode => Error);
            return (Empty_Command_Line, No_File);
         end if;

         Result := Expand_Arg
           (Kernel, Context, Target, CL (J).all, Server,
            Force_File, Main, Subdir, Background, Simulate, Background_Env);

         if Result.Dir /= No_File then
            Final.Dir := Result.Dir;
         end if;

         for J in 0 .. Args_Length (Result.Args) loop
            Append_Argument (Final.Args, Nth_Arg (Result.Args, J), One_Arg);
         end loop;
      end loop;

      return Final;

   exception
      when Invalid_Argument =>
         Insert
           (Kernel, (-"Invalid context, cannot build"),
            Mode => Console.Error);
         return (Empty_Command_Line, No_File);
   end Expand_Command_Line;

   -------------------
   -- Launch_Target --
   -------------------

   procedure Launch_Target
     (Kernel      : GPS.Kernel.Kernel_Handle;
      Registry    : Build_Config_Registry_Access;
      Target_Name : String;
      Mode_Name   : String;
      Force_File  : Virtual_File;
      Extra_Args  : Argument_List_Access;
      Quiet       : Boolean;
      Synchronous : Boolean;
      Dialog      : Dialog_Mode;
      Main        : String;
      Background  : Boolean;
      Directory   : Virtual_File := No_File)
   is
      Prj            : constant Project_Type := Get_Project (Kernel);
      Dir            : Virtual_File := No_File;
      T              : Target_Access;
      Full           : Expansion_Result;
      Command_Line   : Argument_List_Access;
      All_Extra_Args : Argument_List_Access;

      procedure Launch_For_Mode
        (Mode       : String;
         Quiet      : Boolean;
         Shadow     : Boolean;
         Background : Boolean);
      --  Compute and launch the command, for the given mode

      ---------------------
      -- Launch_For_Mode --
      ---------------------

      procedure Launch_For_Mode
        (Mode       : String;
         Quiet      : Boolean;
         Shadow     : Boolean;
         Background : Boolean)
      is

         Subdir : constant Filesystem_String := Get_Mode_Subdir (Mode);
         Server : Server_Type;
         Data   : Build_Callback_Data_Access;
         Background_Env : Extending_Environment;

         function Expand_Cmd_Line (CL : String) return String;
         --  Callback for Single_Target_Dialog

         ---------------------
         -- Expand_Cmd_Line --
         ---------------------

         function Expand_Cmd_Line (CL : String) return String is
            CL_Args   : Argument_List_Access := Argument_String_To_List (CL);
            Mode_Args : Argument_List_Access :=
                          Apply_Mode_Args (Get_Model (T), Mode, CL_Args.all);
            Res       : constant Expansion_Result :=
              Expand_Command_Line
                (Kernel,
                 Mode_Args.all & All_Extra_Args.all,
                 T,
                 Server,
                 Force_File, Main, Subdir, Shadow,
                 Simulate => True,
                 Background_Env => Background_Env);

         begin
            Free (CL_Args);
            Free (Mode_Args);
            return To_Display_String (Res.Args);
         end Expand_Cmd_Line;

      begin
         --  Compute the extra args, taking into account the mode and the
         --  extra args explicitely passed.

         if Extra_Args /= null then
            All_Extra_Args := new Argument_List'(Extra_Args.all);
         else
            All_Extra_Args := new Argument_List (1 .. 0);
         end if;

         if Is_Server_In_Mode (Mode) then
            Server := Get_Mode_Server (Mode);
         else
            Server := Get_Server (T);
         end if;

         if (not Shadow)
           and then (not Background)
           and then
            (Dialog = Force_Dialog
              or else (Dialog = Force_Dialog_Unless_Disabled_By_Target
                        and then Get_Properties (T).Launch_Mode
                        /= Manually_With_No_Dialog)
              or else (Dialog = Default
                        and then Get_Properties (T).Launch_Mode
                        = Manually_With_Dialog))
         then
            --  Use the single target dialog to get the unexpanded command line
            Single_Target_Dialog
              (Registry        => Registry,
               Parent          => Get_Main_Window (Kernel),
               Tooltips        => Get_Tooltips (Kernel),
               Target          => Target_Name,
               History         => Get_History (Kernel),
               Expand_Cmd_Line => Expand_Cmd_Line'Unrestricted_Access,
               Result          => Command_Line);

            if Command_Line = null then
               --  The dialog was cancelled: return
               Unchecked_Free (All_Extra_Args);
               return;
            end if;

            declare
               CL_Mode : Argument_List_Access :=
                           Apply_Mode_Args
                             (Get_Model (T), Mode, Command_Line.all);
            begin
               Full := Expand_Command_Line
                 (Kernel, CL_Mode.all & All_Extra_Args.all, T,
                  Server, Force_File, Main, Subdir, False, False,
                  Background_Env);
               Free (Command_Line);
               Free (CL_Mode);
            end;

         else
            --  Get the unexpanded command line from the target
            if Background then
               Background_Env :=
                 Create_Extending_Environment (Kernel, Force_File, Server);
            end if;

            declare
               CL      : constant Argument_List :=
                           Get_Command_Line_Unexpanded (Registry, T);
               CL_Mode : Argument_List_Access :=
                           Apply_Mode_Args (Get_Model (T), Mode, CL);
            begin
               --  Sanity check that the command line contains at least one
               --  item (the command itself). It can happen that this is not
               --  the case if the user has modified the command by hand.

               if CL_Mode'Length = 0 then
                  Insert
                    (Kernel,
                     -"Command line is empty for target: " & Target_Name,
                     Mode => Error);
                  Free (CL_Mode);
                  Unchecked_Free (All_Extra_Args);
                  return;
               end if;

               --  Expand the command line

               if All_Extra_Args = null then
                  Full := Expand_Command_Line
                    (Kernel, CL_Mode.all, T, Server, Force_File, Main,
                     Subdir, Background, False, Background_Env);
               else
                  Full := Expand_Command_Line
                    (Kernel, CL_Mode.all & All_Extra_Args.all, T,
                     Server, Force_File, Main, Subdir, Background, False,
                     Background_Env);
               end if;

               Free (CL_Mode);
            end;
         end if;

         --  Trace the command line, for debug purposes
         if Full = (Empty_Command_Line, No_File) then
            Trace (Me, "Macro expansion resulted in empty command line");
            Unchecked_Free (All_Extra_Args);
            return;
         end if;

         --  Launch the build command

         if Full.Dir /= No_File then
            Dir := Full.Dir;
         else
            if Directory /= No_File then
               Dir := Directory;
            else
               Dir := GNATCOLL.VFS.Dir (Project_Path (Prj));
            end if;
         end if;

         --  Apparently codefix depends on Error_Category to work properly,
         --  but we need to set the category properly, at least for CodePeer
         --  targets???

         Data := new Build_Callback_Data;
         Data.Target_Name := To_Unbounded_String (Target_Name);

         if Background then
            Data.Category_Name := To_Unbounded_String
              (Current_Background_Build_Id);
         else
            Data.Category_Name := To_Unbounded_String (Error_Category);

            if Main /= "" then
               Set_Last_Main (Target_Name, Main);
            end if;
         end if;

         Data.Mode_Name   := To_Unbounded_String (Mode);
         Data.Quiet := Quiet;
         Data.Shadow := Shadow;
         Data.Background := Background;
         Data.Background_Env := Background_Env;

         if Get_Category (T) = "CodePeer" then
            Data.Category_Name := To_Unbounded_String ("CodePeer");
            Launch_Build_Command
              (Kernel, Full.Args, Data, Server,
               Synchronous, Uses_Shell (T), "", Dir);
         else
            if Is_Run (T) then
               Launch_Build_Command
                 (Kernel, Full.Args, Data, Server,
                  Synchronous, Uses_Shell (T),
                  "Run: " & GNAT.Directory_Operations.Base_Name (Main), Dir);
            else
               Launch_Build_Command
                 (Kernel, Full.Args, Data, Server,
                  Synchronous, Uses_Shell (T), "", Dir);
            end if;
         end if;

         Unchecked_Free (All_Extra_Args);
      end Launch_For_Mode;

   begin
      --  If there is already a background build running, interrupt it
      --  and clean up before launching a new build.
      Interrupt_Background_Build;

      --  Get the target

      T := Get_Target_From_Name (Registry, Target_Name);

      if T = null then
         --  This should never happen
         Insert
           (Kernel,
            (-"Build target not found in registry: ") & Target_Name);
         return;
      end if;

      if Mode_Name = "" then
         declare
            Modes : Argument_List := Get_List_Of_Modes (Get_Model (T));
         begin
            for J in Modes'Range loop
               --  All modes after Modes'First are Shadow modes
               Launch_For_Mode
                 (Modes (J).all, Quiet, J > Modes'First,
                  Background);
            end loop;

            Free (Modes);
         end;
      else
         Launch_For_Mode (Mode_Name, Quiet, False, Background);
      end if;
   end Launch_Target;

   -------------
   -- Execute --
   -------------

   overriding
   function Execute
     (Command : access Build_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      --  ??? We should use the command context
      pragma Unreferenced (Context);
   begin
      Launch_Target
        (Kernel       => Command.Kernel,
         Registry     => Command.Registry,
         Target_Name  => To_String (Command.Target_Name),
         Mode_Name    => "",
         Force_File   => No_File,
         Extra_Args   => null,
         Quiet        => Command.Quiet,
         Dialog       => Command.Dialog,
         Synchronous  => False,
         Background   => False,
         Main         => To_String (Command.Main));
      return Success;
   end Execute;

   ------------
   -- Create --
   ------------

   procedure Create
     (Item        : out Build_Command_Access;
      Kernel      : GPS.Kernel.Kernel_Handle;
      Registry    : Build_Config_Registry_Access;
      Target_Name : String;
      Main        : String;
      Quiet       : Boolean;
      Dialog      : Dialog_Mode) is
   begin
      Item := new Build_Command;
      Item.Kernel := Kernel;
      Item.Registry := Registry;
      Item.Target_Name := To_Unbounded_String (Target_Name);
      Item.Main := To_Unbounded_String (Main);
      Item.Dialog := Dialog;
      Item.Quiet := Quiet;
   end Create;

   -------------
   -- Execute --
   -------------

   overriding
   function Execute
     (Command : access Build_Main_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Context);

      Target_Type : constant String := To_String (Command.Target_Type);
      Data        : aliased String_Hooks_Args :=
                      (Hooks_Data with
                       Length => Target_Type'Length,
                       Value  => Target_Type);
      Mains       : Any_Type := Run_Hook_Until_Not_Empty
        (Command.Kernel,
         Compute_Build_Targets_Hook,
         Data'Unchecked_Access);

   begin
      if Mains.T /= List_Type then
         Insert
           (Command.Kernel,
            (-"The command for determining the target type of target " &
             Target_Type & (-" returned a ") & Mains.T'Img
               & (-("but should return a LIST_TYPE "
               & " (containing a pair display_name/full_name)"))),
             Mode => Error);

         Free (Mains);
         return Failure;
      end if;

      if Command.Main not in Mains.List'Range then
         Insert (Command.Kernel,
                 (-"This project does not contain") & Command.Main'Img
                 & " " & Target_Type & (-" targets"), Mode => Error);
         Free (Mains);
         return Failure;
      end if;

      Launch_Target
        (Kernel      => Command.Kernel,
         Registry    => Command.Registry,
         Target_Name => To_String (Command.Target_Name),
         Mode_Name   => "",
         Force_File  => No_File,
         Extra_Args  => null,
         Quiet       => Command.Quiet,
         Dialog      => Command.Dialog,
         Synchronous => False,
         Background  => False,
         Main        => Mains.List (Command.Main).Tuple (2).Str);

      Free (Mains);
      return Success;
   end Execute;

   ------------
   -- Create --
   ------------

   procedure Create
     (Item        : out Build_Main_Command_Access;
      Kernel      : GPS.Kernel.Kernel_Handle;
      Registry    : Build_Config_Registry_Access;
      Target_Name : String;
      Target_Type : String;
      Main        : Natural;
      Quiet       : Boolean;
      Dialog      : Dialog_Mode)
   is
   begin
      Item := new Build_Main_Command;
      Item.Kernel := Kernel;
      Item.Registry := Registry;
      Set_Unbounded_String (Item.Target_Name, Target_Name);
      Set_Unbounded_String (Item.Target_Type, Target_Type);
      Item.Main := Main;
      Item.Dialog := Dialog;
      Item.Quiet := Quiet;
   end Create;

end Build_Command_Manager;
