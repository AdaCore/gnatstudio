------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2008-2012, AdaCore                     --
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

with Ada.Unchecked_Deallocation;

with GNATCOLL.Utils;              use GNATCOLL.Utils;

with Build_Command_Utils;           use Build_Command_Utils;
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
with Remote;                      use Remote;
with Extending_Environments;      use Extending_Environments;
with Traces;                      use Traces;
with GNATCOLL.Any_Types;          use GNATCOLL.Any_Types;
with GNATCOLL.Arg_Lists;          use GNATCOLL.Arg_Lists;
with GNATCOLL.Projects;           use GNATCOLL.Projects;

package body Build_Command_Manager is

   Me : constant Debug_Handle := Create ("Build_Command_Manager");

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Argument_List, Argument_List_Access);

   type Build_Command_Adapter is new Abstract_Build_Command_Adapter with record
      Kernel         : GPS.Kernel.Kernel_Handle;
      Context        : Selection_Context;
      Background_Env : Extending_Environment;
   end record;
   type Build_Command_Adapter_Access is access all Build_Command_Adapter;

   function Expand_Command_Line
     (Kernel     : GPS.Kernel.Kernel_Handle;
      CL         : Argument_List;
      Target     : Target_Access;
      Server     : Server_Type;
      Force_File : Virtual_File;
      Main       : Virtual_File;
      Subdir     : Filesystem_String;
      Background : Boolean;
      Simulate   : Boolean;
      Background_Env : Extending_Environment) return Expansion_Result;
   --  Expand all macros contained in CL using the GPS macro language.
   --  User must free the result.
   --  CL must contain at least one element.
   --  If Simulate is true, never fail on unknown parameters.

   overriding
   function Get_Last_Main_For_Background_Target
     (Adapter : Build_Command_Adapter;
      Target : Target_Access) return Virtual_File;
   --  Return the Main to use for building Target as a background build.
   --  This is either the last main that was used, if it exists, or the first
   --  main defined for this target, if it exists.
   --  The full path to the target is returned.
   --  If the target is not found, "" is returned.

   overriding
   function Get_Background_Project_Full_Name
     (Adapter : Build_Command_Adapter) return Filesystem_String;

   overriding
   function Substitute
     (Adapter : Build_Command_Adapter;
      Param     : String;
      Quoted    : Boolean;
      Done      : access Boolean;
      Server    : Server_Type := GPS_Server;
      For_Shell : Boolean := False) return String;
   --  Wrapper around GPS.Kernel.Macros.Substitute

   overriding
   procedure Console_Insert
     (Adapter : in out Build_Command_Adapter;
      Text   : String;
      Add_LF : Boolean := True;
      Mode   : Console_Message_Type := Info);
   --  Wrapper around GPS.Kernel.Console.Insert

   overriding
   procedure Remove_Error_Builder_Message_From_File
     (Adapter : Build_Command_Adapter;
      File     : Virtual_File);
   --  Removes all messages for specified file in the error category.
   --  Do nothing when there is no such category or file.

   overriding
   function Get_Background_Environment_File
     (Adapter : Build_Command_Adapter) return Virtual_File;

   overriding
   function Get_Scenario_Variables
     (Adapter : Build_Command_Adapter) return Scenario_Variable_Array;

   -----------------------------------------
   -- Get_Last_Main_For_Background_Target --
   -----------------------------------------

   overriding
   function Get_Last_Main_For_Background_Target
     (Adapter : Build_Command_Adapter;
      Target : Target_Access) return Virtual_File
   is
      Kernel : constant GPS.Kernel.Kernel_Handle := Adapter.Kernel;
      Last : constant Virtual_File := Get_Last_Main (Get_Name (Target));
   begin
      if Last = No_File then
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
               return No_File;
            end if;

            declare
               The_Main : constant Virtual_File :=
                            GNATCOLL.VFS.Create
                              (+Mains.List (1).Tuple (2).Str);
            begin
               Set_Last_Main (Get_Name (Target), The_Main);

               return The_Main;
            end;
         end;
      else
         return Last;
      end if;
   end Get_Last_Main_For_Background_Target;

   --------------------------------------
   -- Get_Background_Project_Full_Name --
   --------------------------------------

   overriding
   function Get_Background_Project_Full_Name
     (Adapter : Build_Command_Adapter) return Filesystem_String is
   begin
      return Get_Project (Adapter.Background_Env).Full_Name.all;
   end Get_Background_Project_Full_Name;

   ----------------------------
   -- Get_Scenario_Variables --
   ----------------------------

   overriding
   function Get_Scenario_Variables
     (Adapter : Build_Command_Adapter) return Scenario_Variable_Array is
   begin
      return Scenario_Variables (Adapter.Kernel);
   end Get_Scenario_Variables;

   ----------------
   -- Substitute --
   ----------------

   overriding
   function Substitute
     (Adapter : Build_Command_Adapter;
      Param     : String;
      Quoted    : Boolean;
      Done      : access Boolean;
      Server    : Server_Type := GPS_Server;
      For_Shell : Boolean := False) return String
   is
      pragma Unreferenced (For_Shell);
   begin
      return GPS.Kernel.Macros.Substitute
                 (Param, Adapter.Context, Quoted, Done, Server);
   end Substitute;

   --------------------
   -- Console_Insert --
   --------------------

   overriding
   procedure Console_Insert
     (Adapter : in out Build_Command_Adapter;
      Text   : String;
      Add_LF : Boolean := True;
      Mode   : Console_Message_Type := Info) is
      M : Message_Type;
   begin
      case Mode is
         when Info =>
            M := Info;
         when Error =>
            M := Error;
         when Verbose =>
            M := Verbose;
      end case;
      Console.Insert (Adapter.Kernel, Text, Add_LF, M);
   end Console_Insert;

   --------------------------------------------
   -- Remove_Error_Builder_Message_From_File --
   --------------------------------------------

   overriding
   procedure Remove_Error_Builder_Message_From_File
     (Adapter : Build_Command_Adapter;
      File     : Virtual_File) is
   begin
      Get_Messages_Container (Adapter.Kernel).Remove_File
                       (Error_Category, File, Builder_Message_Flags);
   end Remove_Error_Builder_Message_From_File;

   -------------------------------------
   -- Get_Background_Environment_File --
   -------------------------------------

   overriding
   function Get_Background_Environment_File
     (Adapter : Build_Command_Adapter) return Virtual_File is
   begin
      return Get_File (Adapter.Background_Env);
   end Get_Background_Environment_File;

   procedure Free_Adapter is new Ada.Unchecked_Deallocation
     (Build_Command_Adapter, Build_Command_Adapter_Access);

   -------------------------
   -- Expand_Command_Line --
   -------------------------

   function Expand_Command_Line
     (Kernel     : GPS.Kernel.Kernel_Handle;
      CL         : Argument_List;
      Target     : Target_Access;
      Server     : Server_Type;
      Force_File : Virtual_File;
      Main       : Virtual_File;
      Subdir     : Filesystem_String;
      Background : Boolean;
      Simulate   : Boolean;
      Background_Env : Extending_Environment) return Expansion_Result
   is
      Context : constant Selection_Context := Get_Current_Context (Kernel);
      Adapter : Build_Command_Adapter_Access := new Build_Command_Adapter;
      Res     : Expansion_Result;
   begin
      Adapter.Kernel := Kernel;
      Adapter.Background_Env := Background_Env;
      Adapter.Context := Context;

      Initialize
        (Adapter.all,
         Get_Registry (Kernel),
         Get_Project (Get_Kernel (Context)),
         Get_Kernel (Context).Get_Toolchains_Manager,
         File_Information (Context),
         GPS.Kernel.Macros.Special_Character,
         GPS.Kernel.Preferences.Trusted_Mode.Get_Pref,
         GPS.Kernel.Preferences.Execute_Command.Get_Pref,
         Multi_Language_Builder.Get_Pref);

      Res := Expand_Command_Line
        (Abstract_Build_Command_Adapter_Access (Adapter), CL, Target, Server,
         Force_File, Main, Subdir, Background, Simulate);
      Free_Adapter (Adapter);
      return Res;
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
      Main        : Virtual_File;
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

         Subdir : constant Filesystem_String :=
            Get_Mode_Subdir (Registry, Mode);
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
                          Apply_Mode_Args (Registry, Get_Model (T), Mode,
                                           CL_Args.all);
            Res       : constant Expansion_Result :=
                          Expand_Command_Line
                            (Kernel,
                             Mode_Args.all & All_Extra_Args.all,
                             T,
                             Server,
                             Force_File, Main, Subdir, Shadow,
                             Simulate       => True,
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

         Server := Get_Server (Registry, Mode, T);

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
                             (Registry, Get_Model (T), Mode, Command_Line.all);
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
                           Apply_Mode_Args (Registry, Get_Model (T), Mode, CL);
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
         if Full = (Empty_Command_Line, No_File, To_Unbounded_String ("")) then
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

         Data := new Build_Callback_Data;
         Data.Target_Name := To_Unbounded_String (Target_Name);

         --  For background compilation synthetic messages category name is
         --  used. For non-background compilation target's messages category is
         --  used when defined, otherwise Error_Category is used for backward
         --  compatibility and compatibility with codefix.

         if Background then
            Data.Category_Name :=
              To_Unbounded_String (Current_Background_Build_Id);

         else
            Data.Category_Name := Get_Messages_Category (T);

            if Data.Category_Name = Null_Unbounded_String then
               Data.Category_Name := To_Unbounded_String (Error_Category);
            end if;

            if Main /= No_File then
               Set_Last_Main (Target_Name, Main);
            end if;
         end if;

         Data.Mode_Name      := To_Unbounded_String (Mode);
         Data.Quiet          := Quiet;
         Data.Shadow         := Shadow;
         Data.Background     := Background;
         Data.Background_Env := Background_Env;

         if Is_Run (T) then
            Launch_Build_Command
              (Kernel, Full.Args, Data, Server,
               Synchronous, Uses_Shell (T),
               "Run: " & Main.Display_Base_Name, Dir);

         else
            Launch_Build_Command
              (Kernel, Full.Args, Data, Server,
               Synchronous, Uses_Shell (T), "", Dir);
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
         Main         => Command.Main);
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
      Main        : Virtual_File;
      Quiet       : Boolean;
      Dialog      : Dialog_Mode) is
   begin
      Item := new Build_Command;
      Item.Kernel := Kernel;
      Item.Registry := Registry;
      Item.Target_Name := To_Unbounded_String (Target_Name);
      Item.Main := Main;
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
         Main        => Create (+Mains.List (Command.Main).Tuple (2).Str));

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
