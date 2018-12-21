------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2008-2019, AdaCore                     --
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

with Commands.Builder;            use Commands.Builder;
with GPS.Kernel;                  use GPS.Kernel;
with GPS.Kernel.Contexts;         use GPS.Kernel.Contexts;
with GPS.Kernel.Macros;           use GPS.Kernel.Macros;
with GPS.Kernel.Messages;         use GPS.Kernel.Messages;
with GPS.Kernel.Preferences;      use GPS.Kernel.Preferences;
with GPS.Kernel.Project;          use GPS.Kernel.Project;
with GPS.Kernel.Hooks;            use GPS.Kernel.Hooks;
with GPS.Intl;                    use GPS.Intl;
with GNATCOLL.Any_Types;          use GNATCOLL.Any_Types;

with Gtk.Text_View;               use Gtk.Text_View;

with GPS.Kernel.Console;          use GPS.Kernel.Console;
with GPS.Kernel.Interactive;      use GPS.Kernel.Interactive;

package body Build_Command_Manager is

   type Build_Command_Adapter is new Abstract_Build_Command_Adapter with record
      Context        : Selection_Context;
      Background_Env : Extending_Environment;
      Builder        : Builder_Context;
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
   --  Wrapper around GPS.Kernel.Console.Insert

   overriding procedure Remove_Error_Builder_Message_From_File
     (Adapter : Build_Command_Adapter;
      File     : Virtual_File);
   --  Removes all messages for specified file in the error category.
   --  Do nothing when there is no such category or file.

   overriding function Get_Background_Environment_File
     (Adapter : Build_Command_Adapter) return Virtual_File;

   overriding function Get_Scenario_Variables
     (Adapter : Build_Command_Adapter) return Scenario_Variable_Array;

   -----------------------------------------
   -- Get_Last_Main_For_Background_Target --
   -----------------------------------------

   overriding function Get_Last_Main_For_Background_Target
     (Adapter : Build_Command_Adapter;
      Target : Target_Access) return Virtual_File
   is
      Kernel : constant GPS.Kernel.Kernel_Handle :=
        GPS.Kernel.Kernel_Handle (Adapter.Builder.Kernel);
      Last : constant Virtual_File := Get_Last_Main
        (Adapter.Builder, Get_Name (Target));
   begin
      if Last = No_File then
         --  There is no last-launched main: compute the list of
         --  mains for this target

         declare
            Targets : constant Unbounded_String :=
              Get_Properties (Target).Target_Type;
            Mains  : constant Any_Type := Compute_Build_Targets_Hook.Run
               (Kernel   => Kernel,
                Str      => To_String (Targets));
         begin
            if Mains.Length = 0 then
               return No_File;
            end if;

            declare
               The_Main : constant Virtual_File :=
                            GNATCOLL.VFS.Create
                              (+Mains.List (1).Tuple (2).Str);
            begin
               Set_Last_Main (Adapter.Builder, Get_Name (Target), The_Main);
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
      return Scenario_Variables (Kernel_Handle (Adapter.Builder.Kernel));
   end Get_Scenario_Variables;

   ----------------
   -- Substitute --
   ----------------

   overriding function Substitute
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

   overriding procedure Console_Insert
     (Adapter : in out Build_Command_Adapter;
      Text    : String;
      Add_LF  : Boolean := True;
      Mode    : Message_Type := Info) is
   begin
      Kernel_Handle (Adapter.Builder.Kernel).Insert (Text, Add_LF, Mode);
   end Console_Insert;

   --------------------------------------------
   -- Remove_Error_Builder_Message_From_File --
   --------------------------------------------

   overriding procedure Remove_Error_Builder_Message_From_File
     (Adapter : Build_Command_Adapter;
      File     : Virtual_File)
   is
      Build : Build_Information := Adapter.Builder.Get_Last_Build;
   begin
      --  Store info about compiling file in build information
      if Build.Force_File = No_File then
         Build.Force_File := File;
         Adapter.Builder.Set_Last_Build (Build);
      end if;

      Get_Messages_Container
        (Kernel_Handle (Adapter.Builder.Kernel)).Remove_File
          (Commands.Builder.Error_Category, File, Builder_Message_Flags);
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
     (Builder    : Builder_Context;
      CL         : Command_Line;
      Target     : Target_Access;
      Server     : Server_Type;
      Force_File : Virtual_File;
      Main       : Virtual_File;
      Main_Project : Project_Type;
      Subdir     : Filesystem_String;
      Background : Boolean;
      Simulate   : Boolean;
      Background_Env : Extending_Environment) return Expansion_Result
   is
      Kernel  : constant Kernel_Handle := Kernel_Handle (Builder.Kernel);
      Context : constant Selection_Context := Get_Current_Context (Kernel);
      Adapter : Build_Command_Adapter_Access := new Build_Command_Adapter;
      Res     : Expansion_Result;
   begin
      Adapter.Background_Env := Background_Env;
      Adapter.Context := Context;
      Adapter.Builder := Builder;

      Initialize
        (Adapter.all,
         Builder.Kernel,
         Get_Project (Kernel),
         Kernel.Get_Toolchains_Manager,
         File_Information (Context),
         GPS.Kernel.Macros.Special_Character,
         GPS.Kernel.Preferences.Trusted_Mode.Get_Pref,
         GPS.Kernel.Preferences.Execute_Command.Get_Pref,
         Multi_Language_Builder.Get_Pref);

      Res := Expand_Command_Line
        (Abstract_Build_Command_Adapter_Access (Adapter), CL, Target, Server,
         Force_File,
         Main         => Main,
         Main_Project => Main_Project,
         Subdir       => Subdir,
         Background   => Background,
         Simulate     => Simulate);
      Free_Adapter (Adapter);
      return Res;
   end Expand_Command_Line;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Build_Command;
      Context : Interactive_Command_Context) return Command_Return_Type is
   begin
      Launch_Target
        (Target_Name  => To_String (Command.Target_Name),
         Mode_Name    => "",
         Force_File   => No_File,
         Extra_Args   => null,
         Quiet        => Command.Quiet,
         Dialog       => Command.Dialog,
         Via_Menu     => Context.Via_Menu,
         Synchronous  => False,
         Background   => False,
         Main         => Command.Main,
         Main_Project => Command.Main_Project,
         Builder      => Command.Builder);
      return Success;
   end Execute;

   ------------
   -- Create --
   ------------

   procedure Create
     (Item        : out Build_Command_Access;
      Builder     : Builder_Context;
      Target_Name : String;
      Main        : Virtual_File;
      Main_Project : Project_Type;
      Quiet       : Boolean;
      Dialog      : Dialog_Mode) is
   begin
      Item := new Build_Command;
      Item.Builder := Builder;
      Item.Target_Name := To_Unbounded_String (Target_Name);
      Item.Main := Main;
      Item.Main_Project := Main_Project;
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
      Target_Type : constant String := To_String (Command.Target_Type);
      Kernel      : constant Kernel_Handle :=
        Kernel_Handle (Command.Builder.Kernel);
      Mains       : Any_Type := Compute_Build_Targets_Hook.Run
         (Kernel => Kernel,
          Str    => Target_Type);
   begin
      if Mains.T /= List_Type then
         Insert
           (Kernel,
            (-"The command for determining the target type of target " &
             Target_Type & (-" returned a ") & Mains.T'Img
               & (-("but should return a LIST_TYPE "
               & " (containing a pair display_name/full_name)"))),
             Mode => Error);

         Free (Mains);
         return Failure;
      end if;

      if Command.Main not in Mains.List'Range then
         Insert (Kernel_Handle (Command.Builder.Kernel),
                 (-"This project does not contain") & Command.Main'Img
                 & " " & Target_Type & (-" targets"), Mode => Error);
         Free (Mains);
         return Failure;
      end if;

      Launch_Target
        (Target_Name => To_String (Command.Target_Name),
         Mode_Name    => "",
         Force_File   => No_File,
         Extra_Args   => null,
         Quiet        => Command.Quiet,
         Dialog       => Command.Dialog,
         Via_Menu     => Context.Via_Menu,
         Synchronous  => False,
         Background   => False,
         Main         => Create (+Mains.List (Command.Main).Tuple (2).Str),
         Main_Project =>
           Kernel.Get_Project_Tree.Project_From_Name
             (Mains.List (Command.Main).Tuple (3).Str),
         Builder      => Command.Builder);

      Free (Mains);
      return Success;
   end Execute;

   ------------
   -- Create --
   ------------

   procedure Create
     (Item        : out Build_Main_Command_Access;
      Builder     : Builder_Context;
      Target_Name : String;
      Target_Type : String;
      Main        : Natural;
      Quiet       : Boolean;
      Dialog      : Dialog_Mode)
   is
   begin
      Item := new Build_Main_Command;
      Item.Builder := Builder;
      Set_Unbounded_String (Item.Target_Name, Target_Name);
      Set_Unbounded_String (Item.Target_Type, Target_Type);
      Item.Main := Main;
      Item.Dialog := Dialog;
      Item.Quiet := Quiet;
   end Create;

   -----------------------
   -- Get_Build_Console --
   -----------------------

   function Get_Build_Console
     (Kernel              : GPS.Kernel.Kernel_Handle;
      Shadow              : Boolean;
      Background          : Boolean;
      Create_If_Not_Exist : Boolean;
      New_Console_Name    : String := "";
      Toolbar_Name        : String := "") return Interactive_Console
   is
      Console : Interactive_Console;
   begin
      if New_Console_Name /= "" then
         Console := Create_Interactive_Console
           (Kernel              => Kernel,
            Title               => New_Console_Name,
            History             => "interactive",
            Create_If_Not_Exist => True,
            Module              => null,
            Force_Create        => False,
            ANSI_Support        => True,
            Accept_Input        => True,
            Toolbar_Name        => Toolbar_Name);

         Modify_Font (Get_View (Console), View_Fixed_Font.Get_Pref);

         return Console;
      end if;

      if Background then
         return Create_Interactive_Console
           (Kernel              => Kernel,
            Title               => -"Background Builds",
            History             => "interactive",
            Create_If_Not_Exist => Create_If_Not_Exist,
            Module              => null,
            Force_Create        => False,
            Accept_Input        => False);

      elsif Shadow then
         return Create_Interactive_Console
           (Kernel              => Kernel,
            Title               => -"Auxiliary Builds",
            History             => "interactive",
            Create_If_Not_Exist => Create_If_Not_Exist,
            Module              => null,
            Force_Create        => False,
            Accept_Input        => False);
      else
         return Get_Console
            (Kernel, Create_If_Not_Exist => Create_If_Not_Exist);
      end if;
   end Get_Build_Console;

end Build_Command_Manager;
