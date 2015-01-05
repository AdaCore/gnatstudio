------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2003-2015, AdaCore                     --
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

with Ada.Strings;                      use Ada.Strings;
with Ada.Unchecked_Deallocation;

with GNATCOLL.Arg_Lists;               use GNATCOLL.Arg_Lists;
with GNATCOLL.Projects;                use GNATCOLL.Projects;
with GNATCOLL.Utils;                   use GNATCOLL.Utils;

with GPS.Intl;                         use GPS.Intl;
with GPS.Tools_Output;                 use GPS.Tools_Output;
with Build_Configurations;             use Build_Configurations;
with Extending_Environments;           use Extending_Environments;

package body Commands.Builder is

   Shell_Env : constant String := Getenv ("SHELL").all;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Argument_List, Argument_List_Access);

   -------------------
   -- Launch_Target --
   -------------------

   procedure Launch_Target
     (Builder     : Builder_Context;
      Target_Name : String;
      Mode_Name   : String;
      Force_File  : Virtual_File;
      Extra_Args  : Argument_List_Access;
      Quiet       : Boolean;
      Synchronous : Boolean;
      Dialog      : Dialog_Mode;
      Main        : Virtual_File;
      Background  : Boolean;
      Directory   : Virtual_File := No_File;
      On_Exit     : Subprogram_Type := null)
   is
      T              : Target_Access;
      All_Extra_Args : Argument_List_Access;

      procedure Launch_For_Mode
        (T          : Target_Access;
         Mode       : String;
         Quiet      : Boolean;
         Shadow     : Boolean;
         Background : Boolean);
      --  Compute and launch the command, for the given mode

      ---------------------
      -- Launch_For_Mode --
      ---------------------

      procedure Launch_For_Mode
        (T          : Target_Access;
         Mode       : String;
         Quiet      : Boolean;
         Shadow     : Boolean;
         Background : Boolean)
      is
         Server         : Server_Type;
         Background_Env : Extending_Environment;
         Category_Name  : Unbounded_String;
         The_Exit       : Subprogram_Type;

      begin
         Server := Get_Server (Builder.Registry, Mode, T);

         --  Get the unexpanded command line from the target
         if Background then
            Background_Env := Create_Extending_Environment
              (Builder.Kernel,
               Force_File,

               --  We want to recompile the given file in all possible contexts
               --  to detect errors as early as possible. For instance, when
               --  using an aggregate project we want to compile the file in
               --  all the projects to which it belongs.
               --  The simplest is therefore to pass the root project.
               --  since using Info_Set (Force_File).First_Element.Project will
               --  only use one of the possible projects.

               Builder.Kernel.Registry.Tree.Root_Project);
         end if;

         --  For background compilation synthetic messages category name is
         --  used. For non-background compilation target's messages category is
         --  used when defined, otherwise Error_Category is used for backward
         --  compatibility and compatibility with codefix.

         if Background then
            Category_Name :=
              To_Unbounded_String (Builder.Current_Background_Build_Id);
            The_Exit := null;
         else
            Category_Name := Get_Messages_Category (T);

            if Category_Name = Null_Unbounded_String then
               Category_Name := To_Unbounded_String (Error_Category);
            end if;

            if Main /= No_File then
               Set_Last_Main (Builder, Target_Name, Main);
            end if;

            The_Exit := On_Exit;
         end if;

         --  Configure output parser fabrics
         Launch_Build_Command
           (Builder          => Builder,
            Build            => (Target     => T,
                                 Main       => Main,
                                 Force_File => Force_File,
                                 Env        => Background_Env,
                                 Category   => Category_Name,
                                 Mode       => To_Unbounded_String (Mode),
                                 Background => Background,
                                 Shadow     => Shadow,
                                 Quiet      => Quiet,
                                 Console    => null,
                                 Full       => (Dir    => Directory,
                                                others => <>),
                                 Extra_Args => All_Extra_Args,
                                 Dialog     => Dialog,
                                 Launch     => True,
                                 On_Exit    => The_Exit),
            Server           => Server,
            Synchronous      => Synchronous);
      end Launch_For_Mode;

   begin
      --  Get the target
      T := Get_Target_From_Name (Builder.Registry, Target_Name);

      if T = null then
         --  This should never happen
         Builder.Kernel.Messages_Window.Insert
           ((-"Build target not found in registry: ") & Target_Name);
         return;
      end if;

      --  Compute the extra args not null array pointer.
      if Extra_Args /= null then
         All_Extra_Args := new Argument_List'(Extra_Args.all);
      else
         All_Extra_Args := new Argument_List (1 .. 0);
      end if;

      if Mode_Name = "" then
         declare
            Modes : Argument_List := Get_List_Of_Modes
              (Builder.Kernel.Get_Build_Mode,
               Builder.Registry,
               Get_Model (T));
         begin
            for J in Modes'Range loop
               --  All modes after Modes'First are Shadow modes
               Launch_For_Mode
                 (T, Modes (J).all, Quiet, J > Modes'First,
                  Background);
            end loop;

            Free (Modes);
         end;
      else
         Launch_For_Mode
           (T, Mode_Name, Quiet, False, Background);
      end if;

      Unchecked_Free (All_Extra_Args);
   end Launch_Target;

   --------------------------
   -- Launch_Build_Command --
   --------------------------

   procedure Launch_Build_Command
     (Builder          : Builder_Context;
      Build            : Build_Information;
      Server           : Server_Type;
      Synchronous      : Boolean)
   is
      procedure Expand_Command_Line (Result : in out Build_Information);

      -------------------------
      -- Expand_Command_Line --
      -------------------------

      procedure Expand_Command_Line (Result : in out Build_Information) is
         Mode    : constant String := To_String (Result.Mode);
         CL      : constant Argument_List :=
           Get_Command_Line_Unexpanded (Builder.Registry, Result.Target);
         CL_Mode : Argument_List_Access :=
           Apply_Mode_Args (Builder.Registry, Get_Model (Result.Target),
                            Mode, CL);
         Subdir  : constant Filesystem_String :=
           Get_Mode_Subdir (Builder.Registry, Mode);
      begin
         Result.Full := Expand_Command_Line
           (Builder, CL_Mode.all & Result.Extra_Args.all, Result.Target,
            Server, Result.Force_File, Result.Main, Subdir, Result.Background,
            False);

         Free (CL_Mode);
      end Expand_Command_Line;

      Result          : Build_Information;
      CL              : Arg_List;
      Success         : Boolean := False;
      Cmd_Name        : Unbounded_String;
      Created_Command : Command_Access;
      Output_Parser   : Tools_Output_Parser_Access;
   begin
      --  Store last build information into Builder
      Builder.Set_Last_Build (Build);

      Output_Parser  := Builder.New_Parser_Chain (Get_Name (Build.Target));

      --  Retrive build information modified by parsers
      Result := Builder.Get_Last_Build;

      --  Do nothing if one of parsers requests canceling of Launch
      if not Result.Launch then
         return;
      end if;

      --  Normally command line expansion done during parser initialization,
      --  because it could include GUI interaction. This is "last chance"
      --  expand command line for CLI tool.
      if Result.Full.Args = Empty_Command_Line then
         Expand_Command_Line (Result);
      end if;

      if not Build.Quiet then
         Append_To_Build_Output
           (Builder,
            To_Display_String (Result.Full.Args), Get_Name (Build.Target),
            Build.Shadow, Build.Background);
      end if;

      Cmd_Name := To_Unbounded_String (Get_Name (Build.Target));

      if Build.Mode /= "default" then
         Cmd_Name := Cmd_Name & " (" & Build.Mode & ")";
      end if;

      --  If Use_Shell, and if the SHELL environment variable is defined,
      --  then call the command through $SHELL -c "command line".
      if Uses_Shell (Build.Target)
        and then Shell_Env /= ""
        and then Is_Local (Server)
      then
         Append_Argument (CL, Shell_Env, One_Arg);
         Append_Argument (CL, "-c", One_Arg);
         Append_Argument (CL, To_Display_String (Result.Full.Args), One_Arg);
      else
         CL := Result.Full.Args;
      end if;

      if Uses_Python (Build.Target) then
         --  Interpret as a Python script execution
         declare
            P : constant Scripting_Language :=
              Builder.Kernel.Scripts.Lookup_Scripting_Language ("Python");
            E : aliased Boolean;
            C : Unbounded_String;
         begin
            --  Construct the string to launch
            for J in 0 .. Args_Length (CL) loop
               Append (C, Unbounded_String'(Nth_Arg (CL, J)));
            end loop;

            --  Launch
            declare
               Output : constant String := GNATCOLL.Scripts.Execute_Command
                 (Script  => P,
                  Command => To_String (C),
                  Errors  => E'Access);
               pragma Unreferenced (Output);
            begin
               null;
            end;
         end;

      else
         --  Interpret as an executable run

         if Synchronous then
            Builder.Kernel.Process_Launcher.Launch_Process
              (CL              => CL,
               Server          => Server,
               Directory       => Result.Full.Dir,
               Output_Parser   => Output_Parser,
               Show_Command_To => Result.Console,
               Success         => Success);
         else
            Builder.Kernel.Process_Launcher.Launch_Process_In_Background
              (CL              => CL,
               Server          => Server,
               Directory       => Result.Full.Dir,
               Output_Parser   => Output_Parser,
               Show_Command_To => Result.Console,
               Success         => Success,
               Show_In_Task_Manager => not Build.Background,
               Name_In_Task_Manager => To_String (Cmd_Name),
               Block_Exit           => not (Build.Shadow
                 or else Build.Background
                 or else Build.Quiet),
               Created_Command      => Created_Command);

            --  ??? check value of Success

            if Success and then Build.Background then
               Background_Build_Started (Builder, Created_Command);
            end if;
         end if;
      end if;
   end Launch_Build_Command;

end Commands.Builder;
